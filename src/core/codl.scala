package cellulose

import java.io.*
import gossamer.*
import eucalyptus.*
import rudiments.*
import contextual.*
import quagmire.*

given Realm(t"codl")

enum Token:
  case Indent, Peer
  case Outdent(n: Int)
  case Argument
  case Tab(n: Int)
  case Item(text: Text, line: Int, col: Int, block: Boolean = false)
  case Comment(text: Text, line: Int, col: Int)
  case Blank
  case Error(error: CodlError)

case class Proto(key: Maybe[Text] = Unset, line: Int = 0, col: Int = 0, children: List[Node] = Nil,
                     meta: Maybe[Meta] = Unset, schema: Schema = Schema.Free, params: Int = 0,
                     multiline: Boolean = false, ids: Map[Text, (Int, Int)] = Map()):
  
  def commit(child: Proto): Proto throws CodlError =
    val ids2 = if child.schema.arity != Arity.Unique then ids else
      child.key.fm(ids): ckey =>
        ids.get(ckey).foreach: (line1, col1) =>
          Codl.fail(line, col, DuplicateId(ckey, line1, col1))
      
        ids.updated(ckey, (child.line, child.col))
    val peerIds = children.map(_.id).sift[Text].join(t"{", t",", t"}")
    copy(children = child.close :: children, params = params + 1, ids = ids2)

  def substitute(data: Data): Proto throws CodlError =
    copy(children = Node(data) :: children, params = params + 1)

  def setMeta(meta: Maybe[Meta]): Proto = copy(meta = meta)

  def close: Node throws CodlError =
    key.fm(Node(Unset, meta)):
      case key: Text =>
        val meta2 = meta.mm { m => m.copy(comments = m.comments.reverse) }
        val node = Node(Data(key, IArray.from(children.reverse), Layout(params, multiline), schema), meta2)
        
        schema.requiredKeys.foreach: key =>
          if !node.data.mm(_.has(key)).or(false) then Codl.fail(line, col, MissingKey(node.key.or(t"?"), key))

        node

object Codl:

  def fail(line: Int, col: Int, issue: CodlError.Issue): Nothing throws CodlError =
    throw CodlError(line, col, issue)

  def parse(reader: Reader, schema: Schema = Schema.Free, subs: List[Data] = Nil, fromStart: Boolean = false)
           (using Log)
           : Doc throws CodlError =
    val (margin, stream) = tokenize(reader, fromStart)
    val baseSchema: Schema = schema
    @tailrec
    def recur(tokens: LazyList[Token], focus: Proto, peers: List[Node], stack: List[(Proto, List[Node])],
                  lines: Int, subs: List[Data])
             : Doc =
      
      def schema: Schema = stack.headOption.fold(baseSchema)(_.head.schema.option.get)
      
      inline def go(tokens: LazyList[Token] = tokens.tail, focus: Proto = focus, peers: List[Node] = peers,
                        stack: List[(Proto, List[Node])] = stack, lines: Int = lines, subs: List[Data] = subs)
                   : Doc =
        recur(tokens, focus, peers, stack, lines, subs)
      
      tokens match
        case token #:: tail => token match
          case Token.Error(err) => throw err
          case Token.Peer => focus.key match
            case key: Text =>
              go(focus = Proto(), peers = focus.close :: peers)
          
            case Unset =>
              go(focus = Proto(Unset, meta = focus.meta.or(if lines == 0 then Unset else Meta(lines))))
            
            case _ =>
              throw Mistake("Should never match")
            
          case Token.Indent =>
            if focus.key.unset then fail(focus.line, focus.col, IndentAfterComment)
            go(focus = Proto(), peers = Nil, stack = (focus -> peers) :: stack)
          
          case Token.Outdent(n) => stack match
            case Nil =>
              go(LazyList())
            
            case (proto, rest) :: stack2 =>
              val next = if n == 1 then Token.Peer else Token.Outdent(n - 1)
              val focus2 = proto.copy(children = focus.close :: peers ::: proto.children)

              go(next #:: tail, focus = focus2, peers = rest, stack = stack2)
          
          case Token.Tab(n) =>
            go()
          
          case Token.Blank => focus.meta match
            case Unset            => go(lines = lines + 1)
            case Meta(l, _, _, _) => go(focus = Proto(), peers = focus.close :: peers, lines = lines + 1)
          
          case Token.Argument =>
            go(focus = focus.substitute(subs.head), subs = subs.tail)

          case Token.Item(word, line, col, block) =>
            val meta2: Maybe[Meta] = focus.meta.or(if lines == 0 then Unset else Meta(blank = lines))
            
            focus.key match
              case Unset =>
                val fschema = if schema == Schema.Free then schema else schema(word).or:
                  fail(line, col, InvalidKey(word, word))

                if fschema.unique && peers.exists(_.data.mm(_.key) == word) then fail(line, col, DuplicateKey(word, word))
                
                go(focus = Proto(word, line, col, meta = meta2, schema = fschema, ids = focus.ids), lines = 0)
              
              case key: Text => focus.schema match
                case field@Field(_, _)            => go(focus = focus.commit(Proto(word, line, col)), lines = 0)
                case Schema.Free                  => go(focus = focus.commit(Proto(word, line, col)), lines = 0)
                
                case struct@Struct(_, _)          => struct.param(focus.children.length) match
                  case Unset                         => fail(line, col, SurplusParams(word, key))
                  
                  case entry: Schema.Entry        => val peer = Proto(word, line, col, schema = entry.schema)
                                                     go(focus = focus.commit(peer), lines = 0)
              
              case _ =>
                throw Mistake("Should never match")
          
          case Token.Comment(txt, line, col) => focus.key match
            case Unset => val meta = focus.meta.or(Meta())
                          go(focus = Proto(line = line, col = col, meta = meta.copy(blank = lines, comments = txt :: meta.comments)))
            
            case key: Text => go(focus = focus.setMeta(focus.meta.or(Meta()).copy(remark = txt, blank = lines)))
          
            case _ => throw Mistake("Should never match")


        case empty => stack match
          case Nil =>
            Doc(IArray.from((focus.close :: peers).reverse), baseSchema, margin)
          
          case _ =>
            go(LazyList(Token.Outdent(stack.length + 1)))
    
    if stream.isEmpty then Doc() else recur(stream, Proto(), Nil, Nil, 0, subs.reverse)

  def tokenize(in: Reader, fromStart: Boolean = false)(using Log): (Int, LazyList[Token]) =
    val reader: PositionReader = PositionReader(in)

    enum State:
      case Word, Hash, Comment, Indent, Space, Tab, Margin
    
    import State.*

    @tailrec
    def cue(count: Int = 0): (Character, Int) throws CodlError =
      val ch = reader.next()
      if ch.char == '\n' || ch.char == ' ' then cue(count + 1) else (ch, count)
    
    val (first: Character, start: Int) = try cue(0) catch case err: CodlError => ???
    val margin: Int = first.column

    def istream(char: Character, state: State = Indent, indent: Int = margin, count: Int): LazyList[Token] =
      stream(char, state, indent, count)
    
    @tailrec
    def stream(char: Character, state: State = Indent, indent: Int = margin, count: Int = start)
              : LazyList[Token] =
      inline def next(): Character throws CodlError = reader.next()
      
      inline def recur(state: State, indent: Int = indent, count: Int = count + 1): LazyList[Token] =
        val char = try next() catch case err: CodlError => Character('\n', err.line, err.col)
        stream(char, state, indent, count)
      
      inline def irecur(state: State, indent: Int = indent, count: Int = count + 1): LazyList[Token] =
        val char = try next() catch case err: CodlError => Character('\n', err.line, err.col)
        istream(char, state, indent, count)
      
      inline def diff: Int = char.column - indent

      def token(): Token = state match
        case Comment => Token.Comment(reader.get(), reader.start()(0), reader.start()(1) - 1)
        case Margin  => val text = reader.get()
                        val trimmed = if text.last == '\n' then text.drop(1, Rtl) else text
                        Token.Item(trimmed, reader.start()(0), reader.start()(1), true)
        case Word    => val word = reader.get()
                        if word == t"\u0000" then Token.Argument
                        else Token.Item(word, reader.start()(0), reader.start()(1), false)
        case _       => ???

      def put(next: State, stop: Boolean = false): LazyList[Token] = token() #:: irecur(next)
      
      inline def col(char: Character): Int = if fromStart then count else char.column
      inline def consume(next: State): LazyList[Token] =
        reader.put(char)
        recur(next)

      inline def block(): LazyList[Token] =
        if diff >= 4 then consume(Margin)
        else if char.char == ' ' then recur(Margin)
        else token() #:: istream(char, count = count + 1)

      inline def fail(next: State, error: CodlError, adjust: Maybe[Int] = Unset): LazyList[Token] =
        Token.Error(error) #:: irecur(next, indent = adjust.or(char.column))

      inline def newline(next: State): LazyList[Token] =
        if diff > 4 then fail(Margin, CodlError(char.line, col(char), SurplusIndent), indent)
        else if char.column < margin then fail(Indent, CodlError(char.line, col(char), InsufficientIndent), margin)
        else if diff%2 != 0 then fail(Indent, CodlError(char.line, col(char), UnevenIndent(margin, char.column)), char.column + 1)
        else diff match
          case 2 => Token.Indent #:: irecur(next, indent = char.column)
          case 0 => Token.Peer #:: irecur(next, indent = char.column)
          case n => Token.Outdent(-diff/2) #:: irecur(next, indent = char.column)

      char.char match
        case _ if char == Character.End => state match
          case Indent | Space | Tab | Hash => LazyList()
          case Comment | Word | Margin     => LazyList(token())
        
        case '\n' => state match
          case Word | Comment       => put(Indent)
          case Margin               => block()
          case Indent | Space | Tab => Token.Blank #:: irecur(Indent)
          case _                    => recur(Indent)
        
        case ' ' => state match
          case Space | Tab => recur(Tab)
          case Indent      => recur(Indent)
          case Word        => put(Space)
          case Comment     => consume(Comment)
          case Margin      => block()
          case Hash        => reader.get(); recur(Comment)
        
        case '#' => state match
          case Space | Tab    => consume(Hash)
          case Word | Comment => consume(state)
          case Indent         => if diff == 4 then recur(Margin) else newline(Comment)
          case Margin         => block()
          case Hash           => consume(Word)
        
        case ch => state match
          case Space | Tab | Word => consume(Word)
          case Comment            => consume(Comment)
          case Indent             => reader.put(char); if diff == 4 then recur(Margin) else newline(Word)
          case Margin             => block()
          
          case Hash => char.char match
            case '!' if first.line == 0 && first.column == 1 => consume(Comment)
            case ch                                          => consume(Word)

    (first.column, stream(first).drop(1))

  case class State(parts: List[Text], subs: List[Data]):
    def content: Text = parts.reverse.join(t"\u0000")

  object Prefix extends Interpolator[List[Data], State, Doc]:
    protected def complete(state: State): Doc =
      try Codl.parse(StringReader(state.content.s), Schema.Free, state.subs.reverse, fromStart = true)(using
          logging.silent(using parasitism.threading.platform))
      catch
        case err: CodlError => err match
          case CodlError(_, off, issue) => throw InterpolationError(t"read error: $issue", off)
    
    def initial: State = State(Nil, Nil)
    def skip(state: State): State = insert(state, List(Data(t"_")))
    def insert(state: State, data: List[Data]): State = state.copy(subs = data.reverse ::: state.subs)
    def parse(state: State, next: Text): State = state.copy(parts = next :: state.parts).tap(complete(_))