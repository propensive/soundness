package cellulose

import java.io.*
import gossamer.*
import eucalyptus.*
import rudiments.*
import contextual.*
import quagmire.*

given Realm(t"codl")

enum Token:
  case Indent, Peer, Blank, Argument
  case Line(text: Text)
  case Outdent(n: Int)
  case Tab(n: Int)
  case Item(text: Text, line: Int, col: Int, block: Boolean = false)
  case Comment(text: Text, line: Int, col: Int)
  case Error(error: CodlError)

object Codl:

  def read[T: Codec](text: Text)(using Log): T throws AggregateError | IncompatibleTypeError = // FIXME: Should only be aggregate error
    summon[Codec[T]].schema.parse(text).as[T]
  
  def parse(reader: Reader, schema: Schema = Schema.Free, subs: List[Data] = Nil, fromStart: Boolean = false)
           (using Log)
           : Doc throws AggregateError =
    val (margin, stream) = tokenize(reader, fromStart)
    val baseSchema: Schema = schema
    
    case class Proto(key: Maybe[Text] = Unset, line: Int = 0, col: Int = 0, children: List[Node] = Nil,
                        meta: Maybe[Meta] = Unset, schema: Schema = Schema.Free, params: Int = 0,
                        multiline: Boolean = false, ids: Map[Text, (Int, Int)] = Map()):
      
      def commit(child: Proto): (Proto, List[CodlError]) =
        // val ids2 = if child.schema.arity != Arity.Unique then ids else
        //   child.key.fm(ids): ckey =>
        //     ids.get(ckey).foreach: (line1, col1) =>
        //       Codl.fail(line, col, DuplicateId(ckey, line1, col1))
          
        //     ids.updated(ckey, (child.line, child.col))
        //val peerIds = children.map(_.id).sift[Text].join(t"{", t",", t"}")

        val (closed, errors2) = child.close
        (copy(children = closed :: children, params = params + 1), errors2)

      def substitute(data: Data): Proto = copy(children = Node(data) :: children, params = params + 1)
      def setMeta(meta: Maybe[Meta]): Proto = copy(meta = meta)

      def close: (Node, List[CodlError]) =
        key.fm((Node(Unset, meta), Nil)):
          case key: Text =>
            val meta2 = meta.mm { m => m.copy(comments = m.comments.reverse) }
            val data = Data(key, IArray.from(children.reverse), Layout(params, multiline), schema)
            val node = Node(data, meta2)
            
            val errors = schema.requiredKeys.to(List).flatMap: key =>
              if !node.data.mm(_.has(key)).or(false)
              then List(CodlError(line, col, node.key.or(t"?").length, MissingKey(node.key.or(t"?"), key)))
              else Nil

            (node, errors)

    @tailrec
    def recur(tokens: LazyList[Token], focus: Proto, peers: List[Node], stack: List[(Proto, List[Node])],
                  lines: Int, subs: List[Data], errors: List[CodlError], body: LazyList[Text])
             : Doc =
      
      def schema: Schema = stack.headOption.fold(baseSchema)(_.head.schema.option.get)
      
      inline def go(tokens: LazyList[Token] = tokens.tail, focus: Proto = focus, peers: List[Node] = peers,
                        stack: List[(Proto, List[Node])] = stack, lines: Int = lines, subs: List[Data] = subs,
                        errors: List[CodlError] = errors, body: LazyList[Text] = LazyList())
                   : Doc =
        recur(tokens, focus, peers, stack, lines, subs, errors, body)
      
      tokens match
        case LazyList() => stack match
          case Nil =>
            val (closed, errors2) = focus.close
            val allErrors = errors2 ::: errors
            val children = if closed.blank then peers.reverse else (closed :: peers).reverse
            if allErrors.isEmpty then Doc(IArray.from(children), baseSchema, margin, body)
            else throw AggregateError(allErrors.reverse)
          
          case _ =>
            go(LazyList(Token.Outdent(stack.length + 1)))
    
        case token #:: tail => token match
          case Token.Line(_) =>
            go(tokens = LazyList(), body = tokens.collect { case Token.Line(txt) => txt })
          case Token.Error(err) => go(errors = err :: errors)
          case Token.Peer => focus.key match
            case key: Text =>
              val (closed, errors2) = focus.close
              go(focus = Proto(), peers = closed :: peers, errors = errors2 ::: errors)
          
            case Unset =>
              go(focus = Proto(Unset, meta = focus.meta.or(if lines == 0 then Unset else Meta(lines))))
            
            case _ =>
              throw Mistake("Should never match")
            
          case Token.Indent =>
            val errors2 = if focus.key.unset then CodlError(focus.line, focus.col, 1, IndentAfterComment) :: errors else errors
            go(focus = Proto(), peers = Nil, stack = (focus -> peers) :: stack, errors = errors2)
          
          case Token.Outdent(n) => stack match
            case Nil =>
              go(LazyList())
            
            case (proto, rest) :: stack2 =>
              val next = if n == 1 then Token.Peer else Token.Outdent(n - 1)
              val (closed, errors2) = focus.close
              val focus2 = proto.copy(children = closed :: peers ::: proto.children)

              go(next #:: tail, focus = focus2, peers = rest, stack = stack2, errors = errors2 ::: errors)
          
          case Token.Tab(n) =>
            go()
          
          case Token.Blank => focus.meta match
            case Unset            => go(lines = lines + 1)
            case Meta(l, _, _, _) => val (closed, errors2) = focus.close
                                     go(focus = Proto(), peers = closed :: peers, lines = lines + 1, errors = errors2 ::: errors)
          
          case Token.Argument =>
            go(focus = focus.substitute(subs.head), subs = subs.tail)

          case Token.Item(word, line, col, block) =>
            val meta2: Maybe[Meta] = focus.meta.or(if lines == 0 then Unset else Meta(blank = lines))
            
            focus.key match
              case Unset =>
                val (fschema: Schema, errors2: List[CodlError]) =
                  if schema == Schema.Free then (schema, errors)
                  else schema(word).mm((_, errors)).or:
                    (Schema.Free, List(CodlError(line, col, word.length, InvalidKey(word, word))))

                val errors3 =
                  if fschema.unique && peers.exists(_.data.mm(_.key) == word)
                  then CodlError(line, col, word.length, DuplicateKey(word, word)) :: errors2
                  else errors2
                
                go(focus = Proto(word, line, col, meta = meta2, schema = fschema, ids = focus.ids), lines = 0,
                    errors = errors3)
              
              case key: Text => focus.schema match
                case field@Field(_, _)            => val (focus2, errors2) = focus.commit(Proto(word, line, col))
                                                     go(focus = focus2, lines = 0, errors = errors2 ::: errors)
                
                case Schema.Free                  => val (focus2, errors2) = focus.commit(Proto(word, line, col))
                                                     go(focus = focus2, lines = 0, errors = errors2 ::: errors)
                
                case struct@Struct(_, _)          => struct.param(focus.children.length) match
                  case Unset                         => val error = CodlError(line, col, word.length, SurplusParams(word, key))
                                                        go(errors = error :: errors)
                  
                  case entry: Schema.Entry        => val peer = Proto(word, line, col, schema = entry.schema)
                                                     val (focus2, errors2) = focus.commit(peer)
                                                     go(focus = focus2, lines = 0, errors = errors2 ::: errors)
              
              case _ =>
                throw Mistake("Should never match")
          
          case Token.Comment(txt, line, col) => focus.key match
            case Unset => val meta = focus.meta.or(Meta())
                          go(focus = Proto(line = line, col = col, meta = meta.copy(blank = lines, comments = txt :: meta.comments)))
            
            case key: Text => go(focus = focus.setMeta(focus.meta.or(Meta()).copy(remark = txt, blank = lines)))
          
            case _ => throw Mistake("Should never match")


    if stream.isEmpty then Doc() else recur(stream, Proto(), Nil, Nil, 0, subs.reverse, Nil, LazyList())

  def tokenize(in: Reader, fromStart: Boolean = false)(using Log): (Int, LazyList[Token]) =
    val reader: PositionReader = PositionReader(in)

    enum State:
      case Word, Hash, Comment, Indent, Space, Tab, Margin, Line
    
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
      inline def next(): Character =
        try reader.next() catch case err: CodlError => Character('\n', err.line, err.col)
      
      inline def recur(state: State, indent: Int = indent, count: Int = count + 1): LazyList[Token] =
        stream(next(), state, indent, count)
      
      inline def line(): LazyList[Token] =
        reader.get()
        val char = next()
        if char.char != '\n' && char != Character.End then ???
        else if char == Character.End then
          LazyList()
        else stream(next(), Line, indent, count + 1)
      
      inline def irecur(state: State, indent: Int = indent, count: Int = count + 1): LazyList[Token] =
        istream(next(), state, indent, count)
      
      inline def diff: Int = char.column - indent
      inline def col(char: Character): Int = if fromStart then count else char.column
      
      def put(next: State, stop: Boolean = false): LazyList[Token] = token() #:: irecur(next)

      def token(): Token = state match
        case Comment => Token.Comment(reader.get(), reader.start()(0), reader.start()(1) - 1)
        case Margin  => val text = reader.get()
                        val trimmed = if text.last == '\n' then text.drop(1, Rtl) else text
                        Token.Item(trimmed, reader.start()(0), reader.start()(1), true)
        case Word    => val word = reader.get()
                        if word == t"\u0000" then Token.Argument
                        else Token.Item(word, reader.start()(0), reader.start()(1), false)
        case _       => ???

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
        if diff > 4 then fail(Margin, CodlError(char.line, col(char), 1, SurplusIndent), indent)
        else if char.column < margin then fail(Indent, CodlError(char.line, col(char), 1, InsufficientIndent), margin)
        else if diff%2 != 0 then fail(Indent, CodlError(char.line, col(char), 1, UnevenIndent(margin, char.column)), char.column + 1)
        else diff match
          case 2 => Token.Indent #:: irecur(next, indent = char.column)
          case 0 => Token.Peer #:: irecur(next, indent = char.column)
          case n => Token.Outdent(-diff/2) #:: irecur(next, indent = char.column)

      char.char match
        case _ if char == Character.End => state match
          case Indent | Space | Tab | Hash => LazyList()
          case Comment | Word | Margin     => LazyList(token())
          case Line                        => LazyList(Token.Line(reader.get()))
        
        case '\n' => state match
          case Word | Comment       => put(Indent)
          case Margin               => block()
          case Indent | Space | Tab => Token.Blank #:: irecur(Indent)
          case Line                 => Token.Line(reader.get()) #:: irecur(Line)
          case _                    => recur(Indent)
        
        case ' ' => state match
          case Space | Tab    => recur(Tab)
          case Indent         => recur(Indent)
          case Word           => put(Space)
          case Comment | Line => consume(state)
          case Margin         => block()
          case Hash           => reader.get(); recur(Comment)
        
        case '#' => state match
          case Space | Tab    => consume(Hash)
          case Line           => consume(Line)
          case Comment        => line()
          case Word           => consume(Word)
          case Indent         => if diff == 4 then recur(Margin) else newline(Comment)
          case Margin         => block()
          case Hash           => consume(Word)
        
        case ch => state match
          case Space | Tab | Word => consume(Word)
          case Line | Comment     => consume(state)
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
        case err: AggregateError => err.errors.head match
          case CodlError(_, off, _, issue) => throw InterpolationError(t"read error: $issue", off)
    
    def initial: State = State(Nil, Nil)
    def skip(state: State): State = insert(state, List(Data(t"_")))
    def insert(state: State, data: List[Data]): State = state.copy(subs = data.reverse ::: state.subs)
    def parse(state: State, next: Text): State = state.copy(parts = next :: state.parts).tap(complete(_))