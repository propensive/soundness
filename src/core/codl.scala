package cellulose

import java.io.*
import gossamer.*
import eucalyptus.*
import rudiments.*
import quagmire.*

given Realm(t"codl")

enum Token:
  case Indent, Peer
  case Outdent(n: Int)
  case Tab(n: Int)
  case Item(text: Text, line: Int, col: Int, block: Boolean = false)
  case Comment(text: Text, line: Int, col: Int)
  case Blank

case class Proto(key: Maybe[Text] = Unset, children: List[Node] = Nil, meta: Maybe[Meta] = Unset,
                     schema: Schema = Schema.Free, params: Int = 0, multiline: Boolean = false):
  
  def commit(child: Proto): Proto throws CodlValidationError =
    // println(s"Adding $child to $this")
    copy(children = child.close :: children, params = params + 1)
  
  def setMeta(meta: Maybe[Meta]): Proto = copy(meta = meta)

  def close: Node throws CodlValidationError =
    // println(s"Closing $this")
    key.fm(Node(Unset, meta)):
      case key: Text =>
        val meta2 = meta.mm { m => m.copy(comments = m.comments.reverse) }
        val node = Node(Data(key, IArray.from(children.reverse), Layout(params, multiline), schema), meta2)
        
        schema.requiredKeys.foreach: key =>
          if !node.data.mm(_.has(key)).or(false) then Codl.fail(node.key, MissingKey(key))
          //then unsafely(throw new Exception(s"FAILED: ${this}"))

        node

object Codl:

  def fail(key: Maybe[Text], issue: CodlValidationError.Issue): Nothing throws CodlValidationError =
    throw CodlValidationError(key, issue)

  def parse(reader: Reader, schema: Schema = Schema.Free)(using Log)
           : Doc throws CodlParseError | CodlValidationError =
    val (margin, stream) = tokenize(reader)
    val baseSchema: Schema = schema
    @tailrec
    def recur(tokens: LazyList[Token], focus: Proto, peers: List[Node], stack: List[(Proto, List[Node])],
                  lines: Int)
             : Doc =
      // println(s"Got ${tokens.head}")
      // println(s"foucs = ${focus.children.toList.map(_.key)}")

      
      def schema: Schema = stack.headOption.fold(baseSchema)(_.head.schema.option.get)
      
      inline def go(tokens: LazyList[Token] = tokens.tail, focus: Proto = focus, peers: List[Node] = peers,
                        stack: List[(Proto, List[Node])] = stack, lines: Int = lines)
                   : Doc =
        recur(tokens, focus, peers, stack, lines)
      
      tokens match
        case head #:: tail => head match
          case Token.Peer => focus.key match
            case key: Text =>
              go(focus = Proto(), peers = focus.close :: peers)
          
            case Unset =>
              go(focus = Proto(Unset, Nil, focus.meta.or(if lines == 0 then Unset else Meta(lines))))
            
            case _ =>
              throw Mistake("Should never match")
            
          case Token.Indent =>
            go(focus = Proto(), peers = Nil, stack = (focus -> peers) :: stack)
          
          case Token.Outdent(n) => stack match
            case Nil =>
              go(LazyList())
            
            case (proto, rest) :: stack2 =>
              // println(s"Recovered $proto from the stack")
              // println(s"stack.h.h is ${stack.head.head}")
              val next = if n == 1 then Token.Peer else Token.Outdent(n - 1)
              val focus2 = proto.copy(children = focus.close :: peers ::: proto.children)

              // focus.schema.fm(Nil)(_.requiredKeys).foreach: key =>
              //   if !focus.has(key) then fail(focus.key, MissingKey(key))

              go(next #:: tail, focus = focus2, peers = rest, stack = stack2)
          
          case Token.Tab(n) =>
            go()
          
          case Token.Blank => focus.meta match
            case Unset            => go(lines = lines + 1)
            case Meta(l, _, _, _) => go(focus = Proto(), peers = focus.close :: peers, lines = lines + 1)
          
          case Token.Item(word, line, col, block) =>
            val meta2: Maybe[Meta] = focus.meta.or(if lines == 0 then Unset else Meta(blank = lines))
            
            focus.key match
              case Unset =>
                val fschema = if schema == Schema.Free then schema else schema(word).or:
                  fail(word, InvalidKey(word))

                if fschema.unique && peers.exists(_.data.mm(_.key) == word) then fail(word, DuplicateKey(word))
                
                go(focus = Proto(word, Nil, meta2, fschema), lines = 0)
              
              case key: Text => focus.schema match
                case field@Field(_, _)   => go(focus = focus.commit(Proto(word)), lines = 0)
                case Schema.Free         => go(focus = focus.commit(Proto(word)), lines = 0)
                
                case struct@Struct(_, _) => struct.param(focus.children.length) match
                  case Unset                => fail(word, SurplusParams(key))
                  
                  case entry: Schema.Entry  => // println(s"Set entry $entry")
                                               val peer = Proto(word, schema = entry.schema)
                                               go(focus = focus.commit(peer), lines = 0)
              
              case _ =>
                throw Mistake("Should never match")
          
          case Token.Comment(txt, _, _) => focus.key match
            case Unset => val meta = focus.meta.or(Meta())
                          go(focus = Proto(meta = meta.copy(blank = lines, comments = txt :: meta.comments)))
            
            case key: Text => go(focus = focus.setMeta(focus.meta.or(Meta()).copy(remark = txt, blank = lines)))
          
            case _ => throw Mistake("Should never match")


        case empty => stack match
          case Nil =>
            // focus.schema.fm(Nil)(_.requiredKeys).foreach: key =>
            //   if !focus.has(key) then fail(focus.key, MissingKey(key))
            
            Doc(IArray.from((focus.close :: peers).reverse), baseSchema, margin)
          
          case _ =>
            go(LazyList(Token.Outdent(stack.length + 1)))
    
    if stream.isEmpty then Doc()
    else recur(stream, Proto(), Nil, Nil, 0)

  def tokenize(in: Reader)(using Log): (Int, LazyList[Token]) throws CodlParseError =
    val reader: PositionReader = PositionReader(in)

    enum State:
      case Word, Hash, Comment, Indent, Space, Tab, Margin
    
    import State.*

    @tailrec
    def cue(): Character =
      val ch = reader.next()
      if ch.char == '\n' || ch.char == ' ' then cue() else ch
    
    val first: Character = cue()
    val margin: Int = first.column

    def istream(char: Character, state: State = Indent, indent: Int = margin): LazyList[Token] =
      stream(char, state, indent)
    
    @tailrec
    def stream(char: Character, state: State = Indent, indent: Int = margin): LazyList[Token] =
      inline def next(): Character = reader.next()
      inline def recur(state: State, indent: Int = indent): LazyList[Token] = stream(next(), state, indent)
      inline def irecur(state: State, indent: Int = indent): LazyList[Token] = istream(next(), state, indent)
      inline def diff: Int = char.column - indent

      def token(): Token = state match
        case Comment => Token.Comment(reader.get(), reader.start()(0), reader.start()(1))
        case Margin  => val text = reader.get()
                        val trimmed = if text.last == '\n' then text.drop(1, Rtl) else text
                        Token.Item(trimmed, reader.start()(0), reader.start()(1), true)
        case Word    => Token.Item(reader.get(), reader.start()(0), reader.start()(1), false)
        case _       => ???

      def put(next: State, stop: Boolean = false): LazyList[Token] = token() #:: irecur(next)
      
      inline def consume(next: State): LazyList[Token] =
        reader.put(char)
        recur(next)

      inline def block(): LazyList[Token] =
        if diff >= 4 then consume(Margin)
        else if char.char == ' ' then recur(Margin)
        else token() #:: istream(char)
        
      inline def newline(next: State): LazyList[Token] =
        if diff > 4 then throw CodlParseError(char.line, char.column, SurplusIndent)
        else if diff%2 != 0 then throw CodlParseError(char.line, char.column, UnevenIndent(margin, char.column))
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
