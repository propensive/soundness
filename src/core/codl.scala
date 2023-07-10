/*
    Cellulose, version [unreleased]. Copyright 2023 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package cellulose

import gossamer.*
import eucalyptus.*
import rudiments.*
import spectacular.*, booleanStyles.trueFalse
import digression.*
import contextual.*
import turbulence.*
import chiaroscuro.*

import language.experimental.captureChecking

given Realm = Realm(t"codl")

enum CodlToken:
  case Indent, Peer, Blank, Argument
  case Line(text: Text)
  case Outdent(n: Int)
  case Tab(n: Int)
  case Item(text: Text, line: Int, col: Int, block: Boolean = false)
  case Comment(text: Text, line: Int, col: Int)
  case Error(error: CodlError)

object CodlToken:
  //given Similar[CodlToken] = _.productPrefix == _.productPrefix

  given Debug[CodlToken] =
    case Indent                       => t"Indent"
    case Peer                         => t"Peer"
    case Blank                        => t"Blank"
    case Argument                     => t"Argument"
    case Line(text)                   => t"Line($text)"
    case Outdent(n)                   => t"Outdent($n)"
    case Tab(n)                       => t"Tab($n)"
    case Item(text, line, col, block) => t"Item($text, $line, $col, $block)"
    case Comment(text, line, col)     => t"Comment($text, $line, $col)"
    case Error(error)                 => t"Error(${error.message})"
  
object Codl:
  def read
      [ValueType: Codec]
      (source: Any)
      (using readable: Readable[source.type, Text], aggregate: CanThrow[AggregateError[CodlError]],
          codlRead: CanThrow[CodlReadError])
      : ValueType^{readable, aggregate} =
    summon[Codec[ValueType]].schema.parse(readable.read(source)).as[ValueType]
  
  def parse
      [SourceType]
      (source: SourceType, schema: CodlSchema = CodlSchema.Free, subs: List[Data] = Nil,
          fromStart: Boolean = false)
      (using readable: Readable[SourceType, Text], aggregate: CanThrow[AggregateError[CodlError]])
      : CodlDoc^{readable, aggregate} =
    val (margin, stream) = tokenize(readable.read(source), fromStart)
    val baseSchema: CodlSchema = schema
    
    case class Proto(key: Maybe[Text] = Unset, line: Int = 0, col: Int = 0, children: List[CodlNode] = Nil,
                        meta: Maybe[Meta] = Unset, schema: CodlSchema = CodlSchema.Free, params: Int = 0,
                        multiline: Boolean = false, ids: Map[Text, (Int, Int)] = Map()):
      
      def commit(child: Proto): (Proto, List[CodlError]) =
        val (closed, errors2) = child.close
        (copy(children = closed :: children, params = params + 1), errors2)

      def substitute(data: Data): Proto = copy(children = CodlNode(data) :: children, params = params + 1)
      def setMeta(meta: Maybe[Meta]): Proto = copy(meta = meta)

      def close: (CodlNode, List[CodlError]) =
        key.fm((CodlNode(Unset, meta), Nil)):
          case key: Text =>
            val meta2 = meta.mm { m => m.copy(comments = m.comments.reverse) }
            val data = Data(key, IArray.from(children.reverse), Layout(params, multiline), schema)
            val node = CodlNode(data, meta2)
            
            val errors = schema.requiredKeys.to(List).flatMap: key =>
              if !node.data.mm(_.has(key)).or(false)
              then List(CodlError(line, col, node.key.or(t"?").length, MissingKey(node.key.or(t"?"), key)))
              else Nil

            (node, errors)

    @tailrec
    def recur(tokens: LazyList[CodlToken], focus: Proto, peers: List[CodlNode], stack: List[(Proto, List[CodlNode])],
                  lines: Int, subs: List[Data], errors: List[CodlError], body: LazyList[Text])
             : CodlDoc =
      
      def schema: CodlSchema = stack.headOption.fold(baseSchema)(_.head.schema.option.get)
      
      inline def go(tokens: LazyList[CodlToken] = tokens.tail, focus: Proto = focus, peers: List[CodlNode] = peers,
                        stack: List[(Proto, List[CodlNode])] = stack, lines: Int = lines, subs: List[Data] = subs,
                        errors: List[CodlError] = errors, body: LazyList[Text] = LazyList())
                   : CodlDoc =
        recur(tokens, focus, peers, stack, lines, subs, errors, body)
      
      tokens match
        case token #:: tail => token match
          case CodlToken.Line(_) =>
            go(tokens = LazyList(), body = tokens.collect { case CodlToken.Line(txt) => txt })
          case CodlToken.Error(err) => go(errors = err :: errors)
          case CodlToken.Peer => focus.key match
            case key: Text =>
              val (closed, errors2) = focus.close
              go(focus = Proto(), peers = closed :: peers, errors = errors2 ::: errors)
          
            case Unset =>
              go(focus = Proto(Unset, meta = focus.meta.or(if lines == 0 then Unset else Meta(lines))))
            
            case _ =>
              throw Mistake("Should never match")
            
          case CodlToken.Indent =>
            val errors2 = if focus.key.unset then CodlError(focus.line, focus.col, 1, IndentAfterComment) :: errors else errors
            go(focus = Proto(), peers = Nil, stack = (focus -> peers) :: stack, errors = errors2)
          
          case CodlToken.Outdent(n) => stack match
            case Nil =>
              go(LazyList())
            
            case (proto, rest) :: stack2 =>
              val next = if n == 1 then CodlToken.Peer else CodlToken.Outdent(n - 1)
              val (closed, errors2) = focus.close
              val focus2 = proto.copy(children = closed :: peers ::: proto.children)

              go(next #:: tail, focus = focus2, peers = rest, stack = stack2, errors = errors2 ::: errors)
          
          case CodlToken.Tab(n) =>
            go()
          
          case CodlToken.Blank => focus.meta match
            case Unset            =>
              go(lines = lines + 1)
            case Meta(l, _, _, _) =>
              val (closed, errors2) = focus.close
              go(focus = Proto(), peers = closed :: peers, lines = lines + 1, errors = errors2 ::: errors)
          
          case CodlToken.Argument =>
            go(focus = focus.substitute(subs.head), subs = subs.tail)

          case CodlToken.Item(word, line, col, block) =>
            val meta2: Maybe[Meta] = focus.meta.or(if lines == 0 then Unset else Meta(blank = lines))
            
            focus.key match
              case Unset =>
                val (fschema: CodlSchema, errors2: List[CodlError]) =
                  if schema == CodlSchema.Free then (schema, errors)
                  else schema(word).mm((_, errors)).or:
                    (CodlSchema.Free, List(CodlError(line, col, word.length, InvalidKey(word, word))))

                val errors3 =
                  if fschema.unique && peers.exists(_.data.mm(_.key) == word)
                  then CodlError(line, col, word.length, DuplicateKey(word, word)) :: errors2
                  else errors2
                
                go(focus = Proto(word, line, col, meta = meta2, schema = fschema, ids = focus.ids), lines = 0,
                    errors = errors3)
              
              case key: Text => focus.schema match
                case field@Field(_, _)            => val (focus2, errors2) = focus.commit(Proto(word, line, col))
                                                     go(focus = focus2, lines = 0, errors = errors2 ::: errors)
                
                case CodlSchema.Free              => val (focus2, errors2) = focus.commit(Proto(word, line, col))
                                                     go(focus = focus2, lines = 0, errors = errors2 ::: errors)
                
                case struct@Struct(_, _)          => struct.param(focus.children.length) match
                  case Unset                         => val error = CodlError(line, col, word.length, SurplusParams(word, key))
                                                        go(errors = error :: errors)
                  
                  case entry: CodlSchema.Entry    => val peer = Proto(word, line, col, schema = entry.schema)
                                                     val (focus2, errors2) = focus.commit(peer)
                                                     go(focus = focus2, lines = 0, errors = errors2 ::: errors)
              
              case _ =>
                throw Mistake("Should never match")
          
          case CodlToken.Comment(txt, line, col) => focus.key match
            case Unset => val meta = focus.meta.or(Meta())
                          go(focus = Proto(line = line, col = col, meta = meta.copy(blank = lines, comments = txt :: meta.comments)))
            
            case key: Text => go(focus = focus.setMeta(focus.meta.or(Meta()).copy(remark = txt, blank = lines)))
          
            case _ => throw Mistake("Should never match")
        
        case _ => stack match
          case Nil =>
            val (closed, errors2) = focus.close
            val allErrors: List[CodlError] = errors2 ::: errors
            val children = if closed.blank then peers.reverse else (closed :: peers).reverse
            if allErrors.isEmpty then CodlDoc(IArray.from(children), baseSchema, margin, body)
            else throw AggregateError(allErrors.reverse)
          
          case _ =>
            go(LazyList(CodlToken.Outdent(stack.length + 1)))
    


    if stream.isEmpty then CodlDoc() else recur(stream, Proto(), Nil, Nil, 0, subs.reverse, Nil, LazyList())

  def tokenize(in: LazyList[Text]^, fromStart: Boolean = false): (Int, LazyList[CodlToken]^{in}) =
    val reader: PositionReader = new PositionReader(in.map(identity))

    enum State:
      case Word, Hash, Comment, Indent, Space, Tab, Margin, Line
    
    import State.*

    @tailrec
    def cue(count: Int = 0)(using CanThrow[CodlError]): (Character, Int) =
      val ch = reader.next()
      if ch.char == '\n' || ch.char == ' ' then cue(count + 1) else (ch, count)
    
    val (first: Character, start: Int) = try cue(0) catch case err: CodlError => ???
    val margin: Int = first.column

    def istream(char: Character, state: State = Indent, indent: Int = margin, count: Int): LazyList[CodlToken] =
      stream(char, state, indent, count)
    
    @tailrec
    def stream(char: Character, state: State = Indent, indent: Int = margin, count: Int = start)
              : LazyList[CodlToken] =
      //println(s"'${if char.char == '\n' then "\\n" else char.char}', $state, $indent, $count) diff=$diff")
      inline def next(): Character =
        try reader.next() catch case err: CodlError => Character('\n', err.line, err.col)
      
      inline def recur(state: State, indent: Int = indent, count: Int = count + 1): LazyList[CodlToken] =
        stream(next(), state, indent, count)
      
      inline def line(): LazyList[CodlToken] =
        reader.get()
        val char = next()
        if char.char != '\n' && char != Character.End then ???
        else if char == Character.End then
          LazyList()
        else stream(next(), Line, indent, count + 1)
      
      inline def irecur(state: State, indent: Int = indent, count: Int = count + 1): LazyList[CodlToken] =
        istream(next(), state, indent, count)
      
      inline def diff: Int = char.column - indent
      inline def col(char: Character): Int = if fromStart then count else char.column
      
      def put(next: State, stop: Boolean = false): LazyList[CodlToken] = token() #:: irecur(next)

      def token(): CodlToken = state match
        case Comment => CodlToken.Comment(reader.get(), reader.start()(0), reader.start()(1) - 1)
        case Margin  => val text = reader.get()
                        val trimmed = if text.last == '\n' then text.drop(1, Rtl) else text
                        CodlToken.Item(trimmed, reader.start()(0), reader.start()(1), true)
        case Word    => val word = reader.get()
                        if word == t"\u0000" then CodlToken.Argument
                        else CodlToken.Item(word, reader.start()(0), reader.start()(1), false)
        case _       => ???

      inline def consume(next: State): LazyList[CodlToken] =
        reader.put(char)
        recur(next)

      inline def block(): LazyList[CodlToken] =
        if diff >= 4 || char.char == '\n' then consume(Margin)
        else if char.char == ' ' then recur(Margin)
        else token() #:: istream(char, count = count + 1, indent = indent)

      inline def fail(next: State, error: CodlError, adjust: Maybe[Int] = Unset): LazyList[CodlToken] =
        CodlToken.Error(error) #:: irecur(next, indent = adjust.or(char.column))

      inline def newline(next: State): LazyList[CodlToken] =
        if diff > 4 then fail(Margin, CodlError(char.line, col(char), 1, SurplusIndent), indent)
        else if char.column < margin then fail(Indent, CodlError(char.line, col(char), 1, InsufficientIndent), margin)
        else if diff%2 != 0 then fail(Indent, CodlError(char.line, col(char), 1, UnevenIndent(margin, char.column)), char.column + 1)
        else diff match
          case 2 => CodlToken.Indent #:: irecur(next, indent = char.column)
          case 0 => CodlToken.Peer #:: irecur(next, indent = char.column)
          case n => CodlToken.Outdent(-diff/2) #:: irecur(next, indent = char.column)

      char.char match
        case _ if char == Character.End => state match
          case Indent | Space | Tab | Hash => LazyList()
          case Comment | Word | Margin     => LazyList(token())
          case Line                        => LazyList(CodlToken.Line(reader.get()))
        
        case '\n' => state match
          case Word | Comment       => put(Indent)
          case Margin               => block()
          case Indent | Space | Tab => CodlToken.Blank #:: irecur(Indent)
          case Line                 => CodlToken.Line(reader.get()) #:: irecur(Line)
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

  object Prefix extends Interpolator[List[Data], State, CodlDoc]:
    protected def complete(state: State): CodlDoc = ???
      // try Codl.parse(state.content, CodlSchema.Free, state.subs.reverse, fromStart = true)
      // catch case error: AggregateError[CodlError] => ???
    
    def initial: State = State(Nil, Nil)
    def skip(state: State): State = insert(state, List(Data(t"_")))
    def insert(state: State, data: List[Data]): State = state.copy(subs = data.reverse ::: state.subs)
    def parse(state: State, next: Text): State = state.copy(parts = next :: state.parts).tap(complete(_))
