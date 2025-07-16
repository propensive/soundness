                                                                                                  /*
┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓
┃                                                                                                  ┃
┃                                                   ╭───╮                                          ┃
┃                                                   │   │                                          ┃
┃                                                   │   │                                          ┃
┃   ╭───────╮╭─────────╮╭───╮ ╭───╮╭───╮╌────╮╭────╌┤   │╭───╮╌────╮╭────────╮╭───────╮╭───────╮   ┃
┃   │   ╭───╯│   ╭─╮   ││   │ │   ││   ╭─╮   ││   ╭─╮   ││   ╭─╮   ││   ╭─╮  ││   ╭───╯│   ╭───╯   ┃
┃   │   ╰───╮│   │ │   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   ╰─╯  ││   ╰───╮│   ╰───╮   ┃
┃   ╰───╮   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   ╭────╯╰───╮   │╰───╮   │   ┃
┃   ╭───╯   ││   ╰─╯   ││   ╰─╯   ││   │ │   ││   ╰─╯   ││   │ │   ││   ╰────╮╭───╯   │╭───╯   │   ┃
┃   ╰───────╯╰─────────╯╰────╌╰───╯╰───╯ ╰───╯╰────╌╰───╯╰───╯ ╰───╯╰────────╯╰───────╯╰───────╯   ┃
┃                                                                                                  ┃
┃    Soundness, version 0.39.0.                                                                    ┃
┃    © Copyright 2021-25 Jon Pretty, Propensive OÜ.                                                ┃
┃                                                                                                  ┃
┃    The primary distribution site is:                                                             ┃
┃                                                                                                  ┃
┃        https://soundness.dev/                                                                    ┃
┃                                                                                                  ┃
┃    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file     ┃
┃    except in compliance with the License. You may obtain a copy of the License at                ┃
┃                                                                                                  ┃
┃        https://www.apache.org/licenses/LICENSE-2.0                                               ┃
┃                                                                                                  ┃
┃    Unless required by applicable law or agreed to in writing,  software distributed under the    ┃
┃    License is distributed on an "AS IS" BASIS,  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,    ┃
┃    either express or implied. See the License for the specific language governing permissions    ┃
┃    and limitations under the License.                                                            ┃
┃                                                                                                  ┃
┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛
                                                                                                  */
package cellulose

import anticipation.*
import contextual.*
import contingency.*
import fulminate.*
import gossamer.*
import prepositional.*
import proscenium.*
import rudiments.*
import turbulence.*
import vacuous.*

erased trait Codl

object Codl:
  def read[value: CodlDecoder](using Void)[source](source: source)
       (using readable: source is Readable by Text)
  : value raises CodlError raises CodlReadError =

      summon[CodlDecoder[value]].schema.parse(readable.stream(source)).as[value]


  def parse[source]
       (source:    source,
        schema:    CodlSchema = CodlSchema.Free,
        subs:      List[Data] = Nil,
        fromStart: Boolean    = false)
       (using readable: source is Readable by Text, aggregate: Tactic[CodlError])
  : CodlDoc =

      val (margin, stream) =
        tokenize(readable.stream(source), fromStart)(using aggregate.diagnostics)

      val baseSchema: CodlSchema = schema

      case class Proto
                  (key:      Optional[Text]   = Unset,
                  line:     Int              = 0,
                  col:      Int              = 0,
                  children:  List[CodlNode]  = Nil,
                  extra:     Optional[Extra] = Unset,
                  schema:    CodlSchema      = CodlSchema.Free,
                  params:    Int             = 0,
                  multiline: Boolean         = false):

        def commit(child: Proto): (Optional[(Text, (Int, Int))], Proto) =
          val closed = child.close

          val uniqueId2 =
            if child.schema.arity == Arity.Unique
            then (child.key.vouch, (child.line, child.col)) else Unset

          (uniqueId2, copy(children = closed :: children, params = params + 1))

        def substitute(data: Data): Proto =
          copy(children = CodlNode(data) :: children, params = params + 1)

        def setExtra(extra: Optional[Extra]): Proto = copy(extra = extra)

        def close: CodlNode =
          key.lay(CodlNode(Unset, extra)):
            case key: Text =>
              val extra2 = extra.let { m => m.copy(comments = m.comments.reverse) }

              val data = Data
                          (key,
                          IArray.from(children.reverse),
                          Layout(params, multiline, col - margin),
                          schema)

              val node = CodlNode(data, extra2)

              schema.requiredKeys.each: key =>
                if !node.data.let(_.has(key)).or(false) then raise:
                  CodlError(line, col, node.key.or(t"?").length, MissingKey(node.key.or(t"?"), key))

              node


      @tailrec
      def recur
          (tokens:  Stream[CodlToken],
            focus:   Proto,
            peers:   List[CodlNode],
            peerIds: Map[Text, (Int, Int)],
            stack:   List[(Proto, List[CodlNode])],
            lines:   Int,
            subs:    List[Data],
            body:    Stream[Char],
            tabs:    List[Int])
      : CodlDoc =

          def schema: CodlSchema = stack.prim.lay(baseSchema)(_.head.schema)

          inline def go
                      (tokens:  Stream[CodlToken]             = tokens.tail,
                       focus:   Proto                         = focus,
                       peers:   List[CodlNode]                = peers,
                       peerIds: Map[Text, (Int, Int)]         = peerIds,
                       stack:   List[(Proto, List[CodlNode])] = stack,
                       lines:   Int                           = lines,
                       subs:    List[Data]                    = subs,
                       body:    Stream[Char]                  = Stream(),
                       tabs:    List[Int]                     = Nil)
          : CodlDoc =

              recur(tokens, focus, peers, peerIds, stack, lines, subs, body, tabs)


          tokens match
            case token #:: tail => token match
              case CodlToken.Body(stream) =>
                go(tokens = Stream(), body = stream)

              case CodlToken.Error(error) =>
                raise(error)
                go()

              case CodlToken.Peer => focus.key match
                case key: Text =>
                  val closed = focus.close
                  go(focus = Proto(), peers = closed :: peers)

                case _ =>
                  val proto =
                    Proto(Unset, extra = focus.extra.or(if lines == 0 then Unset else Extra(lines)))

                  go(focus = proto)

              case CodlToken.Indent =>
                if focus.key.absent
                then raise(CodlError(focus.line, focus.col, 1, IndentAfterComment))

                go(focus = Proto(), peers = Nil, stack = (focus -> peers) :: stack)

              case CodlToken.Outdent(n) => stack match
                case Nil =>
                  go(Stream())

                case (proto, rest) :: stack2 =>
                  val next = if n == 1 then CodlToken.Peer else CodlToken.Outdent(n - 1)
                  val closed = focus.close

                  val focus2 = proto.copy(children = closed :: peers ::: proto.children)

                  go(next #:: tail, focus = focus2, peers = rest, stack = stack2)

              case CodlToken.Blank => focus.extra match
                case Unset            =>
                  go(lines = lines + 1)
                case Extra(l, _, _) =>
                  val closed = focus.close

                  go(focus = Proto(), peers = closed :: peers, lines = lines + 1)

              case CodlToken.Argument =>
                go(focus = focus.substitute(subs.head), subs = subs.tail)

              case CodlToken.Item(word, line, col, block) =>
                val extra2: Optional[Extra] =
                  focus.extra.or(if lines == 0 then Unset else Extra(blank = lines))

                focus.key match
                  case key: Text => focus.schema match
                    case field@Field(_, _) =>
                      val (uniqueId, focus2) =
                        focus.commit(Proto(word, line, col, multiline = block))

                      uniqueId.let: uniqueId =>
                        if peerIds.contains(uniqueId(0)) then
                          val first = peerIds(uniqueId(0))
                          val duplicate = DuplicateId(uniqueId(0), first(0), first(1))
                          raise(CodlError(line, col, uniqueId(0).length, duplicate))

                      val peerIds2 = uniqueId.let(peerIds.updated(_, _)).or(peerIds)
                      go(focus = focus2, peerIds = peerIds2, lines = 0)

                    case CodlSchema.Free =>
                      val (uniqueId, focus2) =
                        focus.commit(Proto(word, line, col, multiline = block))

                      uniqueId.let: uniqueId =>
                        if peerIds.contains(uniqueId(0)) then
                          val first = peerIds(uniqueId(0))
                          val duplicate = DuplicateId(uniqueId(0), first(0), first(1))
                          raise(CodlError(line, col, uniqueId(0).length, duplicate))

                      val peerIds2 = uniqueId.let(peerIds.updated(_, _)).or(peerIds)
                      go(focus = focus2, peerIds = peerIds2, lines = 0)

                    case struct@Struct(_, _) => struct.param(focus.children.length) match
                      case Unset =>
                        raise(CodlError(line, col, word.length, SurplusParams(word, key)))
                        go()

                      case entry: CodlSchema.Entry =>
                        val peer = Proto(word, line, col, schema = entry.schema, multiline = block)
                        val (uniqueId, focus2) = focus.commit(peer)

                        uniqueId.let: uniqueId =>
                          if peerIds.contains(uniqueId(0)) then
                            val first = peerIds(uniqueId(0))
                            val duplicate = DuplicateId(uniqueId(0), first(0), first(1))
                            raise(CodlError(line, col, uniqueId(0).length, duplicate))

                        val peerIds2 = uniqueId.let(peerIds.updated(_, _)).or(peerIds)
                        go(focus = focus2, peerIds = peerIds2, lines = 0)

                  case _ =>
                    val fschema: CodlSchema =
                      if schema == CodlSchema.Free then schema
                      else schema(word).or:
                        raise(CodlError(line, col, word.length, InvalidKey(word, word)))
                        CodlSchema.Free

                    if fschema.unique && peers.exists(_.data.let(_.key) == word)
                    then raise(CodlError(line, col, word.length, DuplicateKey(word, word)))

                    go(focus = Proto(word, line, col, extra = extra2, schema = fschema,
                        multiline = block), lines = 0)

              case CodlToken.Comment(txt, line, col) => focus.key match
                case key: Text =>
                  go(focus = focus.setExtra(focus.extra.or(Extra()).copy(remark = txt,
                                                                         blank  = lines)))

                case _ =>
                  val extra = focus.extra.or(Extra())

                  go(focus = Proto
                              (line  = line,
                               col   = col,
                               extra = extra.copy(blank = lines, comments = txt :: extra.comments)))

            case _ => stack match
              case Nil =>
                val closed = focus.close
                val children = if closed.blank then peers.reverse else (closed :: peers).reverse

                CodlDoc(IArray.from(children), baseSchema, margin, body)

              case _ =>
                go(Stream(CodlToken.Outdent(stack.length + 1)))

      if stream.isEmpty
      then CodlDoc() else recur(stream, Proto(), Nil, Map(), Nil, 0, subs.reverse, Stream(), Nil)


  def tokenize(in: Stream[Text], fromStart: Boolean = false)(using Diagnostics)
  : (Int, Stream[CodlToken]) raises CodlError =

      val reader: PositionReader = new PositionReader(in.map(identity))

      enum State:
        case Word, Hash, Comment, Indent, Space, Margin
        case Pending(ch: Character)

      import State.*

      @tailrec
      def cue(count: Int = 0)(using Tactic[CodlError]): (Character, Int) =
        val ch = reader.next()
        if ch.char == '\n' || ch.char == ' ' then cue(count + 1) else (ch, count)

      val (first: Character, start: Int) = cue(0)
      val margin: Int = first.column


      def istream
          (char:    Character,
            state:   State     = Indent,
            indent:  Int       = margin,
            count:   Int,
            padding: Boolean)
      : Stream[CodlToken] =

          stream(char, state, indent, count, padding)


      @tailrec
      def stream
          (char:    Character,
            state:   State     = Indent,
            indent:  Int       = margin,
            count:   Int       = start,
            padding: Boolean)
      : Stream[CodlToken] =

          inline def next(): Character =
            try reader.next()
            catch case err: CodlError => Character('\n', err.line, err.col)


          inline def recur
                      (state:   State,
                      indent:  Int     = indent,
                      count:   Int     = count + 1,
                      padding: Boolean = padding)
          : Stream[CodlToken] =

              stream(next(), state, indent, count, padding)


          inline def body(): Stream[CodlToken] =
            reader.get()
            val char = next()

            if char.char != '\n' && char != Character.End
            then fail(Comment, CodlError(char.line, col(char), 1, BadTermination))
            else
              if char == Character.End then Stream()
              else Stream(CodlToken.Body(reader.charStream()))


          inline def irecur
                      (state: State,
                      indent: Int      = indent,
                      count: Int       = count + 1,
                      padding: Boolean = padding)
          : Stream[CodlToken] =

              istream(next(), state, indent, count, padding)


          inline def diff: Int = char.column - indent
          inline def col(char: Character): Int = if fromStart then count else char.column


          def put(next: State, stop: Boolean = false, padding: Boolean = padding)
          : Stream[CodlToken] =

              token() #:: irecur(next, padding = padding)


          def token(): CodlToken = state.absolve match
            case Comment =>
              CodlToken.Comment(reader.get(), reader.start()(0), reader.start()(1) - 1)

            case Margin =>
              val text: Text = reader.get()
              val trimmed = if text.s.last == '\n' then text.skip(1, Rtl) else text
              CodlToken.Item(trimmed, reader.start()(0), reader.start()(1), true)

            case Word | Pending(_) =>
              val word = reader.get()
              if word == t"\u0000" then CodlToken.Argument
              else CodlToken.Item(word, reader.start()(0), reader.start()(1), false)

          inline def consume(next: State, padding: Boolean = padding): Stream[CodlToken] =
            reader.put(char)
            recur(next, padding = padding)

          inline def block(): Stream[CodlToken] =
            if diff >= 4 || char.char == '\n' then consume(Margin, padding = false)
            else if char.char == ' ' then recur(Margin, padding = false)
            else token() #:: istream(char, count = count + 1, indent = indent, padding = false)


          def fail(next: State, error: CodlError, adjust: Optional[Int] = Unset)
          : Stream[CodlToken] =

              CodlToken.Error(error) #:: irecur(next, indent = adjust.or(char.column))


          def newline(next: State): Stream[CodlToken] =
            if diff > 4 then fail(Margin, CodlError(char.line, col(char), 1, SurplusIndent), indent)
            else if char.column < margin
            then fail(Indent, CodlError(char.line, col(char), 1, InsufficientIndent), margin)
            else if diff%2 != 0 then
              fail
               (Indent,
                CodlError(char.line, col(char), 1, UnevenIndent(margin, char.column)),
                char.column + 1)
            else diff match
              case 2 => CodlToken.Indent #:: irecur(next, indent = char.column)
              case 0 => CodlToken.Peer #:: irecur(next, indent = char.column)
              case n => CodlToken.Outdent(-diff/2) #:: irecur(next, indent = char.column)

          char.char match
            case _ if char == Character.End => state match
              case Indent | Space | Hash | Pending(_) => Stream()
              case Comment | Word | Margin            => Stream(token())

            case '\n' => state match
              case Word | Comment | Pending(_) => put(Indent, padding = false)
              case Margin                      => block()
              case Indent | Space              => CodlToken.Blank
                                                  #:: irecur(Indent, padding = false)
              case _                           => recur(Indent, padding = false)

            case ' ' => state match
              case Space              => recur(Space, padding = true)
              case Pending(_)         => put(Space)
              case Indent             => recur(Indent)
              case Word               => if padding then recur(Pending(char)) else put(Space)
              case Comment            => consume(state)
              case Margin             => block()
              case Hash               => reader.get(); recur(Comment)

            case '#' => state match
              case Pending(_) | Space => consume(Hash)
              case Comment            => body()
              case Word               => consume(Word)
              case Indent             => if diff == 4 then recur(Margin) else newline(Comment)
              case Margin             => block()
              case Hash               => consume(Word)

            case ch => state match
              case Pending(ch)    => reader.put(ch); consume(Word)
              case Space | Word   => consume(Word)
              case Comment        => consume(state)
              case Indent         => reader.put(char)
                                    if diff == 4 then recur(Margin) else newline(Word)
              case Margin         => block()

              case Hash => char.char match
                case '!' if first.line == 0 && first.column == 1 => consume(Comment)
                case ch                                          => consume(Word)

      (first.column, stream(first, padding = false).drop(1))


  case class State(parts: List[Text], subs: List[Data]):
    def content: Text = parts.reverse.join(t"\u0000")

  object Prefix extends Interpolator[List[Data], State, CodlDoc]:
    protected def complete(state: State): CodlDoc = ???
      // try Codl.parse(state.content, CodlSchema.Free, state.subs.reverse, fromStart = true)
      // catch case error: AggregateError[CodlError] => ???

    def initial: State = State(Nil, Nil)
    def skip(state: State): State = insert(state, List(Data(t"_")))

    def insert(state: State, data: List[Data]): State =
      state.copy(subs = data.reverse ::: state.subs)

    def parse(state: State, next: Text): State =
      state.copy(parts = next :: state.parts).tap(complete(_))
