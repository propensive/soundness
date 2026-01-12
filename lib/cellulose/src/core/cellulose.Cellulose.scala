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
┃    Soundness, version 0.49.0.                                                                    ┃
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
import denominative.*
import distillate.*
import fulminate.*
import gossamer.*
import prepositional.*
import proscenium.*
import rudiments.*
import spectacular.*
import turbulence.*
import vacuous.*
import wisteria.*
import zephyrine.*

export Cellulose.Codl

object Cellulose extends Cellulose2:
  opaque type Codl = List[Codllike]

  import Codl.Issue.*

  extension (codl: Codl) def list: List[Codllike] = codl

  object Codl extends Format:
    def name: Text = t"CoDL"

    def apply(value: List[Codllike]): Codl = value
    def wrap(value: Text): Codl = Codl(List(Codllike(IArray(CodlNode(Atom(value))))))

    def decodableField[value: Decodable in Text]: value is Decodable in Codl raises CodlError =
      decodable(value.decoded(_))

    def decodable[value](lambda: Text => value): value is Decodable in Codl raises CodlError =
      codl =>
        codl.list.prim.lest(CodlError(CodlError.Reason.BadFormat(Unset)))
        . children match
            case IArray(CodlNode(Atom(value, _, _, _), _)) => lambda(value)

            case _ =>
              abort(CodlError(CodlError.Reason.BadFormat(Unset)))

    def field[encodable: Encodable in Text]: encodable is Encodable in Codl =
      value => Codl(List(Codllike(IArray(CodlNode(Atom(encodable.encoded(value)))))))

    case class Position(line: Int, column: Int, length: Int) extends Format.Position:
      def describe: Text = t"line $line, column $column"

    inline given derivedEncodable: [value] => value is Encodable in Codl = compiletime.summonFrom:
      case given (`value` is Encodable in Text) => Codl.field[value]
      case given ProductReflection[`value`]     => EncodableDerivation.derived[value]

    given booleanEncodable: Boolean is Encodable in Codl =
      value => Codl.wrap(if value then t"yes" else t"no")

    given textEncodable: Text is Encodable in Codl = value => Codl.wrap(value)

    given optionalEncodable: [inner, value >: Unset.type: Mandatable to inner]
          => (encoder: => inner is Encodable in Codl)
          => value is Encodable in Codl =

      _.let(_.asInstanceOf[inner]).lay(Codl(Nil))(encoder.encoded(_))


    given optionEncodable: [encodable: Encodable in Codl]
          => Option[encodable] is Encodable in Codl =

        case None        => Codl(List())
        case Some(value) => encodable.encoded(value)

    given listEncodable: [element] => (element: => element is Encodable in Codl)
          => List[element] is Encodable in Codl =

        value => Codl(value.map(element.encoded(_).list.head))

    given setEncodable: [element: Encodable in Codl] => Set[element] is Encodable in Codl =
      value => Codl(value.map(element.encoded(_).list.head).to(List))

    inline given derivedDecodable: [value] => Tactic[CodlError] => value is Decodable in Codl =
      compiletime.summonFrom:
        case given (`value` is Decodable in Text) => decodableField[`value`]
        case given ProductReflection[`value`]     => DecodableDerivation().derived[`value`]

    given booleanDecodable: Tactic[CodlError] => Boolean is Decodable in Codl =
      decodable(_ == t"yes")

    given textDecodable: Tactic[CodlError] => Text is Decodable in Codl = decodable(identity(_))
    given unitDecodable: Unit is Decodable in Codl = _ => ()

    given optionalDecodable: [value >: Unset.type: Mandatable]
          => (decoder: => value.Result is Decodable in Codl)
          =>  value is Decodable in Codl =
      codl => if codl.list.nil then Unset else decoder.decoded(codl)

    given optionDecodable: [decodable] => (decoder: => decodable is Decodable in Codl)
          => Option[decodable] is Decodable in Codl =
      codl => if codl.list.nil then None else Some(decoder.decoded(codl))

    given listDecodable: [element]
          => (decodable: => element is Decodable in Codl, schematic: => element is CodlSchematic)
          => List[element] is Decodable in Codl =

      value => schematic.schema() match
        case Field(_) => value.list.flatMap(_.children).map: node =>
          decodable.decoded(Codl(List(CodlDoc(node))))

        case struct: Struct =>
          value.list.map(List(_)).map(Codl(_)).map(decodable.decoded(_))

    given setDecodable: [element: CodlSchematic] => (decodable: => element is Decodable in Codl)
          => Set[element] is Decodable in Codl =
      value =>
        element.schema() match
          case Field(_) =>
            value.list.flatMap(_.children).map: node =>
              decodable.decoded(Codl(List(CodlDoc(node))))
            . to(Set)

          case struct: Struct =>
            value.list.map(List(_)).map(Codl(_)).map(decodable.decoded(_)).to(Set)

    enum Issue extends Format.Issue:
      case UnexpectedCarriageReturn
      case BadSubstitution
      case BadTermination
      case CarriageReturnMismatch(required: Boolean)
      case UnevenIndent(initial: Int, indent: Int)
      case IndentAfterComment, SurplusIndent, InsufficientIndent

      case MissingKey(point: Text, key: Text)
      case DuplicateKey(point: Text, key: Text)
      case SurplusParams(point: Text, cmd: Text)
      case InvalidKey(point: Text, key: Text)
      case DuplicateId(point: Text, line: Int, col: Int)

      def describe: Message = this match
        case BadSubstitution           => m"a substitution cannot be made at this point"
        case SurplusIndent             => m"too much indentation was given"
        case InsufficientIndent        => m"insufficient indentation was specified"
        case MissingKey(point, key)    => m"the value $key was missing at $point"
        case DuplicateKey(point, key)  => m"the unique key $key has already been used at $point"
        case SurplusParams(point, key) => m"too many parameters for the key $key at $point"
        case InvalidKey(point, key)    => m"the key $key was invalid at $point"

        case DuplicateId(point, line, col) =>
          m"the unique ID has been used before at $line:$col, $point"


        case UnexpectedCarriageReturn =>
          m"""a carriage return character ('\\r') was followed by a character other than a newline
              ('\\n')"""

        case CarriageReturnMismatch(true) =>
          m"""a newline character ('\\n') was found without a preceding carriage return ('\\r'),
              which does not match the document's prior newline convention"""

        case CarriageReturnMismatch(false) =>
          m"""a carriage return ('\\r') was encountered, which does not match the document's prior
              newline convention"""

        case UnevenIndent(initial, indent) =>
          m"""the indentation level of ${indent - initial} (with a margin of $initial) is not an
              exact multiple of 2"""

        case IndentAfterComment =>
          m"indentation was given after a comment; the comment should be aligned with its next key"

        case BadTermination =>
          m"two # symbols terminates the document and must appear alone on a line"


    def read[entity: {Decodable in Codl, CodlSchematic}](using Void)[streamable: Streamable by Text]
         (source: streamable)
    : entity raises ParseError raises CodlError =

        entity.schema().parse(source.stream[Text]).as[entity]


    def parse[source]
         (source:    source,
          schema:    CodlSchema = CodlSchema.Free,
          subs:      List[Atom] = Nil,
          fromStart: Boolean    = false)
        (using streamable: source is Streamable by Text, aggregate: Tactic[ParseError])
    : CodlDoc =

        val (margin, stream) =
          tokenize(streamable.stream(source), fromStart)(using aggregate.diagnostics)

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

          def substitute(data: Atom): Proto =
            copy(children = CodlNode(data) :: children, params = params + 1)

          def setExtra(extra: Optional[Extra]): Proto = copy(extra = extra)

          def close: CodlNode =
            key.lay(CodlNode(Unset, extra)):
              case key: Text =>
                val extra2 = extra.let { m => m.copy(comments = m.comments.reverse) }

                val data = Atom
                            (key,
                            IArray.from(children.reverse),
                            Layout(params, multiline, col - margin),
                            schema)

                val node = CodlNode(data, extra2)

                schema.requiredKeys.each: key =>
                  if !node.data.let(_.has(key)).or(false) then raise:
                    ParseError
                     (Codl,
                      Position(line, col, node.key.or(t"?").length),
                      MissingKey(node.key.or(t"?"), key))

                node


        @tailrec
        def recur
             (tokens:  Stream[CodlToken],
              focus:   Proto,
              peers:   List[CodlNode],
              peerIds: Map[Text, (Int, Int)],
              stack:   List[(Proto, List[CodlNode])],
              lines:   Int,
              subs:    List[Atom],
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
                        subs:    List[Atom]                    = subs,
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
                      Proto
                       (Unset, extra = focus.extra.or(if lines == 0 then Unset else Extra(lines)))

                    go(focus = proto)

                case CodlToken.Indent =>
                  if focus.key.absent
                  then
                    raise(ParseError(Codl, Position(focus.line, focus.col, 1), IndentAfterComment))

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
                      case field@Field(_) =>
                        val (uniqueId, focus2) =
                          focus.commit(Proto(word, line, col, multiline = block))

                        uniqueId.let: uniqueId =>
                          if peerIds.contains(uniqueId(0)) then
                            val first = peerIds(uniqueId(0))
                            val duplicate = DuplicateId(uniqueId(0), first(0), first(1))
                            raise
                             (ParseError(Codl, Position(line, col, uniqueId(0).length), duplicate))

                        val peerIds2 = uniqueId.let(peerIds.updated(_, _)).or(peerIds)
                        go(focus = focus2, peerIds = peerIds2, lines = 0)

                      case CodlSchema.Free =>
                        val (uniqueId, focus2) =
                          focus.commit(Proto(word, line, col, multiline = block))

                        uniqueId.let: uniqueId =>
                          if peerIds.contains(uniqueId(0)) then
                            val first = peerIds(uniqueId(0))
                            val duplicate = DuplicateId(uniqueId(0), first(0), first(1))
                            raise
                             (ParseError(Codl, Position(line, col, uniqueId(0).length), duplicate))

                        val peerIds2 = uniqueId.let(peerIds.updated(_, _)).or(peerIds)
                        go(focus = focus2, peerIds = peerIds2, lines = 0)

                      case struct@Struct(_, _) => struct.param(focus.children.length) match
                        case Unset =>
                          raise
                           (ParseError
                             (Codl, Position(line, col, word.length), SurplusParams(word, key)))

                          go()

                        case entry: CodlSchema.Entry =>
                          val peer =
                            Proto(word, line, col, schema = entry.schema, multiline = block)

                          val (uniqueId, focus2) = focus.commit(peer)

                          uniqueId.let: uniqueId =>
                            if peerIds.contains(uniqueId(0)) then
                              val first = peerIds(uniqueId(0))
                              val duplicate = DuplicateId(uniqueId(0), first(0), first(1))
                              raise
                               (ParseError
                                 (Codl, Position(line, col, uniqueId(0).length), duplicate))

                          val peerIds2 = uniqueId.let(peerIds.updated(_, _)).or(peerIds)
                          go(focus = focus2, peerIds = peerIds2, lines = 0)

                    case _ =>
                      val fschema: CodlSchema =
                        if schema == CodlSchema.Free then schema
                        else schema(word).or:
                          raise
                           (ParseError
                             (Codl, Position(line, col, word.length), InvalidKey(word, word)))
                          CodlSchema.Free

                      if fschema.unique && peers.exists(_.data.let(_.key) == word)
                      then
                        raise
                         (ParseError
                           (Codl, Position(line, col, word.length), DuplicateKey(word, word)))

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
                                extra = extra.copy
                                         (blank = lines, comments = txt :: extra.comments)))

              case _ => stack match
                case Nil =>
                  val closed = focus.close
                  val children = if closed.blank then peers.reverse else (closed :: peers).reverse

                  CodlDoc(IArray.from(children), baseSchema, margin, body)

                case _ =>
                  go(Stream(CodlToken.Outdent(stack.length + 1)))

        if stream.nil
        then CodlDoc() else recur(stream, Proto(), Nil, Map(), Nil, 0, subs.reverse, Stream(), Nil)


    def tokenize(in: Stream[Text], fromStart: Boolean = false)(using Diagnostics)
    : (Int, Stream[CodlToken]) raises ParseError =

        val reader: PositionReader = new PositionReader(in.map(identity))

        enum State:
          case Word, Hash, Comment, Indent, Space, Margin
          case Pending(ch: Character)

        import State.*

        @tailrec
        def cue(count: Int = 0)(using Tactic[ParseError]): (Character, Int) =
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
              try reader.next() catch
                case error: ParseError => error.absolve match
                  case ParseError(Codl, Codl.Position(line, column, _), _) =>
                    Character('\n', line, column)


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
              then
                fail(Comment, ParseError(Codl, Position(char.line, col(char), 1), BadTermination))
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


            def fail(next: State, error: ParseError, adjust: Optional[Int] = Unset)
            : Stream[CodlToken] =

                CodlToken.Error(error) #:: irecur(next, indent = adjust.or(char.column))


            def newline(next: State): Stream[CodlToken] =
              if diff > 4
              then
                fail
                 (Margin,
                  ParseError(this, Position(char.line, col(char), 1), SurplusIndent),
                  indent)

              else if char.column < margin
              then
                fail
                 (Indent,
                  ParseError(Codl, Position(char.line, col(char), 1), InsufficientIndent),
                  margin)
              else if diff%2 != 0 then
                fail
                 (Indent,
                  ParseError
                   (Codl, Position(char.line, col(char), 1), UnevenIndent(margin, char.column)),
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


    case class State(parts: List[Text], subs: List[Atom]):
      def content: Text = parts.reverse.join(t"\u0000")

    object Prefix extends Interpolator[List[Atom], State, CodlDoc]:
      protected def complete(state: State): CodlDoc = ???
        // try Codl.parse(state.content, CodlSchema.Free, state.subs.reverse, fromStart = true)
        // catch case error: AggregateError[ParseError] => ???

      def initial: State = State(Nil, Nil)
      def skip(state: State): State = insert(state, List(Atom(t"_")))

      def insert(state: State, data: List[Atom]): State =
        state.copy(subs = data.reverse ::: state.subs)

      def parse(state: State, next: Text): State =
        state.copy(parts = next :: state.parts).tap(complete(_))
