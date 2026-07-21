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
┃    Soundness, version 0.64.0.                                                                    ┃
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
package enigmatic

import java.lang as jl

import anticipation.*
import contingency.*
import fulminate.*
import gossamer.*
import kaleidoscope.*
import monotonous.*, alphabets.base64Standard
import prepositional.*
import rudiments.*
import turbulence.*
import vacuous.*
import zephyrine.*

object Pem:
  // Streaming, cursor-based parsing: the input is consumed line by line, and
  // the base64 body accumulates in a single builder — nothing else of the
  // input is retained, so a PEM document parses from any source in bounded
  // memory (modulo its payload).
  def parse(text: Text)(using Diagnostics): Pem raises PemError = parse(Cursor(text))

  // The first PEM block of the input: leading whitespace is skipped (the
  // legacy parser trimmed the whole document), then the first line must be a
  // `BEGIN` boundary.
  private def parse[cap^](cursor: Cursor[Text, cap]^)(using Diagnostics)
  :   Pem raises PemError =

    while !cursor.finished
          && (cursor.peek == ' ' || cursor.peek == '\t'
              || cursor.peek == '\n' || cursor.peek == '\r')
    do cursor.next()

    nextLine(cursor).lay(abort(PemError(PemError.Reason.BeginMissing))):
      case r"-----* *BEGIN ${PemLabel(label)}([ A-Z]+) *-----*" => block(cursor, label)
      case _                                                    => abort:
                                                                     PemError:
                                                                       PemError.Reason.BeginMissing

  // Every PEM block of the input, lazily: one block parses per forced cell,
  // and content between blocks (comments, subject lines in certificate
  // chains) is skipped. An input with no blocks yields the empty list.
  private def parseAll[cap^](cursor: Cursor[Text, cap]^)
    ( using Diagnostics, Tactic[PemError] )
  :   LazyList[Pem] =

    def recur(): LazyList[Pem] = nextLine(cursor).lay(LazyList()):
      case r"-----* *BEGIN ${PemLabel(label)}([ A-Z]+) *-----*" => block(cursor, label) #:: recur()
      case _                                                    => recur()

    LazyList.defer(recur())

  // The body of a block, after its `BEGIN` line: base64 lines accumulate
  // (verbatim, as the legacy parser joined them) until an `END` boundary.
  // Like the legacy parser, the `END` label is not required to match the
  // `BEGIN` label, though the line is trimmed before matching (a relaxation).
  private def block[cap^](cursor: Cursor[Text, cap]^, label: PemLabel)(using Diagnostics)
  :   Pem raises PemError =

    val body = jl.StringBuilder()
    var data: Optional[Data] = Unset

    while data.absent do
      nextLine(cursor).lay(abort(PemError(PemError.Reason.EndMissing))): line =>
        line.trim match
          case r"-----* *END $endLabel([ A-Z]+) *-----*" =>
            data =
              mitigate:
                case SerializationError(_, _) => PemError(PemError.Reason.BadBase64)

              . protect(body.toString.tt.deserialize[Base64])

          case _ =>
            body.append(line.s)

    Pem(label, data.vouch)

  // The next line of the input (excluding its terminator), or `Unset` at
  // end-of-stream. Only `\n` terminates a line, as the legacy `cut(t"\n")`
  // did; a trailing `\r` stays on the line (and fails the boundary patterns).
  private def nextLine[cap^](cursor: Cursor[Text, cap]^): Optional[Text] =
    if cursor.finished then Unset else cursor.hold:
      val start = cursor.mark

      while !cursor.finished && !(cursor.peek == '\n') do cursor.next()

      val line = cursor.grab(start, cursor.mark)
      if !cursor.finished then cursor.next()
      line

  // Sealed per the codec-thunk pattern (see rep/DECISIONS.md): the
  // resolution-scoped tactic shares the instance's given-resolution lifetime.
  given aggregable: (Diagnostics, Tactic[PemError]) => Pem is Aggregable by Text =
    caps.unsafe.unsafeAssumePure:
      new Aggregable:
        type Self = Pem
        type Operand = Text

        def aggregate(stream: LazyList[Text]): Pem = parse(Cursor(stream.iterator))

        override def accept(stream: (Stream[Text] over Credit)^): Pem =
          // The non-consume `accept` crosses to the consuming factory as a
          // neutral reference; each accept delivers a single-use stream.
          parse(Cursor(stream.asInstanceOf[AnyRef].asInstanceOf[(Stream[Text] over Credit)^]))

  // A certificate chain (or any multi-block document) as a lazy sequence of
  // its blocks.
  given aggregableAll: (Diagnostics, Tactic[PemError]) => LazyList[Pem] is Aggregable by Text =
    caps.unsafe.unsafeAssumePure:
      new Aggregable:
        type Self = LazyList[Pem]
        type Operand = Text

        def aggregate(stream: LazyList[Text]): LazyList[Pem] = parseAll(Cursor(stream.iterator))

        override def accept(stream: (Stream[Text] over Credit)^): LazyList[Pem] =
          // See `aggregable` above.
          parseAll(Cursor(stream.asInstanceOf[AnyRef].asInstanceOf[(Stream[Text] over Credit)^]))

  // The armored form, one line at a time: the `serialize` counterpart for
  // streaming consumers (each line carries its terminator).
  given streamable: Pem is Streamable by Text over Credit = pem =>
    def groups(index: Int): LazyList[Text] =
      if index >= pem.data.length then LazyList(t"-----END ${pem.label}-----\n")
      else t"${pem.data.slice(index, index + 48).serialize[Base64]}\n" #:: groups(index + 48)

    Stream((t"-----BEGIN ${pem.label}-----\n" #:: LazyList.defer(groups(0))).iterator)

case class Pem(label: PemLabel, data: Data):
  def serialize: Text =
    Seq
      ( Seq(t"-----BEGIN $label-----"),
        data.grouped(48).to(Seq).map(_.serialize[Base64]),
        Seq(t"-----END $label-----") )

    . flatten
    . join(t"\n")
