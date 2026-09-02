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

import scala.caps

import java.lang as jl

import anticipation.*
import denominative.*
import contingency.*
import distillate.*
import fulminate.*
import gossamer.*
import kaleidoscope.*
import monotonous.*, alphabets.base64Standard
import prepositional.*
import rudiments.*
import spectacular.*
import turbulence.*
import vacuous.*
import zephyrine.*

object Pem:
  // Armor a DER document, e.g. `Pem(Pem.Label.Certificate, certificate.in[Der])`. The label is
  // always given explicitly: it is a fact about the value, not a mode of the encoding.
  def apply(label: Pem.Label, der: Der): Pem = Pem(label, der.data)

  // The DER payload of a PEM block, so that `pem.as[Der]` reaches the armored bytes as a document
  // rather than as anonymous `Data`. The label is not checked against the content: every PEM label
  // this module knows armors DER.
  given derDecodable: Der is Decodable in Pem = pem => Der(pem.data)

  // `text.read[Asn1 in Pem]` — and, through distillate's identity decodable, `text.read[Der in
  // Pem]` — reading the armor and decoding its DER payload in one step. Sealed per the codec-thunk
  // pattern (see rep/DECISIONS.md), like the `Pem` aggregables below.
  given aggregableIn: [value]
  =>  ( decodable: (value is Decodable in Der)^ )
  =>  ( Diagnostics, Tactic[Pem.Error] )
  =>  ( (value in Pem) is Aggregable by Text ) =

    caps.unsafe.unsafeAssumePure:
      new Aggregable:
        type Self = value in Pem
        type Operand = Text

        def aggregate(stream: Chain[Text]): value in Pem =
          // `Cursor` is built from a stdlib `Iterator`, which the opaque `Chain` cannot yield.
          decode(parse(Cursor(stream)))

        override def accept(stream: (Stream[Text] over Credit)^): value in Pem =
          // See `aggregable` below.
          val neutral: AnyRef = stream.asInstanceOf[AnyRef]
          decode(parse(Cursor(neutral.asInstanceOf[(Stream[Text] over Credit)^])))

        private def decode(pem: Pem): value in Pem =
          decodable.decoded(Der(pem.data)).asInstanceOf[value in Pem]

  // Streaming, cursor-based parsing: the input is consumed line by line, and
  // the base64 body accumulates in a single builder — nothing else of the
  // input is retained, so a PEM document parses from any source in bounded
  // memory (modulo its payload).
  //
  // Not public: `text.read[Pem]` (through `aggregable`, below) is the entry point.
  private[enigmatic] def parse(text: Text)(using Diagnostics): Pem raises Pem.Error =
    parse(Cursor(text))

  // The first PEM block of the input: leading whitespace is skipped (the
  // legacy parser trimmed the whole document), then the first line must be a
  // `BEGIN` boundary.
  // A real `using` clause rather than the `raises` sugar: a context-function result would
  // hide the `cursor` parameter, which the separation checker rejects.
  private def parse[cap^](cursor: Cursor[Text, cap]^)(using Diagnostics, Tactic[Pem.Error]^)
  :   Pem =

    while !cursor.finished
          && (cursor.peek == ' ' || cursor.peek == '\t'
              || cursor.peek == '\n' || cursor.peek == '\r')
    do cursor.next()

    nextLine(cursor).lay(abort(Pem.Error(Pem.Error.Reason.BeginMissing))):
      case r"-----* *BEGIN ${Pem.Label(label)}([ A-Z]+) *-----*" => block(cursor, label)
      case _                                                    => abort:
                                                                     Pem.Error:
                                                                       Pem.Error.Reason.BeginMissing

  // Every PEM block of the input, lazily: one block parses per forced cell,
  // and content between blocks (comments, subject lines in certificate
  // chains) is skipped. An input with no blocks yields the empty list.
  private def parseAll[cap^](cursor: Cursor[Text, cap]^)
    ( using Diagnostics, Tactic[Pem.Error] )
  :   Chain[Pem] =

    def recur(): Chain[Pem] = nextLine(cursor).lay(Chain()):
      case r"-----* *BEGIN ${Pem.Label(label)}([ A-Z]+) *-----*" => block(cursor, label) #:: recur()
      case _                                                    => recur()

    Chain.defer(recur())

  // The body of a block, after its `BEGIN` line: base64 lines accumulate
  // (verbatim, as the legacy parser joined them) until an `END` boundary.
  // Like the legacy parser, the `END` label is not required to match the
  // `BEGIN` label, though the line is trimmed before matching (a relaxation).
  private def block[cap^](cursor: Cursor[Text, cap]^, label: Pem.Label)
    ( using Diagnostics, Tactic[Pem.Error]^ )
  :   Pem =

    val body = jl.StringBuilder()

    def recur(): Data =
      nextLine(cursor).lay(abort(Pem.Error(Pem.Error.Reason.EndMissing))): line =>
        line.trim match
          case r"-----* *END $endLabel([ A-Z]+) *-----*" =>
            mitigate:
              case Serialization.Error(_, _) => Pem.Error(Pem.Error.Reason.BadBase64)

            // `[Data]` stated, not inferred: the conformance check on a macro expansion
            // dealiases opaque types, and `Data` is a transparent alias over an opaque one.
            // Inferred, the two sides of the check were spelled differently — `Data` against
            // its own dealiasing, `scala.Array[Byte]` — and did not match.
            . protect[Data](body.toString.tt.deserialize[Base64])

          case _ =>
            body.append(line.s)
            recur()

    Pem(label, recur())

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
  given aggregable: (Diagnostics, Tactic[Pem.Error]) => Pem is Aggregable by Text =
    caps.unsafe.unsafeAssumePure:
      new Aggregable:
        type Self = Pem
        type Operand = Text

        // `Cursor` is built from a stdlib `Iterator`, which the opaque `Chain` cannot yield.
        def aggregate(stream: Chain[Text]): Pem = parse(Cursor(stream))

        override def accept(stream: (Stream[Text] over Credit)^): Pem =
          // The non-consume `accept` crosses to the consuming factory as a
          // neutral reference; each accept delivers a single-use stream.
          parse(Cursor(stream.asInstanceOf[AnyRef].asInstanceOf[(Stream[Text] over Credit)^]))

  // A certificate chain (or any multi-block document) as a lazy sequence of
  // its blocks.
  given aggregableAll: (Diagnostics, Tactic[Pem.Error]) => Chain[Pem] is Aggregable by Text =
    caps.unsafe.unsafeAssumePure:
      new Aggregable:
        type Self = Chain[Pem]
        type Operand = Text

        // `Cursor` is built from a stdlib `Iterator`, which the opaque `Chain` cannot yield.
        def aggregate(stream: Chain[Text]): Chain[Pem] = parseAll(Cursor(stream))

        override def accept(stream: (Stream[Text] over Credit)^): Chain[Pem] =
          // See `aggregable` above.
          parseAll(Cursor(stream.asInstanceOf[AnyRef].asInstanceOf[(Stream[Text] over Credit)^]))

  // The armored form is multi-line and base64-encoded, so it is not what an inspection shows:
  // the label identifies the block, and the payload is rendered as full-width hexadecimal, on
  // one line and with nothing dropped.
  given inspectable: [pem <: Pem] => pem is Inspectable = pem =>
    t"Pem(${Pem.Label.showable.text(pem.label)}:${Inspection.hex(pem.data)})"

  // The armored form, one line at a time: the `serialize` counterpart for
  // streaming consumers (each line carries its terminator).
  given streamable: Pem is Streamable by Text over Credit = pem =>
    def groups(index: Int): Chain[Text] =
      if index >= pem.data.length then Chain(t"-----END ${pem.label}-----\n")
      else t"${pem.data.segment((index).z till (index + 48).z).serialize[Base64]}\n" #:: groups(index + 48)

    Stream(t"-----BEGIN ${pem.label}-----\n" #:: Chain.defer(groups(0)))

  // PemError → Pem.Error
  object Error:
    given communicable: Reason is Communicable =
      case Reason.BadBase64    => m"could not parse the BASE-64 PEM message"
      case Reason.BeginMissing => m"the BEGIN line could not be found"
      case Reason.EndMissing   => m"the END line could not be found"
      case Reason.EmptyFile    => m"the file was empty"

    enum Reason(val number: Int) extends Clarification:
      case BeginMissing extends Reason(1)
      case EndMissing   extends Reason(2)
      case BadBase64    extends Reason(3)
      case EmptyFile    extends Reason(4)

  case class Error(reason: Pem.Error.Reason)(using Diagnostics)
  extends fulminate.Error(389, reason.number)(m"could not parse PEM content because $reason")

  // PemLabel → Pem.Label
  object Label:
    lazy val index: Map[Text, Pem.Label] =
      (0 to 17).map(fromOrdinal(_)).indexBy(_.toString.tt.uncamel.map(_.upper).join(t" "))

    given showable: Pem.Label is Showable =
      case Proprietary(label) => label
      case other              => other.toString.tt.uncamel.map(_.upper).join(t" ")

    def unapply(text: Text): Some[Pem.Label] = Some(index.at(text).or(Proprietary(text)))

  enum Label:
    case Certificate, CertificateRequest, NewCertificateRequest, PrivateKey, RsaPrivateKey,
      DsaPrivateKey, EcPrivateKey, EncryptedPrivateKey, PublicKey, Pkcs7, Cms, DhParameters,
      X509Crl, AttributeCertificate, EncryptedMessage, SignedMessage, RsaPublicKey, DsaPublicKey

    case Proprietary(label: Text)

case class Pem(label: Pem.Label, data: Data):
  def serialize: Text =
    List
      ( List(t"-----BEGIN $label-----"),
        data.batched(48).map(_.serialize[Base64]),
        List(t"-----END $label-----") )

    . flat
    . join(t"\n")
