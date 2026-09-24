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
package stratiform

import scala.collection.immutable.Seq

import soundness.*

import strategies.throwUnsafely
import errorDiagnostics.stackTracesDiagnostics
import denominative.dysasymptotics.linearSize

// Acceptances (BinTEL §8.4): the built-in `acceptance` schema against the
// specification's pinned vectors, the `base-256` and `schema-signature`
// codecs, the resolver's recognition of the pinned coordinate and
// signature, and the four interchangeable forms of the message — TEL
// text, framed and bare BinTEL, and BASE-256 — round-tripping the
// specification's worked request.
object AcceptanceTests extends Suite(m"Stratiform acceptance tests"):

  private def resource(name: String): Data =
    val stream = getClass.getResourceAsStream(s"/stratiform/corpus/$name").nn
    try Array.unsafeFrozen(stream.readAllBytes().nn) finally stream.close()

  private def resourceText(name: String): Text =
    Text(String(Array.unsafeJvm(resource(name)), "UTF-8").trim.nn)

  private def hexBytes(hex: String): Seq[Byte] =
    val array = new scala.Array[Byte](hex.length/2)
    array.indices.foreach: i =>
      array(i) = java.lang.Integer.parseInt(hex.substring(i*2, i*2 + 2), 16).toByte
    array.toSeq

  private def hex(data: Data): String =
    data.readable.toSeq.map { byte => f"${byte & 0xff}%02x" }.mkString

  private def bytes(values: Int*): Data =
    Array.unsafeFrozen(values.map(_.toByte).toArray)

  private lazy val schemaDoc: Tel = resource("acceptance.tel").read[Tel]

  private lazy val schemaElement: Tel.Element =
    Tel.Type.assign(schemaDoc, Tels.Axiom.tels)

  private lazy val request: Text = resourceText("acceptance-request.tel")

  private val pinnedHash: String =
    "ed8e981cd770b7d75067e05dfd87d8b5c37e97763872febf513c125ab1a8b245"

  // The pragma line of a text-form acceptance.
  private val header: Text = t"tel 1.0 specification.tel/acceptance:1.0.0\n\n"

  def run(): Unit =
    suite(m"The built-in acceptance schema"):
      test(m"acceptance.tel encodes to the 453-byte reference document root"):
        val encoded = schemaElement.bintel(Tels.Axiom.tels)
        (encoded.length, encoded.readable.toSeq == hexBytes(resourceText("acceptance.bintel.hex").s))
      . assert(_ == (453, true))

      test(m"acceptance.tel matches the pinned BLAKE3-256 value hash"):
        hex(schemaElement.valueHash(Tels.Axiom.tels).data)
      . assert(_ == pinnedHash)

      test(m"the pinned signature is the palimpsest of the value hash"):
        Base256.encode(SchemaSignature.fromDocument(schemaDoc, Tels.Axiom.tels))
      . assert(_ == Tel.Acceptance.signature)

      test(m"the pinned signature is 33 bytes with one component"):
        Tel.Acceptance.Signature(Tel.Acceptance.signatureData).count
      . assert(_ == 1)

      test(m"the hand-written axiom is equivalent to the reconstructed canonical schema"):
        Tels.Reconstructor.equivalent(Tels.Reconstructor.fromTel(schemaDoc), Tels.Axiom.acceptance)
      . assert(identity)

      test(m"the axiom validates as a schema"):
        Tels.Validation.validate(Tels.Axiom.acceptance).name
      . assert(_ == t"acceptance")

    suite(m"Resolution (§8.2 step 1)"):
      test(m"the pinned coordinate resolves to the built-in schema"):
        val pragma = Tel.Pragma((1, 0), Tel.Pragma.Reference.acceptance, List(), Unset, Unset)
        val resolved = SchemaResolver.resolve(pragma)
        (resolved.step, resolved.schema.name)
      . assert(_ == (Tels.Resolution.Step.Builtin, t"acceptance"))

      test(m"the coordinate without a version pin resolves too"):
        val reference = Tel.Pragma.Reference(t"specification.tel", t"acceptance", Unset)
        val pragma = Tel.Pragma((1, 0), reference, List(), Unset, Unset)
        SchemaResolver.resolve(pragma).schema.name
      . assert(_ == t"acceptance")

      test(m"the pinned signature resolves to the built-in schema"):
        val pragma = Tel.Pragma((1, 0), Unset, List(), Tel.Acceptance.signature, Unset)
        val resolved = SchemaResolver.resolve(pragma)
        (resolved.step, Base256.encode(resolved.signature))
      . assert(_ == (Tels.Resolution.Step.Builtin, Tel.Acceptance.signature))

      test(m"another version pin is not the built-in schema"):
        val selector = Tel.Pragma.Reference.Selector.Version(2, 0, 0)
        val reference = Tel.Pragma.Reference(t"specification.tel", t"acceptance", selector)
        val pragma = Tel.Pragma((1, 0), reference, List(), Unset, Unset)
        capture[Tels.Resolution.Error](SchemaResolver.resolve(pragma)).reason match
          case Tels.Resolution.Error.Reason.Unresolved(_, _) => true
          case _                                              => false
      . assert(identity)

      test(m"tels still resolves through the built-in table"):
        val pragma = Tel.Pragma((1, 0), Tel.Pragma.Reference.tels, List(), Unset, Unset)
        SchemaResolver.resolve(pragma).schema.name
      . assert(_ == t"tels")

    suite(m"The base-256 and schema-signature codecs"):
      test(m"base-256 encodes a text to the bytes it spells"):
        Tel.Codec.base256.encode(t"ḀḁЂ") match
          case Tel.Codec.Encoded.Bytes(data) => data.readable.toSeq
          case _                             => Seq()
      . assert(_ == Seq[Byte](0, 1, 2))

      test(m"base-256 accepts the empty string"):
        Tel.Codec.base256.encode(t"") match
          case Tel.Codec.Encoded.Bytes(data) => data.length
          case _                             => -1
      . assert(_ == 0)

      test(m"base-256 rejects a character outside the alphabet"):
        Tel.Codec.base256.encode(t"self-contained") match
          case Tel.Codec.Encoded.Invalid(_) => true
          case _                            => false
      . assert(identity)

      test(m"base-256 decodes bytes to their text"):
        Tel.Codec.base256.decode(bytes(0, 1, 2))
      . assert(_ == Tel.Codec.Decoded.Value(t"ḀḁЂ"))

      test(m"schema-signature accepts the pinned signature"):
        Tel.Codec.schemaSignature.encode(Tel.Acceptance.signature) match
          case Tel.Codec.Encoded.Bytes(data) => data.length
          case _                             => -1
      . assert(_ == 33)

      test(m"schema-signature rejects a 34-byte value"):
        Tel.Codec.schemaSignature.encode(Tel.Acceptance.signature+t"Ḁ") match
          case Tel.Codec.Encoded.Invalid(_) => true
          case _                            => false
      . assert(identity)

      test(m"schema-signature rejects a bad cadence byte"):
        Tel.Codec.schemaSignature.encode(Base256.encode(bytes(Seq.fill(33)(0)*))) match
          case Tel.Codec.Encoded.Invalid(_) => true
          case _                            => false
      . assert(identity)

      test(m"schema-signature rejects a character outside the alphabet"):
        Tel.Codec.schemaSignature.encode(t"self-contained") match
          case Tel.Codec.Encoded.Invalid(_) => true
          case _                            => false
      . assert(identity)

      test(m"signatureComponents counts components from the length"):
        val three = bytes((Seq.fill(38)(0) :+ 0x79)*)
        val four  = bytes((Seq.fill(40)(0) :+ 0x79)*)
        (Tel.Codec.signatureComponents(three), Tel.Codec.signatureComponents(four))
      . assert(_ == (3, 4))

      test(m"signatureComponents rejects 35 bytes"):
        Tel.Codec.signatureComponents(bytes((Seq.fill(34)(0) :+ 0x79)*))
      . assert(_ == Unset)

      test(m"the builtins bind both names and nothing else"):
        val bindings = Tel.Codec.Bindings.builtins
        ( bindings(t"base-256").present,
          bindings(t"schema-signature").present,
          bindings(t"decimal-varint").present )
      . assert(_ == (true, true, false))

      test(m"orElse consults the second binding after the first"):
        val second: Tel.Codec.Bindings = name =>
          if name == t"decimal-varint" then Tel.Codec.base256 else Unset

        val bindings = Tel.Codec.Bindings.none.orElse(second)
        (bindings(t"decimal-varint").present, bindings(t"base-256").present)
      . assert(_ == (true, false))

    suite(m"Reading and writing the worked request"):
      lazy val parsed: Tel.Acceptance = request.as[Tel.Acceptance]

      test(m"the request has two alternatives in preference order"):
        parsed.alternatives.map: alternative =>
          (alternative.selfContained, alternative.anyPublished, alternative.components.size)
      . assert(_ == List((false, false, 2), (true, false, 0)))

      test(m"the first alternative names a base with one layer"):
        parsed.alternatives.prim.let(_.schema.count)
      . assert(_ == 2)

      test(m"the second alternative names the base alone"):
        parsed.alternatives match
          case _ :: second :: _ => second.schema.count
          case _                => 0
      . assert(_ == 1)

      test(m"the components are four-byte prefixes"):
        parsed.alternatives.prim.let: alternative =>
          alternative.components.map { component => (component.prefix.length, component.full) }
      . assert(_ == List((4, false), (4, false)))

      test(m"the bare form is 92 bytes, as the specification tabulates"):
        parsed.bintel.length
      . assert(_ == 92)

      test(m"the framed form is 39 bytes longer"):
        parsed.framed.length
      . assert(_ == 131)

      test(m"the bare form round-trips"):
        Tel.Acceptance(parsed.bintel)
      . assert(_ == parsed)

      test(m"the framed form round-trips"):
        Tel.Acceptance(parsed.framed)
      . assert(_ == parsed)

      test(m"the BASE-256 form round-trips"):
        Tel.Acceptance(parsed.text)
      . assert(_ == parsed)

      test(m"the text form round-trips"):
        parsed.encode.as[Tel.Acceptance]
      . assert(_ == parsed)

      test(m"the text form names the pinned coordinate and writes one line per alternative"):
        parsed.encode.cut(t"\n").filter(!_.s.isEmpty).map: line =>
          line.cut(t" ") match
            case first :: _ => first
            case _          => t""
      . assert(_ == List(t"tel", t"accept", t"accept"))

      test(m"the framed form carries the built-in schema's signature"):
        Base256.encode(Bintel.unframe(parsed.framed).signature)
      . assert(_ == Tel.Acceptance.signature)

      test(m"the value hash is that of the bare form, whichever form was read"):
        (hex(Tel.Acceptance(parsed.framed).valueHash.data), hex(parsed.valueHash.data))
      . assert { (framed, bare) => framed == bare }

      test(m"a document with no schema identification is read as an acceptance"):
        val text = t"accept ${Tel.Acceptance.signature} self-contained\n"
        text.as[Tel.Acceptance].alternatives.prim.let(_.selfContained)
      . assert(_ == true)

      test(m"components may be written as compound children"):
        val text =
          t"${header}accept ${Tel.Acceptance.signature}\n  component ḀḁЂЃ\n  component ĄąĆć\n"

        text.as[Tel.Acceptance].alternatives.prim.let(_.components.map(_.text))
      . assert(_ == List(t"ḀḁЂЃ", t"ĄąĆć"))

    suite(m"Rejections"):
      test(m"a malformed signature is E312"):
        capture[Tel.Error](t"${header}accept ḀḀḀḀ\n".as[Tel.Acceptance]).reason.number
      . assert(_ == 312)

      test(m"a component shorter than four bytes is E315"):
        capture[Tel.Error](t"${header}accept ${Tel.Acceptance.signature} ḀḀḀ\n".as[Tel.Acceptance])
        . reason.number
      . assert(_ == 315)

      test(m"a flag written after a component is consumed as a component and rejected"):
        val text = t"${header}accept ${Tel.Acceptance.signature} ḀḀḀḀ self-contained\n"
        capture[Tel.Error](text.as[Tel.Acceptance]).reason.number
      . assert(_ == 312)

      test(m"an acceptance with no alternative is E307"):
        capture[Tel.Error](header.as[Tel.Acceptance]).reason.number
      . assert(_ == 307)

      test(m"a pragma naming another schema is rejected"):
        val text = t"tel 1.0 specification.tel/tels:2.0.0\n\naccept ${Tel.Acceptance.signature}\n"
        capture[Tel.Acceptance.Error](text.as[Tel.Acceptance]).reason match
          case Tel.Acceptance.Error.Reason.WrongSchema(_) => true
          case _                                      => false
      . assert(identity)

      test(m"a pragma with another version pin is rejected"):
        val text = t"tel 1.0 specification.tel/acceptance:2.0.0\n\naccept ${Tel.Acceptance.signature}\n"
        capture[Tel.Acceptance.Error](text.as[Tel.Acceptance]).reason match
          case Tel.Acceptance.Error.Reason.WrongSchema(_) => true
          case _                                      => false
      . assert(identity)

      test(m"a framed document under another signature is rejected"):
        val parsed = request.as[Tel.Acceptance]
        val framed = Bintel.frame(parsed.bintel, Base256.decode(SchemaResolver.telsSignature))
        capture[Tel.Acceptance.Error](Tel.Acceptance(framed)).reason match
          case Tel.Acceptance.Error.Reason.WrongSchema(_) => true
          case _                                      => false
      . assert(identity)

      test(m"BASE-256 text outside the alphabet is a Base256.Error"):
        capture[Base256.Error](Tel.Acceptance(t"not base-256")).reason match
          case Base256.Error.Reason.NotInAlphabet(_) => true
      . assert(identity)

      test(m"Signature rejects a value that is not structurally a signature"):
        capture[Tel.Acceptance.Error](Tel.Acceptance.Signature(bytes(1, 2, 3))).reason
      . assert(_ == Tel.Acceptance.Error.Reason.MalformedSignature(3))

      test(m"Component rejects a three-byte prefix"):
        capture[Tel.Acceptance.Error](Tel.Acceptance.Component(bytes(1, 2, 3))).reason
      . assert(_ == Tel.Acceptance.Error.Reason.ComponentLength(3))

      test(m"Component accepts a full 32-byte hash"):
        Tel.Acceptance.Component(bytes(Seq.fill(32)(7)*)).full
      . assert(identity)
