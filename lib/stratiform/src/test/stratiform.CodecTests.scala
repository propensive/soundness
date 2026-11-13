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
┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛
                                                                                                  */
package stratiform

import soundness.*

import strategies.throwUnsafely
import errorDiagnostics.stackTracesDiagnostics
import codepages.utf8Codepage
import denominative.dysasymptotics.linearSize

// Scalar encodings (spec §21.7): the codec interface and its laws C1–C3,
// the binding mechanism, validation-time E312/E313, and the BinTEL
// encoded-scalar paths with B13–B15. Mirrors the Rust reference's codec
// battery (`ref/tel/src/bintel.rs`) with the same `decimal-varint` toy
// codec: canonical decimal integer text ↔ BinTEL varint bytes, with a
// deliberately lenient decoder (overlong varints accepted) that the B15
// canonicality test exploits.
object CodecTests extends Suite("tel-codec", m"Stratiform codec tests"):

  object DecimalVarint extends Tel.Codec:
    def encode(text: Text): Tel.Codec.Encoded =
      val s = text.s
      val canonical =
        s.nonEmpty && s.forall(_.isDigit) && (s.length == 1 || !s.startsWith("0"))

      if !canonical
      then Tel.Codec.Encoded.Invalid(t"not a canonical decimal integer")
      else Tel.Codec.Encoded.Bytes(Varint.encode(s.toLong))

    // Deliberately lenient, like the reference's toy codec: an overlong
    // varint is accepted (so that B15 has something to catch), unlike the
    // §4 structural varints, which `Varint.decode` rejects as B02.
    def decode(bytes: Data): Tel.Codec.Decoded =
      var value = 0L
      var shift = 0
      var i = 0
      var done = false

      while !done && i < bytes.length && shift < 64 do
        val b = bytes.readUnchecked(i) & 0xff // i ranges over bytes
        value |= (b & 0x7fL) << shift
        shift += 7
        i += 1
        if (b & 0x80) == 0 then done = true

      if !done then Tel.Codec.Decoded.Failure(t"malformed varint")
      else if i != bytes.length then Tel.Codec.Decoded.Failure(t"trailing bytes after varint")
      else Tel.Codec.Decoded.Value(value.toString.tt)

  val bindings: Tel.Codec.Bindings = name =>
    if name == t"decimal-varint" then DecimalVarint else Unset

  private def schemaOf(text: Text): Tels =
    Tels.Validation.validate(text.read[Tel].as[Tels])

  private val amountSchema: Text =
    Text("""|tel 1.0
        |
        |name codec-demo
        |
        |scalar Amount
        |  validate string
        |  encoding decimal-varint
        |
        |document
        |  field amount Amount
        |""".stripMargin)

  case class Collected(codes: List[Int] = Nil)(using Diagnostics)
  extends Error(m"${codes.size} collected codes"):
    def +(code: Int): Collected = Collected(codes :+ code)

  private def validationCodes(schemaText: Text, document: Text): List[Int] =
    val schema = schemaOf(schemaText)
    val tel = document.read[Tel]

    validate[Tel.Focus](Collected()):
      case error: Tel.Error => accrual + error.reason.number
    . protect:
        Tel.Type.assign(tel, schema, Tel.Validator.Registry.builtins, bindings)
        ()
    . codes

  // The first scalar Value reachable from an element tree, depth-first.
  private def firstValue(element: Tel.Element): Optional[Text] = element match
    case value: Tel.Element.Value => value.text

    case node: Tel.Element.Node =>
      var result: Optional[Text] = Unset

      node.children.each: child =>
        if result.absent then result = firstValue(child)

      result

  private def bytesOf(values: Int*): Data = values.map(_.toByte).to[Array]

  def run(): Unit =
    suite(m"Codec laws (C1–C3)"):
      test(m"C1: encoding is deterministic"):
        (DecimalVarint.encode(t"300"), DecimalVarint.encode(t"300")).absolve match
          case (Tel.Codec.Encoded.Bytes(a), Tel.Codec.Encoded.Bytes(b)) =>
            a.stdlib == b.stdlib
      . assert(_ == true)

      test(m"C2: decode inverts encode over accepted texts"):
        scala.List(t"0", t"127", t"128", t"300", t"18446744073709551", t"1")
        . forall: text =>
            DecimalVarint.encode(text).absolve match
              case Tel.Codec.Encoded.Bytes(bytes) => DecimalVarint.decode(bytes).absolve match
                case Tel.Codec.Decoded.Value(recovered) => recovered == text
      . assert(_ == true)

      test(m"C3: encode inverts decode over canonical byte sequences"):
        scala.List(0L, 127L, 128L, 300L)
        . forall: n =>
            val bytes = Varint.encode(n)
            DecimalVarint.decode(bytes).absolve match
              case Tel.Codec.Decoded.Value(text) => DecimalVarint.encode(text).absolve match
                case Tel.Codec.Encoded.Bytes(re) => re.stdlib == bytes.stdlib
      . assert(_ == true)

      test(m"non-canonical texts are rejected"):
        scala.List(t"", t"007", t"12a", t"-1", t" 3")
        . forall: text =>
            DecimalVarint.encode(text) match
              case Tel.Codec.Encoded.Invalid(_) => true
              case _                            => false
      . assert(_ == true)

    suite(m"Validation-time encoding checks (E312/E313)"):
      test(m"a codec-rejected value accrues E312"):
        validationCodes(amountSchema, t"tel 1.0\n\namount 007\n")
      . assert(_ == List(312))

      test(m"an accepted value is clean"):
        validationCodes(amountSchema, t"tel 1.0\n\namount 300\n")
      . assert(_ == List())

      test(m"an unresolved encoding name accrues E313"):
        val schema = Text("""|tel 1.0
                         |
                         |name codec-demo
                         |
                         |scalar Amount
                         |  validate string
                         |  encoding hex-nibbles
                         |
                         |document
                         |  field amount Amount
                         |""".stripMargin)

        validationCodes(schema, t"tel 1.0\n\namount 300\n")
      . assert(_ == List(313))

      test(m"with no binding configured, encoding checks are skipped"):
        val schema = schemaOf(amountSchema)
        val tel = t"tel 1.0\n\namount 007\n".read[Tel]

        validate[Tel.Focus](Collected()):
          case error: Tel.Error => accrual + error.reason.number
        . protect:
            Tel.Type.assign(tel, schema)
            ()
        . codes
      . assert(_ == List())

    suite(m"BinTEL encoded scalars (§7.1)"):
      test(m"an encoded scalar produces exact codec bytes"):
        val schema = schemaOf(amountSchema)
        val element = Tel.Type.assign(t"tel 1.0\n\namount 300\n".read[Tel], schema)
        // child count 1, keyword index 0, byte length 2, varint(300) = AC 02.
        Bintel.encode(element, schema, bindings).stdlib
      . assert(_ == bytesOf(0x01, 0x00, 0x02, 0xAC, 0x02).stdlib)

      test(m"atom-derived and default-derived scalars pass through the codec"):
        val schema = schemaOf(Text("""|tel 1.0
                                  |
                                  |name codec-demo
                                  |
                                  |record Item
                                  |  field amount Amount
                                  |  field count Count 7
                                  |
                                  |scalar Amount
                                  |  validate string
                                  |  encoding decimal-varint
                                  |
                                  |scalar Count
                                  |  validate string
                                  |  encoding decimal-varint
                                  |
                                  |document
                                  |  field item Item
                                  |""".stripMargin))

        val element = Tel.Type.assign(t"tel 1.0\n\nitem 300\n".read[Tel], schema)
        Bintel.encode(element, schema, bindings).stdlib
      . assert(_ == bytesOf(0x01, 0x00, 0x02, 0x00, 0x02, 0xAC, 0x02, 0x01, 0x01, 0x07)
          . stdlib.toSeq)

      test(m"the value hash is taken over the codec bytes"):
        val schema = schemaOf(amountSchema)
        val element = Tel.Type.assign(t"tel 1.0\n\namount 300\n".read[Tel], schema)
        val digest = Blake3.hashOf(Bintel.encode(element, schema, bindings), 32)
        val expected = Blake3.hashOf(bytesOf(0x01, 0x00, 0x02, 0xAC, 0x02), 32)
        digest.stdlib == expected.stdlib
      . assert(_ == true)

      test(m"encoding a codec-rejected value raises"):
        val schema = schemaOf(amountSchema)
        val element = Tel.Type.assign(t"tel 1.0\n\namount 007\n".read[Tel], schema)
        capture[Tel.Error](Bintel.encode(element, schema, bindings)).reason
      . assert(_ == Tel.Error.Reason.EncodingRejected)

      test(m"encoding with no binding raises B13"):
        val schema = schemaOf(amountSchema)
        val element = Tel.Type.assign(t"tel 1.0\n\namount 300\n".read[Tel], schema)
        capture[Bintel.Error](Bintel.encode(element, schema)).reason
      . assert(_ == Bintel.Error.Reason.CodecUnresolved)

      test(m"decoding codec bytes without a binding raises B13"):
        val schema = schemaOf(amountSchema)
        val element = Tel.Type.assign(t"tel 1.0\n\namount 300\n".read[Tel], schema)
        val body = Bintel.encode(element, schema, bindings)
        capture[Bintel.Error](Bintel.decode(body, schema)).reason
      . assert(_ == Bintel.Error.Reason.CodecUnresolved)

      test(m"B14: bytes the codec rejects fail the decode"):
        val schema = schemaOf(amountSchema)
        // 1 child, keyword index 0, length 1, 0x80: a truncated varint.
        val body = bytesOf(0x01, 0x00, 0x01, 0x80)
        capture[Bintel.Error](Bintel.decode(body, schema, bindings)).reason
      . assert(_ == Bintel.Error.Reason.CodecDecodeFailed)

      test(m"non-canonical bytes decode leniently without the B15 check"):
        val schema = schemaOf(amountSchema)
        // Overlong varint for 300: AC 82 00.
        val body = bytesOf(0x01, 0x00, 0x03, 0xAC, 0x82, 0x00)
        firstValue(Bintel.decode(body, schema, bindings))
      . assert(_ == t"300")

      test(m"B15: the canonicality check rejects overlong bytes"):
        val schema = schemaOf(amountSchema)
        val body = bytesOf(0x01, 0x00, 0x03, 0xAC, 0x82, 0x00)
        capture[Bintel.Error](Bintel.decode(body, schema, bindings, checkCanonical = true))
        . reason
      . assert(_ == Bintel.Error.Reason.CodecNoncanonical)

      test(m"an encoded document round-trips through body bytes"):
        val schema = schemaOf(amountSchema)
        val element = Tel.Type.assign(t"tel 1.0\n\namount 300\n".read[Tel], schema)
        val body = Bintel.encode(element, schema, bindings)
        firstValue(Bintel.decode(body, schema, bindings, checkCanonical = true))
      . assert(_ == t"300")

    suite(m"Schema round-trip preserves encodings"):
      test(m"a schema's declared encoding survives BinTEL and reconstruction"):
        // Encode the schema *document* under the axiom, decode it, and
        // reconstruct: the ScalarDefinition must still name its codec.
        // Guards the twin `resolveType` copies (core and binary), either
        // of which could silently drop `encoding`.
        val schemaDoc = amountSchema.read[Tel]
        val body = schemaDoc.bintel(Tels.Axiom.tels)
        val element = Bintel.decode(body, Tels.Axiom.tels)
        val reconstructed = element.as[Tels]
        reconstructed.scalars.seek(_.name == t"Amount").let(_.encoding)
      . assert(_ == t"decimal-varint")

    suite(m"Staged parser (Bintel.Parsable) encoded scalars"):
      given (Payment is Bintel.Parsable) = BintelInlinable.parsable[Payment]

      def paymentBytes: Data =
        val schema = Tels.tels[Payment](t"payment")
        val element = Tel.Type.assign(t"tel 1.0\n\namount 300\n".read[Tel], schema)
        Bintel.encode(element, schema, bindings)

      test(m"the derived schema carries the declared encoding"):
        Tels.tels[Payment](t"payment").document.members.head.absolve match
          case f: Tels.Field => f.fieldType.absolve match
            case s: Tels.Scalar => s.encoding
      . assert(_ == t"decimal-varint")

      test(m"an encoded scalar is written as codec bytes under the derived schema"):
        paymentBytes.stdlib
      . assert(_ == bytesOf(0x01, 0x00, 0x02, 0xAC, 0x02).stdlib)

      test(m"the generated parser decodes an encoded scalar through the codec"):
        Bintel.parse[Payment](paymentBytes, bindings).amount.value
      . assert(_ == 300L)

      test(m"parsing without a binding raises B13"):
        capture[Bintel.Error](Bintel.parse[Payment](paymentBytes)).reason
      . assert(_ == Bintel.Error.Reason.CodecUnresolved)

      test(m"B14: bytes the codec rejects fail the generated parser"):
        val body = bytesOf(0x01, 0x00, 0x01, 0x80)
        capture[Bintel.Error](Bintel.parse[Payment](body, bindings)).reason
      . assert(_ == Bintel.Error.Reason.CodecDecodeFailed)

      test(m"non-canonical bytes parse leniently without the B15 check"):
        val body = bytesOf(0x01, 0x00, 0x03, 0xAC, 0x82, 0x00)
        Bintel.parse[Payment](body, bindings).amount.value
      . assert(_ == 300L)

      test(m"B15: the canonicality check rejects overlong bytes"):
        val body = bytesOf(0x01, 0x00, 0x03, 0xAC, 0x82, 0x00)
        capture[Bintel.Error](Bintel.parse[Payment](body, bindings, checkCanonical = true))
        . reason
      . assert(_ == Bintel.Error.Reason.CodecNoncanonical)

      test(m"an unencoded sibling field still parses as UTF-8 text"):
        val schema = Tels.tels[Invoice](t"invoice")
        val element = Tel.Type.assign(t"tel 1.0\n\namount 300\nmemo lunch\n".read[Tel], schema)
        val body = Bintel.encode(element, schema, bindings)
        given (Invoice is Bintel.Parsable) = BintelInlinable.parsable[Invoice]
        val invoice = Bintel.parse[Invoice](body, bindings)
        (invoice.amount.value, invoice.memo)
      . assert(_ == (300L, t"lunch"))

    suite(m"Standard codecs"):
      def codec(name: Text): Tel.Codec =
        Tel.Codec.Bindings.standard(name).or(throw IllegalArgumentException(name.s))


      def hex(data: Data): Text =
        data.stdlib.map { byte => "%02X".format(byte & 0xff) }.mkString.tt

      def encoded(name: Text, text: Text): Text = codec(name).encode(text) match
        case Tel.Codec.Encoded.Bytes(data)      => hex(data)
        case Tel.Codec.Encoded.Invalid(message) => t"invalid"

      def decoded(name: Text, text: Text): Text = codec(name).encode(text) match
        case Tel.Codec.Encoded.Invalid(message) => t"invalid"

        case Tel.Codec.Encoded.Bytes(data) => codec(name).decode(data) match
          case Tel.Codec.Decoded.Value(text)      => text
          case Tel.Codec.Decoded.Failure(message) => t"failure"

      val vectors: List[(Text, Text, Text)] = List
        ( (t"u8", t"0", t"00"),
          (t"u8", t"255", t"FF"),
          (t"u16", t"256", t"0100"),
          (t"u32", t"4294967295", t"FFFFFFFF"),
          (t"u64", t"1", t"0000000000000001"),
          (t"u64", t"18446744073709551615", t"FFFFFFFFFFFFFFFF"),
          (t"i8", t"-128", t"00"),
          (t"i8", t"-1", t"7F"),
          (t"i8", t"0", t"80"),
          (t"i8", t"127", t"FF"),
          (t"i16", t"-32768", t"0000"),
          (t"i16", t"32767", t"FFFF"),
          (t"i64", t"-9223372036854775808", t"0000000000000000"),
          (t"i64", t"-1", t"7FFFFFFFFFFFFFFF"),
          (t"i64", t"0", t"8000000000000000"),
          (t"i64", t"9223372036854775807", t"FFFFFFFFFFFFFFFF"),
          (t"f64", t"0", t"8000000000000000"),
          (t"f64", t"-0", t"7FFFFFFFFFFFFFFF"),
          (t"f64", t"1", t"BFF0000000000000"),
          (t"f64", t"-1", t"400FFFFFFFFFFFFF"),
          (t"f64", t"0.1", t"BFB999999999999A"),
          (t"f64", t"0.5", t"BFE0000000000000"),
          (t"f64", t"100", t"C059000000000000"),
          (t"f64", t"0.000001", t"BEB0C6F7A0B5ED8D"),
          (t"f64", t"1e-7", t"BE7AD7F29ABCAF48"),
          (t"f64", t"1e+21", t"C44B1AE4D6E2EF50"),
          (t"f64", t"123456789012345680000", t"C41AC53A7E04BCDA"),
          (t"f64", t"3.141592653589793", t"C00921FB54442D18"),
          (t"f64", t"5e-324", t"8000000000000001"),
          (t"f64", t"1.7976931348623157e+308", t"FFEFFFFFFFFFFFFF"),
          (t"f64", t"Infinity", t"FFF0000000000000"),
          (t"f64", t"-Infinity", t"000FFFFFFFFFFFFF"),
          (t"uuid", t"00000000-0000-0000-0000-000000000000", t"00000000000000000000000000000000"),
          (t"uuid", t"0192b1c2-3d4e-7f5a-8b6c-9d0e1f2a3b4c", t"0192B1C23D4E7F5A8B6C9D0E1F2A3B4C"),
          (t"uuid", t"ffffffff-ffff-ffff-ffff-ffffffffffff", t"FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF"),
          ( t"blake3-256",
            t"ed8e981cd770b7d75067e05dfd87d8b5c37e97763872febf513c125ab1a8b245",
            t"ED8E981CD770B7D75067E05DFD87D8B5C37E97763872FEBF513C125AB1A8B245" ) )

      test(m"every normative vector encodes to its bytes"):
        vectors.map { (name, text, bytes) => encoded(name, text) }
      . assert(_ == vectors.map(_(2)))

      test(m"every normative vector decodes back to its text"):
        vectors.map { (name, text, bytes) => decoded(name, text) }
      . assert(_ == vectors.map(_(1)))

      val rejected: List[(Text, Text)] = List
        ( (t"u8", t"256"), (t"u8", t"007"), (t"u8", t"+1"), (t"u8", t"-0"), (t"u8", t"1 "),
          (t"u8", t""), (t"u64", t"18446744073709551616"), (t"i8", t"-0"), (t"i8", t"128"),
          (t"i8", t"-129"), (t"i16", t"--1"), (t"f64", t"1.0"), (t"f64", t"01"), (t"f64", t"1e5"),
          (t"f64", t".5"), (t"f64", t"1E5"), (t"f64", t"+1"), (t"f64", t"1e+05"),
          (t"f64", t"0.10"), (t"f64", t"100000.0"), (t"f64", t"NaN"), (t"f64", t"1e999"),
          (t"uuid", t"0192B1C2-3D4E-7F5A-8B6C-9D0E1F2A3B4C"),
          (t"uuid", t"0192b1c23d4e7f5a8b6c9d0e1f2a3b4c"),
          (t"uuid", t"0192b1c2-3d4e7f5a-8b6c-9d0e-1f2a3b4c"),
          ( t"blake3-256",
            t"ED8E981CD770B7D75067E05DFD87D8B5C37E97763872FEBF513C125AB1A8B245" ),
          (t"blake3-256", t"ed8e98") )

      test(m"a text which is not canonical is rejected"):
        rejected.map(encoded(_, _))
      . assert(_ == rejected.map { pair => t"invalid" })

      test(m"plain renderings of binary64 values are accepted"):
        List(t"1", t"100000", t"0.5", t"0.1").map(decoded(t"f64", _))
      . assert(_ == List(t"1", t"100000", t"0.5", t"0.1"))

      test(m"a subnormal value rounds up into the next power of ten"):
        decoded(t"f64", t"1e-323")
      . assert(_ == t"1e-323")

      test(m"bytes of the wrong width are rejected"):
        codec(t"u32").decode(bytesOf(0x00, 0x01)) match
          case Tel.Codec.Decoded.Failure(message) => true
          case Tel.Codec.Decoded.Value(text)      => false
      . assert(_ == true)

      test(m"the bytes of a NaN are rejected"):
        codec(t"f64").decode(bytesOf(0xff, 0xf8, 0, 0, 0, 0, 0, 0)) match
          case Tel.Codec.Decoded.Failure(message) => true
          case Tel.Codec.Decoded.Value(text)      => false
      . assert(_ == true)

      test(m"the bytes of integers sort as the integers do"):
        val texts = List(t"-9223372036854775808", t"-300", t"-1", t"0", t"1", t"255", t"256")
        val bytes = texts.map(encoded(t"i64", _)).stdlib
        bytes.zip(bytes.tail).forall { (left, right) => left.s.compareTo(right.s) < 0 }
      . assert(_ == true)

      test(m"a name which is not a standard codec does not resolve"):
        Tel.Codec.Bindings.standard(t"u128").absent
      . assert(_ == true)

// Fixtures for the staged encoded-scalar tests: a scalar type whose BinTEL
// form is the `decimal-varint` codec's bytes, declared once via the
// `Tel.Encoded` marker — the schema derivation and the staged parser both
// read the same declaration.
class Money(val value: Long)

object Money:
  given decodable: Money is Decodable in Text = text => Money(text.s.toLong)
  given encoded: Money is Tel.Encoded["decimal-varint"] = Tel.Encoded()

case class Payment(amount: Money)
case class Invoice(amount: Money, memo: Text)
