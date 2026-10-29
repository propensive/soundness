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

import scala.language.unsafeNulls

import java.lang as jl

import adversaria.name
import anticipation.*
import aperture.*
import contingency.*
import denominative.*
import denominative.dysasymptotics.linearSize // the suite reads list sizes; the cost is irrelevant here
import fulminate.*
import larceny.*
import gastronomy.*
import gossamer.*
import hieroglyph.*
import panopticon.*
import parasite.*

// The `Tel` lens and optic instances live in `stratiform.optics` now, outside `Tel`'s implicit
// scope, so they must be imported by name.
import stratiform.optics.{telEachOptical, telLens, telOrdinalOptical}
import prepositional.*
import probably.*
import rudiments.*
import spectacular.*
import turbulence.*
import vacuous.*
import zephyrine.*

import zephyrine.lineation.linefeedByte

import strategies.throwUnsafely
import errorDiagnostics.stackTracesDiagnostics
import codepages.utf8Codepage
import threads.virtualThreads
import probates.cancelProbate
import Tel.given
import Tests.*

// The BinTEL and direct-parsing suites, split from `Tests` so that neither object exceeds the
// JVM's constant-pool limit.
object BintelTests extends Suite("bintel", m"Stratiform BinTEL tests"):
  def run(): Unit =
    suite(m"BASE-256 codec"):
      test(m"alphabet has 256 entries"):
        Base256.alphabet.length
      . assert(_ == 256)

      test(m"alphabet satisfies codepoint ≡ index (mod 256)"):
        (0 until 256).forall(i => Base256.alphabet.at(i.z).lay(false)(_.toInt % 256 == i))
      . assert(_ == true)

      test(m"alphabet entries are pairwise distinct"):
        Base256.alphabet.to[Set].size
      . assert(_ == 256)

      test(m"ASCII digits encode to themselves"):
        (0x30 to 0x39).forall(b => Base256.alphabet.at(b.z) == b.toChar)
      . assert(_ == true)

      test(m"ASCII uppercase letters encode to themselves"):
        (0x41 to 0x5A).forall(b => Base256.alphabet.at(b.z) == b.toChar)
      . assert(_ == true)

      test(m"ASCII lowercase letters encode to themselves"):
        (0x61 to 0x7A).forall(b => Base256.alphabet.at(b.z) == b.toChar)
      . assert(_ == true)

      test(m"round-trip all 256 byte values"):
        val data: Data = (0 to 255).map(_.toByte).toArray.asInstanceOf[Array[Byte]]
        Base256.decode(Base256.encode(data)).to[List]
      . assert(_ == List.from((0 to 255).map(_.toByte)))

      test(m"empty bytes round-trip to empty text"):
        Base256.encode(Array.empty[Byte])
      . assert(_ == t"")

      test(m"empty text round-trips to empty bytes"):
        Base256.decode(t"").length
      . assert(_ == 0)

      test(m"encoded length in characters equals input length in bytes"):
        val data: Data = (0 to 255).map(_.toByte).toArray.asInstanceOf[Array[Byte]]
        Base256.encode(data).s.length
      . assert(_ == 256)

      test(m"permissive decode accepts non-alphabet chars by residue"):
        Base256.decode(t"A ").stdlib
      . assert(_ == Seq(0x41.toByte, 0x20.toByte))

      test(m"strict decode accepts the alphabet"):
        val data: Data = (0 to 255).map(_.toByte).toArray.asInstanceOf[Array[Byte]]
        Base256.decodeStrict(Base256.encode(data)).to[List]
      . assert(_ == List.from((0 to 255).map(_.toByte)))

      test(m"strict decode reports every non-alphabet character by code-point index"):
        val input = Text("A B" + new String(Character.toChars(0x1f600)) + "C")
        capture[Base256.Error](Base256.decodeStrict(input)).reason match
          case Base256.Error.Reason.NotInAlphabet(offenders) =>
            offenders.map(o => (o.position, o.codepoint))
      . assert(_ == scala.collection.immutable.List((1, 0x20), (3, 0x1f600)))

      test(m"permissive decode counts a supplementary character as one character"):
        Base256.decode(Text("A" + new String(Character.toChars(0x1f600)))).size
      . assert(_ == 2)

      // §11 normative test vectors.
      val vectors: scala.collection.immutable.List[(scala.collection.immutable.List[Int], String)] =
        scala.collection.immutable.List
          ( scala.collection.immutable.List(0x00)             -> "Ḁ",
            scala.collection.immutable.List(0x01)             -> "ḁ",
            scala.collection.immutable.List(0x30)             -> "0",
            scala.collection.immutable.List(0x41)             -> "A",
            scala.collection.immutable.List(0x7f)             -> "ſ",
            scala.collection.immutable.List(0xff)             -> "ǿ",
            scala.collection.immutable.List(0x00, 0x01, 0xff) -> "Ḁḁǿ",
            scala.collection.immutable.List(0x41, 0x42, 0x43) -> "ABC" )

      vectors.foreach: (bytes, encoded) =>
        test(m"§11 vector: ${Text(bytes.map(b => f"$b%02X").mkString(" "))} encodes to $encoded"):
          Base256.encode(bytes.map(_.toByte).toArray.asInstanceOf[Array[Byte]]).s
        . assert(_ == encoded)

        test(m"§11 vector: $encoded decodes strictly"):
          Base256.decodeStrict(Text(encoded)).to[List].map(_ & 0xff)
        . assert(_ == List.from(bytes))

      test(m"§11 identity vector: encoding 00..FF yields the alphabet"):
        val data: Data = (0 to 255).map(_.toByte).toArray.asInstanceOf[Array[Byte]]
        Base256.encode(data).s == Base256.alphabet.stdlib.mkString
      . assert(_ == true)

      test(m"§11 shared vector: the tels value hash"):
        val hex = "d440b01e327c62c41ac641047f2c4d8df3cbe94abb24db33f189226b7b8b7ad3"
        val bytes = hex.grouped(2).map(java.lang.Integer.parseInt(_, 16).toByte).toArray
        Base256.encode(bytes.asInstanceOf[Array[Byte]]).s
      . assert(_ == "ÔŀưḞ2żbτȚÆAĄſЬMẍỳϋῩJλḤӛ3ñẉḢkŻẋzǓ")

    suite(m"BinTEL §4 varint"):
      def hex(data: Data): String =
        val sb = new java.lang.StringBuilder
        var i = 0
        while i < data.length do
          sb.append(f"${data.readUnchecked(i) & 0xff}%02X") // bounded by the loop
          if i + 1 < data.length then sb.append(' ')
          i += 1
        sb.toString

      val vectors: List[(Long, String)] = List(
        0L     -> "00",
        1L     -> "01",
        127L   -> "7F",
        128L   -> "80 01",
        255L   -> "FF 01",
        16383L -> "FF 7F",
        16384L -> "80 80 01"
      )

      vectors.each: (value, expected) =>
        test(m"encodes $value as $expected"):
          hex(Varint.encode(value))
        . assert(_ == expected)

        test(m"decodes $expected back to $value"):
          val parts = expected.split(" ").map(java.lang.Integer.parseInt(_, 16).toByte)
          Varint.decode(parts.asInstanceOf[Array[Byte]], 0).value
        . assert(_ == value)

      test(m"round-trips every value in 0..1023"):
        (0L to 1023L).forall: n =>
          Varint.decode(Varint.encode(n), 0).value == n
      . assert(identity)

      test(m"round-trips powers of two up to 2^62"):
        (0 to 62).map(_.toLong).forall: i =>
          val n = 1L << i
          Varint.decode(Varint.encode(n), 0).value == n
      . assert(identity)

      test(m"decode returns next offset"):
        val data: Data = scala.Array[Byte](0x80.toByte, 0x01, 0x42).asInstanceOf[Array[Byte]]
        Varint.decode(data, 0).next
      . assert(_ == 2)

      test(m"decode raises on truncated continuation"):
        val data: Data = scala.Array[Byte](0x80.toByte).asInstanceOf[Array[Byte]]
        capture[Varint.Error](Varint.decode(data, 0)).reason
      . assert(_ == Varint.Error.Reason.Truncated)

      test(m"decode rejects an overlong encoding"):
        val data: Data = scala.Array[Byte](0x80.toByte, 0x00).asInstanceOf[Array[Byte]]
        capture[Varint.Error](Varint.decode(data, 0)).reason
      . assert(_ == Varint.Error.Reason.Overlong)

      test(m"decode rejects a padded non-zero encoding"):
        val data: Data = scala.Array[Byte](0x81.toByte, 0x00).asInstanceOf[Array[Byte]]
        capture[Varint.Error](Varint.decode(data, 0)).reason
      . assert(_ == Varint.Error.Reason.Overlong)

      test(m"decode rejects a value wider than 64 bits"):
        val data: Data =
          scala.Array.fill[Byte](9)(0xff.toByte).:+(0x02.toByte).asInstanceOf[Array[Byte]]
        capture[Varint.Error](Varint.decode(data, 0)).reason
      . assert(_ == Varint.Error.Reason.Overflow)

      test(m"encode rejects negative input"):
        try
          Varint.encode(-1L)
          false
        catch case _: IllegalArgumentException => true
      . assert(identity)

    val nameSchema = Tels(
      name     = t"contact",
      document = Tels.Struct(
        members = Array(Tels.Field
         ( Tels.Polarity.Implicit, Tels.Polarity.Implicit,
           t"name", Tels.Scalar(Array(t"string")), Unset )),
        validators = Array.empty),
      layers   = Array.empty,
      sigil    = Unset,
      records  = Array.empty,
      scalars  = Array.empty,
      selects  = Array.empty)

    def hex(data: Data): String =
      val sb = new java.lang.StringBuilder
      var i = 0
      while i < data.length do
        sb.append(f"${data.readUnchecked(i) & 0xff}%02X") // bounded by the loop
        if i + 1 < data.length then sb.append(' ')
        i += 1
      sb.toString

    def hexBytes(s: String): Seq[Byte] =
      val arr = new scala.Array[Byte](s.length / 2)
      var i = 0
      while i < arr.length do
        arr(i) = jl.Integer.parseInt(s.substring(i * 2, i * 2 + 2), 16).toByte
        i += 1
      arr.toSeq

    suite(m"BinTEL §7 node encoder"):

      test(m"empty struct encodes as a single 00 child-count"):
        val root = Tel.Element.Node(Unset, nameSchema.document, Array.empty)
        hex(root.bintel(nameSchema))
      . assert(_ == "00")

      test(m"single scalar child via tel.bintel(schema)"):
        hex(t"name Alice\n".read[Tel].bintel(nameSchema))
      . assert(_ == "01 00 05 41 6C 69 63 65")

      test(m"empty scalar value encodes as zero-length"):
        val scalar = Tels.Scalar(Array.empty)
        val value = Tel.Element.Value(0, scalar, t"")
        val root = Tel.Element.Node(Unset, nameSchema.document, Array(value))
        hex(root.bintel(nameSchema))
      . assert(_ == "01 00 00")

      test(m"UTF-8 byte length is encoded, not character count"):
        // "café" = 0x63 0x61 0x66 0xC3 0xA9 = 5 bytes, 4 chars
        val scalar = Tels.Scalar(Array.empty)
        val value = Tel.Element.Value(0, scalar, t"café")
        val root = Tel.Element.Node(Unset, nameSchema.document, Array(value))
        hex(root.bintel(nameSchema))
      . assert(_ == "01 00 05 63 61 66 C3 A9")

      test(m"flag node encodes as just its keyword index"):
        val flagNode = Tel.Element.Node(0, Tels.Flag, Array.empty)
        val flagSchema = Tels(
          name     = t"feature",
          document = Tels.Struct(
            members = Array(Tels.Field
             ( Tels.Polarity.Loose, Tels.Polarity.Implicit,
               t"enabled", Tels.Flag, Unset )),
            validators = Array.empty),
          layers   = Array.empty,
          sigil    = Unset,
          records  = Array.empty,
          scalars  = Array.empty,
          selects  = Array.empty)
        val root = Tel.Element.Node(Unset, flagSchema.document, Array(flagNode))
        hex(root.bintel(nameSchema))
      . assert(_ == "01 00")

      test(m"nested struct emits kidx + count + children recursively"):
        val innerScalar = Tels.Scalar(Array.empty)
        val innerStruct = Tels.Struct(
          members = Array(Tels.Field
           ( Tels.Polarity.Implicit, Tels.Polarity.Implicit,
             t"host", innerScalar, Unset )),
          validators = Array.empty)
        val outerStruct = Tels.Struct(
          members = Array(Tels.Field
           ( Tels.Polarity.Implicit, Tels.Polarity.Implicit,
             t"config", innerStruct, Unset )),
          validators = Array.empty)

        val configNode = Tel.Element.Node(
          0, innerStruct,
          Array(Tel.Element.Value(0, innerScalar, t"example.com")))

        val root = Tel.Element.Node(Unset, outerStruct, Array(configNode))
        hex(root.bintel(nameSchema))
      . assert(_ == "01 00 01 00 0B 65 78 61 6D 70 6C 65 2E 63 6F 6D")

      test(m"large keyword index uses multi-byte varint"):
        val scalar = Tels.Scalar(Array.empty)
        val value = Tel.Element.Value(128, scalar, t"x")
        val root = Tel.Element.Node(Unset, nameSchema.document, Array(value))
        hex(root.bintel(nameSchema))
      . assert(_ == "01 80 01 01 78")

      test(m"§7.2 canonical order — children reordered by member index"):
        // Build a root whose children appear in reverse member order in
        // source. The encoder must emit them in member order so that
        // independent member groups produce identical bytes regardless
        // of source ordering.
        val scalar = Tels.Scalar(Array.empty)
        val struct = Tels.Struct(
          members = Array(
            Tels.Field(Tels.Polarity.Implicit, Tels.Polarity.Implicit,
                       t"first",  scalar, Unset),
            Tels.Field(Tels.Polarity.Implicit, Tels.Polarity.Implicit,
                       t"second", scalar, Unset)),
          validators = Array.empty)
        val children = Array[Tel.Element](
          Tel.Element.Value(1, scalar, t"B"),
          Tel.Element.Value(0, scalar, t"A"))
        val root = Tel.Element.Node(Unset, struct, children)
        hex(root.bintel(nameSchema))
      . assert(_ == "02 00 01 41 01 01 42")

      test(m"§7.2 canonical order is stable within a member"):
        // Two values at the same member index must stay in source order.
        val scalar = Tels.Scalar(Array.empty)
        val struct = Tels.Struct(
          members = Array(Tels.Field(Tels.Polarity.Implicit, Tels.Polarity.Loose,
                                       t"item", scalar, Unset)),
          validators = Array.empty)
        val children = Array[Tel.Element](
          Tel.Element.Value(0, scalar, t"first"),
          Tel.Element.Value(0, scalar, t"second"))
        val root = Tel.Element.Node(Unset, struct, children)
        // 2 children, then two Value(0, len, text)s.
        hex(root.bintel(nameSchema))
      . assert(_ == "02 00 05 66 69 72 73 74 00 06 73 65 63 6F 6E 64")

    suite(m"BinTEL §7.8 decoder"):
      test(m"empty struct round-trips"):
        val root = Tel.Element.Node(Unset, nameSchema.document, Array.empty)
        val bytes = root.bintel(nameSchema)
        val decoded = Bintel.decode(bytes, nameSchema)
        decoded match
          case Tel.Element.Node(_, _, c) => c.length
          case _                          => -1
      . assert(_ == 0)

      test(m"single scalar value round-trips"):
        val original = t"name Alice\n".read[Tel]
        val bytes = original.bintel(nameSchema)
        val decoded = Bintel.decode(bytes, nameSchema)
        decoded match
          case Tel.Element.Node(_, _, children) =>
            children.to[List].sweep:
              case Tel.Element.Value(_, _, t) => t
          case _ => Nil
      . assert(_ == List(t"Alice"))

      test(m"a value derives its own schema and round-trips through BinTEL"):
        val shape: Tests.Shape2 = Tests.Shape2.Rectangle(3, 4)
        val schema = Tels.tels[Tests.Shape2](t"shape")

        def values(element: Tel.Element): List[Text] = element match
          case Tel.Element.Node(_, _, children) => children.to[List].bind(values)
          case Tel.Element.Value(_, _, text)    => List(text)

        values(Bintel.decode(shape.bintel, schema))
      . assert(_ == List(t"3", t"4"))

      test(m"a sum round-trips bytes-to-typed-value through bintel/read"):
        val shape: Tests.Shape2 = Tests.Shape2.Rectangle(3, 4)
        Bintel.read[Tests.Shape2](shape.bintel)
      . assert(_ == Tests.Shape2.Rectangle(3, 4))

      test(m"a fieldless variant round-trips bytes-to-typed-value"):
        val shape: Tests.Shape2 = Tests.Shape2.Dot
        Bintel.read[Tests.Shape2](shape.bintel)
      . assert(_ == Tests.Shape2.Dot)

      test(m"a product round-trips bytes-to-typed-value through bintel/read"):
        Bintel.read[Tests.Person](Tests.Person(t"Alice", 30).bintel)
      . assert(_ == Tests.Person(t"Alice", 30))

      test(m"empty scalar value round-trips"):
        val scalar = Tels.Scalar(Array.empty)
        val root = Tel.Element.Node
                    (Unset, nameSchema.document, Array(Tel.Element.Value(0, scalar, t"")))
        val bytes = root.bintel(nameSchema)
        val decoded = Bintel.decode(bytes, nameSchema)
        decoded match
          case Tel.Element.Node(_, _, children) =>
            children.to[List].sweep:
              case Tel.Element.Value(_, _, t) => t
          case _ => Nil
      . assert(_ == List(t""))

      test(m"UTF-8 multi-byte scalar round-trips"):
        val scalar = Tels.Scalar(Array.empty)
        val root = Tel.Element.Node
                    (Unset, nameSchema.document, Array(Tel.Element.Value(0, scalar, t"café")))
        val bytes = root.bintel(nameSchema)
        val decoded = Bintel.decode(bytes, nameSchema)
        decoded match
          case Tel.Element.Node(_, _, children) =>
            children.to[List].sweep:
              case Tel.Element.Value(_, _, t) => t
          case _ => Nil
      . assert(_ == List(t"café"))

      test(m"flag element round-trips"):
        val flagSchema = Tels(
          name     = t"feature",
          document = Tels.Struct(
            members = Array(Tels.Field
             ( Tels.Polarity.Loose, Tels.Polarity.Implicit,
               t"enabled", Tels.Flag, Unset )),
            validators = Array.empty),
          layers   = Array.empty,
          sigil    = Unset,
          records  = Array.empty,
          scalars  = Array.empty,
          selects  = Array.empty)
        val root = Tel.Element.Node
                    (Unset, flagSchema.document, Array(Tel.Element.Node(0, Tels.Flag, Array.empty)))
        val bytes = root.bintel(nameSchema)
        val decoded = Bintel.decode(bytes, flagSchema)
        decoded match
          case Tel.Element.Node(_, _, Array(Tel.Element.Node(_, Tels.Flag, _))) => true
          case _                                                                  => false
      . assert(identity)

      test(m"nested struct round-trips"):
        val innerScalar = Tels.Scalar(Array.empty)
        val innerStruct = Tels.Struct(
          members = Array(Tels.Field
           ( Tels.Polarity.Implicit, Tels.Polarity.Implicit,
             t"host", innerScalar, Unset )),
          validators = Array.empty)
        val outerStruct = Tels.Struct(
          members = Array(Tels.Field
           ( Tels.Polarity.Implicit, Tels.Polarity.Implicit,
             t"config", innerStruct, Unset )),
          validators = Array.empty)
        val outerSchema = Tels(
          name = t"app", document = outerStruct, layers = Array.empty,
          sigil = Unset, records = Array.empty, scalars = Array.empty,
          selects = Array.empty)

        val configNode = Tel.Element.Node(
          0, innerStruct,
          Array(Tel.Element.Value(0, innerScalar, t"example.com")))
        val root = Tel.Element.Node(Unset, outerStruct, Array(configNode))

        val bytes = root.bintel(nameSchema)
        val decoded = Bintel.decode(bytes, outerSchema)
        decoded match
          case Tel.Element.Node(_, _, Array(Tel.Element.Node(_, _, inner))) =>
            inner.to[List].sweep:
              case Tel.Element.Value(_, _, t) => t
          case _ => Nil
      . assert(_ == List(t"example.com"))

      test(m"trailing bytes after document root raise Bintel.Error"):
        val original = t"name Alice\n".read[Tel]
        val bytes = original.bintel(nameSchema)
        val padded = bytes :+ 0xff.toByte
        capture[Bintel.Error](Bintel.decode(padded, nameSchema)).reason
      . assert(_ == Bintel.Error.Reason.TrailingBytes)

      test(m"truncated input raises Bintel.Error"):
        val original = t"name Alice\n".read[Tel]
        val bytes = original.bintel(nameSchema)
        val truncated = bytes.segment(0.z till (bytes.length - 1).z)
        // B02 (inside a varint), B06 (inside a value) or B09 (otherwise),
        // depending on where the truncation lands (§10 precedence).
        val reason = capture[Bintel.Error](Bintel.decode(truncated, nameSchema)).reason
        reason == Bintel.Error.Reason.UnexpectedEoi ||
          reason == Bintel.Error.Reason.ValueTruncated ||
          reason == Bintel.Error.Reason.VarintError
      . assert(identity)

      test(m"an overlong varint in a body is B02"):
        val bytes: Data = scala.Array[Byte](0x81.toByte, 0x00).asInstanceOf[Array[Byte]]
        capture[Bintel.Error](Bintel.decode(bytes, nameSchema)).reason
      . assert(_ == Bintel.Error.Reason.VarintError)

      test(m"a body with no bytes at all is B02"):
        capture[Bintel.Error](Bintel.decode(Array.empty[Byte], nameSchema)).reason
      . assert(_ == Bintel.Error.Reason.VarintError)

      test(m"a child count exceeding the bytes remaining is rejected"):
        // Count 100 with nothing after it: unsatisfiable, so rejected before
        // any allocation (§11).
        val bytes: Data = scala.Array[Byte](0x64).asInstanceOf[Array[Byte]]
        capture[Bintel.Error](Bintel.decode(bytes, nameSchema)).reason
      . assert(_ == Bintel.Error.Reason.UnexpectedEoi)

      test(m"out-of-range keyword index raises Bintel.Error"):
        // Manually craft a body with one child whose keyword index is
        // out of range for the schema. nameSchema has 1 flat-keyword
        // entry (index 0); we use index 5.
        val bytes: Data =
          scala.Array[Byte](
            0x01,                   // child-count 1
            0x05,                   // keyword index 5 (out of range)
            0x00                    // scalar length 0
          ).asInstanceOf[Array[Byte]]
        capture[Bintel.Error](Bintel.decode(bytes, nameSchema)).reason
      . assert(_ == Bintel.Error.Reason.BadKeywordIndex)

    suite(m"BinTEL §6 file framing"):
      val sig32: Data = scala.Array.fill[Byte](32)(0x55.toByte).asInstanceOf[Array[Byte]]
      val sig34: Data = scala.Array.fill[Byte](34)(0xAA.toByte).asInstanceOf[Array[Byte]]

      test(m"magic number bytes are B2 C4 B5 BB"):
        hex(Bintel.magic)
      . assert(_ == "B2 C4 B5 BB")

      def concat(a: Data, b: Data): Data =
        Array.concat(a, b)

      test(m"frame prepends magic, document length, signature-length varint, signature"):
        val body: Data = scala.Array[Byte](0x01, 0x02).asInstanceOf[Array[Byte]]
        val framed = Bintel.frame(body, sig32)
        // magic (4) + document length varint (1: 0x23) + sigLen varint (1: 0x20)
        // + signature (32) + body (2) = 40
        framed.length
      . assert(_ == 40)

      test(m"frame writes the document length, then the signature length, after the magic"):
        val body: Data = scala.Array[Byte](0x01).asInstanceOf[Array[Byte]]
        val framed = Bintel.frame(body, sig32)
        // The document length counts every byte after itself: 1 + 32 + 1 = 34.
        framed.segment(0.z till 6.z).to[List]
      . assert(_ == List(0xB2.toByte, 0xC4.toByte, 0xB5.toByte, 0xBB.toByte, 0x22.toByte, 0x20.toByte))

      test(m"frame rejects too-short signature"):
        val tooShort: Data = scala.Array.fill[Byte](1)(0).asInstanceOf[Array[Byte]]
        val body: Data     = Array.empty[Byte]
        capture[Bintel.Error](Bintel.frame(body, tooShort)).reason
      . assert(_ == Bintel.Error.Reason.BadSignatureLength)

      test(m"frame rejects signature with reserved hash-size index"):
        // XOR-fold ⇒ 0xA0, naming reserved s = 10
        val bad: Data = scala.Array[Byte](0xA0.toByte, 0, 0, 0, 0).asInstanceOf[Array[Byte]]
        val body: Data = Array.empty[Byte]
        capture[Bintel.Error](Bintel.frame(body, bad)).reason
      . assert(_ == Bintel.Error.Reason.BadSignatureLength)

      test(m"unframe recovers signature, body and continuation"):
        val body: Data = scala.Array[Byte](0x01, 0x02, 0x03).asInstanceOf[Array[Byte]]
        val framed = Bintel.frame(body, sig32)
        val Bintel.Framed(sig, recovered, continuation) = Bintel.unframe(framed)
        (sig.stdlib, recovered.stdlib, continuation)
      . assert(_ == (sig32.stdlib, Seq[Byte](0x01, 0x02, 0x03), 41))

      test(m"unframe rejects bad magic"):
        val bytes: Data = scala.Array.fill[Byte](40)(0).asInstanceOf[Array[Byte]]
        capture[Bintel.Error](Bintel.unframe(bytes)).reason
      . assert(_ == Bintel.Error.Reason.BadMagic)

      test(m"a document length exceeding the input is B09"):
        val bytes: Data =
          scala.Array[Byte](0xB2.toByte, 0xC4.toByte, 0xB5.toByte, 0xBB.toByte, 0x20.toByte)
            .asInstanceOf[Array[Byte]]
        capture[Bintel.Error](Bintel.unframe(bytes)).reason
      . assert(_ == Bintel.Error.Reason.UnexpectedEoi)

      test(m"a truncated document length varint is B02"):
        val bytes: Data =
          scala.Array[Byte](0xB2.toByte, 0xC4.toByte, 0xB5.toByte, 0xBB.toByte, 0x80.toByte)
            .asInstanceOf[Array[Byte]]
        capture[Bintel.Error](Bintel.unframe(bytes)).reason
      . assert(_ == Bintel.Error.Reason.VarintError)

      test(m"a truncated magic number is B09"):
        val bytes: Data = scala.Array[Byte](0xB2.toByte, 0xC4.toByte).asInstanceOf[Array[Byte]]
        capture[Bintel.Error](Bintel.unframe(bytes)).reason
      . assert(_ == Bintel.Error.Reason.UnexpectedEoi)

      test(m"documentExtent delimits a document without decoding it"):
        val body: Data = scala.Array[Byte](0x01, 0x02, 0x03).asInstanceOf[Array[Byte]]
        val framed = Bintel.frame(body, sig32)
        Bintel.documentExtent(framed)
      . assert(_ == 41)

      test(m"documentExtent of a document in a stream is its continuation offset"):
        val body: Data = scala.Array[Byte](0x01, 0x02, 0x03).asInstanceOf[Array[Byte]]
        val framed = Bintel.frame(body, sig32)
        val stream = concat(framed, framed)
        Bintel.documentExtent(stream, Bintel.documentExtent(stream))
      . assert(_ == 82)

      test(m"larger signatures of permitted lengths are accepted"):
        val body: Data = scala.Array[Byte](0x01).asInstanceOf[Array[Byte]]
        val framed = Bintel.frame(body, sig34)
        Bintel.unframe(framed).signature.length
      . assert(_ == 34)

      test(m"frame ↔ unframe round-trip for non-trivial body"):
        val original: Data = (0 to 99).map(_.toByte).toArray.asInstanceOf[Array[Byte]]
        val framed = Bintel.frame(original, sig32)
        val recovered = Bintel.unframe(framed).body.stdlib
        recovered == original.stdlib
      . assert(_ == true)

      test(m"tel.bintelDocument produces a file beginning with magic"):
        val bytes = t"name Alice\n".read[Tel].bintelDocument(nameSchema, sig32)
        bytes.segment(0.z till 4.z).to[List]
      . assert(_ == List(0xB2.toByte, 0xC4.toByte, 0xB5.toByte, 0xBB.toByte))

      test(m"decodeDocument round-trips through frame + decode"):
        val bytes = t"name Alice\n".read[Tel].bintelDocument(nameSchema, sig32)
        val doc = Bintel.decodeDocument(bytes, nameSchema)
        doc.root match
          case Tel.Element.Node(_, _, children) =>
            children.to[List].sweep:
              case Tel.Element.Value(_, _, t) => t
          case _ => Nil
      . assert(_ == List(t"Alice"))

      def firstName(document: Bintel.Document): Text = document.root match
        case Tel.Element.Node(_, _, children) =>
          children.to[List].sweep { case Tel.Element.Value(_, _, t) => t }.head.or(t"?")
        case _ => t"?"

      test(m"decodeDocument reports the continuation offset"):
        val bytes = t"name Alice\n".read[Tel].bintelDocument(nameSchema, sig32)
        Bintel.decodeDocument(bytes, nameSchema).continuation == bytes.length
      . assert(_ == true)

      test(m"single-document decoding ignores the continuation"):
        val bytes = t"name Alice\n".read[Tel].bintelDocument(nameSchema, sig32)
        val junk: Data = scala.Array[Byte](0x7f, 0x7f).asInstanceOf[Array[Byte]]
        val doc = Bintel.decodeDocument(concat(bytes, junk), nameSchema)
        (firstName(doc), doc.continuation == bytes.length)
      . assert(_ == (t"Alice", true))

      test(m"a declared length longer than the structure is B16"):
        val bytes = t"name Alice\n".read[Tel].bintelDocument(nameSchema, sig32)
        val forged = Array.allocate[Byte](bytes.length + 1)
        forged.place(bytes)
        forged(4) = (bytes.at(4.z).or(0.toByte) + 1).toByte
        capture[Bintel.Error](Bintel.decodeDocument(Array.freeze(forged), nameSchema))
        . reason
      . assert(_ == Bintel.Error.Reason.DeclaredLengthMismatch)

      test(m"a declared length shorter than the structure fails inside the body"):
        val bytes = t"name Alice\n".read[Tel].bintelDocument(nameSchema, sig32)
        val forged = Array.thaw(bytes)
        forged(4) = (bytes.at(4.z).or(0.toByte) - 1).toByte
        capture[Bintel.Error](Bintel.decodeDocument(Array.freeze(forged), nameSchema))
        . reason
      . assert(_ == Bintel.Error.Reason.ValueTruncated)

      test(m"decodeWholeDocument accepts exactly one document"):
        val bytes = t"name Alice\n".read[Tel].bintelDocument(nameSchema, sig32)
        firstName(Bintel.decodeWholeDocument(bytes, nameSchema))
      . assert(_ == t"Alice")

      test(m"decodeWholeDocument rejects a continuation as B08"):
        val bytes = t"name Alice\n".read[Tel].bintelDocument(nameSchema, sig32)
        capture[Bintel.Error](Bintel.decodeWholeDocument(concat(bytes, bytes), nameSchema)).reason
      . assert(_ == Bintel.Error.Reason.TrailingBytes)

      test(m"decodeStream yields every document of a stream in order"):
        val alice = t"name Alice\n".read[Tel].bintelDocument(nameSchema, sig32)
        val bob   = t"name Bob\n".read[Tel].bintelDocument(nameSchema, sig32)
        Bintel.decodeStream(concat(alice, bob), nameSchema).map(firstName)
      . assert(_ == List(t"Alice", t"Bob"))

      test(m"decodeStream reports each document's continuation"):
        val alice = t"name Alice\n".read[Tel].bintelDocument(nameSchema, sig32)
        val bob   = t"name Bob\n".read[Tel].bintelDocument(nameSchema, sig32)
        val docs = Bintel.decodeStream(concat(alice, bob), nameSchema).map(_.continuation)
        docs == List(alice.length, alice.length + bob.length)
      . assert(_ == true)

      test(m"decodeStream of empty input yields no documents"):
        Bintel.decodeStream(Array.empty[Byte], nameSchema).map(_.continuation)
      . assert(_ == List())

      test(m"a continuation that is not BinTEL is B01 at that position"):
        val alice = t"name Alice\n".read[Tel].bintelDocument(nameSchema, sig32)
        val junk: Data = scala.Array[Byte](0x7f, 0x7f, 0x7f, 0x7f, 0x7f).asInstanceOf[Array[Byte]]
        capture[Bintel.Error](Bintel.decodeStream(concat(alice, junk), nameSchema)).reason
      . assert(_ == Bintel.Error.Reason.BadMagic)

    suite(m"BinTEL §9 textual encoding"):
      val sig32: Data = scala.Array.fill[Byte](32)(0x55.toByte).asInstanceOf[Array[Byte]]

      test(m"text begins with βτελ (the four BASE-256 chars for the magic bytes)"):
        val bytes = t"name Alice\n".read[Tel].bintelDocument(nameSchema, sig32)
        Bintel.text(bytes).s.substring(0, 4)
      . assert(_ == "βτελ")

      test(m"text/fromText round-trip"):
        val source = t"name Alice\n".read[Tel].bintelDocument(nameSchema, sig32)
        val text = Bintel.text(source)
        val recovered = Bintel.fromText(text)
        recovered.stdlib == source.stdlib
      . assert(_ == true)

    suite(m"BinTEL §8.2 schema signature"):
      // BinTEL-pinned Cadence(initial = 4, regular = 2, hashSize = 32).
      def synthetic(seed: Int): Data =
        val arr = new scala.Array[Byte](32)
        var i = 0
        while i < 32 do
          arr(i) = ((seed * 31 + i * 17) & 0xff).toByte
          i += 1
        arr.asInstanceOf[Array[Byte]]

      val h0 = synthetic(1)
      val h1 = synthetic(2)
      val h2 = synthetic(3)

      test(m"single-component signature length is 33 (32 + 1 cadence byte)"):
        SchemaSignature.encode(List(h0)).size
      . assert(_ == 33)

      test(m"single-component signature begins with the component hash"):
        SchemaSignature.encode(List(h0)).segment(0.z till 32.z).to[List] == h0.to[List]
      . assert(_ == true)

      test(m"two-component signature length is 37 (32 + 4 + 1)"):
        SchemaSignature.encode(List(h0, h1)).size
      . assert(_ == 37)

      test(m"three-component signature length is 39 (32 + 4 + 2 + 1)"):
        SchemaSignature.encode(List(h0, h1, h2)).size
      . assert(_ == 39)

      test(m"empty hash list raises BadSignatureLength"):
        capture[Bintel.Error](SchemaSignature.encode(Nil)).reason
      . assert(_ == Bintel.Error.Reason.BadSignatureLength)

      test(m"wrong-size hash raises BadSignatureLength"):
        val bad: Data = scala.Array.fill[Byte](16)(0).asInstanceOf[Array[Byte]]
        capture[Bintel.Error](SchemaSignature.encode(List(bad))).reason
      . assert(_ == Bintel.Error.Reason.BadSignatureLength)

      test(m"single-component signature decodes back to the hash"):
        val sig = SchemaSignature.encode(List(h0))
        val recovered = SchemaSignature.decode(sig, List(h0))
        recovered.map(_.stdlib) == List(h0.stdlib)
      . assert(_ == true)

      test(m"two-component signature round-trips through encode/decode"):
        val sig = SchemaSignature.encode(List(h0, h1))
        val recovered = SchemaSignature.decode(sig, List(h0, h1, h2))
        recovered.map(_.stdlib) == List(h0.stdlib, h1.stdlib)
      . assert(_ == true)

      test(m"three-component signature round-trips through encode/decode"):
        val sig = SchemaSignature.encode(List(h0, h1, h2))
        val recovered = SchemaSignature.decode(sig, List(h0, h1, h2))
        recovered.map(_.stdlib) == List(h0.stdlib, h1.stdlib, h2.stdlib)
      . assert(_ == true)

      test(m"decode with reserved hash-size index raises BadSignatureLength"):
        // XOR-fold ⇒ 0xA0, naming reserved s = 10
        val bad: Data = scala.Array[Byte](0xA0.toByte, 0, 0, 0, 0).asInstanceOf[Array[Byte]]
        capture[Bintel.Error](SchemaSignature.decode(bad, List(h0))).reason
      . assert(_ == Bintel.Error.Reason.BadSignatureLength)

      test(m"decode raises BadSignature when library is missing components"):
        val sig = SchemaSignature.encode(List(h0, h1))
        capture[Bintel.Error](SchemaSignature.decode(sig, List(h2))).reason
      . assert(_ == Bintel.Error.Reason.BadSignature)

    suite(m"BinTEL §8.1 schema signature from document"):
      test(m"single-component signature for a no-layer schema is 33 bytes"):
        val stream = getClass.getResourceAsStream("/stratiform/corpus/tels.tel").nn
        val source =
          val arr = stream.readAllBytes().nn
          stream.close()
          Array.unsafeFrozen(arr)

        val sig = SchemaSignature.fromDocument(source.read[Tel], Tels.Axiom.tels)
        sig.length
      . assert(_ == 33)

      test(m"no-layer schema signature begins with the 32-byte BLAKE3 value hash"):
        val stream = getClass.getResourceAsStream("/stratiform/corpus/tels.tel").nn
        val source =
          val arr = stream.readAllBytes().nn
          stream.close()
          Array.unsafeFrozen(arr)

        val sig = SchemaSignature.fromDocument(source.read[Tel], Tels.Axiom.tels)
        val bintel = Tel.Type.assign(source.read[Tel], Tels.Axiom.tels).bintel(Tels.Axiom.tels)
        val hash = Blake3.hashOf(bintel, 32)
        sig.segment(0.z till 32.z).to[List] == hash.to[List]
      . assert(_ == true)

      test(m"schema with a single layer produces a 37-byte signature"):
        val src = """tel 1.0
                    |
                    |name basic
                    |
                    |record Item
                    |  field key Identifier
                    |
                    |document
                    |  field key Identifier
                    |
                    |layer ext
                    |  scalar Number identifier
                    |""".stripMargin.tt
        val sig = SchemaSignature.fromDocument(src.read[Tel], Tels.Axiom.tels)
        sig.length
      . assert(_ == 37)

      test(m"two-layer schema produces a 39-byte signature"):
        val src = """tel 1.0
                    |
                    |name multi
                    |
                    |record Item
                    |  field key Identifier
                    |
                    |document
                    |  field key Identifier
                    |
                    |layer ext1
                    |  scalar Number identifier
                    |
                    |layer ext2
                    |  scalar Symbol identifier
                    |""".stripMargin.tt
        val sig = SchemaSignature.fromDocument(src.read[Tel], Tels.Axiom.tels)
        sig.length
      . assert(_ == 39)

    suite(m"BinTEL §3 value hash"):
      test(m"valueHash is deterministic"):
        val tel = t"name Alice\n".read[Tel]
        val a = tel.valueHash(nameSchema).data.stdlib
        val b = tel.valueHash(nameSchema).data.stdlib
        a == b
      . assert(_ == true)

      test(m"valueHash differs when value differs"):
        val a = t"name Alice\n".read[Tel].valueHash(nameSchema).data.stdlib
        val b = t"name Bob\n".read[Tel].valueHash(nameSchema).data.stdlib
        a == b
      . assert(_ == false)

      test(m"valueHash output is 32 bytes"):
        t"name Alice\n".read[Tel].valueHash(nameSchema).data.length
      . assert(_ == 32)

      test(m"§3 canonical tels.tel value hash is deterministic"):
        val stream = getClass.getResourceAsStream("/stratiform/corpus/tels.tel").nn
        val source =
          val arr = stream.readAllBytes().nn
          stream.close()
          Array.unsafeFrozen(arr)

        val a = Tel.Type.assign(source.read[Tel], Tels.Axiom.tels).valueHash(Tels.Axiom.tels).data.stdlib
        val b = Tel.Type.assign(source.read[Tel], Tels.Axiom.tels).valueHash(Tels.Axiom.tels).data.stdlib
        (a.length, a == b)
      . assert(_ == (32, true))

      test(m"§3 — canonical tels.tel encodes byte-for-byte against reference"):
        val telStream = getClass.getResourceAsStream("/stratiform/corpus/tels.tel").nn
        val telBytes  =
          val arr = telStream.readAllBytes().nn
          telStream.close()
          Array.unsafeFrozen(arr)

        val refStream = getClass.getResourceAsStream("/stratiform/corpus/tels.bintel.hex").nn
        val refHex    =
          val arr = refStream.readAllBytes().nn
          refStream.close()
          String(arr, "UTF-8").trim

        val refBytes = hexBytes(refHex)
        val element  = Tel.Type.assign(telBytes.read[Tel], Tels.Axiom.tels)
        element.bintel(Tels.Axiom.tels).stdlib == refBytes
      . assert(_ == true)

      test(m"§3 — tels.tel matches the normative BLAKE3-256 value hash"):
        // The single vector to which §3 of BinTEL and §20.5 of the TEL
        // Specification are both pinned.
        val telStream = getClass.getResourceAsStream("/stratiform/corpus/tels.tel").nn
        val telBytes  =
          val arr = telStream.readAllBytes().nn
          telStream.close()
          Array.unsafeFrozen(arr)

        val digest = Tel.Type.assign(telBytes.read[Tel], Tels.Axiom.tels).valueHash(Tels.Axiom.tels)
        digest.data.map(b => f"${b & 0xff}%02x".tt).join
      . assert(_ == "d440b01e327c62c41ac641047f2c4d8df3cbe94abb24db33f189226b7b8b7ad3")

    suite(m"BinTEL §6.2 self-contained mode"):
      val schemaDoc = """name greeting
                        |
                        |document
                        |  field name Identifier
                        |""".stripMargin.tt
      val dataDoc = t"name Alice\n"

      def selfContained(): Data =
        Bintel.selfContained(dataDoc.read[Tel], schemaDoc.read[Tel])

      test(m"self-contained document begins with the B2 C4 B5 BC magic"):
        selfContained().segment(0.z till 4.z).to[List]
      . assert(_ == List[Byte](0xb2.toByte, 0xc4.toByte, 0xb5.toByte, 0xbc.toByte))

      test(m"self-contained text form begins with βτεμ"):
        Bintel.text(selfContained()).s.substring(0, 4)
      . assert(_ == "βτεμ")

      test(m"round-trips: decode recovers the single document-root child"):
        Bintel.decodeDocumentSelfContained(selfContained()).root match
          case Tel.Element.Node(_, _, children) => children.length
          case _                                => -1
      . assert(_ == 1)

      test(m"a self-contained document reports its continuation"):
        val bytes = selfContained()
        Bintel.decodeDocumentSelfContained(bytes).continuation == bytes.length
      . assert(_ == true)

      test(m"documentExtent delimits a self-contained document"):
        val bytes = selfContained()
        Bintel.documentExtent(bytes) == bytes.length
      . assert(_ == true)

      test(m"self-contained and external documents may share a stream"):
        val axiom     = Tels.Axiom.tels
        val sd        = schemaDoc.read[Tel]
        val schema    = Tels.Layers.compose(sd.as[Tels])
        val signature = SchemaSignature.fromDocument(sd, axiom)
        val external  = dataDoc.read[Tel].bintelDocument(schema, signature)
        val self      = selfContained()
        val stream    = Array.concat(self, external)
        Bintel.decodeStream(stream.asInstanceOf[Array[Byte]], schema).map(_.continuation)
      . assert(_ == List(selfContained().length,
                        selfContained().length + dataDoc.read[Tel].bintelDocument(
                          Tels.Layers.compose(schemaDoc.read[Tel].as[Tels]),
                          SchemaSignature.fromDocument(schemaDoc.read[Tel], Tels.Axiom.tels))
                          .size))

      test(m"decodeWholeDocumentSelfContained rejects a continuation as B08"):
        val self   = selfContained()
        val stream = Array.concat(self, self)
        capture[Bintel.Error](Bintel.decodeWholeDocumentSelfContained(stream.asInstanceOf[Array[Byte]]))
        . reason
      . assert(_ == Bintel.Error.Reason.TrailingBytes)

      test(m"decodeDocument on a self-contained document is B01"):
        val schema = Tels.Layers.compose(schemaDoc.read[Tel].as[Tels])
        capture[Bintel.Error](Bintel.decodeDocument(selfContained(), schema)).reason
      . assert(_ == Bintel.Error.Reason.BadMagic)

      test(m"value hash is mode-independent (external == self-contained)"):
        val schema = Tels.Layers.compose(schemaDoc.read[Tel].as[Tels])
        val external = dataDoc.read[Tel].bintel(schema)
        val recovered = Bintel.decodeDocumentSelfContained(selfContained()).root.bintel(schema)
        recovered.stdlib == external.stdlib
      . assert(_ == true)

      test(m"signature not matching the embedded schema raises B11"):
        val axiom      = Tels.Axiom.tels
        val sd         = schemaDoc.read[Tel]
        val schemaBody = sd.bintel(axiom)
        val schema     = Tels.Layers.compose(sd.as[Tels])
        val docBody    = dataDoc.read[Tel].bintel(schema)
        // A valid-length but wrong signature: flip the first body byte and
        // the trailing cadence byte so the XOR-fold length check still passes.
        val wrong = SchemaSignature.fromDocument(sd, axiom).asInstanceOf[scala.Array[Byte]].clone()
        wrong(0) = (wrong(0) ^ 0x01).toByte
        wrong(wrong.length - 1) = (wrong(wrong.length - 1) ^ 0x01).toByte
        val bytes = Bintel.frameSelfContained(wrong.asInstanceOf[Array[Byte]], schemaBody, docBody)
        capture[Bintel.Error](Bintel.decodeDocumentSelfContained(bytes)).reason
      . assert(_ == Bintel.Error.Reason.EmbeddedSignatureMismatch)

      test(m"undecodable embedded schema raises B12"):
        val axiom   = Tels.Axiom.tels
        val sd      = schemaDoc.read[Tel]
        val schema  = Tels.Layers.compose(sd.as[Tels])
        val docBody = dataDoc.read[Tel].bintel(schema)
        val sig     = SchemaSignature.fromDocument(sd, axiom)
        val garbage: Data = Array[Byte](0x7f, 0x7f, 0x7f, 0x7f)
        val bytes = Bintel.frameSelfContained(sig, garbage, docBody)
        capture[Bintel.Error](Bintel.decodeDocumentSelfContained(bytes)).reason
      . assert(_ == Bintel.Error.Reason.EmbeddedSchemaUndecodable)


    suite(m"BinTEL direct parsing (BintelInlinable)"):
      given (Tests.Person is Bintel.Parsable) = BintelInlinable.parsable[Tests.Person]
      given (Tests.Company is Bintel.Parsable) = BintelInlinable.parsable[Tests.Company]
      given (Tests.Team is Bintel.Parsable) = BintelInlinable.parsable[Tests.Team]
      given (Tests.OptField is Bintel.Parsable) = BintelInlinable.parsable[Tests.OptField]

      given (Tests.WithDefault is Bintel.Parsable) =
        BintelInlinable.parsable[Tests.WithDefault]

      given (Tests.Readings is Bintel.Parsable) = BintelInlinable.parsable[Tests.Readings]
      given (Tests.BShape is Bintel.Parsable) = BintelInlinable.parsable[Tests.BShape]

      test(m"a flat struct reads directly from body bytes"):
        Bintel.parse[Tests.Person](Tests.Person(t"Alice", 30).bintel)
      . assert(_ == Tests.Person(t"Alice", 30))

      test(m"the direct read agrees with Bintel.read"):
        val bytes = Tests.Person(t"Iris", 44).bintel
        Bintel.parse[Tests.Person](bytes) == Bintel.read[Tests.Person](bytes)
      . assert(identity)

      test(m"a nested struct inlines through its own generated parser"):
        val company = Tests.Company(t"Acme", Tests.Person(t"Bob", 50))
        Bintel.parse[Tests.Company](company.bintel)
      . assert(_ == Tests.Company(t"Acme", Tests.Person(t"Bob", 50)))

      test(m"a repeatable struct field gathers every occurrence in order"):
        val team = Tests.Team(t"crew", List(Tests.Person(t"A", 1), Tests.Person(t"B", 2)))
        Bintel.parse[Tests.Team](team.bintel)
      . assert(_ == Tests.Team(t"crew", List(Tests.Person(t"A", 1), Tests.Person(t"B", 2))))

      test(m"a repeatable scalar field gathers every occurrence in order"):
        val readings = Tests.Readings(List(3, 1, 4, 1, 5), t"pi")
        Bintel.parse[Tests.Readings](readings.bintel)
      . assert(_ == Tests.Readings(List(3, 1, 4, 1, 5), t"pi"))

      test(m"an empty repeatable field reads as empty"):
        Bintel.parse[Tests.Readings](Tests.Readings(Nil, t"none").bintel)
      . assert(_ == Tests.Readings(Nil, t"none"))

      test(m"an Optional field reads its value when present"):
        Bintel.parse[Tests.OptField](Tests.OptField(7, t"note").bintel)
      . assert(_ == Tests.OptField(7, t"note"))

      test(m"an unset Optional field round-trips as Unset through the encoder"):
        // The encoder omits an unset `Optional` field entirely (it contributes
        // no child to the struct body), so both decode paths see it as absent.
        Bintel.read[Tests.OptField](Tests.OptField(7, Unset).bintel)
      . assert(_ == Tests.OptField(7, Unset))

      test(m"an unset Optional agrees with the AST path"):
        // Both paths now decode the omitted field as `Unset`.
        val bytes = Tests.OptField(7, Unset).bintel
        Bintel.parse[Tests.OptField](bytes) == Bintel.read[Tests.OptField](bytes)
      . assert(identity)

      test(m"an unset Optional reads Unset on the direct path too"):
        Bintel.parse[Tests.OptField](Tests.OptField(7, Unset).bintel)
      . assert(_ == Tests.OptField(7, Unset))

      test(m"a present empty Optional field round-trips as empty text through BinTEL"):
        Bintel.read[Tests.OptField](Tests.OptField(7, t"").bintel)
      . assert(_ == Tests.OptField(7, t""))

      test(m"a truly absent Optional field reads Unset"):
        // A hand-built body: one child, index 0 ("x"), scalar "7" — the
        // `note` field is genuinely absent.
        val bytes = Array[Byte](0x01, 0x00, 0x01, '7'.toByte)
        Bintel.parse[Tests.OptField](bytes)
      . assert(_ == Tests.OptField(7, Unset))

      test(m"a missing field with a declared default takes it"):
        // A hand-built body: one child, index 0 ("name"), scalar "Bob".
        val bytes = Array[Byte](0x01, 0x00, 0x03, 'B'.toByte, 'o'.toByte, 'b'.toByte)
        Bintel.parse[Tests.WithDefault](bytes)
      . assert(_ == Tests.WithDefault(t"Bob", 18))

      test(m"a missing required field raises Absent with its sentinel"):
        // One child, index 1 ("age"), scalar "9" — "name" is missing.
        val bytes = Array[Byte](0x01, 0x01, 0x01, '9'.toByte)
        capture[Tel.Error](Bintel.parse[Tests.Person](bytes)).reason
      . assert(_ == Tel.Error.Reason.Absent)

      test(m"an unparseable scalar raises NotScalar"):
        // Two children: name "x", then age "abc".
        val bytes = Array[Byte]
          (0x02, 0x00, 0x01, 'x'.toByte, 0x01, 0x03, 'a'.toByte, 'b'.toByte, 'c'.toByte)

        capture[Tel.Error](Bintel.parse[Tests.Person](bytes)).reason match
          case Tel.Error.Reason.NotScalar(atom, expected) => (atom.s, expected.s)
          case other                                     => (other.toString, "")
      . assert(_ == ("abc", "Int"))

      test(m"a sum's variant is chosen by its keyword index"):
        val shape: Tests.BShape = Tests.BShape.BRect(3, 4)
        Bintel.parse[Tests.BShape](shape.bintel)
      . assert(_ == Tests.BShape.BRect(3, 4))

      test(m"the other variant round-trips too"):
        val shape: Tests.BShape = Tests.BShape.BCircle(9)
        Bintel.parse[Tests.BShape](shape.bintel)
      . assert(_ == Tests.BShape.BCircle(9))

      test(m"an out-of-range keyword index aborts"):
        // One child with index 9 in a two-field struct.
        val bytes = Array[Byte](0x01, 0x09, 0x01, 'x'.toByte)
        capture[Bintel.Error](Bintel.parse[Tests.Person](bytes)).reason
      . assert(_ == Bintel.Error.Reason.BadKeywordIndex)

      test(m"trailing bytes are rejected"):
        val good = Tests.Person(t"Alice", 30).bintel
        val padded = Array.from(good.to[List].stdlib :+ 0.toByte)
        capture[Bintel.Error](Bintel.parse[Tests.Person](padded)).reason
      . assert(_ == Bintel.Error.Reason.TrailingBytes)

    suite(m"TEL direct parsing recursion"):
      test(m"an inlined recursive type ties through its own nominal Parsable"):
        given (Tests.Tree is Tel.Parsable) = Inlinable.parsable[Tests.Tree]
        val tree = Tests.Tree(t"root", List(Tests.Tree(t"a", Nil)))
        val data: Data = Array.unsafeFrozen(tree.in[Tel].show.s.getBytes("UTF-8").nn)
        data.read[Tests.Tree in Tel]
      . assert(_ == Tests.Tree(t"root", List(Tests.Tree(t"a", Nil))))

      test(m"`in[Text over Tel]` writes what `read[T in Tel]` reads"):
        Tests.Person(t"Alice", 30).in[Text over Tel].read[Tests.Person in Tel]
      . assert(_ == Tests.Person(t"Alice", 30))

      test(m"`in[Data over Tel]` writes what `read[T in Tel]` reads"):
        import codepages.utf8Codepage
        Tests.Person(t"Zoë", 30).in[Data over Tel].read[Tests.Person in Tel]
      . assert(_ == Tests.Person(t"Zoë", 30))
