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
package locomotion

import soundness.*

import strategies.throwUnsafely
import errorDiagnostics.stackTracesDiagnostics
import supervisors.globalSupervisor
import threading.virtualThreading
import probates.panicProbate

case class Sample(@field(1) value: Int) derives CanEqual
case class Point(@field(1) x: Int, @field(2) y: Int) derives CanEqual
case class Person(@field(1) name: Text, @field(2) age: Int) derives CanEqual
case class Wrapper(@field(1) point: Point, @field(2) label: Text) derives CanEqual
case class Tags(@field(1) tags: List[Text]) derives CanEqual
case class Numbers(@field(1) values: List[Int]) derives CanEqual
case class MaybeName(@field(1) name: Optional[Text]) derives CanEqual
case class Unnumbered(first: Int, second: Int) derives CanEqual
case class Sparse(@field(3) a: Int, @field(7) b: Text) derives CanEqual

enum Shape derives CanEqual:
  case Circle(radius: Int)
  case Rectangle(width: Int, height: Int)

case class Typed
   ( @field(1) unsigned:  U32,
     @field(2) unsigned64: U64,
     @field(3) signed:    S32,
     @field(4) signed64:  S64,
     @field(5) fixed:     B32,
     @field(6) fixed64:   B64 )
derives CanEqual

case class Signed(@field(3) value: S32) derives CanEqual
case class Fixed(@field(5) value: B32) derives CanEqual

case class Labels(@field(1) entries: Map[Text, Text]) derives CanEqual
case class Counts(@field(1) counts: Map[Text, Int]) derives CanEqual

// Recursion through a collection (#1429) and a generic product used over a recursive type.
// TEMPORARY (List-flip drain): the staged engine's `Plan.Gather` classifier does not
// yet recognise the opaque `List` alias, so the recursive field stays `sci.List` until
// the staged engines learn the prelude aliases.
case class Tree
  ( @field(1) value: Text,
    @field(2) children: scala.collection.immutable.List[Tree] )
derives CanEqual

case class Defaulted(@field(1) a: Int, @field(2) b: Int = 7) derives CanEqual
case class Boxed[value](@field(1) value: value) derives CanEqual

// A singular nested field whose type parses through the runtime seam (`Tree` has a
// `Parsable` but no `Inlinable`), for the absent-nested-value window.
case class Holder(@field(1) label: Text, @field(2) inner: Tree) derives CanEqual

// The bytes as a chain of `size`-byte chunks, so every multi-byte value whose
// length is not a multiple of `size` straddles a chunk boundary somewhere.
private def chunked(bytes: Data, size: Int): Chain[Data] =
  if bytes.length == 0 then Chain() else
    val split = size.min(bytes.length)
    Chain.cons
     ( bytes.segment((0).z till split.z),
       chunked(bytes.segment(split.z till bytes.length.z), size) )

object Tests extends Suite(m"Locomotion Protobuf Tests"):
  def run(): Unit =
    def wire[value: Encodable in Protobuf](value: value): List[Int] =
      value.in[Protobuf].encode.to[List].map(_.toInt & 0xff)

    suite(m"Wire-format golden vectors"):
      test(m"a single varint field encodes to the canonical bytes"):
        wire(Sample(150))
      . assert(_ == List(0x08, 0x96, 0x01))

      test(m"a string field is length-delimited"):
        wire(Person(t"AB", 0))
      . assert(_ == List(0x0a, 0x02, 0x41, 0x42, 0x10, 0x00))

      test(m"sparse field numbers produce the right tags"):
        wire(Sparse(1, t"")).stdlib.take(2)
      . assert(_ == List(0x18, 0x01))

    suite(m"Round-trips"):
      test(m"single int field"):
        // [test-harness] test round-trip read over fresh chain
        scala.caps.unsafe.unsafeAssumeSeparate:
          proscenium.Chain(Sample(150).in[Protobuf].encode).read[Sample in Protobuf]
      . assert(_ == Sample(150))

      test(m"two int fields, one at its default"):
        proscenium.Chain(Point(0, 5).in[Protobuf].encode).read[Point in Protobuf]
      . assert(_ == Point(0, 5))

      test(m"string and int fields"):
        proscenium.Chain(Person(t"Alice", 30).in[Protobuf].encode).read[Person in Protobuf]
      . assert(_ == Person(t"Alice", 30))

      test(m"read[Protobuf] then as[T] (two-step)"):
        proscenium.Chain(Person(t"Alice", 30).in[Protobuf].encode).read[Protobuf].as[Person]
      . assert(_ == Person(t"Alice", 30))

      test(m"nested message"):
        proscenium.Chain(Wrapper(Point(3, 4), t"origin").in[Protobuf].encode).read[Wrapper in Protobuf]
      . assert(_ == Wrapper(Point(3, 4), t"origin"))

      test(m"sparse field numbers"):
        proscenium.Chain(Sparse(9, t"x").in[Protobuf].encode).read[Sparse in Protobuf]
      . assert(_ == Sparse(9, t"x"))

    suite(m"Repeated fields"):
      test(m"repeated strings round-trip in order"):
        proscenium.Chain(Tags(List(t"a", t"b", t"c")).in[Protobuf].encode).read[Tags in Protobuf]
      . assert(_ == Tags(List(t"a", t"b", t"c")))

      test(m"repeated ints round-trip, keeping default elements"):
        proscenium.Chain(Numbers(List(0, 1, 2)).in[Protobuf].encode).read[Numbers in Protobuf]
      . assert(_ == Numbers(List(0, 1, 2)))

      test(m"repeated ints are packed into one length-delimited field"):
        wire(Numbers(List(3, 270, 86942))).stdlib.take(2)
      . assert(_ == List(0x0a, 0x06))

      test(m"a packed field produced elsewhere decodes back to a List"):
        // The canonical packed encoding of [3, 270, 86942] (matches `protoc`).
        val packed = Array[Byte](0x0a, 0x06, 0x03, 0x8e.toByte, 0x02, 0x9e.toByte, 0xa7.toByte, 0x05)
        proscenium.Chain(packed).read[Numbers in Protobuf]
      . assert(_ == Numbers(List(3, 270, 86942)))

      test(m"empty repeated field writes nothing"):
        wire(Tags(Nil))
      . assert(_ == Nil)

      val tree = Tree(t"root", List(Tree(t"a", Nil), Tree(t"b", List(Tree(t"c", Nil)))))

      test(m"a type recursive through a List round-trips"):
        proscenium.Chain(tree.in[Protobuf].encode).read[Protobuf].as[Tree]
      . assert(_ == tree)

      test(m"a generic product over a recursive type stays structurally derived"):
        proscenium.Chain(Boxed(tree).in[Protobuf].encode).read[Protobuf].as[Boxed[Tree]]
      . assert(_ == Boxed(tree))

    suite(m"Optional presence"):
      test(m"a set optional round-trips"):
        proscenium.Chain(MaybeName(t"set").in[Protobuf].encode).read[MaybeName in Protobuf]
      . assert(_ == MaybeName(t"set"))

      test(m"an unset optional writes nothing and round-trips to Unset"):
        proscenium.Chain(MaybeName(Unset).in[Protobuf].encode).read[MaybeName in Protobuf]
      . assert(_ == MaybeName(Unset))

    suite(m"Sum types (oneof)"):
      test(m"the Circle variant round-trips"):
        val shape: Shape = Shape.Circle(5)
        proscenium.Chain(shape.in[Protobuf].encode).read[Shape in Protobuf]
      . assert(_ == Shape.Circle(5))

      test(m"the Rectangle variant round-trips"):
        val shape: Shape = Shape.Rectangle(3, 4)
        proscenium.Chain(shape.in[Protobuf].encode).read[Shape in Protobuf]
      . assert(_ == Shape.Rectangle(3, 4))

    suite(m"Field-number fallback"):
      test(m"unannotated fields use 1-based declaration order"):
        wire(Unnumbered(150, 0)).stdlib.take(2)
      . assert(_ == List(0x08, 0x96))

      test(m"unannotated message round-trips"):
        proscenium.Chain(Unnumbered(7, 9).in[Protobuf].encode).read[Unnumbered in Protobuf]
      . assert(_ == Unnumbered(7, 9))

    suite(m"Typed integer encodings"):
      val typed = Typed(7.bits.u32, 8L.bits.u64, -3.bits.s32, -4L.bits.s64, 5.bits, 6L.bits)

      test(m"all typed integers round-trip"):
        proscenium.Chain(typed.in[Protobuf].encode).read[Typed in Protobuf]
      . assert(_ == typed)

      test(m"sint32 uses zig-zag (field 3, -1 ⇒ tag 0x18, 0x01)"):
        wire(Signed(-1.bits.s32))
      . assert(_ == List(0x18, 0x01))

      test(m"fixed32 is little-endian 4 bytes (field 5, value 5)"):
        wire(Fixed(5.bits))
      . assert(_ == List(0x2d, 0x05, 0x00, 0x00, 0x00))

    suite(m"Maps"):
      test(m"a string→string map round-trips"):
        val labels = Labels(Map(t"a" -> t"1", t"b" -> t"2"))
        proscenium.Chain(labels.in[Protobuf].encode).read[Labels in Protobuf]
      . assert(_ == Labels(Map(t"a" -> t"1", t"b" -> t"2")))

      test(m"a string→int map round-trips"):
        val counts = Counts(Map(t"x" -> 10, t"y" -> 20))
        proscenium.Chain(counts.in[Protobuf].encode).read[Counts in Protobuf]
      . assert(_ == Counts(Map(t"x" -> 10, t"y" -> 20)))

      test(m"an empty map writes nothing"):
        wire(Labels(Map()))
      . assert(_ == Nil)

      test(m"a single entry encodes as a length-delimited message"):
        wire(Labels(Map(t"a" -> t"b")))
      . assert(_ == List(0x0a, 0x06, 0x0a, 0x01, 0x61, 0x12, 0x01, 0x62))

    suite(m"Parse errors carry a byte offset"):
      def decode(bytes: Byte*): Sample raises Protobuf.Error =
        // [test-harness] test decode helper over fresh chain
        scala.caps.unsafe.unsafeAssumeSeparate:
          proscenium.Chain(Array.from(bytes)).read[Sample in Protobuf]

      test(m"a truncated length-delimited payload reports the offset where data ran out"):
        // field 1, wire type Len, length 5, but only one payload byte present.
        capture[Protobuf.Error](decode(0x0a, 0x05, 0x41))
      . assert(_ == Protobuf.Error(Protobuf.Error.Reason.Truncated(2)))

      test(m"an unexpected wire type reports the offset of the tag"):
        // field 1, wire type 3 (group-start) is not a valid proto3 wire type.
        capture[Protobuf.Error](decode(0x0b))
      . assert(_ == Protobuf.Error(Protobuf.Error.Reason.UnexpectedWireType(3, 0)))

      test(m"a varint longer than ten bytes is malformed"):
        capture[Protobuf.Error]:
          // [test-harness] test read inside capture block
          scala.caps.unsafe.unsafeAssumeSeparate:
            proscenium.Chain(Array.fill(11)(0x80.toByte)).read[Sample in Protobuf]
      . assert(_ == Protobuf.Error(Protobuf.Error.Reason.MalformedVarint(0)))

      test(m"a varint whose value overflows 64 bits is rejected"):
        // nine continuation bytes then a tenth byte contributing more than bit 63.
        capture[Protobuf.Error]:
          decode(0x80.toByte, 0x80.toByte, 0x80.toByte, 0x80.toByte, 0x80.toByte, 0x80.toByte,
              0x80.toByte, 0x80.toByte, 0x80.toByte, 0x02)
      . assert(_ == Protobuf.Error(Protobuf.Error.Reason.Overflow(0)))

    // The direct parser reads chunks as they arrive, so a value split anywhere must read
    // as one that arrived whole, and every error offset is absolute. (The `Protobuf` ADT
    // path reads a whole message, so it is exercised on chunked input only as a control.)
    suite(m"Chunked input"):
      def encoded[value: Encodable in Protobuf](value: value): Data = value.in[Protobuf].encode

      given (Person is Protobuf.Parsable) = Inlinable.parsable[Person]
      given (Point is Protobuf.Parsable) = Inlinable.parsable[Point]
      given (Wrapper is Protobuf.Parsable) = Inlinable.parsable[Wrapper]
      given (Tags is Protobuf.Parsable) = Inlinable.parsable[Tags]
      given (Numbers is Protobuf.Parsable) = Inlinable.parsable[Numbers]
      given (Typed is Protobuf.Parsable) = Inlinable.parsable[Typed]
      given (Counts is Protobuf.Parsable) = Inlinable.parsable[Counts]
      given (Shape is Protobuf.Parsable) = Inlinable.parsable[Shape]
      given (Tree is Protobuf.Parsable) = Inlinable.parsable[Tree]
      given (Holder is Protobuf.Parsable) = Inlinable.parsable[Holder]

      test(m"a message split one byte per chunk reads through the ADT path"):
        chunked(encoded(Wrapper(Point(3, 4), t"origin")), 1).read[Protobuf].as[Wrapper]
      . assert(_ == Wrapper(Point(3, 4), t"origin"))

      test(m"a message split one byte per chunk reads directly"):
        chunked(encoded(Person(t"Ada", 36)), 1).read[Person in Protobuf]
      . assert(_ == Person(t"Ada", 36))

      test(m"a multi-byte varint split mid-value reads whole"):
        chunked(encoded(Point(Int.MaxValue, 300)), 2).read[Point in Protobuf]
      . assert(_ == Point(Int.MaxValue, 300))

      test(m"fixed-width values split mid-value read whole"):
        val typed = Typed(7.bits.u32, 8L.bits.u64, -3.bits.s32, -4L.bits.s64, 5.bits, 6L.bits)
        chunked(encoded(typed), 3).read[Typed in Protobuf]
      . assert(_ == Typed(7.bits.u32, 8L.bits.u64, -3.bits.s32, -4L.bits.s64, 5.bits, 6L.bits))

      test(m"a long string split across chunks reads whole"):
        val name = "x".repeat(300).nn.tt
        chunked(encoded(Person(name, 1)), 7).read[Person in Protobuf]
      . assert(_ == Person("x".repeat(300).nn.tt, 1))

      test(m"a nested message whose length prefix straddles a boundary reads whole"):
        // Field 1 (Len, length 4: Point(3, 4)) then field 2 "origin": the 2-byte chunking
        // puts the length byte at the end of the first chunk.
        chunked(encoded(Wrapper(Point(3, 4), t"origin")), 2).read[Wrapper in Protobuf]
      . assert(_ == Wrapper(Point(3, 4), t"origin"))

      test(m"repeated strings split across chunks gather in order"):
        chunked(encoded(Tags(List(t"alpha", t"beta", t"gamma"))), 4).read[Tags in Protobuf]
      . assert(_ == Tags(List(t"alpha", t"beta", t"gamma")))

      test(m"a packed repeated field split across chunks reads its run"):
        chunked(encoded(Numbers(List(3, 270, 86942, 1))), 2).read[Numbers in Protobuf]
      . assert(_ == Numbers(List(3, 270, 86942, 1)))

      test(m"a map field split across chunks bridges through the seam"):
        chunked(encoded(Counts(Map(t"a" -> 1, t"bb" -> 2))), 3).read[Counts in Protobuf]
      . assert(_ == Counts(Map(t"a" -> 1, t"bb" -> 2)))

      test(m"a oneof whose variant is not the first field dispatches, split across chunks"):
        // An unknown field 9 (varint 1), then Rectangle (field 2) width=3 height=4, then a
        // later occurrence of field 2 that wins (width=5 height=6).
        val bytes =
          Array[Byte](0x48, 0x01, 0x12, 0x04, 0x08, 0x03, 0x10, 0x04, 0x12, 0x04, 0x08, 0x05,
              0x10, 0x06)
        (bytes.read[Shape in Protobuf], chunked(bytes, 3).read[Shape in Protobuf])
      . assert(_ == (Shape.Rectangle(5, 6), Shape.Rectangle(5, 6)))

      test(m"the lowest-numbered oneof variant wins over a later higher one, split"):
        // Rectangle (field 2) first, then Circle (field 1): Circle wins.
        val bytes = Array[Byte](0x12, 0x04, 0x08, 0x03, 0x10, 0x04, 0x0a, 0x02, 0x08, 0x07)
        chunked(bytes, 2).read[Shape in Protobuf]
      . assert(_ == Shape.Circle(7))

      test(m"an absent nested runtime-seam field parses from an empty window"):
        Array[Byte](0x0a, 0x01, 0x78).read[Holder in Protobuf]
      . assert(_ == Holder(t"x", Tree(t"", Nil)))

      test(m"truncation inside a later chunk reports the absolute offset directly"):
        capture[Protobuf.Error]:
          // [test-harness] test read inside capture block
          scala.caps.unsafe.unsafeAssumeSeparate:
            chunked(Array[Byte](0x0a, 0x05, 0x41), 2).read[Person in Protobuf]
      . assert(_ == Protobuf.Error(Protobuf.Error.Reason.Truncated(2)))

      test(m"a truncated varint in a later chunk reports its absolute offset directly"):
        capture[Protobuf.Error]:
          // [test-harness] test read inside capture block
          scala.caps.unsafe.unsafeAssumeSeparate:
            chunked(Array[Byte](0x10, 0x80.toByte), 1).read[Point in Protobuf]
      . assert(_ == Protobuf.Error(Protobuf.Error.Reason.Truncated(2)))

      test(m"a complete field on a live stream reads without waiting for more"):
        // The field is published as soon as it is parsed, and the producer only finishes
        // the stream after it has been seen: the read must complete without the parser
        // asking the source for a byte it has not yet sent.
        val slot = new java.util.concurrent.atomic.AtomicReference[String | Null](null)

        supervise:
          Conduit[Data]() match
           case (intake, stream) =>
            val task = stream.transfer: (stream, _, _) ?=>
              val parser = ProtobufParser(stream())
              val tactic = summon[Tactic[Protobuf.Error]]
              val tag = parser.directTag()(using tactic)
              val saved = parser.directEnterField(tag & 7)(using tactic)
              val text = parser.directStringWindow()(using tactic)
              parser.directLeaveField(saved)(using tactic)
              slot.set(s"${tag >>> 3}:$text")

            intake.put(Array[Byte](0x0a, 0x03, 0x41, 0x42, 0x43))
            intake.flush()
            val deadline = java.lang.System.nanoTime + 2000000000L
            while slot.get == null && java.lang.System.nanoTime < deadline do Thread.sleep(10)
            val result = slot.get
            intake.finish()
            unsafely(task.await())
            result
      . assert(_ == "1:ABC")

    suite(m"HTTP content-type integration"):
      test(m"serialises with the application/protobuf media type"):
        Person(t"Alice", 30).in[Protobuf].generic(0)
      . assert(_ == t"application/protobuf")

      test(m"request/response body round-trips"):
        val message = Person(t"Alice", 30).in[Protobuf]
        message.generic(1).read[Person in Protobuf]
      . assert(_ == Person(t"Alice", 30))

    suite(m"Optics"):
      import conversions.encodableToProtobuf
      // Protobuf is number-keyed: the `Ordinal` selects a field by number (Prim =
      // field 1). Wrapper encodes `point` at field 1 and `label` at field 2.
      def wrapper: Protobuf = Wrapper(Point(3, 4), t"origin").in[Protobuf]

      test(m"field optic replaces a sub-message field by number"):
        wrapper.lens(_(Prim) = Point(7, 8).in[Protobuf]).as[Wrapper]
      . assert(_ == Wrapper(Point(7, 8), t"origin"))

      test(m"field optic leaves other fields unchanged"):
        wrapper.lens(_(Prim) = Point(7, 8)).as[Wrapper].label
      . assert(_ == t"origin")

      test(m"an absent field number is a no-op"):
        wrapper.lens(_(Sen) = Point(7, 8)).as[Wrapper]
      . assert(_ == Wrapper(Point(3, 4), t"origin"))

    suite(m"Direct parsing (Inlinable)"):
      given (Point is Protobuf.Parsable) = Inlinable.parsable[Point]
      given (Person is Protobuf.Parsable) = Inlinable.parsable[Person]
      given (Wrapper is Protobuf.Parsable) = Inlinable.parsable[Wrapper]
      given (Sparse is Protobuf.Parsable) = Inlinable.parsable[Sparse]
      given (Tags is Protobuf.Parsable) = Inlinable.parsable[Tags]
      given (Numbers is Protobuf.Parsable) = Inlinable.parsable[Numbers]
      given (MaybeName is Protobuf.Parsable) = Inlinable.parsable[MaybeName]
      given (Shape is Protobuf.Parsable) = Inlinable.parsable[Shape]
      given (Typed is Protobuf.Parsable) = Inlinable.parsable[Typed]
      given (Counts is Protobuf.Parsable) = Inlinable.parsable[Counts]
      given (Tree is Protobuf.Parsable) = Inlinable.parsable[Tree]
      given (Defaulted is Protobuf.Parsable) = Inlinable.parsable[Defaulted]

      def encoded[value: Encodable in Protobuf](value: value): Data = value.in[Protobuf].encode

      test(m"a flat message reads directly from bytes"):
        encoded(Person(t"Alice", 30)).read[Person in Protobuf]
      . assert(_ == Person(t"Alice", 30))

      test(m"a nested message inlines through its own generated parser"):
        encoded(Wrapper(Point(3, 4), t"origin")).read[Wrapper in Protobuf]
      . assert(_ == Wrapper(Point(3, 4), t"origin"))

      test(m"sparse @field numbers dispatch correctly"):
        encoded(Sparse(9, t"x")).read[Sparse in Protobuf]
      . assert(_ == Sparse(9, t"x"))

      test(m"repeated strings gather in stream order"):
        encoded(Tags(List(t"a", t"b", t"c"))).read[Tags in Protobuf]
      . assert(_ == Tags(List(t"a", t"b", t"c")))

      test(m"a packed repeated field reads its run in place"):
        val packed = Array[Byte](0x0a, 0x06, 0x03, 0x8e.toByte, 0x02, 0x9e.toByte, 0xa7.toByte, 0x05)
        packed.read[Numbers in Protobuf]
      . assert(_ == Numbers(List(3, 270, 86942)))

      test(m"unpacked occurrences of a packable element still gather"):
        // Two unpacked varint occurrences of field 1: 3 and 270.
        val unpacked = Array[Byte](0x08, 0x03, 0x08, 0x8e.toByte, 0x02)
        unpacked.read[Numbers in Protobuf]
      . assert(_ == Numbers(List(3, 270)))

      test(m"the last occurrence of a scalar field wins"):
        // x=1, x=9, y=2.
        val bytes = Array[Byte](0x08, 0x01, 0x08, 0x09, 0x10, 0x02)
        bytes.read[Point in Protobuf]
      . assert(_ == Point(9, 2))

      test(m"an unknown field number is skipped whole"):
        // Field 5 (varint 1), then x=3, y=4.
        val bytes = Array[Byte](0x28, 0x01, 0x08, 0x03, 0x10, 0x04)
        bytes.read[Point in Protobuf]
      . assert(_ == Point(3, 4))

      test(m"a missing scalar field takes its proto3 zero"):
        // Only x=3.
        Array[Byte](0x08, 0x03).read[Point in Protobuf]
      . assert(_ == Point(3, 0))

      test(m"a missing field with a declared default takes it"):
        Array[Byte](0x08, 0x03).read[Defaulted in Protobuf]
      . assert(_ == Defaulted(3, 7))

      test(m"an empty message reads as all-absent"):
        Array[Byte]().read[Point in Protobuf]
      . assert(_ == Point(0, 0))

      test(m"a set optional bridges through its Decodable"):
        encoded(MaybeName(t"set")).read[MaybeName in Protobuf]
      . assert(_ == MaybeName(t"set"))

      test(m"an unset optional reads back to Unset"):
        encoded(MaybeName(Unset)).read[MaybeName in Protobuf]
      . assert(_ == MaybeName(Unset))

      test(m"the Circle variant of a oneof round-trips"):
        encoded(Shape.Circle(5): Shape).read[Shape in Protobuf]
      . assert(_ == Shape.Circle(5))

      test(m"the Rectangle variant of a oneof round-trips"):
        encoded(Shape.Rectangle(3, 4): Shape).read[Shape in Protobuf]
      . assert(_ == Shape.Rectangle(3, 4))

      test(m"typed integers bridge through their Decodables"):
        val typed = Typed(7.bits.u32, 8L.bits.u64, -3.bits.s32, -4L.bits.s64, 5.bits, 6L.bits)
        encoded(typed).read[Typed in Protobuf]
      . assert(_ == Typed(7.bits.u32, 8L.bits.u64, -3.bits.s32, -4L.bits.s64, 5.bits, 6L.bits))

      test(m"a map field bridges through the entry-message Decodable"):
        encoded(Counts(Map(t"a" -> 1, t"b" -> 2))).read[Counts in Protobuf]
      . assert(_ == Counts(Map(t"a" -> 1, t"b" -> 2)))

      test(m"a recursive type degrades its recursive field to the seam"):
        val tree = Tree(t"root", List(Tree(t"a", Nil), Tree(t"b", List(Tree(t"c", Nil)))))
        encoded(tree).read[Tree in Protobuf]
      . assert(_ == Tree(t"root", List(Tree(t"a", Nil), Tree(t"b", List(Tree(t"c", Nil))))))
