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
┃    Soundness, version 0.63.0.                                                                    ┃
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
case class Tree(@field(1) value: Text, @field(2) children: List[Tree]) derives CanEqual

case class Defaulted(@field(1) a: Int, @field(2) b: Int = 7) derives CanEqual
case class Boxed[value](@field(1) value: value) derives CanEqual

object Tests extends Suite(m"Locomotion Protobuf Tests"):
  def run(): Unit =
    def wire[value: Encodable in Protobuf](value: value): List[Int] =
      value.in[Protobuf].encode.to(List).map(_.toInt & 0xff)

    suite(m"Wire-format golden vectors"):
      test(m"a single varint field encodes to the canonical bytes"):
        wire(Sample(150))
      . assert(_ == List(0x08, 0x96, 0x01))

      test(m"a string field is length-delimited"):
        wire(Person(t"AB", 0))
      . assert(_ == List(0x0a, 0x02, 0x41, 0x42, 0x10, 0x00))

      test(m"sparse field numbers produce the right tags"):
        wire(Sparse(1, t"")).take(2)
      . assert(_ == List(0x18, 0x01))

    suite(m"Round-trips"):
      test(m"single int field"):
        LazyList(Sample(150).in[Protobuf].encode).read[Sample in Protobuf]
      . assert(_ == Sample(150))

      test(m"two int fields, one at its default"):
        LazyList(Point(0, 5).in[Protobuf].encode).read[Point in Protobuf]
      . assert(_ == Point(0, 5))

      test(m"string and int fields"):
        LazyList(Person(t"Alice", 30).in[Protobuf].encode).read[Person in Protobuf]
      . assert(_ == Person(t"Alice", 30))

      test(m"read[Protobuf] then as[T] (two-step)"):
        LazyList(Person(t"Alice", 30).in[Protobuf].encode).read[Protobuf].as[Person]
      . assert(_ == Person(t"Alice", 30))

      test(m"nested message"):
        LazyList(Wrapper(Point(3, 4), t"origin").in[Protobuf].encode).read[Wrapper in Protobuf]
      . assert(_ == Wrapper(Point(3, 4), t"origin"))

      test(m"sparse field numbers"):
        LazyList(Sparse(9, t"x").in[Protobuf].encode).read[Sparse in Protobuf]
      . assert(_ == Sparse(9, t"x"))

    suite(m"Repeated fields"):
      test(m"repeated strings round-trip in order"):
        LazyList(Tags(List(t"a", t"b", t"c")).in[Protobuf].encode).read[Tags in Protobuf]
      . assert(_ == Tags(List(t"a", t"b", t"c")))

      test(m"repeated ints round-trip, keeping default elements"):
        LazyList(Numbers(List(0, 1, 2)).in[Protobuf].encode).read[Numbers in Protobuf]
      . assert(_ == Numbers(List(0, 1, 2)))

      test(m"repeated ints are packed into one length-delimited field"):
        wire(Numbers(List(3, 270, 86942))).take(2)
      . assert(_ == List(0x0a, 0x06))

      test(m"a packed field produced elsewhere decodes back to a List"):
        // The canonical packed encoding of [3, 270, 86942] (matches `protoc`).
        val packed = IArray[Byte](0x0a, 0x06, 0x03, 0x8e.toByte, 0x02, 0x9e.toByte, 0xa7.toByte, 0x05)
        LazyList(packed).read[Numbers in Protobuf]
      . assert(_ == Numbers(List(3, 270, 86942)))

      test(m"empty repeated field writes nothing"):
        wire(Tags(Nil))
      . assert(_ == Nil)

      val tree = Tree(t"root", List(Tree(t"a", Nil), Tree(t"b", List(Tree(t"c", Nil)))))

      test(m"a type recursive through a List round-trips"):
        LazyList(tree.in[Protobuf].encode).read[Protobuf].as[Tree]
      . assert(_ == tree)

      test(m"a generic product over a recursive type stays structurally derived"):
        LazyList(Boxed(tree).in[Protobuf].encode).read[Protobuf].as[Boxed[Tree]]
      . assert(_ == Boxed(tree))

    suite(m"Optional presence"):
      test(m"a set optional round-trips"):
        LazyList(MaybeName(t"set").in[Protobuf].encode).read[MaybeName in Protobuf]
      . assert(_ == MaybeName(t"set"))

      test(m"an unset optional writes nothing and round-trips to Unset"):
        LazyList(MaybeName(Unset).in[Protobuf].encode).read[MaybeName in Protobuf]
      . assert(_ == MaybeName(Unset))

    suite(m"Sum types (oneof)"):
      test(m"the Circle variant round-trips"):
        val shape: Shape = Shape.Circle(5)
        LazyList(shape.in[Protobuf].encode).read[Shape in Protobuf]
      . assert(_ == Shape.Circle(5))

      test(m"the Rectangle variant round-trips"):
        val shape: Shape = Shape.Rectangle(3, 4)
        LazyList(shape.in[Protobuf].encode).read[Shape in Protobuf]
      . assert(_ == Shape.Rectangle(3, 4))

    suite(m"Field-number fallback"):
      test(m"unannotated fields use 1-based declaration order"):
        wire(Unnumbered(150, 0)).take(2)
      . assert(_ == List(0x08, 0x96))

      test(m"unannotated message round-trips"):
        LazyList(Unnumbered(7, 9).in[Protobuf].encode).read[Unnumbered in Protobuf]
      . assert(_ == Unnumbered(7, 9))

    suite(m"Typed integer encodings"):
      val typed = Typed(7.bits.u32, 8L.bits.u64, -3.bits.s32, -4L.bits.s64, 5.bits, 6L.bits)

      test(m"all typed integers round-trip"):
        LazyList(typed.in[Protobuf].encode).read[Typed in Protobuf]
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
        LazyList(labels.in[Protobuf].encode).read[Labels in Protobuf]
      . assert(_ == Labels(Map(t"a" -> t"1", t"b" -> t"2")))

      test(m"a string→int map round-trips"):
        val counts = Counts(Map(t"x" -> 10, t"y" -> 20))
        LazyList(counts.in[Protobuf].encode).read[Counts in Protobuf]
      . assert(_ == Counts(Map(t"x" -> 10, t"y" -> 20)))

      test(m"an empty map writes nothing"):
        wire(Labels(Map()))
      . assert(_ == Nil)

      test(m"a single entry encodes as a length-delimited message"):
        wire(Labels(Map(t"a" -> t"b")))
      . assert(_ == List(0x0a, 0x06, 0x0a, 0x01, 0x61, 0x12, 0x01, 0x62))

    suite(m"Parse errors carry a byte offset"):
      def decode(bytes: Byte*): Sample raises ProtobufError =
        LazyList(IArray.from(bytes)).read[Sample in Protobuf]

      test(m"a truncated length-delimited payload reports the offset where data ran out"):
        // field 1, wire type Len, length 5, but only one payload byte present.
        capture[ProtobufError](decode(0x0a, 0x05, 0x41))
      . assert(_ == ProtobufError(ProtobufError.Reason.Truncated(2)))

      test(m"an unexpected wire type reports the offset of the tag"):
        // field 1, wire type 3 (group-start) is not a valid proto3 wire type.
        capture[ProtobufError](decode(0x0b))
      . assert(_ == ProtobufError(ProtobufError.Reason.UnexpectedWireType(3, 0)))

      test(m"a varint longer than ten bytes is malformed"):
        capture[ProtobufError]:
          LazyList(IArray.fill(11)(0x80.toByte)).read[Sample in Protobuf]
      . assert(_ == ProtobufError(ProtobufError.Reason.MalformedVarint(0)))

      test(m"a varint whose value overflows 64 bits is rejected"):
        // nine continuation bytes then a tenth byte contributing more than bit 63.
        capture[ProtobufError]:
          decode(0x80.toByte, 0x80.toByte, 0x80.toByte, 0x80.toByte, 0x80.toByte, 0x80.toByte,
              0x80.toByte, 0x80.toByte, 0x80.toByte, 0x02)
      . assert(_ == ProtobufError(ProtobufError.Reason.Overflow(0)))

    suite(m"HTTP content-type integration"):
      test(m"serialises with the application/protobuf media type"):
        Person(t"Alice", 30).in[Protobuf].generic(0)
      . assert(_ == t"application/protobuf")

      test(m"request/response body round-trips"):
        val message = Person(t"Alice", 30).in[Protobuf]
        message.generic(1).read[Person in Protobuf]
      . assert(_ == Person(t"Alice", 30))

    suite(m"Optics"):
      import protobufConversion.encodable
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
        val packed = IArray[Byte](0x0a, 0x06, 0x03, 0x8e.toByte, 0x02, 0x9e.toByte, 0xa7.toByte, 0x05)
        packed.read[Numbers in Protobuf]
      . assert(_ == Numbers(List(3, 270, 86942)))

      test(m"unpacked occurrences of a packable element still gather"):
        // Two unpacked varint occurrences of field 1: 3 and 270.
        val unpacked = IArray[Byte](0x08, 0x03, 0x08, 0x8e.toByte, 0x02)
        unpacked.read[Numbers in Protobuf]
      . assert(_ == Numbers(List(3, 270)))

      test(m"the last occurrence of a scalar field wins"):
        // x=1, x=9, y=2.
        val bytes = IArray[Byte](0x08, 0x01, 0x08, 0x09, 0x10, 0x02)
        bytes.read[Point in Protobuf]
      . assert(_ == Point(9, 2))

      test(m"an unknown field number is skipped whole"):
        // Field 5 (varint 1), then x=3, y=4.
        val bytes = IArray[Byte](0x28, 0x01, 0x08, 0x03, 0x10, 0x04)
        bytes.read[Point in Protobuf]
      . assert(_ == Point(3, 4))

      test(m"a missing scalar field takes its proto3 zero"):
        // Only x=3.
        IArray[Byte](0x08, 0x03).read[Point in Protobuf]
      . assert(_ == Point(3, 0))

      test(m"a missing field with a declared default takes it"):
        IArray[Byte](0x08, 0x03).read[Defaulted in Protobuf]
      . assert(_ == Defaulted(3, 7))

      test(m"an empty message reads as all-absent"):
        IArray[Byte]().read[Point in Protobuf]
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
