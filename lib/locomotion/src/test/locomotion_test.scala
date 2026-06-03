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
┃    Soundness, version 0.54.0.                                                                    ┃
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

object Tests extends Suite(m"Locomotion Protobuf Tests"):
  def run(): Unit =
    def wire[value: Encodable in Protobuf](value: value): List[Int] =
      value.protobuf.encode.to[List].map(_.toInt & 0xff)

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
        Stream(Sample(150).protobuf.encode).read[Sample over Protobuf]
      . assert(_ == Sample(150))

      test(m"two int fields, one at its default"):
        Stream(Point(0, 5).protobuf.encode).read[Point over Protobuf]
      . assert(_ == Point(0, 5))

      test(m"string and int fields"):
        Stream(Person(t"Alice", 30).protobuf.encode).read[Person over Protobuf]
      . assert(_ == Person(t"Alice", 30))

      test(m"read[Protobuf] then as[T] (two-step)"):
        Stream(Person(t"Alice", 30).protobuf.encode).read[Protobuf].as[Person]
      . assert(_ == Person(t"Alice", 30))

      test(m"nested message"):
        Stream(Wrapper(Point(3, 4), t"origin").protobuf.encode).read[Wrapper over Protobuf]
      . assert(_ == Wrapper(Point(3, 4), t"origin"))

      test(m"sparse field numbers"):
        Stream(Sparse(9, t"x").protobuf.encode).read[Sparse over Protobuf]
      . assert(_ == Sparse(9, t"x"))

    suite(m"Repeated fields"):
      test(m"repeated strings round-trip in order"):
        Stream(Tags(List(t"a", t"b", t"c")).protobuf.encode).read[Tags over Protobuf]
      . assert(_ == Tags(List(t"a", t"b", t"c")))

      test(m"repeated ints round-trip, keeping default elements"):
        Stream(Numbers(List(0, 1, 2)).protobuf.encode).read[Numbers over Protobuf]
      . assert(_ == Numbers(List(0, 1, 2)))

      test(m"repeated ints are packed into one length-delimited field"):
        wire(Numbers(List(3, 270, 86942))).take(2)
      . assert(_ == List(0x0a, 0x06))

      test(m"a packed field produced elsewhere decodes back to a List"):
        // The canonical packed encoding of [3, 270, 86942] (matches `protoc`).
        val packed = IArray[Byte](0x0a, 0x06, 0x03, 0x8e.toByte, 0x02, 0x9e.toByte, 0xa7.toByte, 0x05)
        Stream(packed).read[Numbers over Protobuf]
      . assert(_ == Numbers(List(3, 270, 86942)))

      test(m"empty repeated field writes nothing"):
        wire(Tags(Nil))
      . assert(_ == Nil)

    suite(m"Optional presence"):
      test(m"a set optional round-trips"):
        Stream(MaybeName(t"set").protobuf.encode).read[MaybeName over Protobuf]
      . assert(_ == MaybeName(t"set"))

      test(m"an unset optional writes nothing and round-trips to Unset"):
        Stream(MaybeName(Unset).protobuf.encode).read[MaybeName over Protobuf]
      . assert(_ == MaybeName(Unset))

    suite(m"Sum types (oneof)"):
      test(m"the Circle variant round-trips"):
        val shape: Shape = Shape.Circle(5)
        Stream(shape.protobuf.encode).read[Shape over Protobuf]
      . assert(_ == Shape.Circle(5))

      test(m"the Rectangle variant round-trips"):
        val shape: Shape = Shape.Rectangle(3, 4)
        Stream(shape.protobuf.encode).read[Shape over Protobuf]
      . assert(_ == Shape.Rectangle(3, 4))

    suite(m"Field-number fallback"):
      test(m"unannotated fields use 1-based declaration order"):
        wire(Unnumbered(150, 0)).take(2)
      . assert(_ == List(0x08, 0x96))

      test(m"unannotated message round-trips"):
        Stream(Unnumbered(7, 9).protobuf.encode).read[Unnumbered over Protobuf]
      . assert(_ == Unnumbered(7, 9))

    suite(m"Typed integer encodings"):
      val typed = Typed(7.bits.u32, 8L.bits.u64, -3.bits.s32, -4L.bits.s64, 5.bits, 6L.bits)

      test(m"all typed integers round-trip"):
        Stream(typed.protobuf.encode).read[Typed over Protobuf]
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
        Stream(labels.protobuf.encode).read[Labels over Protobuf]
      . assert(_ == Labels(Map(t"a" -> t"1", t"b" -> t"2")))

      test(m"a string→int map round-trips"):
        val counts = Counts(Map(t"x" -> 10, t"y" -> 20))
        Stream(counts.protobuf.encode).read[Counts over Protobuf]
      . assert(_ == Counts(Map(t"x" -> 10, t"y" -> 20)))

      test(m"an empty map writes nothing"):
        wire(Labels(Map()))
      . assert(_ == Nil)

      test(m"a single entry encodes as a length-delimited message"):
        wire(Labels(Map(t"a" -> t"b")))
      . assert(_ == List(0x0a, 0x06, 0x0a, 0x01, 0x61, 0x12, 0x01, 0x62))
