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
package polaris

import soundness.*

case class Header(magic: Int, version: Short, flags: Byte)

object Header:
  given debufferable: Header is Debufferable = Debufferable.derived

case class Pair(left: Short, right: Short)

object Pair:
  given debufferable: Pair is Debufferable = Debufferable.derived

case class Nested(header: Header, pair: Pair)

object Nested:
  given debufferable: Nested is Debufferable = Debufferable.derived

object Tests extends Suite(m"Polaris Tests"):
  def run(): Unit =
    suite(m"Width tests"):
      test(m"A Byte is one byte wide"):
        byteWidth[Byte]
      . assert(_ == 1)

      test(m"A Short is two bytes wide"):
        byteWidth[Short]
      . assert(_ == 2)

      test(m"An Int is four bytes wide"):
        byteWidth[Int]
      . assert(_ == 4)

      test(m"A Long is eight bytes wide"):
        byteWidth[Long]
      . assert(_ == 8)

      test(m"A derived product's width is the sum of its fields'"):
        byteWidth[Header]
      . assert(_ == 7)

      test(m"A nested product's width is the sum of its fields'"):
        byteWidth[Nested]
      . assert(_ == 11)

    suite(m"Buffer tests"):
      test(m"A new buffer starts at the beginning"):
        Buffer(IArray[Byte](1, 2, 3)).offset
      . assert(_ == 0)

      test(m"A buffer can start at an offset"):
        Buffer(IArray[Byte](1, 2, 3), 2).offset
      . assert(_ == 2)

      test(m"Advancing a buffer moves its position"):
        val buffer = Buffer(IArray[Byte](1, 2, 3))
        buffer.advance(2)
        buffer.offset
      . assert(_ == 2)

      test(m"Reading from a buffer advances it by the value's width"):
        IArray[Byte](0, 0, 0, 1, 9).buffer:
          unpack[Int]
          summon[Buffer].offset
      . assert(_ == 4)

      test(m"Successive reads continue where the last left off"):
        IArray[Byte](0, 1, 0, 2).buffer:
          (unpack[Short], unpack[Short])
      . assert(_ == (1.toShort, 2.toShort))

    suite(m"Primitive unpacking tests"):
      test(m"Unpack a Byte"):
        IArray[Byte](7.toByte).unpackFrom[Byte](0)
      . assert(_ == 7.toByte)

      test(m"Unpack a negative Byte"):
        IArray[Byte]((-1).toByte).unpackFrom[Byte](0)
      . assert(_ == (-1).toByte)

      test(m"Unpack a big-endian Short"):
        IArray[Byte](1, 2).unpackFrom[Short](0)
      . assert(_ == 258.toShort)

      test(m"Unpack a negative Short"):
        IArray[Byte]((-1).toByte, (-1).toByte).unpackFrom[Short](0)
      . assert(_ == (-1).toShort)

      test(m"Unpack a big-endian Int"):
        IArray[Byte](0, 0, 1, 0).unpackFrom[Int](0)
      . assert(_ == 256)

      test(m"Unpack a negative Int"):
        IArray[Byte]((-1).toByte, (-1).toByte, (-1).toByte, (-1).toByte).unpackFrom[Int](0)
      . assert(_ == -1)

      test(m"Unpack a big-endian Long"):
        IArray[Byte](0, 0, 0, 0, 0, 0, 1, 0).unpackFrom[Long](0)
      . assert(_ == 256L)

      test(m"Unpack from a non-zero offset"):
        IArray[Byte](9, 9, 0, 0, 1, 0).unpackFrom[Int](2)
      . assert(_ == 256)

      test(m"Unpacking past the end of the data fails"):
        try IArray[Byte](0, 0).unpackFrom[Int](0).toString.nn
        catch case error: ArrayIndexOutOfBoundsException => "out of bounds"
      . assert(_ == "out of bounds")

    suite(m"Product unpacking tests"):
      test(m"Unpack a derived product"):
        IArray[Byte](0, 0, 1, 0, 0, 3, 7).unpackFrom[Header](0)
      . assert(_ == Header(256, 3.toShort, 7.toByte))

      test(m"Unpack a nested product"):
        IArray[Byte](0, 0, 1, 0, 0, 3, 7, 0, 1, 0, 2).unpackFrom[Nested](0)
      . assert(_ == Nested(Header(256, 3.toShort, 7.toByte), Pair(1.toShort, 2.toShort)))

      test(m"Fields are read in declaration order"):
        IArray[Byte](0, 1, 0, 2).unpackFrom[Pair](0)
      . assert(_ == Pair(1.toShort, 2.toShort))

      test(m"Reading a product advances the buffer by its width"):
        IArray[Byte](0, 1, 0, 2, 0, 3, 0, 4).buffer:
          (unpack[Pair], unpack[Pair])
      . assert(_ == (Pair(1.toShort, 2.toShort), Pair(3.toShort, 4.toShort)))

    suite(m"Array unpacking tests"):
      val data = IArray[Byte](0, 0, 0, 1, 0, 0, 0, 2, 0, 0, 0, 3)

      test(m"Unpack an array of Ints"):
        data.unpackFrom[IArray[Int]](0)(3).to(List)
      . assert(_ == List(1, 2, 3))

      test(m"Unpack a prefix of an array"):
        data.unpackFrom[IArray[Int]](0)(2).to(List)
      . assert(_ == List(1, 2))

      test(m"Unpack an empty array"):
        data.unpackFrom[IArray[Int]](0)(0).to(List)
      . assert(_ == Nil)

      test(m"Unpack an array from an offset"):
        data.unpackFrom[IArray[Int]](4)(2).to(List)
      . assert(_ == List(2, 3))

      test(m"Unpack an array of products"):
        IArray[Byte](0, 1, 0, 2, 0, 3, 0, 4).unpackFrom[IArray[Pair]](0)(2).to(List)
      . assert(_ == List(Pair(1.toShort, 2.toShort), Pair(3.toShort, 4.toShort)))

      test(m"An array continuation does not advance the caller's buffer"):
        data.buffer:
          unpack[IArray[Int]](3)
          summon[Buffer].offset
      . assert(_ == 0)

      test(m"An array continuation can be invoked more than once"):
        data.buffer:
          val read = unpack[IArray[Int]]
          (read(1).to(List), read(2).to(List))
      . assert(_ == (List(1), List(1, 2)))
