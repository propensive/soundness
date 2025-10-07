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
┃    Soundness, version 0.43.0.                                                                    ┃
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
package rudiments

import soundness.*

case class Person(name: Text, age: Int)

object Tests extends Suite(m"Rudiments Tests"):
  def run(): Unit =

    test(m"Dual extraction"):
      object AsInt:
        def unapply(x: String): Option[Char] = Some('I')

      object AsLong:
        def unapply(x: String): Option[Char] = Some('J')

      "123" match
        case AsInt(x) & AsLong(y) => (x, y)
        case _                    => ('X', 'X')
    .assert(_ == ('I', 'J'))

    // test(m"Display a PID"):
    //   Pid(2999).toString
    // .assert(_ == "↯2999")

    suite(m"bin tests"):
      test(m"Specify a byte"):
        val x: Byte = bin"10101010"
        x
      .assert(_ == -86)

      test(m"Specify a short"):
        val x: Short = bin"01111111 11111111"
        x
      .assert(_ == 32767)

      test(m"Specify an integer"):
        val x: Int = bin"00000000 11111111 11111111 11111111"
        x
      .assert(_ == 16777215)

      test(m"Specify a long"):
        val x: Long = bin"10101010 10101010 10101010 10101010 00000000 11111111 11111111 11111111"
        x
      .assert(_ == -6148914694083051521L)

      test(m"Too many bits"):
        demilitarize:
          val long: Long =
            bin"010101010 10101010 10101010 10101010 00000000 11111111 11111111 11111111"

          long
        .map(_.message)
      .assert(_ == List(t"hypotenuse: a binary literal must be 8, 16, 32 or 64 bits long"))

      test(m"Incorrect bit count"):
        demilitarize:
          val x: Long = bin"0101010 10101010 10101010 10101010 00000000 11111111 11111111 11111111"
          x
        .map(_.message)
      .assert(_ == List(t"hypotenuse: a binary literal must be 8, 16, 32 or 64 bits long"))

      test(m"Too many bits for type"):
        demilitarize:
          val x: Byte = bin"00011111 11111111"
          x
      .assert(_.nonEmpty)

      test(m"Non-binary content"):
        demilitarize:
          bin"00011112 11111111"
        .map(_.message)
      .assert(_ == List(t"hypotenuse: a binary value can only contain characters '0' or '1'"))

    suite(m"hex tests"):
      test(m"Specify some bytes"):
        hex"bacdf1e9".to(List)
      .assert(_ == Bytes(-70, -51, -15, -23).to(List))

      test(m"Specify some bytes in uppercase with a space"):
        hex"BACD F1E9".to(List)
      .assert(_ == Bytes(-70, -51, -15, -23).to(List))

      test(m"Non-even number of bytes"):
        demilitarize:
          hex"bacdf1e"
        .map(_.message)
      .assert(_ == List(t"hypotenuse: a hexadecimal value must have an even number of digits"))

      test(m"Non-hex content"):
        demilitarize:
          hex"bacdf1eg"
        .map(_.message)
      .assert(_ == List(t"hypotenuse: g is not a valid hexadecimal character"))

      /*test(m"Convert a byte to hex"):
        126.toByte.hex
      .assert(_ == t"7e")

      test(m"Convert a short to hex"):
        32767.toShort.hex
      .assert(_ == t"7fff")

      test(m"Convert an integer to hex"):
        123456789.hex
      .assert(_ == t"75bcd15")

      test(m"Convert a long to hex"):
        654321123456789L.hex
      .assert(_ == t"2531a0221f715")*/

      // test(m"Pattern match hex"):
      //   t"1234" match
      //     case Hex(value) => value
      //     case _          => 0
      // .assert(_ == 4660)

    suite(m"Collections tests"):
      val numbers = List(t"one", t"two", t"four", t"six", t"eight", t"nine")

      // test(m"Index unique numbers by their first letter"):
      //   safely:
      //     numbers.indexBy(_.prim)
      // .assert(_ == Map('o' -> t"one", 't' -> t"two", 'f' -> t"four", 's' -> t"six", 'e' -> t"eight", 'n' -> t"nine"))

      //test(m"Index unique numbers by their length"):
      //  capture[DuplicateIndexError]:
      //    numbers.indexBy(_.length)
      //.assert(_ == DuplicateIndexError())

      test(m"Sift some options"):
        List(None, Some(1), Some(2), None).sift[Some[Int]]
      .assert(_ == List(Some(1), Some(2)))

      test(m"Sift on singleton type"):
        List.range(0, 10).sift[5]
      .assert(_ == List(5))

      test(m"Sift on a union of singleton types"):
        List.range(0, 10).sift[5 | 7]
      .assert(_ == List(5, 7))

      test(m"Map a List to twins"):
        List(1, 2, 3).bi
      .assert(_ == List((1, 1), (2, 2), (3, 3)))

      test(m"Map a Set to triples"):
        Set(1, 2, 3).tri
      .assert(_ == Set((1, 1, 1), (2, 2, 2), (3, 3, 3)))

      test(m"Take a snapshot of an array"):
        val array = Array[Int](1, 2, 3, 4, 5)
        array(1) = 17
        val snapshot: IArray[Int] = array.snapshot
        array(1) = 42
        snapshot.to(List)
      .assert(_ == List(1, 17, 3, 4, 5))

      test(m"Take Map#upsert as an insertion"):
        val map = Map(1 -> "one", 2 -> "two")
        map.upsert(3, _.or("three"))
      .assert(_ == Map(1 -> "one", 2 -> "two", 3 -> "three"))

      test(m"Take Map#upsert as an update"):
        val map = Map(1 -> "one", 2 -> "two")
        map.upsert(2, _.or("")+"!")
      .assert(_ == Map(1 -> "one", 2 -> "two!"))

      test(m"Collation"):
        val map1 = Map(1 -> List("one"), 2 -> List("two"))
        val map2 = Map(2 -> List("deux"), 3 -> List("trois"))
        map1.collate(map2): (left, right) =>
          left ::: right
      .assert(_ == Map(1 -> List("one"), 2 -> List("two", "deux"), 3 -> List("trois")))

      test(m"runs"):
        List(1, 2, 2, 1, 1, 1, 4, 4).runs
      .assert(_ == List(List(1), List(2, 2), List(1, 1, 1), List(4, 4)))

      test(m"runsBy"):
        List(1, 2, 2, 1, 1, 1, 4, 4).runsBy(_%3)
      .assert(_ == List(List(1), List(2, 2), List(1, 1, 1, 4, 4)))

    suite(m"Longest train tests"):
      test(m"Find longest train of zeros in middle"):
        List(1, 0, 0, 2, 3, 4, 0, 0, 0, 5, 6, 0, 7).longestTrain(_ == 0)
      .assert(_ == (6, 3))

      test(m"Find longest train of zeros at start"):
        List(0, 0, 0, 2, 3, 4, 0, 0, 1, 5, 6, 0, 7).longestTrain(_ == 0)
      .assert(_ == (0, 3))

      test(m"Find longest train of zeros at end"):
        List(0, 0, 1, 2, 3, 4, 0, 0, 1, 5, 6, 0, 0, 0, 0).longestTrain(_ == 0)
      .assert(_ == (11, 4))

    suite(m"Optional tests"):
      val absentInt: Optional[Int] = Unset
      val setInt: Optional[Int] = 42

      test(m"Check whether absent value is absent"):
        absentInt.absent
      .assert(_ == true)

      test(m"Check thet set value is not absent"):
        setInt.absent
      .assert(_ == false)

      test(m"Unsafely vouch a set value"):
        val x: Int = setInt.vouch
        x
      .assert(_ == 42)

      test(m"Provide an alternative for an absent value"):
        absentInt.or(1)
      .assert(_ == 1)

      test(m"Provide an alternative for a set value"):
        setInt.or(1)
      .assert(_ == 42)

      test(m"Presume a default value for an absent value"):
        absentInt.presume
      .assert(_ == 0)

      test(m"Convert an absent value to an Option"):
        absentInt.option
      .assert(_ == None)

      test(m"Convert a set value to an Option"):
        setInt.option
      .assert(_ == Some(42))

      test(m"Fold over a Optional"):
        absentInt.lay(0)(_ + 1)
      .assert(_ == 0)

      test(m"Fold over a set Optional"):
        setInt.lay(0)(_ + 1)
      .assert(_ == 43)

      test(m"Map over an absent Optional"):
        absentInt.let(_ + 1)
      .assert(_ == Unset)

      test(m"Map over a set Optional"):
        setInt.let(_ + 1)
      .assert(_ == 43)

      test(m"Construct a new Optional from a null value"):
        val x: String | Null = null
        Optional(x)
      .assert(_ == Unset)

      test(m"Construct a new Optional from a possibly-null value"):
        val x: String | Null = ""
        Optional(x)
      .assert(_ == "")

      test(m"Convert an option to an optional"):
        val x: Option[Int] = Some(42)
        x.optional
      .assert(_ == 42)

      test(m"Convert an empty Option to an optional"):
        val x: Option[Int] = None
        x.optional
      .assert(_ == Unset)

      // test(m"Presume a value for an empty Option"):
      //   val x: Option[List[Int]] = None
      //   x.presume
      // .assert(_ == Nil)

    suite(m"PID & exit status tests"):
      test(m"Zero exit-status is OK"):
        Exit(0)
      .assert(_ == Exit.Ok)

      test(m"Positive exit-status is a failure"):
        Exit(1)
      .assert(_ == Exit.Fail(1))

      test(m"Ok has exit status 0"):
        Exit.Ok
      .assert(_() == 0)

      test(m"Failure has non-zero exit status"):
        Exit.Fail(3)
      .assert(_() == 3)

    suite(m"Bytes tests"):
      test(m"Construct a `Bytes` literal"):
        Bytes(1, 2, 3)
      .assert(_.length == 3)

      // test(m"Construct a `Bytes` value from a Long"):
      //   Bytes(Long.MaxValue)
      // .assert(_.length == 8)

      test(m"Construct an empty `Bytes`"):
        Bytes()
      .assert(_.length == 0)

    suite(m"Memory tests"):
      test(m"Construct a simple Memory"):
        4.b: Memory
      .assert(_ == Memory(4))

      test(m"Divide one memory by an integer"):
        1024.b/128
      .assert(_ == 8.b)

      test(m"Divide one `Memory` by another"):
        1024.b/128.b
      .assert(_ == 8.0)

      // test(m"Construct a simple Memory in kB"):
      //   4.kb: Memory
      // .assert(_ == Memory(4096))

      // test(m"Construct a simple Memory in MB"):
      //   4.mb: Memory
      // .assert(_ == Memory(4096*1024L))

      // test(m"Construct a simple Memory in GB"):
      //   4.gb: Memory
      // .assert(_ == Memory(4096*1024L*1024L))

      // test(m"Construct a simple Memory in TB"):
      //   4.tb: Memory
      // .assert(_ == Memory(4096*1024L*1024L*1024L))

      /*test(m"Compare bytes with >"):
        4.gb > 4.mb
      .assert(_ == true)

      test(m"Compare bytes with >="):
        4.gb >= 4.mb*1024
      .assert(_ == true)*/

      // test(m"Sort some byte sizes"):
      //   List(1.b, 1.mb, 1.kb).sorted
      // .assert(_ == List(1.b, 1.kb, 1.mb))

    // suite(m"Y-combinator test"):
    //   test(m"Check factorial implementation"):
    //     def factorial(n: Int): Int = fix[Int] { i => if i <= 0 then 1 else i*recur(i - 1) } (n)
    //     factorial(4)
    //   .assert(_ == 24)
