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
┃    Soundness, version 0.46.0.                                                                    ┃
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
package zephyrine

import language.experimental.captureChecking

import soundness.*

import randomization.unseeded

import autopsies.contrastExpectations

object Tests extends Suite(m"Zephyrine tests"):
  val bytes = Bytes.fill(1000)(_.toByte)
  def run(): Unit = stochastic:
    for i <- 1 to 10 do
      val stream = Stream(bytes).shred(10.0, 10.0)//.filter(!_.isEmpty)
      test(m"Conduit always starts at first byte"):
        val conduit = Conduit(stream)
        conduit.datum

      . assert(_ == 0.toByte)

      test(m"Conduit second byte is always 1"):
        val conduit = Conduit(stream)
        conduit.next()
        conduit.datum

      . assert(_ == 1.toByte)

      test(m"Can capture first ten bytes"):
        val conduit = Conduit(stream)
        conduit.take(10)

      . assert(_ === Bytes(0, 1, 2, 3, 4, 5, 6, 7, 8, 9))

      test(m"Can capture second ten bytes"):
        val conduit = Conduit(stream)
        conduit.skip(10)
        conduit.take(10)

      . assert(_ === Bytes(10, 11, 12, 13, 14, 15, 16, 17, 18, 19))

      test(m"Next ten times reaches same datum"):
        val conduit = Conduit(stream)
        for i <- 1 to 10 do conduit.next()
        conduit.datum

      . assert(_ == 10.toByte)

      test(m"Position on next after save"):
        val conduit = Conduit(stream)

        conduit.skip(15)
        conduit.mark()
        conduit.skip(10)
        (conduit.save().last, conduit.datum)

      . assert(_ == (24.toByte, 25.toByte))

      test(m"Position on next after take"):
        val conduit = Conduit(stream)

        conduit.skip(15)
        (conduit.take(10).last, conduit.datum)

      . assert(_ == (24.toByte, 25.toByte))

      test(m"Breaking before starts on consistent datum"):
        val conduit = Conduit(stream)

        conduit.skip(15)
        conduit.truncate()
        conduit.remainder.head.head

      . assert(_ == 15.toByte)

      test(m"Breaking after starts on consistent datum"):
        val conduit = Conduit(stream)

        conduit.skip(15)
        conduit.break()
        conduit.remainder.head.head

      . assert(_ == 16.toByte)

    suite(m"Search tests"):
      test(m"Can find first byte"):
        val stream = Stream(Bytes(0x10, 0x11, 0x12, 0x13), Bytes(0x14, 0x15))
        val conduit = Conduit(stream)
        conduit.search(0x10)

      . assert(_ == true)

      test(m"Can't find nonexistent byte"):
        val stream = Stream(Bytes(0x10, 0x11, 0x12, 0x13), Bytes(0x14, 0x15))
        val conduit = Conduit(stream)
        conduit.search(0x18)

      . assert(_ == false)

      test(m"Can find sequence of two bytes"):
        val stream = Stream(Bytes(0x10, 0x11, 0x12, 0x13), Bytes(0x14, 0x15))
        val conduit = Conduit(stream)
        conduit.search(0x11, 0x12)

      . assert(_ == true)

      test(m"Can find sequence of two bytes"):
        val stream = Stream(Bytes(0x10, 0x11, 0x12, 0x13), Bytes(0x14, 0x15))
        val conduit = Conduit(stream)
        conduit.search(0x11, 0x12)

      . assert(_ == true)

      test(m"Gets correct offset for sequence of two bytes"):
        val stream = Stream(Bytes(0x10, 0x11, 0x12, 0x13), Bytes(0x14, 0x15))
        val conduit = Conduit(stream)
        conduit.search(0x11, 0x12)
        conduit.ordinal

      . assert(_ == Sec)

      test(m"Gets correct offset for sequence of three bytes when they're at the start"):
        val stream = Stream(Bytes(0x10, 0x11, 0x12, 0x13), Bytes(0x14, 0x15))
        val conduit = Conduit(stream)
        conduit.search(0x10, 0x11, 0x12)
        conduit.ordinal

      . assert(_ == Prim)

      test(m"Finds sequence that crosses block boundary"):
        val stream = Stream(Bytes(0x10, 0x11, 0x12, 0x13), Bytes(0x14, 0x15))
        val conduit = Conduit(stream)
        conduit.search(0x13, 0x14)
        conduit.ordinal

      . assert(_ == Quat)


    suite(m"Cursor tests"):
      def hello = Cursor(t"Hello world!".chars.to(List).map(_.show).iterator)
      def numbers = Cursor(t"0123456789abc".chars.to(List).map(_.show).iterator)

      test(m"Iterate over elements"):
        val cursor = hello
        val builder = java.lang.StringBuilder()
        while
          builder.append(cursor.datum(using Unsafe))
          cursor.next()
        do ()

        builder.toString
      . assert(_ == "Hello world!")

      test(m"Capture part of first block"):
        val cursor = hello
        val builder = java.lang.StringBuilder()
        cursor.next()

        cursor.hold:
          val mark = cursor.mark
          for i <- 1 to 2 do cursor.next()
          cursor.clone(mark, cursor.mark)(builder)

        builder.toString
      . assert(_ == "el")

      test(m"Capture spanning block"):
        val cursor = hello
        val builder = java.lang.StringBuilder()
        for i <- 1 to 2 do cursor.next()

        cursor.hold:
          val mark = cursor.mark
          for i <- 1 to 3 do cursor.next()
          cursor.clone(mark, cursor.mark)(builder)

        builder.toString
      . assert(_ == "llo")

      test(m"Capture multiply-spanning block"):
        val cursor = hello
        val builder = java.lang.StringBuilder()
        for i <- 1 to 3 do cursor.next()

        cursor.hold:
          val mark = cursor.mark
          for i <- 1 to 4 do cursor.next()
          cursor.clone(mark, cursor.mark)(builder)

        builder.toString
      . assert(_ == "lo w")

      test(m"Capture multiply-spanning block with nesting"):
        val cursor = hello
        val builder = java.lang.StringBuilder()
        for i <- 1 to 3 do cursor.next()

        cursor.hold:
          val mark1 = cursor.mark
          for i <- 1 to 2 do cursor.next()
          val mark2 = cursor.mark
          cursor.clone(mark1, mark2)(builder)
          for i <- 1 to 2 do cursor.next()

        builder.toString
      . assert(_ == "lo")

      test(m"Capture multiply-spanning block with nesting 2"):
        val cursor = hello
        val builder = java.lang.StringBuilder()
        for i <- 1 to 3 do cursor.next()

        cursor.hold:
          val mark1 = cursor.mark
          for i <- 1 to 2 do cursor.next()
          val mark2 = cursor.mark
          cursor.clone(mark1, mark2)(builder)
          for i <- 1 to 3 do cursor.next()
          cursor.clone(mark1, cursor.mark)(builder)

        builder.toString
      . assert(_ == "lolo wo")

      test(m"Rewind, release and resume"):
        val iterator = Iterator[Text]("one", "two", "three", "four")
        val cursor = Cursor(iterator)
        val builder = new StringBuilder()
        builder.append(cursor.datum(using Unsafe))
        cursor.next().also(builder.append(cursor.datum(using Unsafe)))
        cursor.hold:
          cursor.next().also(builder.append(cursor.datum(using Unsafe)))
          cursor.next().also(builder.append(cursor.datum(using Unsafe)))
          val mark = cursor.mark
          cursor.next()
          cursor.next()
          cursor.next()
          cursor.next()
          cursor.cue(mark)
          cursor.next().also(builder.append(cursor.datum(using Unsafe)))

        cursor.next().also(builder.append(cursor.datum(using Unsafe)))
        cursor.next().also(builder.append(cursor.datum(using Unsafe)))
        cursor.next().also(builder.append(cursor.datum(using Unsafe)))
        cursor.next().also(builder.append(cursor.datum(using Unsafe)))
        cursor.next().also(builder.append(cursor.datum(using Unsafe)))
        cursor.next().also(builder.append(cursor.datum(using Unsafe)))
        cursor.next().also(builder.append(cursor.datum(using Unsafe)))
        builder.toString
      . assert(_ == "onetwothreef")

      test(m"Rewinding"):
        val cursor = numbers
        val builder = java.lang.StringBuilder()
        for i <- 1 to 3 do cursor.next()

        cursor.hold:
          val mark = cursor.mark
          for i <- 1 to 3 do cursor.next()
          cursor.cue(mark)

        cursor.datum(using Unsafe)
      . assert(_ == '3')

      test(m"Rewinding and continuing"):
        val cursor = numbers
        val builder = java.lang.StringBuilder()
        for i <- 1 to 3 do cursor.next()

        cursor.hold:
          val mark = cursor.mark
          for i <- 1 to 3 do cursor.next()
          cursor.cue(mark)

        cursor.next()
        cursor.datum(using Unsafe)
      . assert(_ == '4')

      test(m"Rewinding and continuing to next block"):
        val cursor = numbers
        val builder = java.lang.StringBuilder()
        for i <- 1 to 3 do cursor.next()

        cursor.hold:
          val mark = cursor.mark
          for i <- 1 to 3 do cursor.next()
          cursor.cue(mark)

        for i <- 1 to 2 do cursor.next()
        cursor.datum(using Unsafe)
      . assert(_ == '5')

      test(m"Capture from start to end"):
        val cursor = hello
        val builder = java.lang.StringBuilder()

        cursor.hold:
          val mark = cursor.mark
          while cursor.next() do ()
          cursor.clone(mark, cursor.mark)(builder)

        builder.toString
      . assert(_ == "Hello world!")

      for offset <- 0 to 8 do
        for length <- 1 to 4 do
          test(m"Spans, offset $offset, length $length"):
            val cursor = hello

            for j <- 0 until offset do cursor.next()
            cursor.hold:
              val start = cursor.mark
              for i <- 0 until length do cursor.next()
              cursor.grab(start, cursor.mark)

          . assert(_ == "Hello world!".substring(offset, offset + length).nn)
