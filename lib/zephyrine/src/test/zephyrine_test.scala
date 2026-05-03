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
package zephyrine

import soundness.*

import randomization.unseeded

import supervisors.global
import codicils.panic

object Tests extends Suite(m"Zephyrine tests"):
  val bytes = Data.fill(1000)(_.toByte)
  def run(): Unit = stochastic:

    suite(m"Emitter tests"):
      test(m"mismatched block size"):
        val emitter = Emitter[Text](4, 20)
        emitter.put("one")
        emitter.put("two")
        emitter.finish()
        val it = async(emitter.iterator.to(List))

        unsafely(it.await())
      . assert(_ == List("onet", "wo"))

      test(m"One block, exact size, ready immediately"):
        val emitter = Emitter[Text](4, 3)
        emitter.put("zero")
        emitter.iterator
        if emitter.iterator.hasNext then emitter.iterator.next() else ""
      . assert(_ == "zero")

      test(m"Two blocks, exact size, ready immediately"):
        val emitter = Emitter[Text](4, 2)
        emitter.put("zerofour")
        emitter.iterator
        var out = ""
        if emitter.iterator.hasNext then out += emitter.iterator.next()
        if emitter.iterator.hasNext then out += emitter.iterator.next()
        out
      . assert(_ == "zerofour")

      test(m"More than two blocks, ready immediately"):
        val emitter = Emitter[Text](4, 2)
        emitter.put("zerofoursix")
        emitter.iterator
        var out = ""
        if emitter.iterator.hasNext then out += emitter.iterator.next()
        if emitter.iterator.hasNext then out += emitter.iterator.next()
        out
      . assert(_ == "zerofour")

      test(m"More than two blocks, fragmented, ready immediately"):
        val emitter = Emitter[Text](4, 2)
        emitter.put("12")
        emitter.put("3")
        emitter.put("4")
        emitter.put("5")
        emitter.put("6")
        emitter.put("7")
        emitter.put("8")
        emitter.iterator
        var out = ""
        if emitter.iterator.hasNext then out += emitter.iterator.next()
        if emitter.iterator.hasNext then out += emitter.iterator.next()
        out
      . assert(_ == "12345678")

      test(m"Single long message, with blocking"):
        val emitter = Emitter[Text](4, 2)
        val out = async(emitter.iterator.to(List))
        emitter.put("12345678901234567890")
        emitter.finish()
        unsafely(out.await())
      . assert(_ == List("1234", "5678", "9012", "3456", "7890"))

      test(m"Single long message, with blocking; incomplete final block"):
        val emitter = Emitter[Text](4, 2)
        val out = async(emitter.iterator.to(List))
        emitter.put("123456789012345678")
        emitter.finish()
        unsafely(out.await())
      . assert(_ == List("1234", "5678", "9012", "3456", "78"))

      for i <- 0 to 30 do
        val string = (0 to i).map(_.toString).foldLeft("")(_ + _)
        test(m"String length $i, sent whole, async puts"):
          val emitter = Emitter[Text](5, 2)
          val producer = async:
            emitter.put(string)
            emitter.finish()
          emitter.iterator.foldLeft("")(_ + _)
        . assert(_ == string)

        test(m"String length $i, sent unitarily, async puts"):
          val emitter = Emitter[Text](5, 2)
          val producer = async:
            string.tt.chars.foreach: char =>
              emitter.put(char.toString)
            emitter.finish()
          emitter.iterator.foldLeft("")(_ + _)
        . assert(_ == string)

        test(m"String length $i, sent whole, async reads"):
          val emitter = Emitter[Text](5, 2)
          val output = async(emitter.iterator.foldLeft("")(_ + _))
          emitter.put(string)
          emitter.finish()
          unsafely(output.await())
        . assert(_ == string)

        test(m"String length $i, sent unitarily, async reads"):
          val emitter = Emitter[Text](5, 2)
          val output = async(emitter.iterator.foldLeft("")(_ + _))
          string.tt.chars.each: char =>
            emitter.put(char.toString)
          emitter.finish()
          unsafely(output.await())
        . assert(_ == string)



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

      test(m"Grab spanning multi-character blocks"):
        val cursor = Cursor(Iterator[Text]("hello", "world"))
        for j <- 1 to 2 do cursor.next()
        cursor.hold:
          val start = cursor.mark
          for i <- 1 to 4 do cursor.next()
          cursor.grab(start, cursor.mark)

      . assert(_ == "llow")

      test(m"Grab spanning three multi-character blocks"):
        val cursor = Cursor(Iterator[Text]("one", "two", "three", "four"))
        cursor.hold:
          val start = cursor.mark
          for i <- 1 to 7 do cursor.next()
          cursor.grab(start, cursor.mark)

      . assert(_ == "onetwot")

    suite(m"Cursor[Data] tests"):
      def stream = Stream(bytes).shred(10.0, 10.0).filter(_.nonEmpty)
      def byteCursor = Cursor[Data](stream.iterator)

      test(m"Cursor[Data] starts at first byte"):
        byteCursor.datum(using Unsafe)

      . assert(_ == 0.toByte)

      test(m"Cursor[Data] second byte is 1"):
        val cursor = byteCursor
        cursor.next()
        cursor.datum(using Unsafe)

      . assert(_ == 1.toByte)

      test(m"Cursor[Data] take first ten bytes"):
        val cursor = byteCursor
        cursor.take(Data())(10)

      . assert(_ === Data(0, 1, 2, 3, 4, 5, 6, 7, 8, 9))

      test(m"Cursor[Data] take second ten bytes"):
        val cursor = byteCursor
        for i <- 0 until 10 do cursor.next()
        cursor.take(Data())(10)

      . assert(_ === Data(10, 11, 12, 13, 14, 15, 16, 17, 18, 19))

      test(m"Cursor[Data] grab between marks across block boundary"):
        val cursor = byteCursor
        for i <- 0 until 5 do cursor.next()
        cursor.hold:
          val start = cursor.mark
          for i <- 0 until 10 do cursor.next()
          cursor.grab(start, cursor.mark)

      . assert(_ === Data(5, 6, 7, 8, 9, 10, 11, 12, 13, 14))

      test(m"Cursor[Data] seek finds byte"):
        val cursor = byteCursor
        cursor.seek(15.toByte)
        cursor.datum(using Unsafe)

      . assert(_ == 15.toByte)

      test(m"Cursor[Data] blockTail at start returns whole first block"):
        val singleStream = Stream(Data(10, 11, 12, 13, 14))
        val cursor = Cursor[Data](singleStream.iterator)
        cursor.blockTail

      . assert(_ === Data(10, 11, 12, 13, 14))

      test(m"Cursor[Data] blockTail mid-block returns suffix"):
        val singleStream = Stream(Data(10, 11, 12, 13, 14))
        val cursor = Cursor[Data](singleStream.iterator)
        cursor.next()
        cursor.next()
        cursor.blockTail

      . assert(_ === Data(12, 13, 14))

      test(m"Cursor[Data] advanceBlock from start jumps to next block"):
        val twoBlocks = Stream(Data(1, 2, 3), Data(4, 5, 6))
        val cursor = Cursor[Data](twoBlocks.iterator)
        cursor.advanceBlock()
        cursor.datum(using Unsafe)

      . assert(_ == 4.toByte)

      test(m"Cursor[Data] advanceBlock returns true when next block exists"):
        val twoBlocks = Stream(Data(1, 2, 3), Data(4, 5, 6))
        val cursor = Cursor[Data](twoBlocks.iterator)
        cursor.advanceBlock()

      . assert(_ == true)

      test(m"Cursor[Data] advanceBlock returns false at last block"):
        val twoBlocks = Stream(Data(1, 2, 3), Data(4, 5, 6))
        val cursor = Cursor[Data](twoBlocks.iterator)
        cursor.advanceBlock()
        cursor.advanceBlock()

      . assert(_ == false)

      test(m"Cursor[Data] blockTail + advanceBlock reconstructs stream"):
        val blocks = Stream(Data(1, 2, 3), Data(4, 5), Data(6, 7, 8, 9))
        val cursor = Cursor[Data](blocks.iterator)
        val out = scala.collection.mutable.ArrayBuffer[Byte]()
        out.appendAll(cursor.blockTail.iterator)
        while cursor.advanceBlock() do out.appendAll(cursor.blockTail.iterator)
        Data(out.toArray*)

      . assert(_ === Data(1, 2, 3, 4, 5, 6, 7, 8, 9))

      test(m"Cursor[Data] remainder from start equals full stream"):
        val blocks = Stream(Data(1, 2, 3), Data(4, 5), Data(6, 7))
        val cursor = Cursor[Data](blocks.iterator)
        cursor.remainder.flatten.to(List)

      . assert(_ == List[Byte](1, 2, 3, 4, 5, 6, 7))

      test(m"Cursor[Data] remainder mid-block emits cross-block tail"):
        val blocks = Stream(Data(1, 2, 3, 4, 5), Data(6, 7, 8))
        val cursor = Cursor[Data](blocks.iterator)
        for i <- 0 until 3 do cursor.next()
        cursor.remainder.flatten.to(List)

      . assert(_ == List[Byte](4, 5, 6, 7, 8))

      test(m"Cursor[Data] remainder inside hold still emits unconsumed tail"):
        val blocks = Stream(Data(1, 2, 3, 4, 5), Data(6, 7, 8))
        val cursor = Cursor[Data](blocks.iterator)
        for i <- 0 until 3 do cursor.next()
        cursor.hold(cursor.remainder.flatten.to(List))

      . assert(_ == List[Byte](4, 5, 6, 7, 8))

    suite(m"Cursor2 tests"):
      def hello = Cursor2(t"Hello world!".chars.to(List).map(_.show).iterator)
      def numbers = Cursor2(t"0123456789abc".chars.to(List).map(_.show).iterator)

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
        val cursor = Cursor2(iterator)
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
        for i <- 1 to 3 do cursor.next()

        cursor.hold:
          val mark = cursor.mark
          for i <- 1 to 3 do cursor.next()
          cursor.cue(mark)

        cursor.datum(using Unsafe)
      . assert(_ == '3')

      test(m"Rewinding and continuing"):
        val cursor = numbers
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

      test(m"Grab spanning multi-character blocks"):
        val cursor = Cursor2(Iterator[Text]("hello", "world"))
        for j <- 1 to 2 do cursor.next()
        cursor.hold:
          val start = cursor.mark
          for i <- 1 to 4 do cursor.next()
          cursor.grab(start, cursor.mark)

      . assert(_ == "llow")

      test(m"Grab spanning three multi-character blocks"):
        val cursor = Cursor2(Iterator[Text]("one", "two", "three", "four"))
        cursor.hold:
          val start = cursor.mark
          for i <- 1 to 7 do cursor.next()
          cursor.grab(start, cursor.mark)

      . assert(_ == "onetwot")

      test(m"Pre-filled buffer (Direct mode)"):
        val cursor = Cursor2(t"Hello world!")
        val builder = java.lang.StringBuilder()
        while
          builder.append(cursor.datum(using Unsafe))
          cursor.next()
        do ()
        builder.toString
      . assert(_ == "Hello world!")

      test(m"Cursor2 from explicit loader"):
        val chunks = scala.collection.mutable.Queue(t"abc", t"def", t"ghi")
        val cursor = Cursor2[Text](() => if chunks.isEmpty then Unset else chunks.dequeue())
        val builder = java.lang.StringBuilder()
        while
          builder.append(cursor.datum(using Unsafe))
          cursor.next()
        do ()
        builder.toString
      . assert(_ == "abcdefghi")

    suite(m"Cursor2[Data] tests"):
      def stream = Stream(bytes).shred(10.0, 10.0).filter(_.nonEmpty)
      def byteCursor = Cursor2[Data](stream.iterator)

      test(m"Cursor2[Data] starts at first byte"):
        byteCursor.datum(using Unsafe)

      . assert(_ == 0.toByte)

      test(m"Cursor2[Data] second byte is 1"):
        val cursor = byteCursor
        cursor.next()
        cursor.datum(using Unsafe)

      . assert(_ == 1.toByte)

      test(m"Cursor2[Data] take first ten bytes"):
        val cursor = byteCursor
        cursor.take(Data())(10)

      . assert(_ === Data(0, 1, 2, 3, 4, 5, 6, 7, 8, 9))

      test(m"Cursor2[Data] take second ten bytes"):
        val cursor = byteCursor
        for i <- 0 until 10 do cursor.next()
        cursor.take(Data())(10)

      . assert(_ === Data(10, 11, 12, 13, 14, 15, 16, 17, 18, 19))

      test(m"Cursor2[Data] grab between marks across block boundary"):
        val cursor = byteCursor
        for i <- 0 until 5 do cursor.next()
        cursor.hold:
          val start = cursor.mark
          for i <- 0 until 10 do cursor.next()
          cursor.grab(start, cursor.mark)

      . assert(_ === Data(5, 6, 7, 8, 9, 10, 11, 12, 13, 14))

      test(m"Cursor2[Data] seek finds byte"):
        val cursor = byteCursor
        cursor.seek(15.toByte)
        cursor.datum(using Unsafe)

      . assert(_ == 15.toByte)
