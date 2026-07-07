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
package zephyrine

import soundness.*

import randomization.unseededRandomization

import supervisors.globalSupervisor
import probates.panicProbate

object Tests extends Suite(m"Zephyrine tests"):
  val bytes = Data.fill(1000)(_.toByte)
  def run(): Unit = stochastic:

    suite(m"Producer tests"):
      test(m"mismatched block size"):
        val producer = Producer[Text](4, 20)
        producer.put("one")
        producer.put("two")
        producer.finish()
        val it = async(producer.iterator.to(List))

        unsafely(it.await())
      . assert(_ == List("onet", "wo"))

      test(m"One block, exact size, ready immediately"):
        val producer = Producer[Text](4, 3)
        producer.put("zero")
        producer.iterator
        if producer.iterator.hasNext then producer.iterator.next() else ""
      . assert(_ == "zero")

      test(m"Two blocks, exact size, ready immediately"):
        val producer = Producer[Text](4, 2)
        producer.put("zerofour")
        producer.iterator
        var out = ""
        if producer.iterator.hasNext then out += producer.iterator.next()
        if producer.iterator.hasNext then out += producer.iterator.next()
        out
      . assert(_ == "zerofour")

      test(m"More than two blocks, ready immediately"):
        val producer = Producer[Text](4, 2)
        producer.put("zerofoursix")
        producer.iterator
        var out = ""
        if producer.iterator.hasNext then out += producer.iterator.next()
        if producer.iterator.hasNext then out += producer.iterator.next()
        out
      . assert(_ == "zerofour")

      test(m"More than two blocks, fragmented, ready immediately"):
        val producer = Producer[Text](4, 2)
        producer.put("12")
        producer.put("3")
        producer.put("4")
        producer.put("5")
        producer.put("6")
        producer.put("7")
        producer.put("8")
        producer.iterator
        var out = ""
        if producer.iterator.hasNext then out += producer.iterator.next()
        if producer.iterator.hasNext then out += producer.iterator.next()
        out
      . assert(_ == "12345678")

      test(m"Single long message, with blocking"):
        val producer = Producer[Text](4, 2)
        val out = async(producer.iterator.to(List))
        producer.put("12345678901234567890")
        producer.finish()
        unsafely(out.await())
      . assert(_ == List("1234", "5678", "9012", "3456", "7890"))

      test(m"Single long message, with blocking; incomplete final block"):
        val producer = Producer[Text](4, 2)
        val out = async(producer.iterator.to(List))
        producer.put("123456789012345678")
        producer.finish()
        unsafely(out.await())
      . assert(_ == List("1234", "5678", "9012", "3456", "78"))

      for i <- 0 to 30 do
        val string = (0 to i).map(_.toString).foldLeft("")(_ + _)
        test(m"String length $i, sent whole, async puts"):
          val producer = Producer[Text](5, 2)
          val fiber = async:
            producer.put(string)
            producer.finish()
          producer.iterator.foldLeft("")(_ + _)
        . assert(_ == string)

        test(m"String length $i, sent unitarily, async puts"):
          val producer = Producer[Text](5, 2)
          val fiber = async:
            string.tt.chars.foreach: char =>
              producer.put(char.toString)
            producer.finish()
          producer.iterator.foldLeft("")(_ + _)
        . assert(_ == string)

        test(m"String length $i, sent whole, async reads"):
          val producer = Producer[Text](5, 2)
          val output = async(producer.iterator.foldLeft("")(_ + _))
          producer.put(string)
          producer.finish()
          unsafely(output.await())
        . assert(_ == string)

        test(m"String length $i, sent unitarily, async reads"):
          val producer = Producer[Text](5, 2)
          val output = async(producer.iterator.foldLeft("")(_ + _))
          string.tt.chars.each: char =>
            producer.put(char.toString)
          producer.finish()
          unsafely(output.await())
        . assert(_ == string)

      test(m"Bytes producer copies a non-zero-offset put correctly"):
        // The second `put` lands at buffer index 3, exercising the bytes-path
        // `arraycopy` length (a regression here over-reads the source).
        val producer = Producer[Data](8)
        val output = async(producer.iterator.to(List))
        producer.put(Data.fill(3)(_.toByte))
        producer.put(Data.fill(5)(i => (i + 10).toByte))
        producer.finish()
        unsafely(output.await()).flatMap(_.to(List))
      . assert(_ == List[Byte](0, 1, 2, 10, 11, 12, 13, 14))

      test(m"Synchronous text collection joins puts"):
        Producer.collect[Text](4): producer =>
          producer.put("hello ")
          producer.put("world")
      . assert(_ == "hello world")

      test(m"Synchronous collection of a sub-range"):
        Producer.collect[Text](): producer =>
          producer.put("--hello--", 2.z, 5)
      . assert(_ == "hello")

      test(m"Synchronous bytes collection"):
        Producer.collect[Data](): producer =>
          producer.put(Data.fill(3)(_.toByte))
          producer.put(Data.fill(5)(i => (i + 10).toByte))
      . assert(_.to(List) == List[Byte](0, 1, 2, 10, 11, 12, 13, 14))

      test(m"Push bytes one at a time (synchronous)"):
        Producer.collect[Data](): producer =>
          var i = 0
          while i < 6 do
            producer.push((i*2).toByte)
            i += 1
      . assert(_.to(List) == List[Byte](0, 2, 4, 6, 8, 10))

      test(m"Push bytes across a block boundary (streaming)"):
        val producer = Producer[Data](4)
        val output = async(producer.iterator.to(List))
        var i = 0

        while i < 10 do
          producer.push(i.toByte)
          i += 1

        producer.finish()
        unsafely(output.await()).flatMap(_.to(List))
      . assert(_ == (0 until 10).map(_.toByte).to(List))

      test(m"Push chars (synchronous text)"):
        Producer.collect[Text](): producer =>
          producer.push('h')
          producer.push('i')
      . assert(_ == "hi")



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

    suite(m"Datum tests"):
      test(m"Datum from ASCII byte equals same Byte literal"):
        Datum('-'.toByte) == '-'.toByte
      . assert(identity)

      test(m"Datum from char equals same Char literal"):
        Datum('-') == '-'
      . assert(identity)

      test(m"Datum from byte equals different byte is false"):
        Datum('a'.toByte) == 'b'.toByte
      . assert(_ == false)

      test(m"Datum from byte 0xFF round-trips as unsigned"):
        Datum(0xff.toByte).asInt
      . assert(_ == 255)

      test(m"Datum.End is not equal to any byte"):
        Datum.End == 0.toByte
      . assert(_ == false)

      test(m"Datum.End equals Datum.End"):
        Datum.End == Datum.End
      . assert(identity)

      test(m"Datum.End.isEnd is true"):
        Datum.End.isEnd
      . assert(identity)

      test(m"Datum from byte is not End"):
        Datum('-'.toByte).isEnd
      . assert(_ == false)

      test(m"Cursor[Data].peek returns Datum equal to next byte"):
        val cursor = Cursor[Data](Iterator(Data('a'.toByte, 'b'.toByte)))
        cursor.peek == 'a'.toByte
      . assert(identity)

      test(m"Cursor[Text].peek returns Datum equal to next char"):
        val cursor = Cursor[Text](Iterator(t"xy"))
        cursor.peek == 'x'
      . assert(identity)

      test(m"Cursor[Data].peek at end of stream is Datum.End"):
        val cursor = Cursor[Data](Iterator(Data('a'.toByte)))
        cursor.next()
        cursor.peek == Datum.End
      . assert(identity)

    suite(m"expect tests"):
      import strategies.throwUnsafely
      case class Mismatch() extends Exception

      test(m"Cursor[Data].expect matching advances past the target"):
        val cursor = Cursor[Data](Iterator(Data('a'.toByte, 'b'.toByte)))
        cursor.expect('a')(Mismatch())
        cursor.peek == 'b'
      . assert(identity)

      test(m"Cursor[Data].expect mismatching throws"):
        val cursor = Cursor[Data](Iterator(Data('a'.toByte)))
        try { cursor.expect('z')(Mismatch()); false } catch case _: Mismatch => true
      . assert(identity)

      test(m"Cursor[Data].expect at EOF throws"):
        val cursor = Cursor[Data](Iterator(Data()))
        try { cursor.expect('a')(Mismatch()); false } catch case _: Mismatch => true
      . assert(identity)

      test(m"Cursor[Text].expect matching advances past the target"):
        val cursor = Cursor[Text](Iterator(t"ab"))
        cursor.expect('a')(Mismatch())
        cursor.peek == 'b'
      . assert(identity)

    suite(m"lookahead tests"):
      test(m"lookahead returns result without advancing on success"):
        val cursor = Cursor[Text](Iterator(t"abcd"))
        val ok = cursor.lookahead:
          cursor.next() && cursor.peek == 'b'

        (ok, cursor.peek == 'a')
      . assert(_ == ((true, true)))

      test(m"lookahead returns result without advancing on failure"):
        val cursor = Cursor[Text](Iterator(t"abcd"))
        val ok = cursor.lookahead:
          cursor.next() && cursor.peek == 'z'

        (ok, cursor.peek == 'a')
      . assert(_ == ((false, true)))

      test(m"lookahead inside an outer hold preserves the outer marks"):
        val cursor = Cursor[Text](Iterator(t"abcd"))
        cursor.hold:
          val outer = cursor.mark
          cursor.next()
          val inner = cursor.lookahead:
            cursor.next() && cursor.peek == 'c'

          (inner, cursor.grab(outer, cursor.mark).s)
      . assert(_ == ((true, "a")))

