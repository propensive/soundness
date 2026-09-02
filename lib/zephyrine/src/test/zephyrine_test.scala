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
package zephyrine

import java.util.concurrent.atomic.{AtomicInteger, AtomicLong, AtomicReference}

import soundness.*

import randomization.unseededRandomization

import supervisors.globalSupervisor
import threading.virtualThreading
import strategies.throwUnsafely
import probates.panicProbate

object Tests extends Suite(m"Zephyrine tests"):
  val bytes = Data.fill(1000)(_.toByte)
  def run(): Unit = stochastic:
    supervise:

      suite(m"Producer tests"):
        test(m"mismatched block size"):
          val producer = Producer[Text](4, 20)
          producer.put("one")
          producer.put("two")
          producer.finish()
          val it = async(producer.iterator.to(List))

          unsafely(scala.caps.unsafe.unsafeAssumeSeparate(it.await()))
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
          unsafely(scala.caps.unsafe.unsafeAssumeSeparate(out.await()))
        . assert(_ == List("1234", "5678", "9012", "3456", "7890"))

        test(m"Single long message, with blocking; incomplete final block"):
          val producer = Producer[Text](4, 2)
          val out = async(producer.iterator.to(List))
          producer.put("123456789012345678")
          producer.finish()
          unsafely(scala.caps.unsafe.unsafeAssumeSeparate(out.await()))
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
              string.tt.chars.each: char =>
                producer.put(char.toString)
              producer.finish()
            producer.iterator.foldLeft("")(_ + _)
          . assert(_ == string)

          test(m"String length $i, sent whole, async reads"):
            val producer = Producer[Text](5, 2)
            val output = async(producer.iterator.foldLeft("")(_ + _))
            producer.put(string)
            producer.finish()
            unsafely(scala.caps.unsafe.unsafeAssumeSeparate(output.await()))
          . assert(_ == string)

          test(m"String length $i, sent unitarily, async reads"):
            val producer = Producer[Text](5, 2)
            val output = async(producer.iterator.foldLeft("")(_ + _))
            string.tt.chars.each: char =>
              producer.put(char.toString)
            producer.finish()
            unsafely(scala.caps.unsafe.unsafeAssumeSeparate(output.await()))
          . assert(_ == string)

        test(m"Bytes producer copies a non-zero-offset put correctly"):
          // The second `put` lands at buffer index 3, exercising the bytes-path
          // `arraycopy` length (a regression here over-reads the source).
          val producer = Producer[Data](8)
          val output = async(producer.iterator.to(List))
          producer.put(Data.fill(3)(_.toByte))
          producer.put(Data.fill(5)(i => (i + 10).toByte))
          producer.finish()
          unsafely(scala.caps.unsafe.unsafeAssumeSeparate(output.await())).flatMap(_.readable.to(List))
        . assert(_.map(_.toInt) == List(0, 1, 2, 10, 11, 12, 13, 14))

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
        . assert(_.to[List].map(_.toInt) == List(0, 1, 2, 10, 11, 12, 13, 14))

        test(m"Push bytes one at a time (synchronous)"):
          Producer.collect[Data](): producer =>
            var i = 0
            while i < 6 do
              producer.push((i*2).toByte)
              i += 1
        . assert(_.to[List].map(_.toInt) == List(0, 2, 4, 6, 8, 10))

        test(m"Push bytes across a block boundary (streaming)"):
          val producer = Producer[Data](4)
          val output = async(producer.iterator.to(List))
          var i = 0

          while i < 10 do
            producer.push(i.toByte)
            i += 1

          producer.finish()
          unsafely(scala.caps.unsafe.unsafeAssumeSeparate(output.await())).flatMap(_.readable.to(List))
        . assert(_ == (0 until 10).map(_.toByte).to(List))

        test(m"Push chars (synchronous text)"):
          Producer.collect[Text](): producer =>
            producer.push('h')
            producer.push('i')
        . assert(_ == "hi")



      suite(m"Cursor tests"):
        def hello = Cursor(t"Hello world!".chars.to[List].map(_.show).stdlib.iterator)
        def numbers = Cursor(t"0123456789abc".chars.to[List].map(_.show).stdlib.iterator)

        test(m"Iterate over elements"):
          val cursor = hello
          val builder = java.lang.StringBuilder()
          while
            builder.append(cursor.datum(using Unsafe))
            cursor.next()
          do ()

          builder.toString
        . assert(_ == "Hello world!")

        test(m"region lends the readable window with branded indexes"):
          val cursor = hello
          val builder = java.lang.StringBuilder()

          while
            cursor.lend { region => range => region.visit(range) { index => builder.append(region(index)) } }
            val count = cursor.lend { _ => range => (range: Interval).size }
            cursor.unsafeAdvanceBy(count)(using Unsafe)
            cursor.more
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
            cursor.clone(mark, cursor.mark)(builder.asInstanceOf[cursor.addressable.Target])

          builder.toString
        . assert(_ == "el")

        test(m"Capture spanning block"):
          val cursor = hello
          val builder = java.lang.StringBuilder()
          for i <- 1 to 2 do cursor.next()

          cursor.hold:
            val mark = cursor.mark
            for i <- 1 to 3 do cursor.next()
            cursor.clone(mark, cursor.mark)(builder.asInstanceOf[cursor.addressable.Target])

          builder.toString
        . assert(_ == "llo")

        test(m"Capture multiply-spanning block"):
          val cursor = hello
          val builder = java.lang.StringBuilder()
          for i <- 1 to 3 do cursor.next()

          cursor.hold:
            val mark = cursor.mark
            for i <- 1 to 4 do cursor.next()
            cursor.clone(mark, cursor.mark)(builder.asInstanceOf[cursor.addressable.Target])

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
            cursor.clone(mark1, mark2)(builder.asInstanceOf[cursor.addressable.Target])
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
            cursor.clone(mark1, mark2)(builder.asInstanceOf[cursor.addressable.Target])
            for i <- 1 to 3 do cursor.next()
            cursor.clone(mark1, cursor.mark)(builder.asInstanceOf[cursor.addressable.Target])

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
            cursor.clone(mark, cursor.mark)(builder.asInstanceOf[cursor.addressable.Target])

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
        def stream = Chain(bytes).shred(10.0, 10.0).filter(!_.nil)
        def byteCursor = Cursor[Data](stream.stdlib.iterator)

        test(m"Cursor[Data] starts at first byte"):
          val cursor = byteCursor
          cursor.datum(using Unsafe)

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
          cursor.seek(15.toByte.asInstanceOf[cursor.addressable.Operand])
          cursor.datum(using Unsafe)

        . assert(_ == 15.toByte)

        test(m"Cursor[Data] remainder from start equals full stream"):
          val blocks = Chain(Data(1, 2, 3), Data(4, 5), Data(6, 7))
          val cursor = Cursor[Data](blocks.iterator)
          cursor.remainder.stdlib.map(_.readable).flatten.to(List)

        . assert(_.map(_.toInt) == List(1, 2, 3, 4, 5, 6, 7))

        test(m"Cursor[Data] remainder mid-block emits cross-block tail"):
          val blocks = Chain(Data(1, 2, 3, 4, 5), Data(6, 7, 8))
          val cursor = Cursor[Data](blocks.iterator)
          for i <- 0 until 3 do cursor.next()
          cursor.remainder.stdlib.map(_.readable).flatten.to(List)

        . assert(_.map(_.toInt) == List(4, 5, 6, 7, 8))

        test(m"Cursor[Data] remainder inside hold still emits unconsumed tail"):
          val blocks = Chain(Data(1, 2, 3, 4, 5), Data(6, 7, 8))
          val cursor = Cursor[Data](blocks.iterator)
          for i <- 0 until 3 do cursor.next()
          cursor.hold(cursor.remainder.stdlib.map(_.readable).flatten.to(List))

        . assert(_.map(_.toInt) == List(4, 5, 6, 7, 8))

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
        case class Mismatch()(using Diagnostics) extends Error(m"mismatch")

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

        // Simulates a live keep-alive stream: the source yields a whole
        // two-byte message in its first chunk, and the next chunk (a later,
        // independent message) would only arrive after we reply. Consuming
        // the final byte with `expect` must not pull that second chunk
        // (issue #1301) — on a real socket the pull would deadlock.
        test(m"Cursor[Data].expect on a message's final byte does not refill"):
          class Live() extends Iterator[Data]:
            @scala.caps.unsafe.untrackedCaptures
            var pulls: Int = 0
            def hasNext: Boolean = true

            def next(): Data =
              pulls += 1
              if pulls == 1 then Data('a'.toByte, 'b'.toByte) else Data('X'.toByte)

          val live = Live()
          val cursor = Cursor(live)
          cursor.expect('a')(Mismatch())
          cursor.expect('b')(Mismatch())
          live.pulls
        . assert(_ == 1)

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

      suite(m"Region tests"):
        def sample(size: Int): scala.Array[Byte]^ =
          val buffer = Array.scratch[Byte](size)
          var index = 0

          while index < size do
            buffer(index) = index.toByte
            index += 1

          buffer

        test(m"visit sums a clamped window"):
          val buffer = sample(10)
          var total = 0

          Region.over[Data, Unit](buffer, 1, 9): region =>
            range => region.visit(range) { index => total += region(index).toInt }

          total
        . assert(_ == 36)

        test(m"over clamps an oversized window to the storage"):
          val buffer = sample(4)
          var count = 0

          Region.over[Data, Unit](buffer, 0, 100): region =>
            range => region.visit(range) { _ => count += 1 }

          count
        . assert(_ == 4)

        test(m"visit8 takes the unrolled path and mops up the tail"):
          val buffer = sample(19)
          var whole = 0
          var rest = 0

          Region.over[Data, Unit](buffer, 0, 19): region =>
            range =>
              region.visit8(range)
               ( (i0, i1, i2, i3, i4, i5, i6, i7) =>
                   whole += region(i0) + region(i1) + region(i2) + region(i3)
                     + region(i4) + region(i5) + region(i6) + region(i7) )
               ( index => rest += region(index).toInt )

          (whole, rest)
        . assert(_ == ((120, 51)))

        test(m"capped narrows a window, preserving its brand"):
          val buffer = sample(10)
          var count = 0

          Region.over[Data, Unit](buffer, 2, 10): region =>
            range => region.visit(range.capped(3)) { _ => count += 1 }

          count
        . assert(_ == 3)

        test(m"materialize copies exactly the window"):
          val buffer = sample(10)

          Region.over[Data, Data](buffer, 2, 5): region =>
            range => region.materialize(range)
        . assert(_.to[List].map(_.toInt) == List(2, 3, 4))

        test(m"transfer copies no more than the slate's space"):
          val source = sample(10)
          val target = Array.scratch[Byte](4)

          val copied = Region.over[Data, Int](source, 5, 10): region =>
            range =>
              Slate.over[Data, Int](target, 0, 4): slate =>
                space => region.transfer(range)(slate)(space)

          (copied, target(0), target(3))
        . assert(_ == ((4, 5.toByte, 8.toByte)))

        test(m"slate update writes through branded ordinals"):
          val target = Array.scratch[Byte](6)

          Slate.over[Data, Unit](target, 2, 5): slate =>
            space => slate.visit(space) { index => slate(index) = 7.toByte }

          (target(1), target(2), target(4),
           target(5))
        . assert(_ == ((0.toByte, 7.toByte, 7.toByte, 0.toByte)))

      suite(m"Streaming kernel tests"):
        val small = Array[Byte](1, 2, 3, 4, 5)

        test(m"pump transfers a single-chunk stream"):
          val gather = Gather()
          bytes.stream.pump(gather)
          scala.caps.unsafe.unsafeAssumeSeparate(gather.data).to[List]
        . assert(_ == bytes.to[List])

        test(m"iterator stream transfers all chunks in order"):
          val gather = Gather()
          Stream(Iterator(Array[Byte](1, 2, 3), Array[Byte](), Array[Byte](4, 5))).pump(gather)
          scala.caps.unsafe.unsafeAssumeSeparate(gather.data).to[List]
        . assert(_.to[List].map(_.toInt) == List(1, 2, 3, 4, 5))

        test(m"through doubles each byte"):
          val gather = Gather()
          small.stream.viaDuct(Doubler()).pump(gather)
          scala.caps.unsafe.unsafeAssumeSeparate(gather.data).to[List]
        . assert(_ == (small.to[List]: List[Byte]).flatMap { byte => proscenium.List(byte, byte) })

        test(m"a duct translates downstream demand for its upstream"):
          val recorder = Recorder(small.stream)
          val gather = Gather()
          gather.credit = 10
          scala.caps.unsafe.unsafeAssumeSeparate(recorder.viaDuct(Doubler()).pump(gather))
          recorder.demands.stdlib.last
        . assert(_ == 5L)

        test(m"accepting reports translated demand"):
          val gather = Gather()
          gather.credit = 10
          gather.acceptingDuct(Doubler()).demand.count
        . assert(_ == 5L)

        test(m"accepting transforms pushed data"):
          val gather = Gather()
          val intake = gather.acceptingDuct(Doubler())
          intake.put(small)
          intake.finish()
          scala.caps.unsafe.unsafeAssumeSeparate(gather.data).to[List]
        . assert(_ == (small.to[List]: List[Byte]).flatMap { byte => proscenium.List(byte, byte) })

        test(m"duct flush emits terminal state on finish"):
          val gather = Gather()
          val intake = gather.acceptingDuct(Trailer())
          intake.put(Array[Byte](1, 2))
          intake.finish()
          scala.caps.unsafe.unsafeAssumeSeparate(gather.data).readable.to(List)
        . assert(_.map(_.toInt) == List(1, 2, 99))

        test(m"duct flush emits terminal state at end of a pulled stream"):
          val gather = Gather()
          Stream(Array[Byte](1, 2)).viaDuct(Trailer()).pump(gather)
          scala.caps.unsafe.unsafeAssumeSeparate(gather.data).to[List]
        . assert(_.map(_.toInt) == List(1, 2, 99))

        test(m"conduit transfers data across threads"):
          Conduit[Data]() match
           case (intake, stream) =>
            val gather = Gather()
            val task = scala.caps.unsafe.unsafeAssumeSeparate(async(stream.pump(gather)))
            intake.put(bytes)
            intake.finish()
            unsafely(scala.caps.unsafe.unsafeAssumeSeparate(task.await()))
            scala.caps.unsafe.unsafeAssumeSeparate(gather.data).to[List]
        . assert(_ == bytes.to[List])

        val big: Data = Array.tabulate[Byte](10000)(index => (index%251).toByte)

        test(m"conduit passes a large chunk through after a buffered partial block"):
          Conduit[Data]() match
           case (intake, stream) =>
            val gather = Gather()
            val task = scala.caps.unsafe.unsafeAssumeSeparate(async(stream.pump(gather)))
            intake.put(Data(9))
            intake.put(big)
            intake.finish()
            unsafely(scala.caps.unsafe.unsafeAssumeSeparate(task.await()))
            scala.caps.unsafe.unsafeAssumeSeparate(gather.data).to[List]
        . assert(_ == 9.toByte +: big.to[List])

        // Pump a payload many times the transfer-block size across the conduit,
        // so the reader drains and returns ceiling-sized blocks that the writer
        // reuses from the pool: the data must survive the recycling intact.
        test(m"conduit recycles transfer blocks across many hand-offs"):
          val payload: Data = Array.tabulate[Byte](1000000)(index => (index%251).toByte)
          Conduit[Data]() match
           case (intake, stream) =>
            val gather = Gather()
            val task = scala.caps.unsafe.unsafeAssumeSeparate(async(stream.pump(gather)))
            payload.stream.pump(intake)
            unsafely(scala.caps.unsafe.unsafeAssumeSeparate(task.await()))
            scala.caps.unsafe.unsafeAssumeSeparate(gather.data).to[List] == payload.to[List]
        . assert(identity)

        // A chunk passed through by reference is the caller's immutable data, so
        // the reader must never return its backing to the pool: were it recycled,
        // the ceiling-sized blocks minted for `extra` would overwrite `original`.
        test(m"conduit never recycles a passed-through backing"):
          val original: Data = Array.tabulate[Byte](80000)(index => (index%251).toByte)
          val extra: Data = Array.tabulate[Byte](300000)(index => ((index + 1)%251).toByte)
          Conduit[Data]() match
           case (intake, stream) =>
            val gather = Gather()
            val task = scala.caps.unsafe.unsafeAssumeSeparate(async(stream.pump(gather)))
            intake.put(original)
            extra.stream.pump(intake)
            unsafely(scala.caps.unsafe.unsafeAssumeSeparate(task.await()))
            original.to[List] == Array.tabulate[Byte](80000)(index => (index%251).toByte).to[List]
        . assert(identity)

        test(m"conduit demand reflects buffered data"):
          Conduit[Data]() match
           case (intake, stream) =>
            val before = intake.demand.count
            intake.put(Array[Byte](1, 2, 3))
            val after = intake.demand.count
            before - after
        . assert(_ == 3L)

        test(m"conduit rethrows producer failure at the reader"):
          Conduit[Data]() match
           case (intake, stream) =>
            intake.fail(RuntimeException("boom"))

            try
              stream.refill(Credit(1))
              false
            catch case _: RuntimeException => true
        . assert(identity)

        test(m"credit grant clamps to Int range and zero"):
          val regulation = summon[Credit is Regulation]

          ( regulation.grant(Credit(-5)),
            regulation.grant(Credit(3)),
            regulation.grant(Credit(Long.MaxValue)) )
        . assert(_ == ((0, 3, Int.MaxValue)))

        test(m"credit encode/decode roundtrip"):
          val regulation = summon[Credit is Regulation]
          regulation.decode(regulation.encode(Credit(3456))).count
        . assert(_ == 3456L)

        test(m"cursor over a stream sees all elements across chunk boundaries"):
          val cursor = Cursor[Text](Stream(Iterator(t"ab", t"cd")))
          var out: String = ""

          while !cursor.finished do
            out += cursor.peek.asInt.toChar
            cursor.next()

          out
        . assert(_ == "abcd")

        import charDecoders.utf8Decoder, charEncoders.utf8Encoder, textSanitizers.skipSanitizer

        val exotic = t"héllo → 🎉 fin"

        test(m"char decoder duct reassembles multi-byte characters split across refills"):
          val chunks = exotic.s.getBytes("UTF-8").nn.toSeq.map { byte => Array[Byte](byte) }
          val stream = chunks.iterator.stream.via(summon[CharDecoder])
          val builder = StringBuilder()

          def recur(): Unit = scala.caps.unsafe.unsafeAssumeSeparate:
           stream.refill(Credit(8)) match
            case count: Int =>
              val window = unsafely(stream.storage).asInstanceOf[scala.Array[Char]]
              builder.append(String(window, stream.start, count))
              stream.skip(count)
              scala.caps.unsafe.unsafeAssumeSeparate(recur())

            case _ => ()

          scala.caps.unsafe.unsafeAssumeSeparate(recur())
          builder.toString.tt
        . assert(_ == exotic)

        test(m"char encoder duct emits UTF-8 for supplementary characters"):
          val gather = Gather()
          exotic.stream.via(summon[CharEncoder]).pump(gather)
          scala.caps.unsafe.unsafeAssumeSeparate(gather.data).to[List]
        . assert(_ == Array.unsafeFrozen(exotic.s.getBytes("UTF-8").nn).to[List])

        // Malformed input — a stray continuation, an overlong lead, a
        // truncated sequence mid-stream and a bad continuation — must decode
        // through the duct exactly as the whole-value decoder sanitizes it.
        val malformed = Array[Byte](
          'a'.toByte, 0x80.toByte, 'b'.toByte,                              // stray continuation
          0xc0.toByte, 0xaf.toByte, 'c'.toByte,                             // overlong lead
          0xe4.toByte, 0xb8.toByte, 'd'.toByte,                             // truncated 3-byte
          0xf0.toByte, 0x9f.toByte, 0x8e.toByte, 0x89.toByte, 'e'.toByte,  // valid 🎉
          0xc3.toByte)                                                      // truncated at end

        test(m"char decoder duct sanitizes malformed input like whole-value decoding"):
          val stream = malformed.stream.via(summon[CharDecoder])
          val builder = StringBuilder()

          def recur(): Unit = scala.caps.unsafe.unsafeAssumeSeparate:
           stream.refill(Credit(8)) match
            case count: Int =>
              val window = unsafely(stream.storage).asInstanceOf[scala.Array[Char]]
              builder.append(String(window, stream.start, count))
              stream.skip(count)
              scala.caps.unsafe.unsafeAssumeSeparate(recur())

            case _ => ()

          scala.caps.unsafe.unsafeAssumeSeparate(recur())
          builder.toString.tt
        . assert(_ == summon[CharDecoder].decoded(malformed))

        test(m"charset ducts roundtrip through both directions"):
          val gather = Gather()
          exotic.stream.via(summon[CharEncoder]).pump(gather)
          val decoded = scala.caps.unsafe.unsafeAssumeSeparate(gather.data).stream.via(summon[CharDecoder])
          val builder = StringBuilder()

          def recur(): Unit = decoded.refill(Credit(4)) match
            case count: Int =>
              val window = unsafely(decoded.storage).asInstanceOf[scala.Array[Char]]
              builder.append(String(window, decoded.start, count))
              decoded.skip(count)
              scala.caps.unsafe.unsafeAssumeSeparate(recur())

            case _ => ()

          scala.caps.unsafe.unsafeAssumeSeparate(recur())
          builder.toString.tt
        . assert(_ == exotic)

        test(m"record streams carry heap objects with credit counted in records"):
          val records = Array.from((1 to 100).map { index => s"record-$index" })
          val stream = Stream[Array[String]^{}](records)
          var collected: List[String] = Nil

          def recur(): Unit = scala.caps.unsafe.unsafeAssumeSeparate:
           stream.refill(Credit(7)) match
            case count: Int =>
              val window = unsafely(stream.storage).asInstanceOf[scala.Array[AnyRef]]

              for index <- 0 until count
              do collected = window(stream.start + index).asInstanceOf[String] :: collected

              stream.skip(count)
              scala.caps.unsafe.unsafeAssumeSeparate(recur())

            case _ => ()

          scala.caps.unsafe.unsafeAssumeSeparate(recur())
          collected.reverse
        . assert(_ == (1 to 100).map { index => s"record-$index" }.to(List))

        test(m"flow grants nothing when halted"):
          val regulation = summon[Pace is Regulation]

          ( regulation.grant(Pace.Halted),
            regulation.grant(Pace.Free) > 0,
            regulation.measured(Pace.Measured) )
        . assert(_ == ((0, true, true)))

        test(m"sweep visits every element in order across chunks"):
          var collected: List[Byte] = Nil

          Stream(Iterator(Array[Byte](1, 2, 3), Array[Byte](), Array[Byte](4, 5)))
          . drain: region =>
              range => region.visit(range) { index => collected = region(index) :: collected }

          collected.reverse
        . assert(_.to[List].map(_.toInt) == List(1, 2, 3, 4, 5))

        test(m"memoize drains a byte stream into a single immutable value"):
          Stream(Iterator(Array[Byte](1, 2, 3), Array[Byte](4, 5))).memoize.to[List]
        . assert(_.to[List].map(_.toInt) == List(1, 2, 3, 4, 5))

        test(m"memoize of an empty stream yields an empty value"):
          Iterator.empty[Data].stream.memoize.to[List]
        . assert(_ == List())

        test(m"memoize reassembles a transformed pipeline"):
          small.stream.viaDuct(Doubler()).memoize.to[List]
        . assert(_ == (small.to[List]: List[Byte]).flatMap { byte => proscenium.List(byte, byte) })

        test(m"memoize drains a text stream into a single text value"):
          Stream(Iterator(t"ab", t"cd", t"e")).memoize.s
        . assert(_ == "abcde")

        test(m"truncate limits a stream to its first elements"):
          small.stream.truncate(3).memoize.readable.to(List)
        . assert(_.map(_.toInt) == List(1, 2, 3))

        test(m"truncate across chunk boundaries"):
          Stream(Iterator(Array[Byte](1, 2, 3), Array[Byte](4, 5, 6)))
          . truncate(4).memoize.to[List]
        . assert(_.to[List].map(_.toInt) == List(1, 2, 3, 4))

        test(m"truncate to more than the stream holds yields the whole stream"):
          small.stream.truncate(100).memoize.to[List]
        . assert(_ == small.to[List])

        test(m"truncate to zero yields an empty stream"):
          small.stream.truncate(0).memoize.to[List]
        . assert(_ == List())

        test(m"discard skips a stream's first elements"):
          small.stream.discard(2).memoize.to[List]
        . assert(_.to[List].map(_.toInt) == List(3, 4, 5))

        test(m"discard across chunk boundaries"):
          Stream(Iterator(Array[Byte](1, 2, 3), Array[Byte](4, 5, 6)))
          . discard(4).memoize.to[List]
        . assert(_.to[List].map(_.toInt) == List(5, 6))

        test(m"discard of more than the stream holds yields an empty stream"):
          small.stream.discard(100).memoize.to[List]
        . assert(_ == List())

        test(m"truncate and discard compose to a slice"):
          Stream(Data.fill(20)(_.toByte)).discard(5).truncate(5).memoize.to[List]
        . assert(_.to[List].map(_.toInt) == List(5, 6, 7, 8, 9))

        test(m"truncate composes with a duct"):
          small.stream.truncate(3).viaDuct(Doubler()).memoize.to[List]
        . assert(_.to[List].map(_.toInt) == List(1, 1, 2, 2, 3, 3))

        test(m"gather reduces over regions without boxing"):
          bytes.stream.gather(0L): region =>
            (total, range) =>
              var sum = total
              region.visit(range) { index => sum += (region(index) & 0xff) }
              sum
        . assert(_ == bytes.to[List].map(_ & 0xff).total.toLong)

        test(m"chain yields the stream's chunks in order"):
          Stream(Iterator(Array[Byte](1, 2, 3), Array[Byte](4, 5))).chain.stdlib.to(List)
          . map(_.to[List])
        . assert(_ == List(List(1, 2, 3).map(_.toByte), List(4, 5).map(_.toByte)))

        test(m"chain of an empty stream is empty"):
          Iterator.empty[Data].stream.chain.stdlib.to(List)
        . assert(_ == List())

        test(m"chain construction pulls nothing"):
          var pulled: Int = 0
          val chunks = Iterator(Array[Byte](1.toByte), Array[Byte](2.toByte)).map { chunk => pulled += 1; chunk }
          val list = chunks.stream.chain
          scala.caps.unsafe.unsafeAssumeSeparate(pulled)
        . assert(_ == 0)

        test(m"chain pulls chunks only as cells are forced"):
          var pulled: Int = 0
          val chunks = Iterator(Array[Byte](1.toByte), Array[Byte](2.toByte)).map { chunk => pulled += 1; chunk }
          val list = chunks.stream.chain
          list.stdlib.head
          scala.caps.unsafe.unsafeAssumeSeparate(pulled)
        . assert(_ == 1)

        test(m"chain reassembles a transformed pipeline"):
          val text = Stream(Iterator(Array[Byte](104, 105))).via(summon[CharDecoder]).chain
          text.stdlib.to(List).map(_.s).mkString
        . assert(_ == "hi")

        test(m"records iterates across chunks in order"):
          Stream(Iterator(Array(Row(1), Row(2)), Array(Row(3)))).records.to(List)
        . assert(_ == List(Row(1), Row(2), Row(3)))

        test(m"records of an empty record stream is empty"):
          Iterator.empty[Array[Row]^{}].stream.records.to(List)
        . assert(_ == List())

        test(m"memoize materializes a record stream into one frozen array"):
          val rows: Array[Row]^{} = Stream(Iterator(Array(Row(1), Row(2)), Array(Row(3)))).memoize
          rows.to[List]
        . assert(_ == List(Row(1), Row(2), Row(3)))

        test(m"records composes with truncate"):
          Stream(Iterator(Array(Row(1), Row(2)), Array(Row(3), Row(4)))).truncate(3).records
          . to(List)
        . assert(_ == List(Row(1), Row(2), Row(3)))

        test(m"streamOf lends a bounded sub-stream of a cursor"):
          val cursor = Cursor(Data.fill(10)(_.toByte))
          scala.caps.unsafe.unsafeAssumeSeparate(streamOf(cursor, 4).memoize.to[List])
        . assert(_.to[List].map(_.toInt) == List(0, 1, 2, 3))

        test(m"the lent cursor resumes at the boundary"):
          val cursor = Cursor(Data.fill(10)(_.toByte))
          scala.caps.unsafe.unsafeAssumeSeparate(streamOf(cursor, 4).memoize)
          cursor.remainder.stdlib.to(List).flatMap(_.readable.to(List))
        . assert(_.map(_.toInt) == List(4, 5, 6, 7, 8, 9))

        test(m"streamOf without a length lends the whole remainder"):
          val cursor = Cursor(Data.fill(6)(_.toByte))
          scala.caps.unsafe.unsafeAssumeSeparate(streamOf(cursor).memoize.to[List])
        . assert(_.to[List].map(_.toInt) == List(0, 1, 2, 3, 4, 5))

        test(m"streamOf spans cursor refills"):
          val cursor = Cursor(Iterator(Array[Byte](0, 1, 2), Array[Byte](3, 4, 5), Array[Byte](6.toByte)))
          scala.caps.unsafe.unsafeAssumeSeparate(streamOf(cursor, 5).memoize.to[List])
        . assert(_.to[List].map(_.toInt) == List(0, 1, 2, 3, 4))

        test(m"a lent sub-stream and the resumed cursor partition the input"):
          val cursor = Cursor(Iterator(Array[Byte](0, 1, 2), Array[Byte](3, 4, 5), Array[Byte](6.toByte)))
          val lent = scala.caps.unsafe.unsafeAssumeSeparate(streamOf(cursor, 5).memoize.to[List])
          val rest = cursor.remainder.stdlib.to(List).flatMap(_.readable.to(List))
          (lent, rest)
        . assert { v => (v(0).map(_.toInt), v(1).map(_.toInt)) == (List(0, 1, 2, 3, 4), List(5, 6)) }

        // The ring holds `Integer.highestOneBit(depth.max(2)*2 - 1)` blocks — two, at
        // depth 2 — so publishing exactly that many staging blocks single-threadedly
        // fills it without ever blocking, and demand must then be exactly zero: the
        // state in which the next hand-off would park the writer.
        test(m"demand reaches zero exactly when a hand-off would park"):
          given Buffering = probeBuffering(16, 2)

          Conduit[Data]() match
           case (intake, stream) =>
            for _ <- 1 to 2 do
              intake.reserve(16)
              intake.commit(16)

            intake.demand.count
        . assert(_ == 0L)

        // The reader adopts the ring's whole buffered burst in one step, so the
        // first refill frees every occupied slot at once and demand recovers in
        // full, not block-by-block.
        test(m"demand recovers in a burst as the reader adopts the ring"):
          given Buffering = probeBuffering(16, 2)

          Conduit[Data]() match
           case (intake, stream) =>
            for _ <- 1 to 2 do
              intake.reserve(16)
              intake.commit(16)

            def drain(): Long =
              stream.refill(Credit(16)) match
                case count: Int => stream.skip(count)
                case _          => ()

              intake.demand.count

            (drain(), drain())
        . assert(_ == ((32L, 32L)))

        // Each block-sized `put` is one ring hand-off: the third parks the writer, so
        // its progress counter freezes at two. A refill adopts the ring's whole
        // burst, so it releases both occupied slots at once: the writer lands two
        // more hand-offs before parking again on the next full ring.
        test(m"a writer parks on a full conduit and resumes when drained"):
          given Buffering = probeBuffering(16, 2)

          Conduit[Data]() match
           case (intake, stream) =>
            val chunk: Data = Data.fill(16)(_.toByte)
            val written = AtomicInteger(0)

            val writer = scala.caps.unsafe.unsafeAssumeSeparate:
              onThread: () =>
                for _ <- 1 to 8 do
                  intake.put(chunk)
                  written.incrementAndGet()

                intake.finish()

            awaitProgress(2, written)
            val parked = awaitParked(writer)
            val frozen = written.get()

            stream.refill(Credit(16)) match
              case count: Int => stream.skip(count)
              case _          => ()

            // The unpark is asynchronous, so wait for the released hand-off to land
            // before observing the writer parked again on the next full ring.
            awaitProgress(4, written)
            val reparked = awaitParked(writer)
            val advanced = written.get()

            def drain(total: Int): Int = stream.refill(Credit(16)) match
              case count: Int =>
                stream.skip(count)
                drain(total + count)

              case _ => total

            val rest = drain(0)
            writer.join(10000)
            (parked, frozen, reparked, advanced, 16 + rest)
        . assert(_ == ((true, 2, true, 4, 128)))

        // Committed-but-unconsumed data can occupy at most the ring (two blocks), the
        // reader's adoption buffer (two more, since a burst adoption frees the ring
        // for the writer to refill), the writer's staging block and one sub-block
        // chunk mid-copy: sampling after every drained window must never observe
        // more, however the threads interleave. An unbounded hand-off would exceed
        // this by orders of magnitude.
        test(m"in-flight conduit data never exceeds the configured bound"):
          given Buffering = probeBuffering(16, 2)

          Conduit[Data]() match
           case (intake, stream) =>
            val chunk: Data = Data.fill(8)(_.toByte)
            val produced = AtomicLong(0)

            val writer = scala.caps.unsafe.unsafeAssumeSeparate:
              onThread: () =>
                for _ <- 1 to 2048 do
                  intake.put(chunk)
                  produced.addAndGet(8)

                intake.finish()

            var consumed: Long = 0
            var worst: Long = 0

            def loop(): Unit = stream.refill(Credit(64)) match
              case count: Int =>
                stream.skip(count)
                consumed += count
                worst = worst.max(produced.get() - consumed)
                loop()

              case _ => ()

            loop()
            writer.join(10000)
            (consumed, worst <= 96L)
        . assert(_ == ((16384L, true)))

        // With the ring full, the intake's demand is zero: the pump's refill is
        // granted nothing, it falls into the `reserve(1)` branch, and the writer
        // parks handing off the next block. Draining the reader side releases it and
        // the transfer completes.
        test(m"a pump into a full conduit parks and completes after draining"):
          given Buffering = probeBuffering(16, 2)

          Conduit[Data]() match
           case (intake, stream) =>
            for _ <- 1 to 2 do
              intake.reserve(16)
              intake.commit(16)

            val payload: Data = Data.fill(64)(_.toByte)

            val writer = scala.caps.unsafe.unsafeAssumeSeparate:
              onThread(() => payload.stream.pump(intake))

            val parked = awaitParked(writer)

            def drain(total: Int): Int = stream.refill(Credit(1000)) match
              case count: Int =>
                stream.skip(count)
                drain(total + count)

              case _ => total

            val total = drain(0)
            writer.join(10000)
            (parked, total)
        . assert(_ == ((true, 96)))

        test(m"a two-stage duct chain compounds demand translation"):
          val recorder = Recorder(small.stream)
          val gather = Gather()
          gather.credit = 20

          scala.caps.unsafe.unsafeAssumeSeparate:
            recorder.viaDuct(Doubler()).viaDuct(Doubler()).pump(gather)

          recorder.demands.stdlib.last
        . assert(_ == 5L)

        test(m"a terminal sweep demands the transfer credit from its source"):
          val recorder = Recorder(small.stream)

          scala.caps.unsafe.unsafeAssumeSeparate:
            recorder.drain: region =>
              range => ()

          recorder.demands.stdlib.last
        . assert(_ == summon[Buffering].transfer(Substrate.Bytes).toLong)

      suite(m"Handoff backpressure tests"):
        test(m"offer parks only when the ring is full"):
          val handoff = Handoff(2)
          handoff.offer("a")
          handoff.offer("b")
          val producer = onThread(() => handoff.offer("c"))
          val parked = awaitParked(producer)
          handoff.take()
          producer.join(10000)
          (parked, producer.isAlive)
        . assert(_ == ((true, false)))

        test(m"take parks on an empty ring until an offer"):
          val handoff = Handoff(2)
          val received = AtomicReference[AnyRef | Null](null)
          val consumer = onThread(() => received.set(handoff.take()))
          val parked = awaitParked(consumer)
          handoff.offer("x")
          consumer.join(10000)
          (parked, received.get())
        . assert(_ == ((true, "x")))

        test(m"interrupting a parked producer raises InterruptedException"):
          val handoff = Handoff(2)
          handoff.offer("a")
          handoff.offer("b")
          val caught = AtomicReference[Throwable | Null](null)

          val producer = onThread: () =>
            try handoff.offer("c") catch case error: InterruptedException => caught.set(error)

          val parked = awaitParked(producer)
          producer.interrupt()
          producer.join(10000)
          (parked, caught.get() != null)
        . assert(_ == ((true, true)))

        test(m"interrupting a parked consumer raises InterruptedException"):
          val handoff = Handoff(2)
          val caught = AtomicReference[Throwable | Null](null)

          val consumer = onThread: () =>
            try handoff.take() catch case error: InterruptedException => caught.set(error)

          val parked = awaitParked(consumer)
          consumer.interrupt()
          consumer.join(10000)
          (parked, caught.get() != null)
        . assert(_ == ((true, true)))

        test(m"close releases a parked producer and discards later offers"):
          val handoff = Handoff(2)
          handoff.offer("a")
          handoff.offer("b")
          val producer = onThread(() => handoff.offer("c"))
          val parked = awaitParked(producer)
          handoff.close()
          producer.join(10000)
          handoff.offer("d")
          (parked, producer.isAlive, handoff.size)
        . assert(_ == ((true, false, 0)))

        test(m"free tracks occupancy across offers and takes"):
          val handoff = Handoff(4)
          val initial = handoff.free
          handoff.offer("a")
          val one = handoff.free
          handoff.offer("b")
          val two = handoff.free
          handoff.take()
          val three = handoff.free
          handoff.take()
          val four = handoff.free
          (initial, one, two, three, four)
        . assert(_ == ((4, 3, 2, 3, 4)))


  // A reference-typed record for the boxed-medium (record stream) tests.
  case class Row(id: Int)

  // A byte intake that gathers everything written to it, with a configurable
  // reported demand, for asserting demand translation.
  class Gather() extends Intake[Data]:
    type Transport = Credit

    private val block: Int = 16
    private val storage: addressable.Storage = addressable.allocate(block).asInstanceOf[addressable.Storage]
    private val target: addressable.Target = addressable.blank(64)
    private var mark1: Int = 0
    var credit: Long = Long.MaxValue

    def demand: Credit = Credit(credit)
    protected def buffer0: AnyRef = storage.asInstanceOf[AnyRef]
    def mark: Int = mark1

    update def reserve(min: Int): Int =
      val free = block - mark1

      if free >= min then free else
        drain()
        block

    update def commit(count: Int): Unit =
      mark1 += count
      if mark1 == block then drain()

    update def finish(): Unit = drain()

    update def data: Data =
      drain()
      addressable.build(target)

    private update def drain(): Unit =
      if mark1 > 0 then
        addressable.cloneStorage(storage, 0, mark1)(target)
        mark1 = 0

  // Doubles each byte, like hexadecimal serialization: 1024 elements of
  // downstream demand translate to 512 elements of upstream demand.
  class Doubler() extends Duct[Data, Data]:
    type Transport = Credit
    type Upstream = Credit

    def regulation: Credit is Regulation = summon[Credit is Regulation]

    // Ceiling division, written to avoid overflow when the demand is unbounded
    // (`Long.MaxValue`).
    def translate(demand: Credit): Credit = Credit(demand.count - demand.count/2)

    override def quantum: Int = 2

    def step(source: Region[Data])(range: Interval in source.type)
      ( target: Slate[Data] )(space: Interval in target.type)
    :   Duct.Progress =

      val sourceInterval: Interval = range
      val sourceOffset = sourceInterval.start.n0
      val sourceLength = sourceInterval.size
      val targetInterval: Interval = space
      val targetOffset = targetInterval.start.n0
      val targetSpace = targetInterval.size
      val bytes = unsafely(source.raw.asInstanceOf[scala.Array[Byte]])

      val out: scala.Array[Byte]^ =
        unsafely(target.raw.asInstanceOf[scala.Array[Byte]]).asInstanceOf[scala.Array[Byte]^]

      var consumed: Int = 0
      var produced: Int = 0

      while consumed < sourceLength && produced + 2 <= targetSpace do
        val byte = bytes(sourceOffset + consumed)
        out(targetOffset + produced) = byte
        out(targetOffset + produced + 1) = byte
        consumed += 1
        produced += 2

      Duct.Progress(consumed, produced)

  // The identity transformation, plus a single trailing `99` byte at
  // end-of-stream, exercising the `flush` path.
  class Trailer() extends Duct[Data, Data]:
    type Transport = Credit
    type Upstream = Credit

    private var emitted: Boolean = false

    def regulation: Credit is Regulation = summon[Credit is Regulation]
    def translate(demand: Credit): Credit = demand

    update def step(source: Region[Data])(range: Interval in source.type)
      ( target: Slate[Data] )(space: Interval in target.type)
    :   Duct.Progress =

      val count = source.transfer(range)(target)(space)
      Duct.Progress(count, count)

    override update def flush(target: Slate[Data])(space: Interval in target.type): Int =
      if emitted then 0 else
        emitted = true
        var written = 0

        target.visit(space.capped(1)): ordinal =>
          target(ordinal) = 99.toByte
          written = 1

        written

  // Passes refills through unchanged, recording each demand it receives.
  class Recorder(consume underlying0: (Stream[Data] over Credit)^) extends Stream[Data]:
    type Transport = Credit

    // The adopted stream is held through a neutral carrier: Stream is deliberately not
    // Unscoped, so an exclusive field would be read-only; the accessor re-asserts the
    // ownership this wrapper took at construction.
    private val held: AnyRef = underlying0.asInstanceOf[AnyRef]
    private def underlying: (Stream[Data] over Credit)^ =
      held.asInstanceOf[(Stream[Data] over Credit)^]

    var demands: List[Long] = Nil

    update def refill(demand: Credit): Optional[Int] =
      demands ::= demand.count
      underlying.refill(demand)

    protected def storage0: AnyRef =
      val current = underlying
      unsafely(current.storage).asInstanceOf[AnyRef]
    def start: Int = underlying.start
    def limit: Int = underlying.limit
    update def skip(count: Int): Unit = underlying.skip(count)

  // A tiny buffering policy for the backpressure tests: staging, transfer and
  // hand-off blocks all collapse to `block` and recycling is off, so blocking
  // states are reached with a few tens of bytes and the block arithmetic in
  // assertions is exact.
  def probeBuffering(block: Int, depth0: Int): Buffering = new Buffering:
    def capacity(substrate: Substrate): Int = block
    def depth: Int = depth0
    override def transfer(substrate: Substrate): Int = block
    override def recycle: Boolean = false

  // A platform thread running `action`: the park-observation tests inspect the
  // thread's state, which a virtual thread does not report as `WAITING`.
  def onThread(action: () => Unit): Thread = Thread.ofPlatform().nn.start(() => action()).nn

  // Poll (bounded) until `counter` reaches `expected`; a counter frozen short of it
  // fails the caller's assertion. Only ever used to await progress the test has made
  // inevitable: the bound is the test's patience, not synchronization.
  def awaitProgress(expected: Int, counter: AtomicInteger): Unit =
    var attempts: Int = 0

    while counter.get() < expected && attempts < 5000 do
      attempts += 1
      Thread.sleep(1)

  // Poll (bounded) until `thread` is parked. Only ever used to await a state the
  // test has made inevitable: the bound is the test's patience, not synchronization.
  def awaitParked(thread: Thread): Boolean =
    var attempts: Int = 0

    while thread.getState != Thread.State.WAITING && attempts < 5000 do
      attempts += 1
      Thread.sleep(1)

    thread.getState == Thread.State.WAITING
