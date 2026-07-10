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
      def stream = LazyList(bytes).shred(10.0, 10.0).filter(_.nonEmpty)
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
        val blocks = LazyList(Data(1, 2, 3), Data(4, 5), Data(6, 7))
        val cursor = Cursor[Data](blocks.iterator)
        cursor.remainder.flatten.to(List)

      . assert(_ == List[Byte](1, 2, 3, 4, 5, 6, 7))

      test(m"Cursor[Data] remainder mid-block emits cross-block tail"):
        val blocks = LazyList(Data(1, 2, 3, 4, 5), Data(6, 7, 8))
        val cursor = Cursor[Data](blocks.iterator)
        for i <- 0 until 3 do cursor.next()
        cursor.remainder.flatten.to(List)

      . assert(_ == List[Byte](4, 5, 6, 7, 8))

      test(m"Cursor[Data] remainder inside hold still emits unconsumed tail"):
        val blocks = LazyList(Data(1, 2, 3, 4, 5), Data(6, 7, 8))
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

    suite(m"Streaming kernel tests"):
      val small = IArray[Byte](1, 2, 3, 4, 5)

      test(m"flowTo transfers a single-chunk stream"):
        val gather = Gather()
        Stream(bytes).flowTo(gather)
        gather.data.to(List)
      . assert(_ == bytes.to(List))

      test(m"iterator stream transfers all chunks in order"):
        val gather = Gather()
        Stream(Iterator(IArray[Byte](1, 2, 3), IArray[Byte](), IArray[Byte](4, 5))).flowTo(gather)
        gather.data.to(List)
      . assert(_ == List[Byte](1, 2, 3, 4, 5))

      test(m"through doubles each byte"):
        val gather = Gather()
        Stream(small).through(Doubler()).flowTo(gather)
        gather.data.to(List)
      . assert(_ == small.to(List).flatMap { byte => List(byte, byte) })

      test(m"a duct translates downstream demand for its upstream"):
        val recorder = Recorder(Stream(small))
        val gather = Gather()
        gather.credit = 10
        recorder.through(Doubler()).flowTo(gather)
        recorder.demands.last
      . assert(_ == 5L)

      test(m"accepting reports translated demand"):
        val gather = Gather()
        gather.credit = 10
        gather.accepting(Doubler()).demand.count
      . assert(_ == 5L)

      test(m"accepting transforms pushed data"):
        val gather = Gather()
        val intake = gather.accepting(Doubler())
        intake.put(small)
        intake.finish()
        gather.data.to(List)
      . assert(_ == small.to(List).flatMap { byte => List(byte, byte) })

      test(m"duct flush emits terminal state on finish"):
        val gather = Gather()
        val intake = gather.accepting(Trailer())
        intake.put(IArray[Byte](1, 2))
        intake.finish()
        gather.data.to(List)
      . assert(_ == List[Byte](1, 2, 99))

      test(m"duct flush emits terminal state at end of a pulled stream"):
        val gather = Gather()
        Stream(IArray[Byte](1, 2)).through(Trailer()).flowTo(gather)
        gather.data.to(List)
      . assert(_ == List[Byte](1, 2, 99))

      test(m"conduit transfers data across threads"):
        val conduit = Conduit[Data]()
        val gather = Gather()
        val task = async(conduit.stream.flowTo(gather))
        conduit.put(bytes)
        conduit.finish()
        unsafely(task.await())
        gather.data.to(List)
      . assert(_ == bytes.to(List))

      test(m"conduit demand reflects buffered data"):
        val conduit = Conduit[Data]()
        val before = conduit.demand.count
        conduit.put(IArray[Byte](1, 2, 3))
        val after = conduit.demand.count
        before - after
      . assert(_ == 3L)

      test(m"conduit rethrows producer failure at the reader"):
        val conduit = Conduit[Data]()
        conduit.fail(RuntimeException("boom"))

        try
          conduit.stream.refill(Credit(1))
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
        val chunks = exotic.s.getBytes("UTF-8").nn.toSeq.map { byte => IArray[Byte](byte) }
        val stream = Stream(chunks.iterator).through(summon[CharDecoder])
        val builder = StringBuilder()

        def recur(): Unit = stream.refill(Credit(8)) match
          case count: Int =>
            val window = unsafely(stream.window).asInstanceOf[Array[Char]]
            builder.append(String(window, stream.start, count))
            stream.skip(count)
            recur()

          case _ => ()

        recur()
        builder.toString.tt
      . assert(_ == exotic)

      test(m"char encoder duct emits UTF-8 for supplementary characters"):
        val gather = Gather()
        Stream(exotic).through(summon[CharEncoder]).flowTo(gather)
        gather.data.to(List)
      . assert(_ == exotic.s.getBytes("UTF-8").nn.to(List))

      test(m"charset ducts roundtrip through both directions"):
        val gather = Gather()
        Stream(exotic).through(summon[CharEncoder]).flowTo(gather)
        val decoded = Stream(gather.data).through(summon[CharDecoder])
        val builder = StringBuilder()

        def recur(): Unit = decoded.refill(Credit(4)) match
          case count: Int =>
            val window = unsafely(decoded.window).asInstanceOf[Array[Char]]
            builder.append(String(window, decoded.start, count))
            decoded.skip(count)
            recur()

          case _ => ()

        recur()
        builder.toString.tt
      . assert(_ == exotic)

      test(m"record streams carry heap objects with credit counted in records"):
        val records = IArray.from((1 to 100).map { index => s"record-$index" })
        val stream = Stream[IArray[String]](records)
        var collected: List[String] = Nil

        def recur(): Unit = stream.refill(Credit(7)) match
          case count: Int =>
            val window = unsafely(stream.window).asInstanceOf[Array[AnyRef]]

            for index <- 0 until count
            do collected = window(stream.start + index).asInstanceOf[String] :: collected

            stream.skip(count)
            recur()

          case _ => ()

        recur()
        collected.reverse
      . assert(_ == (1 to 100).map { index => s"record-$index" }.to(List))

      test(m"flow grants nothing when halted"):
        val regulation = summon[Pace is Regulation]

        ( regulation.grant(Pace.Halted),
          regulation.grant(Pace.Free) > 0,
          regulation.measured(Pace.Measured) )
      . assert(_ == ((0, true, true)))


// A byte intake that gathers everything written to it, with a configurable
// reported demand, for asserting demand translation.
class Gather() extends Intake[Data]:
  type Transport = Credit

  private val block: Int = 16
  private val storage: addressable.Storage = addressable.allocate(block)
  private val target: addressable.Target = addressable.blank(64)
  private var mark1: Int = 0
  var credit: Long = Long.MaxValue

  def demand: Credit = Credit(credit)
  protected def buffer0: AnyRef = storage.asInstanceOf[AnyRef]
  def mark: Int = mark1

  def reserve(min: Int): Int =
    val free = block - mark1

    if free >= min then free else
      drain()
      block

  def commit(count: Int): Unit =
    mark1 += count
    if mark1 == block then drain()

  def finish(): Unit = drain()

  def data: Data =
    drain()
    addressable.build(target)

  private def drain(): Unit =
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

  def step
    ( source: input.Storage,
      sourceOffset: Int,
      sourceLength: Int,
      target: output.Storage,
      targetOffset: Int,
      targetSpace: Int )
  :   Duct.Progress =

    var consumed: Int = 0
    var produced: Int = 0
    val target2 = target.asInstanceOf[input.Storage]

    while consumed < sourceLength && produced + 2 <= targetSpace do
      val byte = input.storageAddress(source, sourceOffset + consumed)
      input.storageUpdate(target2, targetOffset + produced, byte)
      input.storageUpdate(target2, targetOffset + produced + 1, byte)
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

  def step
    ( source: input.Storage,
      sourceOffset: Int,
      sourceLength: Int,
      target: output.Storage,
      targetOffset: Int,
      targetSpace: Int )
  :   Duct.Progress =

    val count = sourceLength.min(targetSpace)
    input.transfer(source, sourceOffset, target.asInstanceOf[input.Storage], targetOffset, count)

    Duct.Progress(count, count)

  override def flush(target: output.Storage, targetOffset: Int, targetSpace: Int): Int =
    if emitted then 0 else
      emitted = true
      output.storageUpdate(target, targetOffset, 99.toByte.asInstanceOf[output.Operand])
      1

// Passes refills through unchanged, recording each demand it receives.
class Recorder(underlying: Stream[Data] over Credit) extends Stream[Data]:
  type Transport = Credit

  var demands: List[Long] = Nil

  def refill(demand: Credit): Optional[Int] =
    demands ::= demand.count
    underlying.refill(demand)

  protected def window0: AnyRef = unsafely(underlying.window).asInstanceOf[AnyRef]
  def start: Int = underlying.start
  def limit: Int = underlying.limit
  def skip(count: Int): Unit = underlying.skip(count)
