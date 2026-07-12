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
package turbulence

import java.io as ji

import soundness.*

import charEncoders.utf8Encoder, charDecoders.utf8Decoder, textSanitizers.strictSanitizer
import threading.platformThreading
import strategies.throwUnsafely
import probates.panicProbate
import errorDiagnostics.emptyDiagnostics

import scala.collection.mutable as scm

object Tests extends Suite(m"Turbulence tests"):
  def run(): Unit =

    suite(m"Shredding"):
      given Seed = Seed(1L)
      import randomization.seededRandomization
      val data: Data = Data.fill(1000)(_.toByte)
      val stream: LazyList[Data] = LazyList(data)
      val shredded: Iterable[LazyList[Data]] = stochastic:
        (0 until 100).map: index =>
          stream.shred(20.0, 10.0)

      shredded.each: stream =>
        test(m"correct length after shredding"):
          stream.map(_.length).total
        . assert(_ == 1000)

        test(m"correct content after shredding"):
          stream.reduce(_ ++ _)
        . assert(_ === data)

    suite(m"Streaming Unicode tests"):
      val ascii = IArray(t"", t"a", t"ab", t"abc", t"abcd")

      val strings = for
        asc0 <- List(t"", t"a", t"ab", t"abc") // 4 combinations
        cp2  <- List(t"", t"£")                // 8
        asc1 <- List(t"", t"a", t"ab", t"abc") // 32
        cp3  <- List(t"", t"€")                // 64
        asc2 <- List(t"", t"a", t"ab", t"abc") // 256
        cp4  <- List(t"", t"𐍈")                // 512
        asc3 <- List(t"", t"a", t"ab", t"abc") // 2048
      yield asc0+cp2+asc1+cp3+asc2+cp4

      for
        string <- strings
        bs     <- 1 to 8
      do
        test(m"length tests"):
          val stream = string.data.grouped(bs).to(LazyList)
          val result = stream.read[Text]
          result.data.length
        . assert(_ == string.data.length)

        test(m"roundtrip tests"):
          val stream = string.data.grouped(bs).to(LazyList)
          val result = stream.read[Text]

          result.s
        . assert(_ == string.s)

    val qbf = t"The quick brown fox\njumps over the lazy dog"
    val qbfData = qbf.data

    object Ref:
      given textSource: Ref is Source by Text over Credit =
        ref => Stream(LazyList(t"abc", t"def").iterator)
      given dataSource: Ref is Source by Data over Credit =
        ref => Stream(LazyList(t"abc".data, t"def".data).iterator)

    case class Ref()

    object Ref2:
      given Ref2 is Source by Text over Credit = ref => Stream(LazyList(t"abc", t"def").iterator)

    case class Ref2()

    object Ref3:
      given Ref3 is Source by Data over Credit = ref => Stream(LazyList(t"abc".data, t"def".data).iterator)

    case class Ref3()

    suite(m"Reading tests"):
      test(m"LazyList Text"):
        qbf.lazyList[Text].join
      . assert(_ == qbf)

      test(m"LazyList Data"):
        qbf.lazyList[Data].reduce(_ ++ _).to(List)
      . assert(_ == qbfData.to(List))

      test(m"Read Text as Text"):
        qbf.read[Text].s
      . assert(_ == qbf.s)

      test(m"Read type as Text with Text and Byte Source"):
        Ref().read[Text].s
      . assert(_ == t"abcdef".s)

      test(m"Read type as Data with Text and Byte Source"):
        Ref().read[Data].to(List)
      . assert(_ == t"abcdef".data.to(List))

      test(m"Read some type as Text with only Text Source instance"):
        Ref2().read[Text].s
      . assert(_ == t"abcdef".s)

      test(m"Read some type as Data with only Text Source instance"):
        Ref2().read[Data].to(List)
      . assert(_ == t"abcdef".data.to(List))

      test(m"Read some type as Text with only Data Source instance"):
        Ref3().read[Text].s
      . assert(_ == t"abcdef".s)

      test(m"Read some type as Data with only Data Streamable instance"):
        Ref3().read[Data].to(List)
      . assert(_ == t"abcdef".data.to(List))

      test(m"Read Text as LazyList[Text]"):
        qbf.read[LazyList[Text]].join
      . assert(_ == qbf)

      test(m"Read Text as Data"):
        qbf.read[Data]
      . assert(_.to(List) == qbfData.to(List))

      test(m"Read Text as LazyList[Data]"):
        qbf.read[LazyList[Data]]
      . assert(_.reduce(_ ++ _).to(List) == qbfData.to(List))

      test(m"Read Data as Text"):
        qbfData.read[Text].s
      . assert(_ == qbf.s)

      test(m"Read Data as LazyList[Text]"):
        qbfData.read[LazyList[Text]].join
      . assert(_ == qbf)

      test(m"Read Data as Data"):
        qbfData.read[Data]
      . assert(_.to(List) == qbfData.to(List))

      test(m"Read Data as LazyList[Data]"):
        qbfData.read[LazyList[Data]]
      . assert(_.reduce(_ ++ _).to(List) == qbfData.to(List))

      // test(m"Read Text as Lines"):
      //   qbf.read[LazyList[Line]]
      // .assert(_ == LazyList(Line(t"The quick brown fox"), Line(t"jumps over the lazy dog")))

      // test(m"Read Data as Lines"):
      //   qbfData.read[LazyList[Line]]
      // .assert(_ == LazyList(Line(t"The quick brown fox"), Line(t"jumps over the lazy dog")))

    suite(m"Writing tests"):

      class GeneralStore():
        val arrayBuffer: scm.ArrayBuffer[Byte] = scm.ArrayBuffer()
        def apply(): Text = String(arrayBuffer.toArray, "UTF-8").tt

      object GeneralStore:
        given GeneralStore is Writable by Data = (store, stream) => stream.each: data =>
          data.each: byte =>
            store.arrayBuffer.append(byte)

        given GeneralStore is Writable by Text = (store, texts) => texts.each: text =>
          text.data.each: byte =>
            store.arrayBuffer.append(byte)

      class ByteStore():
        val arrayBuffer: scm.ArrayBuffer[Byte] = scm.ArrayBuffer()
        def apply(): Text = String(arrayBuffer.toArray, "UTF-8").tt

      object ByteStore:
        given ByteStore is Writable by Data = (store, stream) => stream.each: data =>
          data.each: byte =>
            store.arrayBuffer.append(byte)

      class TextStore():
        var text: Text = t""
        def apply(): Text = text

      object TextStore:
        given TextStore is Writable by Text = (store, texts) => texts.each: text =>
          store.text = store.text + text

      test(m"Write Text to some reference with Text and Data instances"):
        val store = GeneralStore()
        qbf.writeTo(store)
        store().s
      . assert(_ == qbf.s)

      test(m"Write Data to some reference with Text and Data instances"):
        val store = GeneralStore()
        qbfData.writeTo(store)
        store().s
      . assert(_ == qbf.s)

      test(m"Write LazyList[Text] with Text and Data instances"):
        val store = GeneralStore()
        LazyList(qbf).writeTo(store)
        store()
      . assert(_ == qbf)

      test(m"Write LazyList[Data] with Text and Data instances"):
        val store = GeneralStore()
        LazyList(qbfData).writeTo(store)
        store()
      . assert(_ == qbf)

      test(m"Write Text to some reference with only a Data instance"):
        val store = ByteStore()
        qbf.writeTo(store)
        store().s
      . assert(_ == qbf.s)

      test(m"Write Data to some reference with only a Data instance"):
        val store = ByteStore()
        qbfData.writeTo(store)
        store().s
      . assert(_ == qbf.s)

      test(m"Write LazyList[Text] with only Data instance"):
        val store = ByteStore()
        LazyList(qbf).writeTo(store)
        store()
      . assert(_ == qbf)

      test(m"Write LazyList[Data] with only Data instance"):
        val store = ByteStore()
        LazyList(qbfData).writeTo(store)
        store()
      . assert(_ == qbf)

      test(m"Write Text to some reference with only a Text instance"):
        val store = TextStore()
        qbf.writeTo(store)
        store().s
      . assert(_ == qbf.s)

      test(m"Write Data to some reference with only a Text instance"):
        val store = TextStore()
        qbfData.writeTo(store)
        store().s
      . assert(_ == qbf.s)

      test(m"Write LazyList[Text] with only Text instance"):
        val store = TextStore()
        LazyList(qbf).writeTo(store)
        store()
      . assert(_ == qbf)

      test(m"Write LazyList[Data] with only Text instance"):
        val store = TextStore()
        LazyList(qbfData).writeTo(store)
        store()
      . assert(_ == qbf)

    // suite(m"Appending tests"):

    //   class GeneralStore():
    //     val arrayBuffer: scm.ArrayBuffer[Byte] = scm.ArrayBuffer()
    //     def apply(): Text = String(arrayBuffer.toArray, "UTF-8").tt

    //   object GeneralStore:
    //     given GeneralStore is Writable by Data = (store, stream) => stream.each: data =>
    //       data.each: byte =>
    //         store.arrayBuffer.append(byte)

    //     given GeneralStore is Writable by Text = (store, texts) => texts.each: text =>
    //       text.data.each: byte =>
    //         store.arrayBuffer.append(byte)

    //   class ByteStore():
    //     val arrayBuffer: scm.ArrayBuffer[Byte] = scm.ArrayBuffer()
    //     def apply(): Text = String(arrayBuffer.toArray, "UTF-8").tt

    //   object ByteStore:
    //     given ByteStore is Writable by Data = (store, stream) => stream.each: data =>
    //       data.each: byte =>
    //         Eof(store.arrayBuffer).write(byte)

    //   class TextStore():
    //     var text: Text = t""
    //     def apply(): Text = text

    //   object TextStore:
    //     given TextStore is Writable by Text = (store, texts) => texts.each: text =>
    //       store.text = store.text + text

      // test(m"Append Text to some reference with Text and Data instances"):
      //   val store = GeneralStore()
      //   qbf.appendTo(store)
      //   store()
      // .assert(_ == qbf)

      // test(m"Append Data to some reference with Text and Data instances"):
      //   val store = GeneralStore()
      //   qbfData.appendTo(store)
      //   store()
      // .assert(_ == qbf)

      // test(m"Append LazyList[Text] with Text and Data instances"):
      //   val store = GeneralStore()
      //   LazyList(qbf).appendTo(store)
      //   store()
      // .assert(_ == qbf)

      // test(m"Append LazyList[Data] with Text and Data instances"):
      //   val store = GeneralStore()
      //   LazyList(qbfData).appendTo(store)
      //   store()
      // .assert(_ == qbf)

      // test(m"Append Text to some reference with only a Data instance"):
      //   val store = ByteStore()
      //   qbf.appendTo(store)
      //   store()
      // .assert(_ == qbf)

      // test(m"Append Data to some reference with only a Data instance"):
      //   val store = ByteStore()
      //   qbfData.appendTo(store)
      //   store()
      // .assert(_ == qbf)

      // test(m"Append LazyList[Text] with only Data instance"):
      //   val store = ByteStore()
      //   LazyList(qbf).appendTo(store)
      //   store()
      // .assert(_ == qbf)

      // test(m"Append LazyList[Data] with only Data instance"):
      //   val store = ByteStore()
      //   LazyList(qbfData).appendTo(store)
      //   store()
      // .assert(_ == qbf)

      // test(m"Append Text to some reference with only a Text instance"):
      //   val store = TextStore()
      //   qbf.appendTo(store)
      //   store()
      // .assert(_ == qbf)

      // test(m"Append Data to some reference with only a Text instance"):
      //   val store = TextStore()
      //   qbfData.appendTo(store)
      //   store()
      // .assert(_ == qbf)

      // test(m"Append LazyList[Text] with only Text instance"):
      //   val store = TextStore()
      //   LazyList(qbf).appendTo(store)
      //   store()
      // .assert(_ == qbf)

      // test(m"Append LazyList[Data] with only Text instance"):
      //   val store = TextStore()
      //   LazyList(qbfData).appendTo(store)
      //   store()
      // .assert(_ == qbf)

    suite(m"Multiplexer tests"):
      val l1 = LazyList(2, 4, 6, 8, 10)
      val l2 = LazyList(1, 3, 5, 7, 9)

      test(m"Check that two multiplexed streams contain all elements"):
        supervise(l1.multiplex(l2).to(Set))
      . assert(_ == Set.range(1, 11))

      test(m"Multiplexed streams preserve first stream order"):
        supervise(l1.multiplex(l2).filter(_%2 == 0))
      . assert(_ == l1)

      for i <- 1 to 10
      do test(m"Multiplexed streams preserve second stream order"):
        supervise(l1.multiplex(l2).filter(_%2 == 1))
      . assert(_ == l2)

    suite(m"Compression tests"):
      test(m"Compress a single block with GZip"):
        LazyList(Data(1, 1, 2, 3, 5, 8, 13, 21, 34)).compress[Gzip].to(List).map(_.to(List))
      . assert(_ == List(List(31, -117, 8, 0, 0, 0, 0, 0, 0, -1), List(99, 100, 100, 98, 102, -27, -32, 21, 85, 2, 0, -56, -16, -118, -53, 9, 0, 0, 0)))

      test(m"Roundtrip compress/decompress a single block with GZip"):
        LazyList(Data(1, 1, 2, 3, 5, 8, 13, 21, 34)).compress[Gzip].decompress[Gzip]
      . assert: stream => stream === LazyList(Data(1, 1, 2, 3, 5, 8, 13, 21, 34))

      val longData = LazyList.continually(IArray.from((0 to 255).map(_.toByte))).take(1000)

      test(m"Roundtrip compress/decompress a long repetitive stream with Gzip"):
        longData.compress[Gzip].decompress[Gzip]
      . assert(_.flatten == longData.flatten)
      test(m"Compress a single block with Zlib"):
        LazyList(Data(1, 1, 2, 3, 5, 8, 13, 21, 34)).compress[Zlib].to(List).map(_.to(List))
      . assert(_ == List(List(120, -100, 98, 100, 100, 98, 102, -27, -32, 21, 85, 2, 0, 0, 0, -1, -1), List(3, 0, 0, -26, 0, 89)))

      test(m"Roundtrip compress/decompress a single block with Zlib"):
        LazyList(Data(1, 1, 2, 3, 5, 8, 13, 21, 34)).compress[Zlib].decompress[Zlib]
      . assert: stream => stream === LazyList(Data(1, 1, 2, 3, 5, 8, 13, 21, 34))

      test(m"Roundtrip compress/decompress a long repetitive stream with Zlib"):
        longData.compress[Zlib].decompress[Zlib]
      . assert: stream => stream === longData

      test(m"Compress a single block with Deflate"):
        LazyList(Data(1, 1, 2, 3, 5, 8, 13, 21, 34)).compress[Deflate].to(List).map(_.to(List))
      . assert(_ == List(List(98, 100, 100, 98, 102, -27, -32, 21, 85, 2, 0, 0, 0, -1, -1), List(3, 0)))

      test(m"Roundtrip compress/decompress a single block with Deflate"):
        LazyList(Data(1, 1, 2, 3, 5, 8, 13, 21, 34)).compress[Deflate].decompress[Deflate]
      . assert: stream => stream === LazyList(Data(1, 1, 2, 3, 5, 8, 13, 21, 34))

      test(m"Roundtrip a long repetitive Deflate stream"):
        longData.compress[Deflate].decompress[Deflate]
      . assert: stream => stream === longData

    suite(m"Framing"):
      test(m"Single 4-byte-prefixed frame in one chunk"):
        LazyList(Data(0, 0, 0, 3, 10, 20, 30)).framed[U32]().map(_.to(List)).to(List)
      . assert(_ == List(List[Byte](10, 20, 30)))

      test(m"Single 2-byte-prefixed frame in one chunk"):
        LazyList(Data(0, 3, 10, 20, 30)).framed[U16]().map(_.to(List)).to(List)
      . assert(_ == List(List[Byte](10, 20, 30)))

      test(m"Multiple 4-byte-prefixed frames in one chunk"):
        val data = Data(0, 0, 0, 2, 1, 2, 0, 0, 0, 3, 3, 4, 5)
        LazyList(data).framed[U32]().map(_.to(List)).to(List)
      . assert(_ == List(List[Byte](1, 2), List[Byte](3, 4, 5)))

      test(m"Frame split across two chunks"):
        val s = LazyList(Data(0, 0, 0, 5, 1, 2), Data(3, 4, 5))
        s.framed[U32]().map(_.to(List)).to(List)
      . assert(_ == List(List[Byte](1, 2, 3, 4, 5)))

      test(m"Length prefix split across chunks"):
        val s = LazyList(Data(0, 0), Data(0, 3, 7, 8, 9))
        s.framed[U32]().map(_.to(List)).to(List)
      . assert(_ == List(List[Byte](7, 8, 9)))

      test(m"Terminator ends stream cleanly"):
        val data = Data(0, 0, 0, 2, 1, 2, 0, 0, 255.toByte, 255.toByte)
        LazyList(data).framed[U32](U32(0xFFFF.bits)).map(_.to(List)).to(List)
      . assert(_ == List(List[Byte](1, 2)))

      test(m"Truncated frame raises StreamError"):
        capture[StreamError]:
          LazyList(Data(0, 0, 0, 5, 1, 2)).framed[U32]().to(List)
      . assert(_ == StreamError(6L.b))

      test(m"Truncated length prefix raises StreamError"):
        capture[StreamError]:
          LazyList(Data(0, 0)).framed[U32]().to(List)
      . assert(_ == StreamError(2L.b))

      test(m"Empty stream yields empty result"):
        LazyList[Data]().framed[U32]().to(List)
      . assert(_ == Nil)

    suite(m"Source and Sink tests"):
      val payload: Data = Data.fill(10000)(_.toByte)

      test(m"input stream source flows to output stream sink"):
        val input = ji.ByteArrayInputStream(payload.mutable(using Unsafe))
        val output = ji.ByteArrayOutputStream()
        val source = summon[ji.ByteArrayInputStream is Source by Data over Credit]
        val sink = summon[ji.ByteArrayOutputStream is Sink by Data over Credit]
        source.stream(input).pump(sink.intake(output))
        output.toByteArray.nn.to(List)
      . assert(_ == payload.to(List))

      test(m"in-memory data source flows to output stream sink"):
        val output = ji.ByteArrayOutputStream()
        val sink = summon[ji.ByteArrayOutputStream is Sink by Data over Credit]
        summon[Data is Source by Data over Credit].stream(payload).pump(sink.intake(output))
        output.toByteArray.nn.to(List)
      . assert(_ == payload.to(List))

      val original = t"The quick brown fox jumps over the lazy dog"*100

      test(m"reader source delivers text across refills"):
        val reader = ji.StringReader(original.s)
        val source = summon[ji.StringReader is Source by Text over Credit]
        val stream = source.stream(reader)
        val builder = StringBuilder()

        def recur(): Unit = stream.refill(Credit(64)) match
          case count: Int =>
            val window = unsafely(stream.window).asInstanceOf[Array[Char]]
            builder.append(String(window, stream.start, count))
            stream.skip(count)
            recur()

          case _ => ()

        recur()
        builder.toString.tt
      . assert(_ == original)

      test(m"memoize view drains a stream as one value"):
        val stream = summon[Data is Source by Data over Credit].stream(payload)
        stream.memoize.to(List)
      . assert(_ == payload.to(List))

      test(m"a LazyList is a Source through its native instance"):
        val output = ji.ByteArrayOutputStream()
        val sink = summon[ji.ByteArrayOutputStream is Sink by Data over Credit]
        val source = summon[LazyList[Data] is Source by Data over Credit]
        source.stream(LazyList(payload, payload)).pump(sink.intake(output))
        output.toByteArray.nn.length
      . assert(_ == payload.length*2)

      test(m"a sink write failure raises StreamError"):
        import unsafeExceptions.canThrowAny

        val broken = new ji.OutputStream():
          override def write(byte: Int): Unit = throw ji.IOException("cut")
          override def write(array: Array[Byte] | Null, off: Int, len: Int): Unit =
            throw ji.IOException("cut")

        val sink = summon[ji.OutputStream is Sink by Data over Credit]

        capture[StreamError]:
          summon[Data is Source by Data over Credit].stream(payload).pump(sink.intake(broken))
      . assert(_ == StreamError(0.b))

      test(m"cancelling a blocked conduit writer releases it"):
        supervise:
          val (intake, stream) = Conduit[Data]()
          val big = Data.fill(100000)(_.toByte)
          val writer = async(intake.put(big))
          writer.cancel()
          true
      . assert(identity)

      test(m"confluence merges all sources completely"):
        supervise:
          val sources = (1 to 4).map { index => Data.fill(1000)(_ => index.toByte) }
          // built in a while-loop: fresh endpoints cannot leave a `map` lambda
          val builder = List.newBuilder[AnyRef]
          var index = 0
          while index < sources.length do
            builder += summon[Data is Source by Data over Credit].stream(sources(index)).asInstanceOf[AnyRef]
            index += 1
          val endpoints = builder.result()

          val merged = Confluence(endpoints.map(_.asInstanceOf[Stream[Data] over Credit])*)
          val gather = Gather2()
          merged.pump(gather)
          gather.data.to(List).sorted
      . assert(_ == (1 to 4).flatMap { index => List.fill(1000)(index.toByte) }.sorted.to(List))

      test(m"manifold delivers the whole stream to every subscriber"):
        supervise:
          val source = summon[Data is Source by Data over Credit].stream(payload)
          val subscribers = Divergence(source, 3)

          val results = subscribers.map: stream =>
            async:
              val gather = Gather2()
              stream.pump(gather)
              gather.data.to(List)

          results.map { task => task.await() }.to(List)
      . assert(_ == List.fill(3)(payload.to(List)))

      val mixed: Data =
        Data.fill(50000) { index => (index%251).toByte } ++ (t"repetition "*500).data

      test(m"gzip duct roundtrips a byte stream"):
        val gather = Gather2()
        summon[Data is Source by Data over Credit].stream(mixed)
        . compress[Gzip].decompress[Gzip].pump(gather)
        gather.data.to(List)
      . assert(_ == mixed.to(List))

      // A duct-chain source has a transient window (its buffer is reused between
      // refills), so the fan-out must snapshot each chunk rather than share it.
      test(m"manifold snapshots a transient source for every subscriber"):
        supervise:
          val source =
            summon[Data is Source by Data over Credit].stream(mixed)
            . compress[Gzip].decompress[Gzip]

          val subscribers = Divergence(source, 3)

          val results = subscribers.map: stream =>
            async:
              val gather = Gather2()
              stream.pump(gather)
              gather.data.to(List)

          results.map { task => task.await() }.to(List)
      . assert(_ == List.fill(3)(mixed.to(List)))

      test(m"confluence snapshots transient sources into the merge"):
        supervise:
          val builder = List.newBuilder[AnyRef]
          var index = 0
          while index < 3 do
            builder +=
              summon[Data is Source by Data over Credit].stream(mixed)
              . compress[Gzip].decompress[Gzip].asInstanceOf[AnyRef]
            index += 1

          val merged = Confluence(builder.result().map(_.asInstanceOf[Stream[Data] over Credit])*)
          val gather = Gather2()
          merged.pump(gather)
          gather.data.length
      . assert(_ == mixed.length*3)

      test(m"deflate duct roundtrips a byte stream"):
        val gather = Gather2()
        summon[Data is Source by Data over Credit].stream(mixed)
        . compress[Deflate].decompress[Deflate].pump(gather)
        gather.data.to(List)
      . assert(_ == mixed.to(List))

      test(m"zlib duct roundtrips a byte stream"):
        val gather = Gather2()
        summon[Data is Source by Data over Credit].stream(mixed)
        . compress[Zlib].decompress[Zlib].pump(gather)
        gather.data.to(List)
      . assert(_ == mixed.to(List))

      test(m"gzip duct output is genuine gzip"):
        val gather = Gather2()
        summon[Data is Source by Data over Credit].stream(mixed).compress[Gzip].pump(gather)
        val stream = java.util.zip.GZIPInputStream(ji.ByteArrayInputStream(gather.data.mutable(using Unsafe)))
        stream.readAllBytes().nn.to(List)
      . assert(_ == mixed.to(List))

      test(m"gzip duct decompresses JDK-produced gzip"):
        val out = ji.ByteArrayOutputStream()
        val zipped = java.util.zip.GZIPOutputStream(out)
        zipped.write(mixed.mutable(using Unsafe))
        zipped.close()
        val gather = Gather2()

        summon[LazyList[Data] is Source by Data over Credit]
        . stream(out.toByteArray.nn.immutable(using Unsafe).grouped(7).to(LazyList))
        . decompress[Gzip].pump(gather)

        gather.data.to(List)
      . assert(_ == mixed.to(List))

      test(m"cancelling a detached flow blocked on an empty conduit releases it"):
        supervise:
          val (intake, stream) = Conduit[Data]()
          val gather = Gather2()
          val pump = stream.flow(gather)
          pump.cancel()
          true
      . assert(identity)

// A byte intake that gathers everything written to it, for exercising the
// pump and cancellation paths.
class Gather2() extends Intake[Data]:
  type Transport = Credit

  private val block: Int = 16
  private val storage: addressable.Storage = addressable.allocate(block).asInstanceOf[addressable.Storage]
  private val target: addressable.Target = addressable.blank(64)
  private var mark1: Int = 0

  def demand: Credit = Credit(Long.MaxValue)
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
