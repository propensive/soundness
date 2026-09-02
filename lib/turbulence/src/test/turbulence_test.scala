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
package turbulence

import scala.caps

import java.io as ji
import java.util.concurrent.atomic.AtomicLong

import soundness.*

import charEncoders.utf8Encoder, charDecoders.utf8Decoder, textSanitizers.strictSanitizer
import threading.platformThreading
import strategies.throwUnsafely
import probates.panicProbate
import errorDiagnostics.emptyDiagnostics

import scala.collection.immutable as sci
import scala.collection.mutable as scm

object Tests extends Suite(m"Turbulence tests"):
  def run(): Unit =

    suite(m"Shredding"):
      given Seed = Seed(1L)
      import randomization.seededRandomization
      val data: Data = Data.fill(1000)(_.toByte)
      val stream: Chain[Data] = Chain(data)
      val shredded: Iterable[Chain[Data]] = stochastic:
        (0 until 100).map: index =>
          stream.shred(20.0, 10.0)

      shredded.each: stream =>
        test(m"correct length after shredding"):
          stream.map(_.readable.length).stdlib.total
        . assert(_ == 1000)

        test(m"correct content after shredding"):
          Array.frozen(stream.stdlib.map(_.readable).reduce(_ ++ _))
        . assert(_ === data)

    suite(m"Streaming Unicode tests"):
      val ascii = Array(t"", t"a", t"ab", t"abc", t"abcd")

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
          val stream = string.in[Data].readable.grouped(bs).map(Array.frozen(_)).to(proscenium.Chain)
          val result = stream.read[Text]
          result.in[Data].readable.length
        . assert(_ == string.in[Data].readable.length)

        test(m"roundtrip tests"):
          val stream = string.in[Data].readable.grouped(bs).map(Array.frozen(_)).to(proscenium.Chain)
          val result = stream.read[Text]

          result.s
        . assert(_ == string.s)

      test(m"a surrogate pair split across chunks encodes correctly"):
        val gothic = t"𐍈"
        val high = gothic.s.charAt(0).toString.tt
        val low = gothic.s.charAt(1).toString.tt

        summon[CharEncoder].encoded(Chain(t"a", high, low, t"b"))
        . stdlib.to(List).map(_.readable).reduce(_ ++ _).to(List)
      . assert(_ == t"a𐍈b".in[Data].readable.to(List))

      test(m"per-char-chunk streams roundtrip through encode and decode"):
        val string = "aë€𐍈z"

        val chunks =
          (0 until string.length).map { index => string.charAt(index).toString.tt }.to(Chain)

        summon[CharDecoder].decoded(summon[CharEncoder].encoded(chunks))
        . stdlib.to(List).map(_.s).mkString
      . assert(_ == "aë€𐍈z")

    val qbf = t"The quick brown fox\njumps over the lazy dog"
    val qbfData = qbf.in[Data]

    object Ref:
      given textSource: Ref is Streamable by Text over Credit =
        ref => Stream(Chain(t"abc", t"def").iterator)
      given dataSource: Ref is Streamable by Data over Credit =
        ref => Stream(Chain(t"abc".in[Data], t"def".in[Data]).iterator)

    case class Ref()

    object Ref2:
      given Ref2 is Streamable by Text over Credit = ref => Stream(Chain(t"abc", t"def").iterator)

    case class Ref2()

    object Ref3:
      given Ref3 is Streamable by Data over Credit = ref => Stream(Chain(t"abc".in[Data], t"def".in[Data]).iterator)

    case class Ref3()

    suite(m"Reading tests"):
      test(m"Bridge Text source to Chain"):
        qbf.source[Text].chain.join
      . assert(_ == qbf)

      test(m"Bridge Data source to Chain"):
        Array.frozen(qbf.source[Data].chain.stdlib.map(_.readable).reduce(_ ++ _)).to[List]
      . assert(_ == qbfData.to[List])

      test(m"Read Text as Text"):
        qbf.read[Text].s
      . assert(_ == qbf.s)

      test(m"Read type as Text with Text and Byte Source"):
        Ref().read[Text].s
      . assert(_ == t"abcdef".s)

      test(m"Read type as Data with Text and Byte Source"):
        Ref().read[Data].to[List]
      . assert(_ == t"abcdef".in[Data].to[List])

      test(m"Read some type as Text with only Text Source instance"):
        Ref2().read[Text].s
      . assert(_ == t"abcdef".s)

      test(m"Read some type as Data with only Text Source instance"):
        Ref2().read[Data].to[List]
      . assert(_ == t"abcdef".in[Data].to[List])

      test(m"Read some type as Text with only Data Source instance"):
        Ref3().read[Text].s
      . assert(_ == t"abcdef".s)

      test(m"Read some type as Data with only Data Streamable instance"):
        Ref3().read[Data].to[List]
      . assert(_ == t"abcdef".in[Data].to[List])

      test(m"Read Text as Chain[Text]"):
        qbf.read[Chain[Text]].join
      . assert(_ == qbf)

      test(m"Read Text as Data"):
        qbf.read[Data]
      . assert(_.to[List] == qbfData.to[List])

      test(m"Read Text as Chain[Data]"):
        qbf.read[Chain[Data]]
      . assert(stream => Array.frozen(stream.stdlib.map(_.readable).reduce(_ ++ _)).to[List] == qbfData.to[List])

      test(m"Read Data as Text"):
        qbfData.read[Text].s
      . assert(_ == qbf.s)

      test(m"Read Data as Chain[Text]"):
        qbfData.read[Chain[Text]].join
      . assert(_ == qbf)

      test(m"Read Data as Data"):
        qbfData.read[Data]
      . assert(_.to[List] == qbfData.to[List])

      test(m"Read Data as Chain[Data]"):
        qbfData.read[Chain[Data]]
      . assert(stream => Array.frozen(stream.stdlib.map(_.readable).reduce(_ ++ _)).to[List] == qbfData.to[List])

      // test(m"Read Text as Lines"):
      //   qbf.read[Chain[Line]]
      // .assert(_ == Chain(Line(t"The quick brown fox"), Line(t"jumps over the lazy dog")))

      // test(m"Read Data as Lines"):
      //   qbfData.read[Chain[Line]]
      // .assert(_ == Chain(Line(t"The quick brown fox"), Line(t"jumps over the lazy dog")))

    suite(m"Writing tests"):

      class GeneralStore():
        val arrayBuffer: scm.ArrayBuffer[Byte] = scm.ArrayBuffer()
        def apply(): Text = String(arrayBuffer.toArray, "UTF-8").tt

      object GeneralStore:
        given GeneralStore is Writable by Data = (store, stream) =>
          zephyrine.chain(stream.asInstanceOf[AnyRef].asInstanceOf[(Stream[Data] over Credit)^]).each: data =>
            data.each: byte =>
              store.arrayBuffer.append(byte)

        given GeneralStore is Writable by Text = (store, stream) =>
          zephyrine.chain(stream.asInstanceOf[AnyRef].asInstanceOf[(Stream[Text] over Credit)^]).each: text =>
            text.in[Data].each: byte =>
              store.arrayBuffer.append(byte)

      class ByteStore():
        val arrayBuffer: scm.ArrayBuffer[Byte] = scm.ArrayBuffer()
        def apply(): Text = String(arrayBuffer.toArray, "UTF-8").tt

      object ByteStore:
        given ByteStore is Writable by Data = (store, stream) =>
          zephyrine.chain(stream.asInstanceOf[AnyRef].asInstanceOf[(Stream[Data] over Credit)^]).each: data =>
            data.each: byte =>
              store.arrayBuffer.append(byte)

      class TextStore():
        @scala.caps.unsafe.untrackedCaptures
        var text: Text = t""
        def apply(): Text = text

      object TextStore:
        given TextStore is Writable by Text = (store, stream) =>
          zephyrine.chain(stream.asInstanceOf[AnyRef].asInstanceOf[(Stream[Text] over Credit)^]).each: text =>
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

      test(m"Write Chain[Text] with Text and Data instances"):
        val store = GeneralStore()
        Chain(qbf).writeTo(store)
        store()
      . assert(_ == qbf)

      test(m"Write Chain[Data] with Text and Data instances"):
        val store = GeneralStore()
        Chain(qbfData).writeTo(store)
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

      test(m"Write Chain[Text] with only Data instance"):
        val store = ByteStore()
        Chain(qbf).writeTo(store)
        store()
      . assert(_ == qbf)

      test(m"Write Chain[Data] with only Data instance"):
        val store = ByteStore()
        Chain(qbfData).writeTo(store)
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

      test(m"Write Chain[Text] with only Text instance"):
        val store = TextStore()
        Chain(qbf).writeTo(store)
        store()
      . assert(_ == qbf)

      test(m"Write Chain[Data] with only Text instance"):
        val store = TextStore()
        Chain(qbfData).writeTo(store)
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

      // test(m"Append Chain[Text] with Text and Data instances"):
      //   val store = GeneralStore()
      //   Chain(qbf).appendTo(store)
      //   store()
      // .assert(_ == qbf)

      // test(m"Append Chain[Data] with Text and Data instances"):
      //   val store = GeneralStore()
      //   Chain(qbfData).appendTo(store)
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

      // test(m"Append Chain[Text] with only Data instance"):
      //   val store = ByteStore()
      //   Chain(qbf).appendTo(store)
      //   store()
      // .assert(_ == qbf)

      // test(m"Append Chain[Data] with only Data instance"):
      //   val store = ByteStore()
      //   Chain(qbfData).appendTo(store)
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

      // test(m"Append Chain[Text] with only Text instance"):
      //   val store = TextStore()
      //   Chain(qbf).appendTo(store)
      //   store()
      // .assert(_ == qbf)

      // test(m"Append Chain[Data] with only Text instance"):
      //   val store = TextStore()
      //   Chain(qbfData).appendTo(store)
      //   store()
      // .assert(_ == qbf)

    suite(m"Relay tests"):
      test(m"records put before draining arrive in order"):
        val relay = Relay[Text]()
        relay.put(t"one")
        relay.put(t"two")
        relay.put(t"three")
        relay.stop()
        relay.stream.records.to(List)
      . assert(_ == List(t"one", t"two", t"three"))

      test(m"records already queued batch into one window"):
        val relay = Relay[Text]()
        relay.put(t"a")
        relay.put(t"b")
        relay.put(t"c")
        relay.stop()
        var windows: Int = 0

        relay.stream.drain: _ =>
          _ => windows += 1

        windows
      . assert(_ == 1)

      test(m"an immediately-stopped relay yields no records"):
        val relay = Relay[Text]()
        relay.stop()
        relay.stream.records.to(List)
      . assert(_ == List())

      test(m"records after stop are not delivered"):
        val relay = Relay[Text]()
        relay.put(t"before")
        relay.stop()
        relay.put(t"after")
        relay.stream.records.to(List)
      . assert(_ == List(t"before"))

      test(m"the reader blocks for records from concurrent producers"):
        supervise:
          val relay = Relay[Text]()
          // Handles collected for concurrent await: sealed per the pure-façade convention
          // (D6; the `Seq[Task].sequence` shape).
          val producers = (1 to 4).map: index =>
            caps.unsafe.unsafeAssumePure:
              async:
                for value <- 1 to 25 do relay.put(t"${index*100 + value}")

          val reader = async(relay.stream.records.to(Set))
          producers.each(_.await())
          relay.stop()
          unsafely(scala.caps.unsafe.unsafeAssumeSeparate(reader.await()))
      . assert(_ == (for index <- 1 to 4; value <- 1 to 25 yield t"${index*100 + value}").to(Set))

      // The relay's contract is that producers never block: it is the many-producer
      // record bus (HTTP/2's outbound frame mux relies on this to avoid distributed
      // deadlock), so its buffering is unbounded by design. A bounded relay would
      // wedge this test, which writes far more than any plausible bound with no
      // reader attached.
      test(m"relay puts never block, even with no reader"):
        val relay = Relay[Text]()
        for _ <- 1 to 100000 do relay.put(t"x")
        relay.stop()
        relay.stream.records.to(List).length
      . assert(_ == 100000)

      test(m"per-producer order is preserved through the relay"):
        supervise:
          val relay = Relay[Text]()
          val producer = async:
            for value <- 1 to 100 do relay.put(t"$value")
            relay.stop()

          unsafely(scala.caps.unsafe.unsafeAssumeSeparate(async(relay.stream.records.to(List)).await()))
      . assert(_ == (1 to 100).to(List).map { value => t"$value" })

    suite(m"Line splitting"):
      test(m"whole-value Data delineate agrees with the stream form"):
        import lineSeparation.adaptiveLinefeedLineSeparation
        val bytes: Data = t"one\ntwo\r\nthree".in[Data]
        bytes.delineate.to[List]
      . assert(_ == List(t"one", t"two", t"three"))

      // Split whole, or fragmented into `chunk`-char pieces — the fragmented
      // rows exercise separator sequences spanning window boundaries. A chunk
      // size of `-1` runs the whole-value form (`text.delineate`, the
      // `Duct.feed` driver), which must agree with the streaming form on
      // every case.
      def splitLines(input: Text, chunk: Int)(using LineSeparation): List[Text] =
        if chunk == -1 then input.delineate.to[List]
        else if chunk == 0 then input.stream.delineate.records.to(List)
        else input.s.grouped(chunk).map(_.tt).stream.delineate.records.to(List)

      // The same input through the byte-level duct (`Stream[Data].delineate`
      // under a UTF-8 decoder, which is ASCII-transparent, so the separators are
      // found in the bytes and each line is decoded whole). Fragmenting by BYTES
      // rather than characters is the point: it cuts multi-byte characters in
      // half at a window boundary, which the byte duct must carry through.
      def splitBytes(input: Text, chunk: Int)(using LineSeparation): List[Text] =
        // A `Data` is a frozen byte array, so each freshly-copied range is frozen
        // by the cast. The chunking is an explicit loop rather than `grouped` +
        // `map`: mapping a cast over an iterator of fresh arrays is not
        // capture-polymorphic enough to typecheck.
        val bytes = input.s.getBytes("UTF-8").nn
        val pieces = scala.collection.mutable.ListBuffer[Data]()
        var index = 0

        if chunk == 0 then pieces += bytes.asInstanceOf[Data] else
          while index < bytes.length do
            val size = chunk.min(bytes.length - index)
            pieces += java.util.Arrays.copyOfRange(bytes, index, index + size).nn.asInstanceOf[Data]
            index += size

        pieces.iterator.stream.delineate.records.to(List)

      def check(policy: Text, cases: List[(Text, List[Text])])(using LineSeparation): Unit =
        for fragment <- List(-1, 0, 1, 3) do
          cases.stdlib.zipWithIndex.each: (row, index) =>
            test(m"$policy, case $index, chunk size $fragment"):
              splitLines(row(0), fragment)
            . assert(_ == row(1))

        // Differential: the char duct above is the reference implementation, and
        // the byte duct must agree with it on every policy, every case and every
        // fragmentation.
        for fragment <- List(0, 1, 3) do
          cases.stdlib.zipWithIndex.each: (row, index) =>
            test(m"$policy, case $index, byte chunk size $fragment"):
              splitBytes(row(0), fragment)
            . assert(_ == row(1))

      suite(m"adaptive linefeeds"):
        import lineSeparation.adaptiveLinefeedLineSeparation

        check(t"adaptive", List(
          (t"", List()),
          (t"a", List(t"a")),
          (t"a\nb", List(t"a", t"b")),
          (t"a\nb\n", List(t"a", t"b")),
          (t"a\rb", List(t"a", t"b")),
          (t"a\r\nb", List(t"a", t"b")),
          (t"a\n\rb", List(t"a", t"b")),
          (t"a\n\nb", List(t"a", t"", t"b")),
          (t"a\r", List(t"a")),
          (t"\n", List(t"")),
          (t"one two\nthree four\r\nfive", List(t"one two", t"three four", t"five"))))

      suite(m"linefeeds"):
        import lineSeparation.linefeedLineSeparation

        check(t"linefeed", List(
          (t"a\nb", List(t"a", t"b")),
          (t"a\rb", List(t"ab")),
          (t"a\r\nb", List(t"a", t"b")),
          (t"a\n\rb", List(t"a", t"b")),
          (t"a\r", List(t"a"))))

      suite(m"strict linefeeds"):
        import lineSeparation.strictLinefeedsLineSeparation

        // NOTE: the packaged policy's action table is (cr = Nl, lf = Lf, ...) —
        // identical to strictCarriageReturn's, which looks inverted for a
        // "linefeeds" policy, but the duct must match the table as it stands.
        check(t"strict linefeed", List(
          (t"a\nb", List(t"a\nb")),
          (t"a\rb", List(t"a", t"b")),
          (t"a\r\nb", List(t"a", t"\nb")),
          (t"a\n\rb", List(t"a\n", t"b"))))

      suite(m"carriage returns"):
        import lineSeparation.carriageReturnLineSeparation

        check(t"carriage return", List(
          (t"a\rb", List(t"a", t"b")),
          (t"a\nb", List(t"ab")),
          (t"a\r\nb", List(t"a", t"b")),
          (t"a\n\rb", List(t"a", t"b")),
          (t"a\r", List(t"a"))))

      suite(m"strict carriage returns"):
        import lineSeparation.strictCarriageReturnLineSeparation

        check(t"strict carriage return", List(
          (t"a\rb", List(t"a", t"b")),
          (t"a\nb", List(t"a\nb")),
          (t"a\r\nb", List(t"a", t"\nb")),
          (t"a\n\rb", List(t"a\n", t"b"))))

      suite(m"carriage return linefeeds"):
        import lineSeparation.carriageReturnLinefeedLineSeparation

        check(t"crlf", List(
          (t"a\r\nb", List(t"a", t"b")),
          (t"a\nb", List(t"a\nb")),
          (t"a\rb", List(t"ab")),
          (t"a\n\rb", List(t"a\n", t"b")),
          (t"a\r\n", List(t"a")),
          (t"a\r", List(t"a"))))

      suite(m"byte streams and long lines"):
        import lineSeparation.adaptiveLinefeedLineSeparation

        test(m"lines splits a byte stream through the character decoder"):
          t"first\nsecond\r\nthird".in[Data].stream.delineate.records.to(List)
        . assert(_ == List(t"first", t"second", t"third"))

        test(m"a line spanning many windows is reassembled"):
          val long = Text(String(scala.Array.fill(10000)('x')))
          val input = long + t"\ny"
          input.s.grouped(7).map(_.tt).stream.delineate.records.to(List)
        . assert(_ == List(Text(String(scala.Array.fill(10000)('x'))), t"y"))

        // Characters of two, three and four bytes, fragmented at every byte
        // boundary: each window boundary lands mid-character, so the byte duct
        // is forced to accumulate a partial character and decode it only once
        // the whole line is present. (The char-level harness cannot cover this:
        // fragmenting *characters* would split the surrogate pair of `🚀`.)
        val multiByte = t"café\n— dash\n数据\r\n🚀 rocket"
        val multiByteLines = List(t"café", t"— dash", t"数据", t"🚀 rocket")

        for chunk <- List(1, 2, 3, 5, 7) do
          test(m"multi-byte characters split across windows, byte chunk size $chunk"):
            splitBytes(multiByte, chunk)
          . assert(_ == multiByteLines)

        // Lines far longer than any window, of three-byte characters, fragmented
        // three bytes at a time. Compared as a boolean: the values themselves run
        // to twelve kilobytes, which the report cannot usefully render.
        for size <- List(10, 100, 700, 1000, 4000) do
          test(m"a long line of multi-byte characters is decoded whole, $size chars"):
            val long = Text(String(scala.Array.fill(size)('数')))
            splitBytes(long + t"\n" + long, 3) == List(long, long)
          . assert(_ == true)

        test(m"lines of an empty byte stream is empty"):
          Iterator.empty[Data].stream.delineate.records.to(List)
        . assert(_ == List())

    suite(m"Source and Sink tests"):
      val payload: Data = Data.fill(10000)(_.toByte)

      test(m"input stream source flows to output stream sink"):
        val input = ji.ByteArrayInputStream(Array.unsafeJvm(payload))
        val output = ji.ByteArrayOutputStream()
        val source = summon[ji.ByteArrayInputStream is Streamable by Data over Credit]
        val sink = summon[ji.ByteArrayOutputStream is Sink by Data over Credit]
        source.stream(input).pump(sink.intake(output))
        Array.unsafeFrozen(output.toByteArray.nn).to[List]
      . assert(_ == payload.readable.to(List))

      test(m"in-memory data source flows to output stream sink"):
        val output = ji.ByteArrayOutputStream()
        val sink = summon[ji.ByteArrayOutputStream is Sink by Data over Credit]
        summon[Data is Streamable by Data over Credit].stream(payload).pump(sink.intake(output))
        Array.unsafeFrozen(output.toByteArray.nn).to[List]
      . assert(_ == payload.readable.to(List))

      val original = t"The quick brown fox jumps over the lazy dog"*100

      test(m"reader source delivers text across refills"):
        val reader = ji.StringReader(original.s)
        val source = summon[ji.StringReader is Streamable by Text over Credit]
        val stream = source.stream(reader)
        val builder = StringBuilder()

        def recur(): Unit = scala.caps.unsafe.unsafeAssumeSeparate:
         stream.refill(Credit(64)) match
          case count: Int =>
            val window = unsafely(stream.storage).asInstanceOf[scala.Array[Char]]
            builder.append(String(window, stream.start, count))
            stream.skip(count)
            scala.caps.unsafe.unsafeAssumeSeparate(recur())

          case _ => ()

        scala.caps.unsafe.unsafeAssumeSeparate(recur())
        builder.toString.tt
      . assert(_ == original)

      test(m"memoize view drains a stream as one value"):
        val stream = summon[Data is Streamable by Data over Credit].stream(payload)
        stream.memoize.to[List]
      . assert(_ == payload.to[List])

      test(m"a Chain is a Source through its native instance"):
        val output = ji.ByteArrayOutputStream()
        val sink = summon[ji.ByteArrayOutputStream is Sink by Data over Credit]
        val source = summon[Chain[Data] is Streamable by Data over Credit]
        source.stream(Chain(payload, payload)).pump(sink.intake(output))
        output.toByteArray.nn.length
      . assert(_ == payload.readable.length*2)

      test(m"a sink write failure raises Truncation.Error"):
        import unsafeExceptions.canThrowAny

        val broken = new ji.OutputStream():
          override def write(byte: Int): Unit = throw ji.IOException("cut")
          override def write(array: scala.Array[Byte] | Null, off: Int, len: Int): Unit =
            throw ji.IOException("cut")

        val sink = summon[ji.OutputStream is Sink by Data over Credit]

        capture[Truncation.Error]:
          summon[Data is Streamable by Data over Credit].stream(payload).pump(sink.intake(broken))
      . assert(_ == Truncation.Error(0.b))

      test(m"cancelling a blocked conduit writer releases it"):
        supervise:
          Conduit[Data]() match
           case (intake, stream) =>
            val big = Data.fill(100000)(_.toByte)
            val writer = async(intake.put(big))
            writer.cancel()
            true
      . assert(identity)

      test(m"confluence merges all sources completely"):
        supervise:
          val sources = (1 to 4).map { index => Data.fill(1000)(_ => index.toByte) }
          // built in a while-loop: fresh endpoints cannot leave a `map` lambda
          val builder = scala.collection.immutable.List.newBuilder[AnyRef]
          var index = 0
          while index < sources.length do
            builder += summon[Data is Streamable by Data over Credit].stream(sources(index)).asInstanceOf[AnyRef]
            index += 1
          val endpoints = builder.result()

          val merged = Confluence(endpoints.map(_.asInstanceOf[Stream[Data] over Credit])*)
          val gather = Gather2()
          merged.pump(gather)
          scala.caps.unsafe.unsafeAssumeSeparate(gather.data).readable.to(List).sorted
      . assert(_ == (1 to 4).flatMap { index => List.fill(1000)(index.toByte) }.sorted.to(List))

      test(m"manifold delivers the whole stream to every subscriber"):
        supervise:
          val source = summon[Data is Streamable by Data over Credit].stream(payload)
          val subscribers = Divergence(source, 3)

          // Handles collected for concurrent await: sealed per the pure-façade convention
          // (D6; the `Seq[Task].sequence` shape).
          val results = subscribers.map: stream =>
            caps.unsafe.unsafeAssumePure:
              async:
                val gather = Gather2()
                scala.caps.unsafe.unsafeAssumeSeparate(stream.pump(gather))
                scala.caps.unsafe.unsafeAssumeSeparate(gather.data).to[List]

          results.map { task => task.await() }.to(List)
      . assert(_ == List.fill(3)(payload.to[List]))

      val mixed: Data =
        Array.frozen(Data.fill(50000) { index => (index%251).toByte }.readable ++ (t"repetition "*500).in[Data].readable)

      // A duct-chain source has a transient window (its buffer is reused between
      // refills), so the fan-out must snapshot each chunk rather than share it.
      test(m"manifold snapshots a transient source for every subscriber"):
        supervise:
          val source =
            summon[Data is Streamable by Data over Credit].stream(mixed)
            . compress[Gzip].decompress[Gzip]

          val subscribers = Divergence(source, 3)

          // Handles collected for concurrent await: sealed per the pure-façade convention
          // (D6; the `Seq[Task].sequence` shape).
          val results = subscribers.map: stream =>
            caps.unsafe.unsafeAssumePure:
              async:
                val gather = Gather2()
                scala.caps.unsafe.unsafeAssumeSeparate(stream.pump(gather))
                scala.caps.unsafe.unsafeAssumeSeparate(gather.data).to[List]

          results.map { task => task.await() }.to(List)
      . assert(_ == List.fill(3)(mixed.to[List]))

      test(m"confluence snapshots transient sources into the merge"):
        supervise:
          val builder = scala.collection.immutable.List.newBuilder[AnyRef]
          var index = 0
          while index < 3 do
            builder +=
              summon[Data is Streamable by Data over Credit].stream(mixed)
              . compress[Gzip].decompress[Gzip].asInstanceOf[AnyRef]
            index += 1

          val merged = Confluence(builder.result().map(_.asInstanceOf[Stream[Data] over Credit])*)
          val gather = Gather2()
          merged.pump(gather)
          scala.caps.unsafe.unsafeAssumeSeparate(gather.data).readable.length
      . assert(_ == mixed.readable.length*3)

      test(m"cancelling a detached flow blocked on an empty conduit releases it"):
        supervise:
          Conduit[Data]() match
           case (intake, stream) =>
            val gather = Gather2()
            val pump = stream.flow(gather)
            pump.cancel()
            true
      . assert(identity)

      // With no reader, the four pumps may pull at most the shared queue's capacity
      // plus one in-flight block each before parking, and no source may run ahead of
      // that bound: the payload is fifty times larger, so an ungated merge fails by
      // an order of magnitude. Draining afterwards proves the parked pumps recover.
      test(m"a slow reader parks every confluence pump uniformly"):
        supervise:
          given Buffering = probeBuffering(16, 2)
          val counters = sci.IndexedSeq.fill(4)(AtomicLong(0))
          val builder = scala.collection.immutable.List.newBuilder[AnyRef]
          var index = 0

          while index < 4 do
            builder += Meter(chunkStream(256), counters(index)).asInstanceOf[AnyRef]
            index += 1

          val merged = Confluence(builder.result().map(_.asInstanceOf[Stream[Data] over Credit])*)
          awaitStability(counters)

          // Queue capacity is `depth.max(sources.length)` transfer blocks: four, plus
          // one snapshotted block in flight per parked pump.
          val bounded = counters.map(_.get()).all(_ <= 80L)
          val gather = Gather2()
          merged.pump(gather)

          ( bounded,
            counters.map(_.get()).all(_ == 4096L),
            scala.caps.unsafe.unsafeAssumeSeparate(gather.data).readable.length )
      . assert(_ == ((true, true, 16384)))

      test(m"cancelling the confluence scope releases parked pumps"):
        supervise:
          val counters = sci.IndexedSeq.fill(4)(AtomicLong(0))

          val outer = async:
            given Buffering = probeBuffering(16, 2)
            val builder = scala.collection.immutable.List.newBuilder[AnyRef]
            var index = 0

            while index < 4 do
              builder += Meter(chunkStream(256), counters(index)).asInstanceOf[AnyRef]
              index += 1

            val merged =
              Confluence(builder.result().map(_.asInstanceOf[Stream[Data] over Credit])*)

            // Never read: the pumps fill the queue and park, and only cancellation
            // of this scope may release them. The sleep is interrupted by `cancel`.
            Thread.sleep(3600000)

          awaitStability(counters)
          outer.cancel()
          true
      . assert(identity)

      // One subscriber is never refilled, so its full ring parks the pump: the
      // metered source must stop within the ring-plus-in-flight bound even while the
      // other subscriber drains freely. Draining the stalled subscriber then releases
      // the pump and both must receive the complete payload.
      test(m"the slowest subscriber gates the divergence pump"):
        supervise:
          given Buffering = probeBuffering(16, 2)
          val counter = AtomicLong(0)
          val subscribers = Divergence(Meter(chunkStream(256), counter), 2)
          val eager = subscribers(0)
          val stalled = subscribers(1)
          val gatherA = Gather2()

          val taskA = caps.unsafe.unsafeAssumePure:
            async:
              scala.caps.unsafe.unsafeAssumeSeparate(eager.pump(gatherA))
              scala.caps.unsafe.unsafeAssumeSeparate(gatherA.data).readable.length

          awaitStability(sci.IndexedSeq(counter))
          val gated = counter.get()
          val gatherB = Gather2()
          scala.caps.unsafe.unsafeAssumeSeparate(stalled.pump(gatherB))

          ( gated <= 64L,
            taskA.await(),
            scala.caps.unsafe.unsafeAssumeSeparate(gatherB.data).readable.length,
            counter.get() )
      . assert(_ == ((true, 4096, 4096, 4096L)))

      // Closing a subscriber's stream closes its ring: the pump, parked on the
      // abandoned subscriber, is released, its later offers to that ring are
      // discarded, and the surviving subscriber still receives the full payload.
      test(m"closing a divergence subscriber releases the pump"):
        supervise:
          given Buffering = probeBuffering(16, 2)
          val counter = AtomicLong(0)
          val subscribers = Divergence(Meter(chunkStream(256), counter), 2)
          val eager = subscribers(0)
          val abandoned = subscribers(1)
          awaitStability(sci.IndexedSeq(counter))
          scala.caps.unsafe.unsafeAssumeSeparate(abandoned.close())
          val gather = Gather2()
          scala.caps.unsafe.unsafeAssumeSeparate(eager.pump(gather))

          ( scala.caps.unsafe.unsafeAssumeSeparate(gather.data).readable.length,
            counter.get() )
      . assert(_ == ((4096, 4096L)))

      test(m"sink.buffered stages by the buffering block size"):
        given Buffering = probeBuffering(7, 2)
        var sizes: sci.List[Int] = sci.Nil

        val intake = Sink.buffered[Unit, Data]
          ((), (_, chunks) => sizes = chunks.stdlib.to(sci.List).map(_.length))

        intake.put(Data.fill(8)(_.toByte))
        intake.finish()
        sizes
      . assert(_ == sci.List(7, 1))

      // `OutputStream.write` permits the caller to reuse its array afterwards, and the chunk
      // is read only when the stream is consumed, so aliasing the caller's array would let a
      // later write rewrite bytes already handed over.
      test(m"a written array is copied, so caller reuse cannot corrupt the stream"):
        val out = StreamOutputStream()
        val bytes = scala.Array[Byte](1, 2, 3)
        out.write(bytes)
        bytes(0) = 99
        out.close()
        out.stream.read[Data].to[List]
      . assert(_ == List(1.toByte, 2.toByte, 3.toByte))

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

// Passes refills through unchanged, counting the bytes the consumer skips past
// into an external counter, so backpressure tests can observe how far a pump has
// pulled a source without touching the live endpoint.
class Meter(consume underlying0: (Stream[Data] over Credit)^, counter: AtomicLong)
extends Stream[Data]:
  type Transport = Credit

  // The adopted stream is held through a neutral carrier: Stream is deliberately not
  // Unscoped, so an exclusive field would be read-only; the accessor re-asserts the
  // ownership this wrapper took at construction.
  private val held: AnyRef = underlying0.asInstanceOf[AnyRef]

  private def underlying: (Stream[Data] over Credit)^ =
    held.asInstanceOf[(Stream[Data] over Credit)^]

  update def refill(demand: Credit): Optional[Int] = underlying.refill(demand)

  protected def storage0: AnyRef =
    val current = underlying
    unsafely(current.storage).asInstanceOf[AnyRef]

  def start: Int = underlying.start
  def limit: Int = underlying.limit

  update def skip(count: Int): Unit =
    counter.addAndGet(count.toLong)
    underlying.skip(count)

  override update def close(): Unit = underlying.close()

// A tiny buffering policy for the backpressure tests: staging, transfer and
// hand-off blocks all collapse to `block` and recycling is off, so blocking
// states are reached with a few tens of bytes and the block arithmetic in
// assertions is exact.
def probeBuffering(block: Int, depth0: Int): Buffering = new Buffering:
  def capacity(substrate: Substrate): Int = block
  def depth: Int = depth0
  override def transfer(substrate: Substrate): Int = block
  override def recycle: Boolean = false

// A transient (non-region-stable) source of `chunks` sixteen-byte blocks: fan-in
// and fan-out snapshot such sources block-by-block, which is the bounded path
// the gating tests exercise.
def chunkStream(chunks: Int): (Stream[Data] over Credit)^ =
  Iterator.fill(chunks)(Data.fill(16)(_.toByte)).stream

// Poll (bounded) until two consecutive samples of every counter agree, i.e. the
// pumps have gone quiet. A too-early return can only under-read a counter, so a
// correct implementation can never fail its bound by sampling here.
def awaitStability(counters: sci.IndexedSeq[AtomicLong]): Unit =
  var previous: sci.IndexedSeq[Long] = counters.map(_.get())
  var stable: Boolean = false
  var attempts: Int = 0

  while !stable && attempts < 500 do
    Thread.sleep(10)
    val current = counters.map(_.get())
    if current == previous then stable = true else previous = current
    attempts += 1
