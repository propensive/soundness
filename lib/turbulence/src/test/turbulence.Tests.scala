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
┃    Soundness, version 0.34.0.                                                                    ┃
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

import soundness.*

import charEncoders.utf8, charDecoders.utf8, textSanitizers.strict
import threadModels.platform
import strategies.throwUnsafely

import scala.collection.mutable as scm

object Tests extends Suite(m"Turbulence tests"):
  def run(): Unit =

    suite(m"Shredding"):
      given Seed = Seed(1L)
      import randomization.seeded
      val bytes: Bytes = Bytes.fill(1000)(_.toByte)
      val stream: Stream[Bytes] = Stream(bytes)
      val shredded: Iterable[Stream[Bytes]] = stochastic:
        (0 until 100).map: index =>
          stream.shred(20.0, 10.0)

      shredded.each: stream =>
        test(m"correct length after shredding"):
          stream.map(_.length).total
        . assert(_ == 1000)

        test(m"correct content after shredding"):
          stream.reduce(_ ++ _)
        . assert(_ === bytes)

    suite(m"Streaming Unicode tests"):
      val ascii = IArray(t"", t"a", t"ab", t"abc", t"abcd")

      val strings = for
        asc0 <- List(t"", t"a", t"ab", t"abc") // 4 combinations
        cp2  <- List(t"", t"£")                // 8
        asc1 <- List(t"", t"a", t"ab", t"abc") // 32
        cp3  <- List(t"", t"€")                // 64
        asc2 <- List(t"", t"a", t"ab", t"abc") // 256
        cp4  <- List(t"")//, t"𐍈")                // 512
        asc3 <- List(t"", t"a", t"ab", t"abc") // 2048
      yield asc0+cp2+asc1+cp3+asc2+cp4

      for
        string <- strings
        bs     <- 1 to 8
      do
        test(m"length tests"):
          val stream = string.bytes.grouped(bs).to(Stream)
          val result = stream.read[Text]
          result.bytes.length
        .assert(_ == string.bytes.length)

        test(m"roundtrip tests"):
          val stream = string.bytes.grouped(bs).to(Stream)
          val result = stream.read[Text]

          result
        .assert(_ == string)

    val qbf = t"The quick brown fox\njumps over the lazy dog"
    val qbfBytes = qbf.bytes

    object Ref:
      given Ref is Readable by Text = ref => Stream(t"abc", t"def")
      given Ref is Readable by Bytes = ref => Stream(t"abc".bytes, t"def".bytes)

    case class Ref()

    object Ref2:
      given Ref2 is Readable by Text = ref => Stream(t"abc", t"def")

    case class Ref2()

    object Ref3:
      given Ref3 is Readable by Bytes = ref => Stream(t"abc".bytes, t"def".bytes)

    case class Ref3()

    suite(m"Reading tests"):
      test(m"Stream Text"):
        qbf.stream[Text].join
      .assert(_ == qbf)

      test(m"Stream Bytes"):
        qbf.stream[Bytes].reduce(_ ++ _).to(List)
      .assert(_ == qbfBytes.to(List))

      test(m"Read Text as Text"):
        qbf.read[Text]
      .assert(_ == qbf)

      test(m"Read some type as Text with Text and Byte Readable instance"):
        Ref().read[Text]
      .assert(_ == t"abcdef")

      test(m"Read some type as Bytes with Text and Byte Readable instance"):
        Ref().read[Bytes].to(List)
      .assert(_ == t"abcdef".bytes.to(List))

      test(m"Read some type as Text with only Text Readable instance"):
        Ref2().read[Text]
      .assert(_ == t"abcdef")

      test(m"Read some type as Bytes with only Text Readable instance"):
        Ref2().read[Bytes].to(List)
      .assert(_ == t"abcdef".bytes.to(List))

      test(m"Read some type as Text with only Bytes Readable instance"):
        Ref3().read[Text]
      .assert(_ == t"abcdef")

      test(m"Read some type as Bytes with only Bytes Readable instance"):
        Ref3().read[Bytes].to(List)
      .assert(_ == t"abcdef".bytes.to(List))

      test(m"Read Text as Stream[Text]"):
        qbf.read[Stream[Text]].join
      .assert(_ == qbf)

      test(m"Read Text as Bytes"):
        qbf.read[Bytes]
      .assert(_.to(List) == qbfBytes.to(List))

      test(m"Read Text as Stream[Bytes]"):
        qbf.read[Stream[Bytes]]
      .assert(_.reduce(_ ++ _).to(List) == qbfBytes.to(List))

      test(m"Read Bytes as Text"):
        qbfBytes.read[Text]
      .assert(_ == qbf)

      test(m"Read Bytes as Stream[Text]"):
        qbfBytes.read[Stream[Text]].join
      .assert(_ == qbf)

      test(m"Read Bytes as Bytes"):
        qbfBytes.read[Bytes]
      .assert(_.to(List) == qbfBytes.to(List))

      test(m"Read Bytes as Stream[Bytes]"):
        qbfBytes.read[Stream[Bytes]]
      .assert(_.reduce(_ ++ _).to(List) == qbfBytes.to(List))

      // test(m"Read Text as Lines"):
      //   qbf.read[Stream[Line]]
      // .assert(_ == Stream(Line(t"The quick brown fox"), Line(t"jumps over the lazy dog")))

      // test(m"Read Bytes as Lines"):
      //   qbfBytes.read[Stream[Line]]
      // .assert(_ == Stream(Line(t"The quick brown fox"), Line(t"jumps over the lazy dog")))

    suite(m"Writing tests"):

      class GeneralStore():
        val arrayBuffer: scm.ArrayBuffer[Byte] = scm.ArrayBuffer()
        def apply(): Text = String(arrayBuffer.toArray, "UTF-8").tt

      object GeneralStore:
        given GeneralStore is Writable by Bytes = (store, stream) => stream.each: bytes =>
          bytes.each: byte =>
            store.arrayBuffer.append(byte)

        given GeneralStore is Writable by Text = (store, texts) => texts.each: text =>
          text.bytes.each: byte =>
            store.arrayBuffer.append(byte)

      class ByteStore():
        val arrayBuffer: scm.ArrayBuffer[Byte] = scm.ArrayBuffer()
        def apply(): Text = String(arrayBuffer.toArray, "UTF-8").tt

      object ByteStore:
        given ByteStore is Writable by Bytes = (store, stream) => stream.each: bytes =>
          bytes.each: byte =>
            store.arrayBuffer.append(byte)

      class TextStore():
        var text: Text = t""
        def apply(): Text = text

      object TextStore:
        given TextStore is Writable by Text = (store, texts) => texts.each: text =>
          store.text = store.text + text

      test(m"Write Text to some reference with Text and Bytes instances"):
        val store = GeneralStore()
        qbf.writeTo(store)
        store()
      .assert(_ == qbf)

      test(m"Write Bytes to some reference with Text and Bytes instances"):
        val store = GeneralStore()
        qbfBytes.writeTo(store)
        store()
      .assert(_ == qbf)

      test(m"Write Stream[Text] to some reference with Text and Bytes instances"):
        val store = GeneralStore()
        Stream(qbf).writeTo(store)
        store()
      .assert(_ == qbf)

      test(m"Write Stream[Bytes] to some reference with Text and Bytes instances"):
        val store = GeneralStore()
        Stream(qbfBytes).writeTo(store)
        store()
      .assert(_ == qbf)

      test(m"Write Text to some reference with only a Bytes instance"):
        val store = ByteStore()
        qbf.writeTo(store)
        store()
      .assert(_ == qbf)

      test(m"Write Bytes to some reference with only a Bytes instance"):
        val store = ByteStore()
        qbfBytes.writeTo(store)
        store()
      .assert(_ == qbf)

      test(m"Write Stream[Text] to some reference with only a Bytes instance"):
        val store = ByteStore()
        Stream(qbf).writeTo(store)
        store()
      .assert(_ == qbf)

      test(m"Write Stream[Bytes] to some reference with only a Bytes instance"):
        val store = ByteStore()
        Stream(qbfBytes).writeTo(store)
        store()
      .assert(_ == qbf)

      test(m"Write Text to some reference with only a Text instance"):
        val store = TextStore()
        qbf.writeTo(store)
        store()
      .assert(_ == qbf)

      test(m"Write Bytes to some reference with only a Text instance"):
        val store = TextStore()
        qbfBytes.writeTo(store)
        store()
      .assert(_ == qbf)

      test(m"Write Stream[Text] to some reference with only a Text instance"):
        val store = TextStore()
        Stream(qbf).writeTo(store)
        store()
      .assert(_ == qbf)

      test(m"Write Stream[Bytes] to some reference with only a Text instance"):
        val store = TextStore()
        Stream(qbfBytes).writeTo(store)
        store()
      .assert(_ == qbf)

    // suite(m"Appending tests"):

    //   class GeneralStore():
    //     val arrayBuffer: scm.ArrayBuffer[Byte] = scm.ArrayBuffer()
    //     def apply(): Text = String(arrayBuffer.toArray, "UTF-8").tt

    //   object GeneralStore:
    //     given GeneralStore is Writable by Bytes = (store, stream) => stream.each: bytes =>
    //       bytes.each: byte =>
    //         store.arrayBuffer.append(byte)

    //     given GeneralStore is Writable by Text = (store, texts) => texts.each: text =>
    //       text.bytes.each: byte =>
    //         store.arrayBuffer.append(byte)

    //   class ByteStore():
    //     val arrayBuffer: scm.ArrayBuffer[Byte] = scm.ArrayBuffer()
    //     def apply(): Text = String(arrayBuffer.toArray, "UTF-8").tt

    //   object ByteStore:
    //     given ByteStore is Writable by Bytes = (store, stream) => stream.each: bytes =>
    //       bytes.each: byte =>
    //         Eof(store.arrayBuffer).write(byte)

    //   class TextStore():
    //     var text: Text = t""
    //     def apply(): Text = text

    //   object TextStore:
    //     given TextStore is Writable by Text = (store, texts) => texts.each: text =>
    //       store.text = store.text + text

      // test(m"Append Text to some reference with Text and Bytes instances"):
      //   val store = GeneralStore()
      //   qbf.appendTo(store)
      //   store()
      // .assert(_ == qbf)

      // test(m"Append Bytes to some reference with Text and Bytes instances"):
      //   val store = GeneralStore()
      //   qbfBytes.appendTo(store)
      //   store()
      // .assert(_ == qbf)

      // test(m"Append Stream[Text] to some reference with Text and Bytes instances"):
      //   val store = GeneralStore()
      //   Stream(qbf).appendTo(store)
      //   store()
      // .assert(_ == qbf)

      // test(m"Append Stream[Bytes] to some reference with Text and Bytes instances"):
      //   val store = GeneralStore()
      //   Stream(qbfBytes).appendTo(store)
      //   store()
      // .assert(_ == qbf)

      // test(m"Append Text to some reference with only a Bytes instance"):
      //   val store = ByteStore()
      //   qbf.appendTo(store)
      //   store()
      // .assert(_ == qbf)

      // test(m"Append Bytes to some reference with only a Bytes instance"):
      //   val store = ByteStore()
      //   qbfBytes.appendTo(store)
      //   store()
      // .assert(_ == qbf)

      // test(m"Append Stream[Text] to some reference with only a Bytes instance"):
      //   val store = ByteStore()
      //   Stream(qbf).appendTo(store)
      //   store()
      // .assert(_ == qbf)

      // test(m"Append Stream[Bytes] to some reference with only a Bytes instance"):
      //   val store = ByteStore()
      //   Stream(qbfBytes).appendTo(store)
      //   store()
      // .assert(_ == qbf)

      // test(m"Append Text to some reference with only a Text instance"):
      //   val store = TextStore()
      //   qbf.appendTo(store)
      //   store()
      // .assert(_ == qbf)

      // test(m"Append Bytes to some reference with only a Text instance"):
      //   val store = TextStore()
      //   qbfBytes.appendTo(store)
      //   store()
      // .assert(_ == qbf)

      // test(m"Append Stream[Text] to some reference with only a Text instance"):
      //   val store = TextStore()
      //   Stream(qbf).appendTo(store)
      //   store()
      // .assert(_ == qbf)

      // test(m"Append Stream[Bytes] to some reference with only a Text instance"):
      //   val store = TextStore()
      //   Stream(qbfBytes).appendTo(store)
      //   store()
      // .assert(_ == qbf)

    suite(m"Multiplexer tests"):
      val l1 = Stream(2, 4, 6, 8, 10)
      val l2 = Stream(1, 3, 5, 7, 9)

      test(m"Check that two multiplexed streams contain all elements"):
        supervise(l1.multiplex(l2).to(Set))
      .assert(_ == Set.range(1, 11))

      test(m"Check that two multiplexed streams contain elements from the first stream in order"):
        supervise(l1.multiplex(l2).filter(_%2 == 0))
      .assert(_ == l1)

      for i <- 1 to 10
      do test(m"Check that two multiplexed streams contain elements from the second stream in order"):
        supervise(l1.multiplex(l2).filter(_%2 == 1))
      .assert(_ == l2)
