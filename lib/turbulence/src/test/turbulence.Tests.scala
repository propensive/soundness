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
┃    Soundness, version 0.53.0.                                                                    ┃
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
import threading.platform
import strategies.throwUnsafely

import scala.collection.mutable as scm

object Tests extends Suite(m"Turbulence tests"):
  def run(): Unit =

    suite(m"Shredding"):
      given Seed = Seed(1L)
      import randomization.seeded
      val data: Data = Data.fill(1000)(_.toByte)
      val stream: Stream[Data] = Stream(data)
      val shredded: Iterable[Stream[Data]] = stochastic:
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
        cp4  <- List(t"")//, t"𐍈")                // 512
        asc3 <- List(t"", t"a", t"ab", t"abc") // 2048
      yield asc0+cp2+asc1+cp3+asc2+cp4

      for
        string <- strings
        bs     <- 1 to 8
      do
        test(m"length tests"):
          val stream = string.data.grouped(bs).to(Stream)
          val result = stream.read[Text]
          result.data.length
        .assert(_ == string.data.length)

        test(m"roundtrip tests"):
          val stream = string.data.grouped(bs).to(Stream)
          val result = stream.read[Text]

          result
        .assert(_ == string)

    val qbf = t"The quick brown fox\njumps over the lazy dog"
    val qbfData = qbf.data

    object Ref:
      given Ref is Streamable by Text = ref => Stream(t"abc", t"def")
      given Ref is Streamable by Data = ref => Stream(t"abc".data, t"def".data)

    case class Ref()

    object Ref2:
      given Ref2 is Streamable by Text = ref => Stream(t"abc", t"def")

    case class Ref2()

    object Ref3:
      given Ref3 is Streamable by Data = ref => Stream(t"abc".data, t"def".data)

    case class Ref3()

    suite(m"Reading tests"):
      test(m"Stream Text"):
        qbf.stream[Text].join
      .assert(_ == qbf)

      test(m"Stream Data"):
        qbf.stream[Data].reduce(_ ++ _).to(List)
      .assert(_ == qbfData.to(List))

      test(m"Read Text as Text"):
        qbf.read[Text]
      .assert(_ == qbf)

      test(m"Read some type as Text with Text and Byte Streamable instance"):
        Ref().read[Text]
      .assert(_ == t"abcdef")

      test(m"Read some type as Data with Text and Byte Streamable instance"):
        Ref().read[Data].to(List)
      .assert(_ == t"abcdef".data.to(List))

      test(m"Read some type as Text with only Text Streamable instance"):
        Ref2().read[Text]
      .assert(_ == t"abcdef")

      test(m"Read some type as Data with only Text Streamable instance"):
        Ref2().read[Data].to(List)
      .assert(_ == t"abcdef".data.to(List))

      test(m"Read some type as Text with only Data Streamable instance"):
        Ref3().read[Text]
      .assert(_ == t"abcdef")

      test(m"Read some type as Data with only Data Streamable instance"):
        Ref3().read[Data].to(List)
      .assert(_ == t"abcdef".data.to(List))

      test(m"Read Text as Stream[Text]"):
        qbf.read[Stream[Text]].join
      .assert(_ == qbf)

      test(m"Read Text as Data"):
        qbf.read[Data]
      .assert(_.to(List) == qbfData.to(List))

      test(m"Read Text as Stream[Data]"):
        qbf.read[Stream[Data]]
      .assert(_.reduce(_ ++ _).to(List) == qbfData.to(List))

      test(m"Read Data as Text"):
        qbfData.read[Text]
      .assert(_ == qbf)

      test(m"Read Data as Stream[Text]"):
        qbfData.read[Stream[Text]].join
      .assert(_ == qbf)

      test(m"Read Data as Data"):
        qbfData.read[Data]
      .assert(_.to(List) == qbfData.to(List))

      test(m"Read Data as Stream[Data]"):
        qbfData.read[Stream[Data]]
      .assert(_.reduce(_ ++ _).to(List) == qbfData.to(List))

      // test(m"Read Text as Lines"):
      //   qbf.read[Stream[Line]]
      // .assert(_ == Stream(Line(t"The quick brown fox"), Line(t"jumps over the lazy dog")))

      // test(m"Read Data as Lines"):
      //   qbfData.read[Stream[Line]]
      // .assert(_ == Stream(Line(t"The quick brown fox"), Line(t"jumps over the lazy dog")))

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
        store()
      .assert(_ == qbf)

      test(m"Write Data to some reference with Text and Data instances"):
        val store = GeneralStore()
        qbfData.writeTo(store)
        store()
      .assert(_ == qbf)

      test(m"Write Stream[Text] to some reference with Text and Data instances"):
        val store = GeneralStore()
        Stream(qbf).writeTo(store)
        store()
      .assert(_ == qbf)

      test(m"Write Stream[Data] to some reference with Text and Data instances"):
        val store = GeneralStore()
        Stream(qbfData).writeTo(store)
        store()
      .assert(_ == qbf)

      test(m"Write Text to some reference with only a Data instance"):
        val store = ByteStore()
        qbf.writeTo(store)
        store()
      .assert(_ == qbf)

      test(m"Write Data to some reference with only a Data instance"):
        val store = ByteStore()
        qbfData.writeTo(store)
        store()
      .assert(_ == qbf)

      test(m"Write Stream[Text] to some reference with only a Data instance"):
        val store = ByteStore()
        Stream(qbf).writeTo(store)
        store()
      .assert(_ == qbf)

      test(m"Write Stream[Data] to some reference with only a Data instance"):
        val store = ByteStore()
        Stream(qbfData).writeTo(store)
        store()
      .assert(_ == qbf)

      test(m"Write Text to some reference with only a Text instance"):
        val store = TextStore()
        qbf.writeTo(store)
        store()
      .assert(_ == qbf)

      test(m"Write Data to some reference with only a Text instance"):
        val store = TextStore()
        qbfData.writeTo(store)
        store()
      .assert(_ == qbf)

      test(m"Write Stream[Text] to some reference with only a Text instance"):
        val store = TextStore()
        Stream(qbf).writeTo(store)
        store()
      .assert(_ == qbf)

      test(m"Write Stream[Data] to some reference with only a Text instance"):
        val store = TextStore()
        Stream(qbfData).writeTo(store)
        store()
      .assert(_ == qbf)

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

      // test(m"Append Stream[Text] to some reference with Text and Data instances"):
      //   val store = GeneralStore()
      //   Stream(qbf).appendTo(store)
      //   store()
      // .assert(_ == qbf)

      // test(m"Append Stream[Data] to some reference with Text and Data instances"):
      //   val store = GeneralStore()
      //   Stream(qbfData).appendTo(store)
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

      // test(m"Append Stream[Text] to some reference with only a Data instance"):
      //   val store = ByteStore()
      //   Stream(qbf).appendTo(store)
      //   store()
      // .assert(_ == qbf)

      // test(m"Append Stream[Data] to some reference with only a Data instance"):
      //   val store = ByteStore()
      //   Stream(qbfData).appendTo(store)
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

      // test(m"Append Stream[Text] to some reference with only a Text instance"):
      //   val store = TextStore()
      //   Stream(qbf).appendTo(store)
      //   store()
      // .assert(_ == qbf)

      // test(m"Append Stream[Data] to some reference with only a Text instance"):
      //   val store = TextStore()
      //   Stream(qbfData).appendTo(store)
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
