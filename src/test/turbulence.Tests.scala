/*
    Turbulence, version 0.26.0. Copyright 2025 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package turbulence

import anticipation.*
import contingency.*, strategies.throwUnsafely
import gossamer.*
import hieroglyph.*, charEncoders.utf8, charDecoders.utf8, badEncodingHandlers.strict
import parasite.*
import probably.*
import proscenium.*
import rudiments.*
import spectacular.*

import scala.collection.mutable as scm

object Tests extends Suite(t"Turbulence tests"):
  def run(): Unit =
    suite(t"Streaming Unicode tests"):
      val ascii = IArray(t"", t"a", t"ab", t"abc", t"abcd")

      val strings = for
        asc0 <- Array(t"", t"a", t"ab", t"abc") // 4 combinations
        cp2  <- Array(t"", t"£")                // 8
        asc1 <- Array(t"", t"a", t"ab", t"abc") // 32
        cp3  <- Array(t"", t"€")                // 64
        asc2 <- Array(t"", t"a", t"ab", t"abc") // 256
        cp4  <- Array(t"")//, t"𐍈")                // 512
        asc3 <- Array(t"", t"a", t"ab", t"abc") // 2048
      yield asc0+cp2+asc1+cp3+asc2+cp4

      for
        string <- strings
        bs     <- 1 to 8
      do
        test(t"length tests"):
          val stream = string.bytes.grouped(bs).to(Stream)
          val result = stream.read[Text]
          result.bytes.length
        .assert(_ == string.bytes.length)

        test(t"roundtrip tests"):
          val stream = string.bytes.grouped(bs).to(Stream)
          val result = stream.read[Text]

          result
        .assert(_ == string)

    val qbf = t"The quick brown fox\njumps over the lazy dog"
    val qbfBytes = qbf.bytes

    object Ref:
      given Readable[Ref, Text] = ref => Stream(t"abc", t"def")
      given Readable[Ref, Bytes] = ref => Stream(t"abc".bytes, t"def".bytes)

    case class Ref()

    object Ref2:
      given Readable[Ref2, Text] = ref => Stream(t"abc", t"def")

    case class Ref2()

    object Ref3:
      given Readable[Ref3, Bytes] = ref => Stream(t"abc".bytes, t"def".bytes)

    case class Ref3()

    suite(t"Reading tests"):
      test(t"Stream Text"):
        qbf.stream[Text].join
      .assert(_ == qbf)

      test(t"Stream Bytes"):
        qbf.stream[Bytes].reduce(_ ++ _).to(List)
      .assert(_ == qbfBytes.to(List))

      test(t"Read Text as Text"):
        qbf.read[Text]
      .assert(_ == qbf)

      test(t"Read some type as Text with Text and Byte Readable instance"):
        Ref().read[Text]
      .assert(_ == t"abcdef")

      test(t"Read some type as Bytes with Text and Byte Readable instance"):
        Ref().read[Bytes].to(List)
      .assert(_ == t"abcdef".bytes.to(List))

      test(t"Read some type as Text with only Text Readable instance"):
        Ref2().read[Text]
      .assert(_ == t"abcdef")

      test(t"Read some type as Bytes with only Text Readable instance"):
        Ref2().read[Bytes].to(List)
      .assert(_ == t"abcdef".bytes.to(List))

      test(t"Read some type as Text with only Bytes Readable instance"):
        Ref3().read[Text]
      .assert(_ == t"abcdef")

      test(t"Read some type as Bytes with only Bytes Readable instance"):
        Ref3().read[Bytes].to(List)
      .assert(_ == t"abcdef".bytes.to(List))

      test(t"Read Text as Stream[Text]"):
        qbf.read[Stream[Text]].join
      .assert(_ == qbf)

      test(t"Read Text as Bytes"):
        qbf.read[Bytes]
      .assert(_.to(List) == qbfBytes.to(List))

      test(t"Read Text as Stream[Bytes]"):
        qbf.read[Stream[Bytes]]
      .assert(_.reduce(_ ++ _).to(List) == qbfBytes.to(List))

      test(t"Read Bytes as Text"):
        qbfBytes.read[Text]
      .assert(_ == qbf)

      test(t"Read Bytes as Stream[Text]"):
        qbfBytes.read[Stream[Text]].join
      .assert(_ == qbf)

      test(t"Read Bytes as Bytes"):
        qbfBytes.read[Bytes]
      .assert(_.to(List) == qbfBytes.to(List))

      test(t"Read Bytes as Stream[Bytes]"):
        qbfBytes.read[Stream[Bytes]]
      .assert(_.reduce(_ ++ _).to(List) == qbfBytes.to(List))

      // test(t"Read Text as Lines"):
      //   qbf.read[Stream[Line]]
      // .assert(_ == Stream(Line(t"The quick brown fox"), Line(t"jumps over the lazy dog")))

      // test(t"Read Bytes as Lines"):
      //   qbfBytes.read[Stream[Line]]
      // .assert(_ == Stream(Line(t"The quick brown fox"), Line(t"jumps over the lazy dog")))

    suite(t"Writing tests"):

      class GeneralStore():
        val arrayBuffer: scm.ArrayBuffer[Byte] = scm.ArrayBuffer()
        def apply(): Text = String(arrayBuffer.toArray, "UTF-8").tt

      object GeneralStore:
        given Writable[GeneralStore, Bytes] = (store, stream) => stream.each: bytes =>
          bytes.each: byte =>
            store.arrayBuffer.append(byte)

        given Writable[GeneralStore, Text] = (store, texts) => texts.each: text =>
          text.bytes.each: byte =>
            store.arrayBuffer.append(byte)

      class ByteStore():
        val arrayBuffer: scm.ArrayBuffer[Byte] = scm.ArrayBuffer()
        def apply(): Text = String(arrayBuffer.toArray, "UTF-8").tt

      object ByteStore:
        given Writable[ByteStore, Bytes] = (store, stream) => stream.each: bytes =>
          bytes.each: byte =>
            store.arrayBuffer.append(byte)

      class TextStore():
        var text: Text = t""
        def apply(): Text = text

      object TextStore:
        given Writable[TextStore, Text] = (store, texts) => texts.each: text =>
          store.text = store.text + text

      test(t"Write Text to some reference with Text and Bytes instances"):
        val store = GeneralStore()
        qbf.writeTo(store)
        store()
      .assert(_ == qbf)

      test(t"Write Bytes to some reference with Text and Bytes instances"):
        val store = GeneralStore()
        qbfBytes.writeTo(store)
        store()
      .assert(_ == qbf)

      test(t"Write Stream[Text] to some reference with Text and Bytes instances"):
        val store = GeneralStore()
        Stream(qbf).writeTo(store)
        store()
      .assert(_ == qbf)

      test(t"Write Stream[Bytes] to some reference with Text and Bytes instances"):
        val store = GeneralStore()
        Stream(qbfBytes).writeTo(store)
        store()
      .assert(_ == qbf)

      test(t"Write Text to some reference with only a Bytes instance"):
        val store = ByteStore()
        qbf.writeTo(store)
        store()
      .assert(_ == qbf)

      test(t"Write Bytes to some reference with only a Bytes instance"):
        val store = ByteStore()
        qbfBytes.writeTo(store)
        store()
      .assert(_ == qbf)

      test(t"Write Stream[Text] to some reference with only a Bytes instance"):
        val store = ByteStore()
        Stream(qbf).writeTo(store)
        store()
      .assert(_ == qbf)

      test(t"Write Stream[Bytes] to some reference with only a Bytes instance"):
        val store = ByteStore()
        Stream(qbfBytes).writeTo(store)
        store()
      .assert(_ == qbf)

      test(t"Write Text to some reference with only a Text instance"):
        val store = TextStore()
        qbf.writeTo(store)
        store()
      .assert(_ == qbf)

      test(t"Write Bytes to some reference with only a Text instance"):
        val store = TextStore()
        qbfBytes.writeTo(store)
        store()
      .assert(_ == qbf)

      test(t"Write Stream[Text] to some reference with only a Text instance"):
        val store = TextStore()
        Stream(qbf).writeTo(store)
        store()
      .assert(_ == qbf)

      test(t"Write Stream[Bytes] to some reference with only a Text instance"):
        val store = TextStore()
        Stream(qbfBytes).writeTo(store)
        store()
      .assert(_ == qbf)

    suite(t"Appending tests"):

      class GeneralStore():
        val arrayBuffer: scm.ArrayBuffer[Byte] = scm.ArrayBuffer()
        def apply(): Text = String(arrayBuffer.toArray, "UTF-8").tt

      object GeneralStore:
        given Appendable[GeneralStore, Bytes] = (store, stream) => stream.each: bytes =>
          bytes.each: byte =>
            store.arrayBuffer.append(byte)

        given Appendable[GeneralStore, Text] = (store, texts) => texts.each: text =>
          text.bytes.each: byte =>
            store.arrayBuffer.append(byte)

      class ByteStore():
        val arrayBuffer: scm.ArrayBuffer[Byte] = scm.ArrayBuffer()
        def apply(): Text = String(arrayBuffer.toArray, "UTF-8").tt

      object ByteStore:
        given Appendable[ByteStore, Bytes] = (store, stream) => stream.each: bytes =>
          bytes.each: byte =>
            store.arrayBuffer.append(byte)

      class TextStore():
        var text: Text = t""
        def apply(): Text = text

      object TextStore:
        given Appendable[TextStore, Text] = (store, texts) => texts.each: text =>
          store.text = store.text + text

      // test(t"Append Text to some reference with Text and Bytes instances"):
      //   val store = GeneralStore()
      //   qbf.appendTo(store)
      //   store()
      // .assert(_ == qbf)

      // test(t"Append Bytes to some reference with Text and Bytes instances"):
      //   val store = GeneralStore()
      //   qbfBytes.appendTo(store)
      //   store()
      // .assert(_ == qbf)

      // test(t"Append Stream[Text] to some reference with Text and Bytes instances"):
      //   val store = GeneralStore()
      //   Stream(qbf).appendTo(store)
      //   store()
      // .assert(_ == qbf)

      // test(t"Append Stream[Bytes] to some reference with Text and Bytes instances"):
      //   val store = GeneralStore()
      //   Stream(qbfBytes).appendTo(store)
      //   store()
      // .assert(_ == qbf)

      // test(t"Append Text to some reference with only a Bytes instance"):
      //   val store = ByteStore()
      //   qbf.appendTo(store)
      //   store()
      // .assert(_ == qbf)

      // test(t"Append Bytes to some reference with only a Bytes instance"):
      //   val store = ByteStore()
      //   qbfBytes.appendTo(store)
      //   store()
      // .assert(_ == qbf)

      // test(t"Append Stream[Text] to some reference with only a Bytes instance"):
      //   val store = ByteStore()
      //   Stream(qbf).appendTo(store)
      //   store()
      // .assert(_ == qbf)

      // test(t"Append Stream[Bytes] to some reference with only a Bytes instance"):
      //   val store = ByteStore()
      //   Stream(qbfBytes).appendTo(store)
      //   store()
      // .assert(_ == qbf)

      // test(t"Append Text to some reference with only a Text instance"):
      //   val store = TextStore()
      //   qbf.appendTo(store)
      //   store()
      // .assert(_ == qbf)

      // test(t"Append Bytes to some reference with only a Text instance"):
      //   val store = TextStore()
      //   qbfBytes.appendTo(store)
      //   store()
      // .assert(_ == qbf)

      // test(t"Append Stream[Text] to some reference with only a Text instance"):
      //   val store = TextStore()
      //   Stream(qbf).appendTo(store)
      //   store()
      // .assert(_ == qbf)

      // test(t"Append Stream[Bytes] to some reference with only a Text instance"):
      //   val store = TextStore()
      //   Stream(qbfBytes).appendTo(store)
      //   store()
      // .assert(_ == qbf)

    suite(t"Multiplexer tests"):
      val l1 = Stream(2, 4, 6, 8, 10)
      val l2 = Stream(1, 3, 5, 7, 9)

      test(t"Check that two multiplexed streams contain all elements"):
        supervise(l1.multiplexWith(l2).to(Set))
      .assert(_ == Set.range(1, 11))

      test(t"Check that two multiplexed streams contain elements from the first stream in order"):
        supervise(l1.multiplexWith(l2).filter(_%2 == 0))
      .assert(_ == l1)

      test(t"Check that two multiplexed streams contain elements from the second stream in order"):
        supervise(l1.multiplexWith(l2).filter(_%2 == 1))
      .assert(_ == l2)
