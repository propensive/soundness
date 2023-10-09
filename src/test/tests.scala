/*
    Turbulence, version [unreleased]. Copyright 2023 Jon Pretty, Propensive OÜ.

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

import probably.*
import gossamer.*
import rudiments.*
import spectacular.*
import anticipation.*
import hieroglyph.*, charEncoders.utf8, charDecoders.utf8, badEncodingHandlers.strict
import perforate.*, errorHandlers.throwUnsafely

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
          val stream = string.bytes.grouped(bs).to(LazyList)
          val result = stream.readAs[Text]
          result.bytes.length
        .assert(_ == string.bytes.length)

        test(t"roundtrip tests"):
          val stream = string.bytes.grouped(bs).to(LazyList)
          val result = stream.readAs[Text]

          result
        .assert(_ == string)

    val qbf = t"The quick brown fox\njumps over the lazy dog"
    val qbfBytes = qbf.bytes

    object Ref:
      given Readable[Ref, Text] = ref => LazyList(t"abc", t"def")
      given Readable[Ref, Bytes] = ref => LazyList(t"abc".bytes, t"def".bytes)

    case class Ref()

    suite(t"Reading tests"):
      test(t"Stream Text"):
        qbf.stream[Text].join
      .assert(_ == qbf)
      
      test(t"Stream Bytes"):
        qbf.stream[Bytes].reduce(_ ++ _).to(List)
      .assert(_ == qbfBytes.to(List))
      
      test(t"Read Text as Text"):
        qbf.readAs[Text]
      .assert(_ == qbf)
      
      test(t"Read some type with unambiguous Readable instance"):
        Ref().readAs[Text]
      .assert(_ == t"abcdef")
      
      test(t"Read Text as LazyList[Text]"):
        qbf.readAs[LazyList[Text]].join
      .assert(_ == qbf)
      
      test(t"Read Text as Bytes"):
        qbf.readAs[Bytes]
      .assert(_.to(List) == qbfBytes.to(List))
      
      test(t"Read Text as LazyList[Bytes]"):
        qbf.readAs[LazyList[Bytes]]
      .assert(_.reduce(_ ++ _).to(List) == qbfBytes.to(List))
      
      test(t"Read Bytes as Text"):
        qbfBytes.readAs[Text]
      .assert(_ == qbf)
      
      test(t"Read Bytes as LazyList[Text]"):
        qbfBytes.readAs[LazyList[Text]].join
      .assert(_ == qbf)
      
      test(t"Read Bytes as Bytes"):
        qbfBytes.readAs[Bytes]
      .assert(_.to(List) == qbfBytes.to(List))
      
      test(t"Read Bytes as LazyList[Bytes]"):
        qbfBytes.readAs[LazyList[Bytes]]
      .assert(_.reduce(_ ++ _).to(List) == qbfBytes.to(List))
      
      // test(t"Read Text as Lines"):
      //   qbf.readAs[LazyList[Line]]
      // .assert(_ == LazyList(Line(t"The quick brown fox"), Line(t"jumps over the lazy dog")))
      
      // test(t"Read Bytes as Lines"):
      //   qbfBytes.readAs[LazyList[Line]]
      // .assert(_ == LazyList(Line(t"The quick brown fox"), Line(t"jumps over the lazy dog")))