/*
    Hieroglyph, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package hieroglyph

import probably.*
import rudiments.*
import vacuous.*
import perforate.*, errorHandlers.throwUnsafely
import larceny.*
// FIXME: resolution of overloaded `displayWidth` does not work
import gossamer.{displayWidth as _, *}

import textMetrics.eastAsianScripts

object Tests extends Suite(t"Hieroglyph tests"):
  def run(): Unit =
    val japanese = t"平ぱ記動テ使村方島おゃぎむ万離ワ学つス携"
    val japaneseBytes = japanese.s.getBytes("UTF-8").nn.immutable(using Unsafe)
    
    suite(t"Character widths"):
      test(t"Check narrow character width"):
        'a'.displayWidth
      .assert(_ == 1)

      test(t"Check Japanese character width"):
        '身'.displayWidth
      .assert(_ == 2)

      test(t"Check displayWidth of string of Japanese text: \"平ぱ記...つス携\""):
        import gossamer.displayWidth
        japanese.displayWidth
      .assert(_ == 40)

    suite(t"Roundtrip decoding"):

      test(t"Decode Japanese from UTF-8"):
        import badEncodingHandlers.skip
        charDecoders.utf8.decode(japaneseBytes)
      .assert(_ == japanese)
    
      for chunk <- 1 to 25 do
        test(t"Decode Japanese text in chunks of size $chunk"):
          import badEncodingHandlers.skip
          charDecoders.utf8.decode(japaneseBytes.grouped(chunk).to(LazyList)).join
        .assert(_ == japanese)
      
      val badUtf8 = Bytes(45, -62, 49, 48)

      test(t"Decode invalid UTF-8 sequence, skipping errors"):
        import badEncodingHandlers.skip
        charDecoders.utf8.decode(badUtf8)
      .assert(_ == t"-10")
      
      test(t"Decode invalid UTF-8 sequence, substituting for a question mark"):
        import badEncodingHandlers.substitute
        charDecoders.utf8.decode(badUtf8)
      .assert(_ == t"-?10")
      
      test(t"Decode invalid UTF-8 sequence, throwing exception"):
        import unsafeExceptions.canThrowAny
        import badEncodingHandlers.strict
        capture[UndecodableCharError](charDecoders.utf8.decode(badUtf8))
      .assert(_ == UndecodableCharError(1, enc"UTF-8"))
    
      test(t"Ensure that decoding is finished"):
        import unsafeExceptions.canThrowAny
        import badEncodingHandlers.strict
        given CharEncoder = enc"UTF-8".encoder
        capture[UndecodableCharError](charDecoders.utf8.decode(t"café".bytes.dropRight(1)))
      .assert(_ == UndecodableCharError(4, enc"UTF-8"))
    
    suite(t"Compile-time tests"):
      test(t"Check that an invalid encoding produces an error"):
        demilitarize(enc"ABCDEF").map(_.message)
      .assert(_ == List(t"hieroglyph: the encoding ABCDEF was not available"))
      
      test(t"Check that a non-encoding encoding does have a `decoder` method"):
        import badEncodingHandlers.skip
        demilitarize(enc"ISO-2022-CN".decoder)
      .assert(_ == List())
      
      test(t"Check that a non-encoding encoding has no encoder method"):
        demilitarize(enc"ISO-2022-CN".encoder)
      .assert(_.length == 1)
      
      test(t"Check that an encoding which can encode has an encoder method"):
        demilitarize(enc"ISO-8859-1".encoder)
      .assert(_ == List())


