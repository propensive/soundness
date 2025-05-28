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
┃    Soundness, version 0.32.0.                                                                    ┃
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
package hieroglyph

import soundness.*

import strategies.throwUnsafely
import textMetrics.eastAsianScripts
import errorDiagnostics.stackTraces

object Tests extends Suite(m"Hieroglyph tests"):
  def run(): Unit =
    val japanese = t"平ぱ記動テ使村方島おゃぎむ万離ワ学つス携"
    val japaneseBytes = japanese.s.getBytes("UTF-8").nn.immutable(using Unsafe)

    suite(m"Character widths"):
      test(m"Check narrow character width"):
        'a'.metrics
      .assert(_ == 1)

      test(m"Check Japanese character width"):
        '身'.metrics
      .assert(_ == 2)

      test(m"Check metrics of string of Japanese text: \"平ぱ記...つス携\""):
        japanese.metrics
      .assert(_ == 40)

    suite(m"Roundtrip decoding"):

      test(m"Decode Japanese from UTF-8"):
        import textSanitizers.skip
        charDecoders.utf8.decoded(japaneseBytes)
      .assert(_ == japanese)

      for chunk <- 1 to 25 do
        test(m"Decode Japanese text in chunks of size $chunk"):
          import textSanitizers.skip
          charDecoders.utf8.decoded(japaneseBytes.grouped(chunk).to(Stream)).join
        .assert(_ == japanese)

      val badUtf8 = Bytes(45, -62, 49, 48)

      test(m"Decode invalid UTF-8 sequence, skipping errors"):
        import textSanitizers.skip
        charDecoders.utf8.decoded(badUtf8)
      .assert(_ == t"-10")

      test(m"Decode invalid UTF-8 sequence, substituting for a question mark"):
        import textSanitizers.substitute
        charDecoders.utf8.decoded(badUtf8)
      .assert(_ == t"-?10")

      test(m"Decode invalid UTF-8 sequence, throwing exception"):
        import unsafeExceptions.canThrowAny
        import textSanitizers.strict
        capture[CharDecodeError](charDecoders.utf8.decoded(badUtf8))
      .assert(_ == CharDecodeError(1, enc"UTF-8"))

      test(m"Ensure that decoding is finished"):
        import unsafeExceptions.canThrowAny
        import textSanitizers.strict
        given CharEncoder = enc"UTF-8".encoder
        capture[CharDecodeError](charDecoders.utf8.decoded(t"café".bytes.dropRight(1)))
      .assert(_ == CharDecodeError(4, enc"UTF-8"))

    suite(m"Compile-time tests"):
      test(m"Check that an invalid encoding produces an error"):
        demilitarize(enc"ABCDEF").map(_.message)
      .assert(_ == List(t"hieroglyph: the encoding ABCDEF was not available"))

      test(m"Check that a non-encoding encoding does have a `decoder` method"):
        import textSanitizers.skip
        demilitarize(enc"ISO-2022-CN".decoder)
      .assert(_ == List())

      test(m"Check that a non-encoding encoding has no encoder method"):
        demilitarize(enc"ISO-2022-CN".encoder)
      .assert(_.length == 1)

      test(m"Check that an encoding which can encode has an encoder method"):
        demilitarize(enc"ISO-8859-1".encoder)
      .assert(_ == List())
