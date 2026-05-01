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
┃    Soundness, version 0.54.0.                                                                    ┃
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

import java.lang as jl

import soundness.*

import strategies.throwUnsafely
import textMetrics.eastAsianScripts
import errorDiagnostics.stackTraces

object Tests extends Suite(m"Hieroglyph tests"):
  def run(): Unit =
    val japanese = t"平ぱ記動テ使村方島おゃぎむ万離ワ学つス携"
    val japaneseData = japanese.s.getBytes("UTF-8").nn.immutable(using Unsafe)

    suite(m"Character widths"):
      test(m"Check narrow character width"):
        'a'.metrics
      . assert(_ == 1)

      test(m"Check Japanese character width"):
        '身'.metrics
      . assert(_ == 2)

      test(m"Check metrics of string of Japanese text: \"平ぱ記...つス携\""):
        japanese.metrics
      . assert(_ == 40)

    suite(m"Roundtrip decoding"):

      test(m"Decode Japanese from UTF-8"):
        import textSanitizers.skip
        charDecoders.utf8.decoded(japaneseData)
      . assert(_ == japanese)

      for chunk <- 1 to 25 do
        test(m"Decode Japanese text in chunks of size $chunk"):
          import textSanitizers.skip
          charDecoders.utf8.decoded(japaneseData.grouped(chunk).to(Stream)).join
        . assert(_ == japanese)

      val badUtf8 = Data(45, -62, 49, 48)

      test(m"Decode invalid UTF-8 sequence, skipping errors"):
        import textSanitizers.skip
        charDecoders.utf8.decoded(badUtf8)
      . assert(_ == t"-10")

      test(m"Decode invalid UTF-8 with question mark substitution"):
        import textSanitizers.substitute
        charDecoders.utf8.decoded(badUtf8)
      . assert(_ == t"-?10")

      test(m"Decode invalid UTF-8 sequence, throwing exception"):
        import textSanitizers.strict
        capture[CharDecodeError](charDecoders.utf8.decoded(badUtf8))
      . assert(_ == CharDecodeError(1, enc"UTF-8"))

      test(m"Ensure that decoding is finished"):
        import textSanitizers.strict
        given CharEncoder = enc"UTF-8".encoder
        capture[CharDecodeError](charDecoders.utf8.decoded(t"café".data.dropRight(1)))
      . assert(_ == CharDecodeError(4, enc"UTF-8"))

    suite(m"Compile-time tests"):
      test(m"Check that an invalid encoding produces an error"):
        demilitarize(enc"ABCDEF").map(_.message)
      . assert(_ == List(t"[↯SN-hi/2] the encoding ABCDEF was not available"))

      test(m"Non-encoding has a decoder method"):
        demilitarize:
          import textSanitizers.skip
          enc"ISO-2022-CN".decoder

        . map(_.message)

      . assert(_ == List())

      test(m"Check that a non-encoding encoding has no encoder method"):
        demilitarize(enc"ISO-2022-CN".encoder).map(_.message)
      . assert(_.length == 1)

      test(m"Encoding has an encoder method"):
        demilitarize(enc"ISO-8859-1".encoder).map(_.message)
      . assert(_ == List())

    suite(m"Wide-Character Width (Kuhn algorithm)"):
      test(m"ASCII letter is width 1"):
        WideCharacterWidth.width('a'.toInt)
      . assert(_ == 1)

      test(m"NUL is zero-width"):
        WideCharacterWidth.width(0)
      . assert(_ == 0)

      test(m"C0 control char is zero-width"):
        WideCharacterWidth.width(0x07)
      . assert(_ == 0)

      test(m"DEL (0x7F) is zero-width"):
        WideCharacterWidth.width(0x7f)
      . assert(_ == 0)

      test(m"combining grave accent is zero-width"):
        WideCharacterWidth.width(0x0300)
      . assert(_ == 0)

      test(m"zero-width joiner is zero-width"):
        WideCharacterWidth.width(0x200d)
      . assert(_ == 0)

      test(m"CJK ideograph is wide"):
        WideCharacterWidth.width('日'.toInt)
      . assert(_ == 2)

      test(m"Hangul syllable is wide"):
        WideCharacterWidth.width(0xac00)
      . assert(_ == 2)

      test(m"Fullwidth Latin is wide"):
        WideCharacterWidth.width(0xff21)
      . assert(_ == 2)

      test(m"narrow halfwidth katakana is width 1"):
        WideCharacterWidth.width(0xff66)
      . assert(_ == 1)

      test(m"emoji man (supplementary plane) is wide via codepoint"):
        WideCharacterWidth.width(0x1f468)
      . assert(_ == 2)

    suite(m"Grapheme cluster boundaries"):
      test(m"empty string yields single sentinel"):
        GraphemeBreak.boundaries(t"").to(List)
      . assert(_ == List(0))

      test(m"ASCII string boundaries"):
        GraphemeBreak.boundaries(t"abc").to(List)
      . assert(_ == List(0, 1, 2, 3))

      test(m"CR LF stays one cluster"):
        GraphemeBreak.boundaries(t"a\r\nb").to(List)
      . assert(_ == List(0, 1, 3, 4))

      test(m"combining diaeresis joins with space"):
        GraphemeBreak.boundaries(Text(" ̈ ")).to(List)
      . assert(_ == List(0, 2, 3))

      test(m"two regional indicators form one flag"):
        GraphemeBreak.boundaries(Text("🇬🇧🇫🇷")).to(List).size
      . assert(_ == 3)

      // UAX #29 conformance against the official GraphemeBreakTest.txt fixture.
      // GB9c (Indic Conjunct Break, rule 9.3) is intentionally not implemented
      // in this first pass, so those cases are tolerated.
      test(m"UAX #29 conformance (excluding GB9c)"):
        val resourcePath = "/hieroglyph/GraphemeBreakTest.txt"
        val stream = getClass.getResourceAsStream(resourcePath).nn
        val lines = scala.io.Source.fromInputStream(stream).getLines().to(List)

        var failures: List[(Int, String)] = Nil

        lines.zipWithIndex.foreach: (rawLine, idx) =>
          val withoutComment =
            if rawLine.indexOf('#') >= 0 then rawLine.substring(0, rawLine.indexOf('#')).nn
            else rawLine

          val trimmed = withoutComment.trim.nn

          if trimmed.nonEmpty && !rawLine.contains("[9.3]") then
            val tokens: List[String] = trimmed.split("\\s+").nn.to(List).map(_.nn)
            val sb = jl.StringBuilder()
            val expected = scala.collection.mutable.ArrayBuffer[Int]()

            tokens.each:
              case "÷" => expected += sb.length
              case "×" => ()
              case hex => sb.appendCodePoint(Integer.parseInt(hex, 16))

            val input = sb.toString.nn.tt
            val actual = GraphemeBreak.boundaries(input).to(List)

            if actual != expected.to(List) then failures = (idx + 1, rawLine) :: failures

        failures.size

      . assert(_ == 0)
