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
package polysyllabic

import scala.collection.mutable.ArrayBuilder

import anticipation.*
import rudiments.*
import vacuous.*

private[polysyllabic] object TexPatterns:
  case class File
    ( patterns:   Array[Text]^{},
      exceptions: Array[Text]^{},
      leftMin:    Int,
      rightMin:   Int )

  // Decode one TeX hyphenation pattern. `hy3ph` ↦ (`hyph`, [0,0,3,0,0]). Digits
  // sit between letters; missing digits are 0. The score array has one slot per
  // inter-letter gap including the before-first and after-last positions, so it
  // is always one longer than the letter count. `.`-anchored patterns keep the
  // `.` as a sentinel letter in the key (the algorithm pads its input the same
  // way).
  def parsePattern(raw: Text): (Text, Array[Byte]^{}) =
    val s = raw.s
    val n = s.length
    var letterCount = 0
    var i = 0

    while i < n do
      if !Character.isDigit(s.charAt(i)) then letterCount += 1
      i += 1

    val scores = Array[Byte](letterCount + 1)
    val letters = new java.lang.StringBuilder(letterCount)
    var letterIndex = 0
    i = 0

    while i < n do
      val c = s.charAt(i)

      if Character.isDigit(c) then scores(letterIndex) = (c - '0').toByte
      else
        letters.append(c)
        letterIndex += 1

      i += 1

    (letters.toString.tt, Array.freeze(scores))

  // Decode one TeX exception entry. `as-so-ciate` ↦ (`associate`, [2, 4]).
  // The break offsets count letter positions in the dehyphenated word.
  def parseException(raw: Text): (Text, Array[Int]^{}) =
    val s = raw.s
    val n = s.length
    val letters = new java.lang.StringBuilder(n)
    val breaks = Array[Int](n)
    var count = 0
    var i = 0

    while i < n do
      val c = s.charAt(i)

      if c == '-' then
        breaks(count) = letters.length
        count += 1
      else letters.append(c)

      i += 1

    val exact = Array[Int](count)
    exact.copyFrom(breaks, 0, 0, count)
    (letters.toString.tt, Array.freeze(exact))

  // Strip `%`-to-end-of-line comments from a TeX file. Backslash-escaped
  // percents are not used in hyphenation pattern files, so a naive scan
  // suffices.
  private def stripComments(text: Text): String =
    val s = text.s
    val out = new java.lang.StringBuilder(s.length)
    var i = 0

    while i < s.length do
      val c = s.charAt(i)

      if c == '%' then
        while i < s.length && s.charAt(i) != '\n' do i += 1
      else
        out.append(c)
        i += 1

    out.toString

  // Find `\directive{ … }` and return the whitespace-separated tokens of its
  // body. Returns an empty array if the directive is absent.
  private def block(content: String, directive: String): Array[Text]^{} =
    val marker = directive + "{"
    val start = content.indexOf(marker)

    if start < 0 then Array.empty[Text] else
      val bodyStart = start + marker.length
      val bodyEnd = content.indexOf('}', bodyStart)

      if bodyEnd < 0 then Array.empty[Text] else
        val body = content.substring(bodyStart, bodyEnd).nn
        val tokens = body.split("\\s+").nn.iterator.map(_.nn).filter(_.length > 0).map(_.tt)
        Array.from(tokens)

  // Look for `\directive=N` or `\directive N`, returning N if found.
  private def intValue(content: String, directive: String): Optional[Int] =
    val idx = content.indexOf(directive)

    if idx < 0 then Unset else
      var i = idx + directive.length
      while i < content.length && (content.charAt(i) == ' ' || content.charAt(i) == '=') do i += 1
      val start = i
      while i < content.length && Character.isDigit(content.charAt(i)) do i += 1

      if i == start then Unset
      else
        try content.substring(start, i).nn.toInt
        catch case _: NumberFormatException => Unset

  // Parse a hyphenation pattern file in the TeX wire format. Recognises
  // `\patterns{…}`, `\hyphenation{…}`, and optional `\lefthyphenmin`/
  // `\righthyphenmin` directives. Falls back to English minima (2/3) when the
  // file has no directives — the convention for hyph-utf8 files that embed the
  // minima in YAML comments instead.
  def parseFile(content: Text): File =
    val stripped = stripComments(content)

    File
      ( patterns   = block(stripped, "\\patterns"),
        exceptions = block(stripped, "\\hyphenation"),
        leftMin    = intValue(stripped, "\\lefthyphenmin").or(2),
        rightMin   = intValue(stripped, "\\righthyphenmin").or(3) )
