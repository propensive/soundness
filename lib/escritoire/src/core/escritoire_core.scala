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
package escritoire

import scala.collection.immutable.IndexedSeq

import anticipation.*
import contingency.*
import denominative.*
import gossamer.*
import gossamer.Textual.concatenable
import hieroglyph.*
import polysyllabic.*
import rudiments.*
import symbolism.*
import vacuous.*

extension [row](data: List[row])
  def tabulation[text: Textual](using tabulable: row is Tabulable[text]): Tabulation[text] =

    tabulable.tabulate(data)

extension [value](value: value)
  def tabulation[text: Textual](using tabular: value is Tabular[text]): Tabulation[text] =
    tabular.tabulate(value)

// `failAttenuation` is a context function `Tactic[TableError] ?=> Attenuation^`: the returned
// `Attenuation` captures the Tactic *parameter*, so the given value itself captures nothing from its
// enclosing scope and these can stay package-level givens — accessing `failAttenuation` or
// `ignoreAttenuation` then captures no capability (unlike a member of an `ExclusiveCapability` object).
package columnAttenuation:
  given failAttenuation: (tactic: Tactic[TableError]) => (Attenuation^{tactic}) =
    (minimum, available) => raise(TableError(minimum, available))

  given ignoreAttenuation: Attenuation = (minimum, available) => ()

package tableStyles:
  import BoxLine.*
  import LineCharset.{Default, Rounded}

  given defaultTableStyle: TableStyle = TableStyle(1, Thick, Thick, Thin, Thick, Thin, Default)
  given thinRoundedTableStyle: TableStyle = TableStyle(1, Thin, Thin, Thin, Thin, Thin, Rounded)
  given horizontalTableStyle: TableStyle = TableStyle(1, Thin, Thin, Thin, Blank, Blank, Default)
  given midOnlyTableStyle: TableStyle = TableStyle(1, Blank, Blank, Thin, Blank, Blank, Default)
  given verticalTableStyle: TableStyle = TableStyle(1, Blank, Blank, Blank, Thin, Thin, Default)
  given minimalTableStyle: TableStyle = TableStyle(1, Unset, Unset, Thin, Blank, Blank, Default)

package columnar:
  // Cumulative display width up to each char position; `widths(i)` is the width
  // of `text.plain.s.substring(0, i)`. `widths.length == text.plain.s.length + 1`.
  private def prefixWidths[textual: Textual](text: textual)(using Char is Measurable)
  :   Array[Int]^{} =

    val plain = text.plain.s
    val n = plain.length
    val buffer = Array[Int](n + 1)
    var total = 0
    var i = 0
    buffer(0) = 0

    while i < n do
      total += summon[Char is Measurable].width(plain.charAt(i))
      buffer(i + 1) = total
      i += 1

    Array.freeze(buffer)

  // Sum of char widths over `text.plain`.
  private def displayWidth[textual: Textual](text: textual)(using Text is Measurable): Int =
    text.plain.metrics

  object Paragraph extends Columnar:
    def longestWord[textual: Textual](text: textual)(using Char is Measurable): Int =
      val plain = text.plain.s
      val widths = prefixWidths(text)
      val n = plain.length
      var max = 0
      var lastStart = 0
      var i = 0

      while i < n do
        if plain.charAt(i) == ' ' then
          val wordWidth = widths.readable(i) - widths.readable(lastStart)
          if wordWidth > max then max = wordWidth
          lastStart = i + 1

        i += 1

      val tailWidth = widths.readable(n) - widths.readable(lastStart)
      if tailWidth > max then max = tailWidth
      max

    def width[textual: Textual](lines: Array[textual]^{}, maxWidth: Int, slack: Double)
      ( using Text is Measurable )
    :   Optional[Int] =

      // `Text is Measurable` (general derivation) is implied by `Char is Measurable`
      // in scope; longestWord needs the per-char measurer.
      given Char is Measurable = _.toString.tt.metrics
      val longestLine = lines.readable.map(displayWidth(_)).max
      lines.readable.map(longestWord(_)).max.max((slack*maxWidth).toInt).min(longestLine)


    def fit[textual: Textual](lines: Array[textual]^{}, width: Int, textAlign: TextAlignment)
      ( using Text is Measurable, Hyphenation )
    :   Series[textual] =

      given measurable: Char is Measurable = _.toString.tt.metrics
      val hyphen = textual(t"-")
      val hyphenWidth = displayWidth(hyphen)
      val hyphenation = summon[Hyphenation]
      val leftMin = hyphenation.leftMin
      val rightMin = hyphenation.rightMin

      def format(text: textual): List[textual] =
        val plain = text.plain.s
        val widths = prefixWidths(text)
        val n = plain.length

        // Find the natural end of the word that contains `position` (the first
        // following space, or `n` if the word runs to the end of the input).
        def wordEnd(position: Int): Int =
          var end = position
          while end < n && plain.charAt(end) != ' ' do end += 1
          end

        // Try to break the current word at the latest hyphenation point that
        // still fits in `width` from `lineStart`. Returns the absolute position
        // (in `plain`) at which to break, or `-1` if no break fits.
        def hyphenationBreak(lineStart: Int, wordStart: Int, wordEnd0: Int): Int =
          val breaks =
            Hyphenation.breakPoints
              ( plain, wordStart, wordEnd0 - wordStart, hyphenation, leftMin, rightMin )

          var best = -1
          var index = 0

          while index < breaks.readable.length do
            val candidate = wordStart + breaks.readable(index)
            val w = widths.readable(candidate) - widths.readable(lineStart) + hyphenWidth
            if w <= width then best = candidate
            index += 1

          best

        // Walk char positions; accumulate display width since `lineStart`.
        // When the next character would overflow `width`, prefer a hyphenation
        // break within the current word; otherwise fall back to wrapping at
        // the most recent space; otherwise let the over-long word run on.
        def recur(position: Int, lineStart: Int, lastSpace: Int, acc: List[textual])
        :   List[textual] =

          if position >= n then
            if lineStart == position then acc else text.segment(lineStart.z thru position.u) :: acc
          else
            val current = plain.charAt(position)

            if current == ' ' then recur(position + 1, lineStart, position, acc)
            else
              val widthSoFar = widths.readable(position + 1) - widths.readable(lineStart)

              if widthSoFar > width then
                val wordStart = if lastSpace > lineStart then lastSpace + 1 else lineStart
                val wordEnd0 = wordEnd(position)
                val breakAt = hyphenationBreak(lineStart, wordStart, wordEnd0)

                if breakAt > lineStart then
                  val segment = text.segment(lineStart.z thru (breakAt - 1).u) + hyphen
                  recur(breakAt, breakAt, breakAt, segment :: acc)
                else if lastSpace > lineStart then
                  val segment = text.segment(lineStart.z thru lastSpace.u)
                  recur(lastSpace + 1, lastSpace + 1, lastSpace + 1, segment :: acc)
                else
                  recur(position + 1, lineStart, lastSpace, acc)
              else
                recur(position + 1, lineStart, lastSpace, acc)

        recur(0, 0, 0, Nil)

      Series.of(lines.readable.to(IndexedSeq).bind(format(_).reverse).toVector)

  object ParagraphOrBreak extends Columnar:
    def width[textual: Textual](lines: Array[textual]^{}, maxWidth: Int, slack: Double)
      ( using Text is Measurable )
    :   Optional[Int] =

      (maxWidth*slack + 1).toInt.min(maxWidth)


    def fit[textual: Textual](lines: Array[textual]^{}, width: Int, textAlign: TextAlignment)
      ( using Text is Measurable, Hyphenation )
    :   Series[textual] =

      given Char is Measurable = _.toString.tt.metrics

      if lines.readable.map(Paragraph.longestWord(_)).max < width
      then Paragraph.fit(lines, width, textAlign)
      else
        var result: List[textual] = Nil

        lines.each: line =>
          val count = (line.length - 1)/width + 1

          (0 until count).each: index =>
            result = line.segment((width*index).z span width) :: result

        Series.of(result.stdlib.reverse.toVector)

  case class Fixed(fixedWidth: Int, ellipsis: Text = t"…") extends Columnar:
    def width[text: Textual](lines: Array[text]^{}, maxWidth: Int, slack: Double)
      ( using Text is Measurable )
    :   Optional[Int] =

      fixedWidth


    def fit[text: Textual](lines: Array[text]^{}, width: Int, textAlign: TextAlignment)
      ( using Text is Measurable, Hyphenation )
    :   Series[text] =

      Series.of:
        lines.readable.toVector.map: line =>
          if line.plain.metrics > width then line.keep(width - ellipsis.length)+text(ellipsis)
          else line

  case class Shortened(fixedWidth: Int, ellipsis: Text = t"…") extends Columnar:
    def width[text: Textual](lines: Array[text]^{}, maxWidth: Int, slack: Double)
      ( using Text is Measurable )
    :   Optional[Int] =

      val naturalWidth = lines.readable.map(_.plain.metrics).max
      (maxWidth*slack).toInt.min(naturalWidth)


    def fit[text: Textual](lines: Array[text]^{}, width: Int, textAlign: TextAlignment)
      ( using Text is Measurable, Hyphenation )
    :   Series[text] =

      Series.of:
        lines.readable.toVector.map: line =>
          if line.plain.metrics > width then line.keep(width - ellipsis.length)+text(ellipsis)
          else line

  case class Collapsible(threshold: Double) extends Columnar:
    def width[text: Textual](lines: Array[text]^{}, maxWidth: Int, slack: Double)
      ( using Text is Measurable )
    :   Optional[Int] =

      if slack > threshold then lines.readable.map(_.plain.metrics).max else Unset


    def fit[text: Textual](lines: Array[text]^{}, width: Int, textAlign: TextAlignment)
      ( using Text is Measurable, Hyphenation )
    :   Series[text] =

      Series.of(lines.readable.toVector)
