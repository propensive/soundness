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
package escritoire

import anticipation.*
import contingency.*
import denominative.*
import gossamer.*
import gossamer.Textual.concatenable
import hieroglyph.*
import rudiments.*
import symbolism.*
import vacuous.*

extension [row](data: Seq[row])
  def tabulation[text: Textual](using tabulable: row is Tabulable[text]): Tabulation[text] =

    tabulable.tabulate(data)

extension [value](value: value)
  def tabulation[text: Textual](using tabular: value is Tabular[text]): Tabulation[text] =
    tabular.tabulate(value)

package columnAttenuation:
  given fail: Tactic[TableError] => Attenuation =
    (minimum, available) => raise(TableError(minimum, available))

  given ignore: Attenuation = (minimum, available) => ()

package tableStyles:
  import BoxLine.*

  given default: TableStyle = TableStyle(1, Thick, Thick, Thin, Thick, Thin, LineCharset.Default)
  given thinRounded: TableStyle = TableStyle(1, Thin, Thin, Thin, Thin, Thin, LineCharset.Rounded)
  given horizontal: TableStyle = TableStyle(1, Thin, Thin, Thin, Blank, Blank, LineCharset.Default)
  given midOnly: TableStyle = TableStyle(1, Blank, Blank, Thin, Blank, Blank, LineCharset.Default)
  given vertical: TableStyle = TableStyle(1, Blank, Blank, Blank, Thin, Thin, LineCharset.Default)
  given minimal: TableStyle = TableStyle(1, Unset, Unset, Thin, Blank, Blank, LineCharset.Default)

package columnar:
  // Cumulative display width up to each char position; `widths(i)` is the width
  // of `text.plain.s.substring(0, i)`. `widths.length == text.plain.s.length + 1`.
  private def prefixWidths[textual: Textual](text: textual)(using Char is Measurable)
  :   IArray[Int] =

    val plain = text.plain.s
    val n = plain.length
    val arr = new Array[Int](n + 1)
    var i = 0
    while i < n do
      arr(i + 1) = arr(i) + summon[Char is Measurable].width(plain.charAt(i))
      i += 1

    arr.immutable(using Unsafe)

  // Sum of char widths over `text.plain`.
  private def displayWidth[textual: Textual](text: textual)(using Text is Measurable): Int =
    text.plain.metrics

  object Prose extends Columnar:
    def longestWord[textual: Textual](text: textual)(using Char is Measurable): Int =
      val plain = text.plain.s
      val widths = prefixWidths(text)
      val n = plain.length
      var max = 0
      var lastStart = 0
      var i = 0
      while i < n do
        if plain.charAt(i) == ' ' then
          val wordWidth = widths(i) - widths(lastStart)
          if wordWidth > max then max = wordWidth
          lastStart = i + 1
        i += 1

      val tailWidth = widths(n) - widths(lastStart)
      if tailWidth > max then max = tailWidth
      max

    def width[textual: Textual](lines: IArray[textual], maxWidth: Int, slack: Double)
      (using Text is Measurable)
    :   Optional[Int] =

      // `Text is Measurable` (general derivation) is implied by `Char is Measurable`
      // in scope; longestWord needs the per-char measurer.
      given Char is Measurable = _.toString.tt.metrics
      val longestLine = lines.map(displayWidth(_)).max
      lines.map(longestWord(_)).max.max((slack*maxWidth).toInt).min(longestLine)


    def fit[textual: Textual](lines: IArray[textual], width: Int, textAlign: TextAlignment)
      (using Text is Measurable)
    :   IndexedSeq[textual] =

      given measurable: Char is Measurable = _.toString.tt.metrics

      def format(text: textual): List[textual] =
        val plain = text.plain.s
        val widths = prefixWidths(text)
        val n = plain.length

        // Walk char positions; accumulate display width since `lineStart`.
        // Break at `lastSpace` once the next char would overflow `width`.
        def recur(position: Int, lineStart: Int, lastSpace: Int, acc: List[textual])
        :   List[textual] =

          if position >= n then
            if lineStart == position then acc else text.segment(lineStart.z thru position.u) :: acc
          else
            val current = plain.charAt(position)
            if current == ' ' then recur(position + 1, lineStart, position, acc)
            else
              val widthSoFar = widths(position + 1) - widths(lineStart)
              if widthSoFar > width && lastSpace > lineStart then
                val segment = text.segment(lineStart.z thru lastSpace.u)
                recur(lastSpace + 1, lastSpace + 1, lastSpace + 1, segment :: acc)
              else recur(position + 1, lineStart, lastSpace, acc)

        recur(0, 0, 0, Nil)

      lines.to(IndexedSeq).flatMap(format(_).reverse)

  object ProseOrBreak extends Columnar:
    def width[textual: Textual](lines: IArray[textual], maxWidth: Int, slack: Double)
      (using Text is Measurable)
    :   Optional[Int] =

      (maxWidth*slack + 1).toInt.min(maxWidth)


    def fit[textual: Textual](lines: IArray[textual], width: Int, textAlign: TextAlignment)
      (using Text is Measurable)
    :   IndexedSeq[textual] =

      given Char is Measurable = _.toString.tt.metrics
      if lines.map(Prose.longestWord(_)).max < width
      then Prose.fit(lines, width, textAlign)
      else
        var result: List[textual] = Nil

        lines.each: line =>
          val count = (line.length - 1)/width + 1
          (0 until count).each: index =>
            result = line.segment((width*index).z span width) :: result

        result.reverse.to(Vector)

  case class Fixed(fixedWidth: Int, ellipsis: Text = t"…") extends Columnar:
    def width[text: Textual](lines: IArray[text], maxWidth: Int, slack: Double)
      (using Text is Measurable)
    :   Optional[Int] = fixedWidth


    def fit[text: Textual](lines: IArray[text], width: Int, textAlign: TextAlignment)
      (using Text is Measurable)
    :   IndexedSeq[text] =

      lines.to(IndexedSeq).map: line =>
        if line.plain.metrics > width then line.keep(width - ellipsis.length)+text(ellipsis)
        else line

  case class Shortened(fixedWidth: Int, ellipsis: Text = t"…") extends Columnar:
    def width[text: Textual](lines: IArray[text], maxWidth: Int, slack: Double)
      (using Text is Measurable)
    :   Optional[Int] =
      val naturalWidth = lines.map(_.plain.metrics).max
      (maxWidth*slack).toInt.min(naturalWidth)


    def fit[text: Textual](lines: IArray[text], width: Int, textAlign: TextAlignment)
      (using Text is Measurable)
    :   IndexedSeq[text] =

      lines.to(IndexedSeq).map: line =>
        if line.plain.metrics > width then line.keep(width - ellipsis.length)+text(ellipsis)
        else line

  case class Collapsible(threshold: Double) extends Columnar:
    def width[text: Textual](lines: IArray[text], maxWidth: Int, slack: Double)
      (using Text is Measurable)
    :   Optional[Int] =
      if slack > threshold then lines.map(_.plain.metrics).max else Unset


    def fit[text: Textual](lines: IArray[text], width: Int, textAlign: TextAlignment)
      (using Text is Measurable)
    :   IndexedSeq[text] =

      lines.to(IndexedSeq)
