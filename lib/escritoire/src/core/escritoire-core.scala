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
┃    Soundness, version 0.27.0.                                                                    ┃
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
import rudiments.*
import vacuous.*

extension [row](data: Seq[row])
  def table[text: Textual](using tabulable: row is Tabulable[text])
  :     Tabulation[text] =

    tabulable.tabulate(data)

extension [value](value: value)
  def table[text: Textual](using tabular: value is Tabular[text]): Tabulation[text] =
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
  object Prose extends Columnar:
    def width[textual: Textual](lines: IArray[textual], maxWidth: Int, slack: Double)
    :     Optional[Int] =

      def longestWord(text: textual, position: Int, lastStart: Int, max: Int): Int =
        if position < text.length then
          if textual.unsafeChar(text, position.z) == ' '
          then longestWord(text, position + 1, position + 1, max.max(position - lastStart))
          else longestWord(text, position + 1, lastStart, max)
        else max.max(position - lastStart)

      val longestLine = lines.map(_.length).max
      lines.map(longestWord(_, 0, 0, 0)).max.max((slack*maxWidth).toInt).min(longestLine)

    def fit[textual: Textual](lines: IArray[textual], width: Int, textAlign: TextAlignment)
    :     IndexedSeq[textual] =

      def format(text: textual, position: Int, lineStart: Int, lastSpace: Int, lines: List[textual])
      :     List[textual] =

        if position < text.length then
          if textual.unsafeChar(text, position.z) == ' '
          then format(text, position + 1, lineStart, position, lines)
          else
            if position - lineStart >= width
            then format
             (text,
              position + 1,
              lastSpace + 1,
              lastSpace,
              text.segment(lineStart.z ~ Ordinal.natural(lastSpace)) :: lines)

            else format(text, position + 1, lineStart, lastSpace, lines)
        else if lineStart == position then lines
        else text.segment(lineStart.z ~ Ordinal.natural(position)) :: lines

      lines.to(IndexedSeq).flatMap(format(_, 0, 0, 0, Nil).reverse)

  case class Fixed(fixedWidth: Int, ellipsis: Text = t"…") extends Columnar:
    def width[text: Textual](lines: IArray[text], maxWidth: Int, slack: Double)
    :     Optional[Int] =
      fixedWidth

    def fit[text: Textual](lines: IArray[text], width: Int, textAlign: TextAlignment)
    :     IndexedSeq[text] =

      lines.to(IndexedSeq).map: line =>
        if line.length > width then line.keep(width - ellipsis.length)+text(ellipsis) else line

  case class Shortened(fixedWidth: Int, ellipsis: Text = t"…") extends Columnar:
    def width[text: Textual](lines: IArray[text], maxWidth: Int, slack: Double)
    :     Optional[Int] =
      val naturalWidth = lines.map(_.length).max
      (maxWidth*slack).toInt.min(naturalWidth)

    def fit[text: Textual](lines: IArray[text], width: Int, textAlign: TextAlignment)
    :     IndexedSeq[text] =

      lines.to(IndexedSeq).map: line =>
        if line.length > width then line.keep(width - ellipsis.length)+text(ellipsis) else line

  case class Collapsible(threshold: Double) extends Columnar:
    def width[text: Textual](lines: IArray[text], maxWidth: Int, slack: Double)
    :     Optional[Int] =
      if slack > threshold then lines.map(_.length).max else Unset

    def fit[text: Textual](lines: IArray[text], width: Int, textAlign: TextAlignment)
    :     IndexedSeq[text] =

      lines.to(IndexedSeq)
