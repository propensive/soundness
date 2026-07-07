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
┃    Soundness, version 0.63.0.                                                                    ┃
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

import language.experimental.pureFunctions

import scala.collection.immutable as sci

import anticipation.*
import fulminate.*
import gossamer.*
import hieroglyph.*
import rudiments.*
import vacuous.*

object Tabulation:
  given printable: [text: {Textual as textual, Printable as printable}]
  =>  ( Text is Measurable, TableStyle, Attenuation, polysyllabic.Hyphenation )
  =>  Tabulation[text] is Printable =

    (tabulation, termcap) =>
      tabulation.grid(termcap.width).render.map(printable.print(_, termcap)).join(t"\n")


abstract class Tabulation[text: ClassTag]():
  type Row

  def columns: IArray[Column[Row, text]]
  def titles: Seq[IArray[IArray[text]]]
  def rows: Seq[IArray[IArray[text]]]
  def dataLength: Int


  def grid(width: Int)
    ( using style: TableStyle, metrics: Text is Measurable, textual: text is Textual )
    ( using attenuation: Attenuation, hyphenation: polysyllabic.Hyphenation )
  :   Grid[text] =

    case class Layout(slack: Double, indices: IArray[Int], widths: IArray[Int], totalWidth: Int):
      lazy val include: sci.BitSet = indices.to(sci.BitSet)

      lazy val columnWidths: IArray[(Int, Column[Row, text], Int)] = IArray.from:
        indices.indices.map: index =>
          val columnIndex = indices(index)
          (columnIndex, columns(columnIndex), widths(index))

    def bisect(include: Int => Boolean): (Layout, Layout) =
      def shrink(slack: Double): Layout =
        val widths: IndexedSeq[Optional[Int]] =
          columns.indices.map: index =>
            val dataMax =
              if !include(index) then 0 else rows.map: cells =>
                columns(index).sizing.width[text](cells(index), width, slack).or(0)

              . maxOption.getOrElse(0)

            val titleMax =
              if !include(index) then 0 else titles.map: cells =>
                columns(index).sizing.width[text](cells(index), width, slack).or(0)

              . maxOption.getOrElse(0)

            dataMax.max(titleMax).puncture(0)

        val indices: IndexedSeq[Int] =
          widths.indices.map { index => widths(index).let(index.waive) }.compact

        val totalWidth = widths.sumBy(_.or(0)) + style.cost(indices.size)

        Layout(slack, IArray.from(indices), IArray.from(widths.compact), totalWidth)

      def recur(min: Layout, max: Layout, gas: Int = 8): (Layout, Layout) =
        if gas == 0 || max.totalWidth - min.totalWidth <= 1 then (min, max)
        else
          val point = shrink((min.slack + max.slack)/2)

          if point.totalWidth == width then (point, point)
          else if point.totalWidth > width then recur(min, point, gas - 1)
          else recur(point, max, gas - 1)

      recur(shrink(0), shrink(1), 8)

    val rowLayout = bisect(_ => true)(0)
    val rowLayout2 = bisect(rowLayout.include(_))(0)

    // We may be able to increase the slack in some of the remaining columns
    if rowLayout2.totalWidth > width then attenuation(rowLayout2.totalWidth, width)

    def lines(data: Seq[IArray[IArray[text]]]): Stream[TableRow[text]] =
      data.to(Stream).map: cells =>
        val tableCells = rowLayout2.columnWidths.map: (index, column, width) =>
          val lines = column.sizing.fit(cells(index), width, column.textAlign)
          TableCell(width, 1, lines, lines.length, column.textAlign)

        val height = tableCells.maxBy(_.minHeight).minHeight

        TableRow(tableCells, false, height)

    val widths = rowLayout2.columnWidths.map(_(2))

    Grid(List(TableSection(widths, lines(titles)), TableSection(widths, lines(rows))), style)
