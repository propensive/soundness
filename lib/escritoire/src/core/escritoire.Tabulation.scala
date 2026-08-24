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

import scala.language.experimental.pureFunctions

import scala.collection.immutable as sci

import anticipation.*
import fulminate.*
import gossamer.*
import hieroglyph.*
import rudiments.*
import tessellate.*
import vacuous.*
import denominative.*
import denominative.dysasymptotics.linearSize

object Tabulation:
  given printable: [text]
  =>  ( textual: text is Textual { type Result = Char }, printable: text is Printable )
  =>  ( Text is Measurable, TableStyle, Attenuation, polysyllabic.Hyphenation )
  =>  Tabulation[text] is Printable =

    (tabulation, termcap) =>
      tabulation.grid(termcap.width).render.map(printable.print(_, termcap)).join(t"\n")

abstract class Tabulation[text: ClassTag]():
  type Row

  def columns: Array[Column[Row, text]]^{}
  def titles: List[Array[Array[text]^{}]^{}]
  def rows: List[Array[Array[text]^{}]^{}]
  def dataLength: Int


  def grid(width: Int)
    ( using style: TableStyle, metrics: Text is Measurable )
    ( using textual: text is Textual { type Result = Char } )
    ( using attenuation: Attenuation^, hyphenation: polysyllabic.Hyphenation )
  :   Grid[text] =

    // Every logical line each column will display, across both titles and data.
    val columnLines: IndexedSeq[Array[text]^{}] =
      columns.readable.indices.map: index =>
        Array.from:
          titles.stdlib.flatMap(_.readable(index).readable) ++ rows.stdlib.flatMap(_.readable(index).readable)

    val flexes: IndexedSeq[Flex] =
      columns.readable.indices.map: index =>
        columns.readUnchecked(index).sizing.flex[text](columnLines(index), width)

    // A column that can never occupy any width (e.g. a `Paragraph` column whose every cell is
    // empty) vanishes entirely, as it would otherwise still cost padding and a rule.
    val visible: IndexedSeq[Int] =
      columns.readable.indices.filter: index =>
        flexes(index).metrics.min > 0 || flexes(index).max.or(flexes(index).metrics.natural) > 0

    // The chrome around k columns is k*columnCost + 1 = (k - 1) gaps of columnCost, plus one
    // more columnCost and the closing edge — so the solver sees those two constants deducted
    // and the rest as inter-track gaps, and its collapse decisions account for the chrome a
    // dropped column saves.
    val solved =
      Flex.solve
        ( Sequence.of(visible.map(flexes(_)).toVector),
          width - style.columnCost - 1,
          style.columnCost )

    val survivors: IndexedSeq[(Int, Int)] =
      visible.indices.flatMap: position =>
        solved.stdlib(position).let { cellWidth => (visible(position), cellWidth) }.option

    val totalWidth = survivors.map(_(1)).sum + style.cost(survivors.size)

    if totalWidth > width then attenuation(totalWidth, width)

    def lines(data: List[Array[Array[text]^{}]^{}]): Chain[TableRow[text]] =
      data.stdlib.to(Chain).map: cells =>
        val tableCells = Array.from:
          survivors.map: (index, cellWidth) =>
            val column = columns.readUnchecked(index)
            val lines = column.sizing.fit[text](cells.readable(index), cellWidth, column.textAlign)

            TableCell
              ( cellWidth, 1, lines, lines.size, column.textAlign, column.verticalAlign )

        val height = tableCells.readable.maxBy(_.minHeight).minHeight

        TableRow(tableCells, false, height)

    val widths = Array.from(survivors.map(_(1)))

    Grid(List(TableSection(widths, lines(titles)), TableSection(widths, lines(rows))), style)
