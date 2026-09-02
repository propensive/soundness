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
import symbolism.*
import tessellate.*
import vacuous.*
import denominative.*
// Per-row decorations are a short `List` read by column position.
import denominative.dysasymptotics.linearAccess

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

  // Per-row, per-column cell decorations, aligned with `rows`; empty means undecorated.
  def decorations: List[List[Optional[text -> text]]] = Nil


  def grid(width: Int)
    ( using style: TableStyle, metrics: Text is Measurable )
    ( using textual: text is Textual { type Result = Char } )
    ( using attenuation: Attenuation^, hyphenation: polysyllabic.Hyphenation )
  :   Grid[text] =

    // Every logical line each column will display, across both titles and data.
    val columnLines: IndexedSeq[Array[text]^{}] =
      columns.readable.indices.map: index =>
        val titleLines: List[text] = titles.bind(_.readable(index))
        val rowLines:   List[text] = rows.bind(_.readable(index))
        val lines:      List[text] = titleLines + rowLines

        lines.to[Array]

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
        // `Sequence.from`, not `.to[Sequence]`: the Factory search here trips the compiler's
        // `wildApprox` assertion (uninstantiated type variable in an implicit-scope walk).
        ( Sequence.from(visible.map(flexes(_))),
          width - style.columnCost - 1,
          style.columnCost )

    val survivors: IndexedSeq[(Int, Int)] =
      visible.indices.flatMap: position =>
        solved.at(position.z).let { cellWidth => (visible(position), cellWidth) }.option

    val totalWidth = survivors.map(_(1)).sum + style.cost(survivors.size)

    if totalWidth > width then attenuation(totalWidth, width)

    def lines
      ( data: List[Array[Array[text]^{}]^{}],
        decorations2: List[List[Optional[text -> text]]] )
    :   Chain[TableRow[text]] =

      // No native iterator: the decorations are consumed one row at a time alongside `data`,
      // which is shorter or longer at will, so this is a `zipAll`, not a `zip`.
      val decorationIterator = decorations2.stdlib.iterator

      data.to[Chain].map: cells =>
        val rowDecorations: List[Optional[text -> text]] =
          if decorationIterator.hasNext then decorationIterator.next() else Nil

        val tableCells = Array.from:
          survivors.map: (index, cellWidth) =>
            val column = columns.readUnchecked(index)
            val lines = column.sizing.fit[text](cells.readable(index), cellWidth, column.textAlign)

            val decoration: Optional[text -> text] = rowDecorations.at(index.z)

            TableCell
              ( cellWidth, 1, lines, lines.size, column.textAlign, column.verticalAlign,
                decoration )

        val height = tableCells.readable.maxBy(_.minHeight).minHeight

        TableRow(tableCells, false, height)

    val widths = Array.from(survivors.map(_(1)))

    Grid
      ( List(TableSection(widths, lines(titles, Nil)), TableSection(widths, lines(rows, decorations))),
        style )
