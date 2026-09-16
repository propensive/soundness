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

import anticipation.*
import denominative.*
import gossamer.*
import hieroglyph.*
import polysyllabic.*
import rudiments.*
import tessellate.*
import vacuous.*

// Per-row decorations are a short `List` read by column position.
import denominative.dysasymptotics.linearAccess

// One row's cells as the lines each will display, with each cell's intrinsic widths: phrased
// once from the row, and independent of any width, so a layout can test a row, admit it, and
// render it at whatever widths it settles on.
case class Cells[text](lines: Array[Array[text]^{}]^{}, metrics: Array[Metrics]^{})

object Cells:
  def of[text: Textual { type Result = Char }](lines: Array[Array[text]^{}]^{})
    ( using metrics: Text is Measurable )
  :   Cells[text] =

    val metrics: Array[Metrics]^{} =
      Array.from(lines.readable.indices.map { index => Columnar.metrics(lines.readable(index)) })

    Cells(lines, metrics)

  // A row phrased through its columns: each cell's text, cut into lines, and measured.
  def of[row, text: {ClassTag, Textual { type Result = Char }}]
    ( columns: Array[Column[row, text]]^{}, row: row )
    ( using metrics: Text is Measurable )
  :   Cells[text] =

    of(columns.map[Array[text]^{}] { column => column.get(row).lines.to[Array] })

object Layout:
  // Empty aggregates, one per column, for a layout that has admitted nothing yet.
  def nothing[row, text](columns: Array[Column[row, text]]^{}): Array[Metrics]^{} =
    Array.from(columns.readable.indices.map { _ => Metrics(0, 0) })

  def aggregate(aggregates: Array[Metrics]^{}, cells: Cells[?]): Array[Metrics]^{} =
    Array.from:
      aggregates.readable.indices.map: index =>
        aggregates.readUnchecked(index).max(cells.metrics.readUnchecked(index))

  // Solves the columns' widths at `width` from their aggregate claims: each column's flex from
  // its aggregate, the columns that can occupy no width at all dropped, and the rest allotted
  // by `Flex.solve`, with the chrome around them deducted.
  def solve[row, text: ClassTag]
    ( columns:    Array[Column[row, text]]^{},
      titles:     List[Cells[text]],
      aggregates: Array[Metrics]^{},
      width:      Int,
      style:      TableStyle )
    ( using metrics: Text is Measurable, attenuation: Attenuation^ )
  :   Layout[row, text] =

    val flexes: IndexedSeq[Flex] =
      columns.readable.indices.map: index =>
        columns.readUnchecked(index).sizing.flex(aggregates.readUnchecked(index), width)

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

    Layout(columns, titles, aggregates, width, survivors, style)

// A table's settled column widths, and the aggregate claims they were solved from: the value
// between measuring a table and rendering it. Immutable, and incremental: a row phrased as
// `Cells` is tested by `accommodates` — within every column's aggregate, it cannot move a
// column — and admitted by `extend`, which yields the same layout when nothing moves and a
// re-solved one otherwise; `stable` says whether the re-solve changed any width. Rows are then
// rendered one at a time against the layout, and a row's height is known without rendering it
// when none of its cells wraps.
case class Layout[row, text: ClassTag]
  ( columns:    Array[Column[row, text]]^{},
    titles:     List[Cells[text]],
    aggregates: Array[Metrics]^{},
    width:      Int,
    survivors:  IndexedSeq[(Int, Int)],
    style:      TableStyle ):

  lazy val widths: Array[Int]^{} = Array.from(survivors.map(_(1)))

  def cells(row: row)(using metrics: Text is Measurable, textual: text is Textual { type Result = Char })
  :   Cells[text] =

    Cells.of(columns, row)

  def accommodates(cells: Cells[text]): Boolean =
    columns.readable.indices.all: index =>
      columns.readUnchecked(index).sizing.accommodates
        (aggregates.readUnchecked(index), cells.metrics.readUnchecked(index))

  // This layout, if the row cannot move a column; else the aggregates widened by the row's
  // cells and the widths re-solved at the same width.
  def extend(cells: Cells[text])(using metrics: Text is Measurable, attenuation: Attenuation^): Layout[row, text] =
    if accommodates(cells) then this
    else Layout.solve(columns, titles, Layout.aggregate(aggregates, cells), width, style)

  def resize(width: Int)(using metrics: Text is Measurable, attenuation: Attenuation^): Layout[row, text] =
    Layout.solve(columns, titles, aggregates, width, style)

  // Whether two layouts show the same columns at the same widths.
  def stable(that: Layout[row, text]): Boolean = survivors == that.survivors

  def row(cells: Cells[text], decorations: List[Optional[text -> text]])
    ( using metrics: Text is Measurable, textual: text is Textual { type Result = Char }, hyphenation: Hyphenation )
  :   TableRow[text] =

    val tableCells = Array.from:
      survivors.map: (index, cellWidth) =>
        val column = columns.readUnchecked(index)
        val lines = column.sizing.fit[text](cells.lines.readable(index), cellWidth, column.textAlign)
        val decoration: Optional[text -> text] = decorations.at(index.z)

        TableCell
          ( cellWidth, 1, lines, lines.size, column.textAlign, column.verticalAlign,
            decoration )

    val height =
      if survivors.isEmpty then 0 else tableCells.readable.maxBy(_.minHeight).minHeight

    TableRow(tableCells, false, height)

  def lines(cells: Cells[text], decorations: List[Optional[text -> text]])
    ( using metrics: Text is Measurable, textual: text is Textual { type Result = Char }, hyphenation: Hyphenation )
  :   List[text] =

    List.from(Grid.rowLines(style, widths, row(cells, decorations)))

  // A row's height in lines, without rendering it where that is possible: a cell whose natural
  // width fits its column is not wrapped, so it stands as many lines as it has; only a cell
  // that must wrap is fitted to find out.
  def height(cells: Cells[text])
    ( using metrics: Text is Measurable, textual: text is Textual { type Result = Char }, hyphenation: Hyphenation )
  :   Int =

    survivors.map: (index, cellWidth) =>
      val column = columns.readUnchecked(index)
      val cellLines = cells.lines.readable(index)

      if cells.metrics.readUnchecked(index).natural <= cellWidth then cellLines.length
      else column.sizing.fit[text](cellLines, cellWidth, column.textAlign).size

    . maxOption.getOrElse(0)

  def titleLines(using metrics: Text is Measurable, textual: text is Textual { type Result = Char }, hyphenation: Hyphenation)
  :   List[text] =

    titles.bind { cells => lines(cells, Nil) }

  def topRule(using metrics: Text is Measurable, textual: text is Textual { type Result = Char }): Optional[text] =
    if style.topLine.absent then Unset else Grid.rule(style, widths, above = false, below = true)

  def titleRule(using metrics: Text is Measurable, textual: text is Textual { type Result = Char }): text =
    Grid.rule(style, widths, above = true, below = true)

  def bottomRule(using metrics: Text is Measurable, textual: text is Textual { type Result = Char }): Optional[text] =
    if style.bottomLine.absent then Unset else Grid.rule(style, widths, above = true, below = false)
