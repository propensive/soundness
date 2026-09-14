                                                                                                  /*
┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓
┃                                                                                                  ┃
┃                                                   ╭───╮                                          ┃
┃                                                   │   │                                          ┃
┃                                                   │   │                                          ┃
┃   ╭───────╮╭─────────╮╭───╮ ╭───╮╭───╮╌────╮╭────╌┄│   │╭──┄┄╌╮╭───╮╌────╮╭────────╮╭───────╮   ┃
┃   │   ╭───╯│   ╭─╮   ││   │ │   ││   ╭─╮   ││   ╭─╮│   ││   ╭─╮││   ╭─╮   ││   ╭─╮  ││   ╭───╯   ┃
┃   │   ╰───╮│   │ │   ││   │ │   ││   │ │   ││   │ ││   ││   ╰─╯││   │ │   ││   ╰─╯  ││   ╰───╮   ┃
┃   ╰───╮   ││   │ │   ││   │ │   ││   │ │   ││   │ ││   ││   ╭──╯│   │ │   ││   ╭────╯╰───╮   │   ┃
┃   ╭───╯   ││   ╰─╯   ││   ╰─╯   ││   │ │   ││   ╰─╯│   ││   ╰─╮ │   │ │   ││   ╰────╮╭───╯   │   ┃
┃   ╰───────╯╰─────────╯╰────╌╰───╯╰───╯ ╰───╯╰────╌┄╰───╯╰─────╯ ╰───╯ ╰───╯╰────────╯╰───────╯   ┃
┃                                                                                                  ┃
┃    Soundness, version 0.65.0.                                                                    ┃
┃    © Copyright 2021-26 Jon Pretty, Propensive OÜ.                                                ┃
┃                                                                                                  ┃
┃    The primary distribution site is:                                                             ┃
┃                                                                                                  ┃
┃        https://soundness.dev/                                                                    ┃
┃                                                                                                  ┃
┃    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file     ┃
┃    except in compliance with the License. You may obtain a copy of the License at               ┃
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
import gossamer.*
import hieroglyph.*
import rudiments.*
import symbolism.*
import tessellate.*

// A flat list arranged in columns, as `ls` lists a directory: as many columns as the width
// admits, each item in one cell, no rules and no titles.
//
// The column count is the greatest for which every column's content fits alongside the
// others, with `gap` cells between neighbours. Equal-width columns — every column as wide as
// the widest item — are preferred; unless `uniform` is set, a layout is also admitted where
// some columns are narrower, when the items which happen to fall in them allow it, and the
// spare width is then shared back out towards equality, no column growing wider than the
// widest item. Items read across each row by default, or down each column with `downward`.
object Columnation:
  def layout[text: Textual { type Result = Char } as textual]
    ( items:    List[text],
      width:    Int,
      gap:      Int           = 2,
      uniform:  Boolean       = false,
      downward: Boolean       = false,
      align:    TextAlignment = TextAlignment.Left )
    ( using Text is Measurable )
  :   List[text] =

    val cells: IndexedSeq[text] = items.to[IndexedSeq]
    val count: Int = cells.length
    val widths: IndexedSeq[Int] = cells.map(Flow.metrics(_).natural)
    val widest: Int = widths.maxOption.getOrElse(0)

    // The position in `cells` of the item at `row` of `column`, in a layout of `columns`
    // columns of `rows` rows; not necessarily present (the last row is ragged).
    def position(columns: Int, rows: Int, column: Int, row: Int): Int =
      if downward then column*rows + row else row*columns + column

    def rowsOf(columns: Int): Int = (count + columns - 1)/columns

    // Each column's natural width: that of its widest item — or, for a uniform layout, of
    // the widest item anywhere.
    def naturals(columns: Int, rows: Int): IndexedSeq[Int] =
      (0 until columns).map: column =>
        if uniform then widest else
          (0 until rows).map(position(columns, rows, column, _)).filter(_ < count)
          . map(widths(_)).maxOption.getOrElse(0)

    // The greatest column count, at most `columns`, whose natural widths fit with their
    // gaps; a single column always does. A downward layout can leave its last columns empty
    // (five items in four columns make two rows, and fill only three), in which case the
    // count is reduced to those occupied before the fit is judged.
    def choose(columns: Int): Int =
      if columns <= 1 then 1 else
        val rows = rowsOf(columns)
        val occupied = rowsOf(rows)

        if downward && occupied < columns then choose(occupied)
        else if naturals(columns, rows).sum + gap*(columns - 1) <= width then columns
        else choose(columns - 1)

    if count == 0 then Nil else
      // No column can be narrower than one cell, which bounds the count to try first.
      val columns: Int = choose(count.min((width + gap)/(1 + gap)).max(1))
      val rows: Int = rowsOf(columns)
      val natural: IndexedSeq[Int] = naturals(columns, rows)

      // Spare width is dealt out towards equal columns, one cell at a time to whichever
      // column is narrowest (the leftmost of several), until none is narrower than the widest
      // item or nothing is spare.
      val fitted: IndexedSeq[Int] =
        def widen(widths: IndexedSeq[Int], spare: Int): IndexedSeq[Int] =
          val narrower: IndexedSeq[Int] = widths.indices.filter(widths(_) < widest)

          if spare <= 0 || narrower.isEmpty then widths else
            val index = narrower.minBy(widths(_))
            widen(widths.updated(index, widths(index) + 1), spare - 1)

        if uniform then natural else widen(natural, width - natural.sum - gap*(columns - 1))

      val spacer: text = Textual(t" "*gap)

      val lines: IndexedSeq[text] =
        (0 until rows).map: row =>
          val present: IndexedSeq[Int] =
            (0 until columns).filter: column => position(columns, rows, column, row) < count

          val last: Int = present.lastOption.getOrElse(0)

          present.map: column =>
            val cell: text = cells(position(columns, rows, column, row))

            // A left-aligned row ends where its last item does, with no trailing padding.
            if column == last && align == TextAlignment.Left then cell
            else align.pad(cell, fitted(column), true)

          . reduce: (left, right) => textual.concat(textual.concat(left, spacer), right)

      List.from(lines)
