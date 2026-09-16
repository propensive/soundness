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


  // The layout of every column at `width`, from the titles' and every row's claims.
  def layout(width: Int)
    ( using style: TableStyle, metrics: Text is Measurable )
    ( using textual: text is Textual { type Result = Char } )
    ( using attenuation: Attenuation^ )
  :   Layout[Row, text] =

    val titleCells: List[Cells[text]] = titles.map(Cells.of(_))
    val rowCells: List[Cells[text]] = rows.map(Cells.of(_))
    val aggregates = (titleCells + rowCells).fold(Layout.nothing(columns))(Layout.aggregate(_, _))
    Layout.solve(columns, titleCells, aggregates, width, style)

  def grid(width: Int)
    ( using style: TableStyle, metrics: Text is Measurable )
    ( using textual: text is Textual { type Result = Char } )
    ( using attenuation: Attenuation^, hyphenation: polysyllabic.Hyphenation )
  :   Grid[text] =

    val titleCells: List[Cells[text]] = titles.map(Cells.of(_))
    val rowCells: List[Cells[text]] = rows.map(Cells.of(_))
    val aggregates = (titleCells + rowCells).fold(Layout.nothing(columns))(Layout.aggregate(_, _))
    val layout = Layout.solve(columns, titleCells, aggregates, width, style)

    def lines(data: List[Cells[text]], decorations2: List[List[Optional[text -> text]]])
    :   Chain[TableRow[text]] =

      // No native iterator: the decorations are consumed one row at a time alongside `data`,
      // which is shorter or longer at will, so this is a `zipAll`, not a `zip`.
      val decorationIterator = decorations2.stdlib.iterator

      data.to[Chain].map: cells =>
        val rowDecorations: List[Optional[text -> text]] =
          if decorationIterator.hasNext then decorationIterator.next() else Nil

        layout.row(cells, rowDecorations)

    Grid
      ( List(TableSection(layout.widths, lines(titleCells, Nil)), TableSection(layout.widths, lines(rowCells, decorations))),
        style )
