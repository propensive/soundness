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

import scala.language.experimental.pureFunctions

import scala.collection.immutable as sci
import scala.collection.immutable.IndexedSeq

import anticipation.*
import fulminate.*
import gossamer.*
import hieroglyph.*
import rudiments.*
import spectacular.*
import symbolism.*
import tessellate.*
import vacuous.*

object Grid:
  given printable: [text]
  =>  ( textual: text is Textual { type Result = Char }, printable: text is Printable )
  =>  ( Text is Measurable )
  =>  Grid[text] is Printable =

    (layout, termcap) => layout.render.map(printable.print(_, termcap)).join(t"\n")

  // One row's visual lines against the widths its columns settled on: `row.height` of them,
  // each cell padded to its width and decorated, joined by the rules' edge glyphs.
  //
  // The edges are BARE rule glyphs: the gutter padding belongs to each cell, so that a
  // cell's decoration (a background, say) covers its whole width — padding included —
  // while the rules stay undecorated. Line widths are unchanged: the padding moved, it
  // was not added, so the `rule` joint arithmetic below is unaffected.
  def rowLines[text](style: TableStyle, widths: Array[Int]^{}, row: TableRow[text])
    ( using metrics: Text is Measurable, textual: text is Textual { type Result = Char } )
  :   IndexedSeq[text] =

    val pad = t" "*style.padding
    val leftEdge = Textual(t"${style.charset(top = style.sideLines, bottom = style.sideLines)}")
    val rightEdge = leftEdge
    val midEdge = Textual(t"${style.charset(top = style.innerLines, bottom = style.innerLines)}")

    (0 until row.height).map: lineNumber =>
      widths.readable.indices.map: index =>
        val cell = row(index)

        val offset = cell.verticalAlign match
          case VerticalAlignment.Top    => 0
          case VerticalAlignment.Middle => (row.height - cell.minHeight)/2
          case VerticalAlignment.Bottom => row.height - cell.minHeight

        val line = lineNumber - offset

        val body: text =
          if line >= 0 && line < cell.minHeight
          then
            cell.textAlign.pad
              ( cell(line), widths.readUnchecked(index), line == cell.minHeight - 1 )

          else
            Textual((t" "*widths.readUnchecked(index)))

        val padded: text = textual.concat(textual.concat(Textual(pad), body), Textual(pad))
        cell.decorate.lay(padded) { decoration => decoration(padded) }

      . join(leftEdge, midEdge, rightEdge)

  // A horizontal rule across columns of the given widths. Every rule adjoins at least one
  // row of columns, whose shared `widths` it takes unconditionally; `above`/`below` say which
  // side(s) those columns are on.
  def rule[text](style: TableStyle, widths: Array[Int]^{}, above: Boolean, below: Boolean)
    ( using metrics: Text is Measurable, textual: text is Textual { type Result = Char } )
  :   text =

    val width = widths.total + style.cost(widths.length)

    def joints: sci.BitSet = widths.readable.scan(0)(_ + _ + style.padding*2 + 1).to(sci.BitSet)

    val ascenders = if above then joints else sci.BitSet()
    val descenders = if below then joints else sci.BitSet()

    val horizontal =
      if !above then style.topLine
      else if !below then style.bottomLine
      else style.titleLine

    Textual:
      Text.fill(width): index =>
        def vertical(bitSet: sci.BitSet, line: BoxLine): BoxLine =
          if bitSet.has(index) then line else BoxLine.Blank

        if index == 0 then
          style.charset
            ( top    = vertical(ascenders, style.sideLines),
              right  = horizontal.or(BoxLine.Blank),
              bottom = vertical(descenders, style.sideLines),
              left   = BoxLine.Blank )
        else if index == (width - 1) then
          style.charset
            ( top    = vertical(ascenders, style.sideLines),
              right  = BoxLine.Blank,
              bottom = vertical(descenders, style.sideLines),
              left   = horizontal.or(BoxLine.Blank) )
        else
          style.charset
            ( top    = vertical(ascenders, style.innerLines),
              right  = horizontal.or(BoxLine.Blank),
              bottom = vertical(descenders, style.innerLines),
              left   = horizontal.or(BoxLine.Blank) )

case class Grid[text](sections: List[TableSection[text]], style: TableStyle):
  def render
    ( using metrics: Text is Measurable, textual: text is Textual { type Result = Char } )
  :   Chain[text] =

    def recur(widths: Array[Int]^{}, rows: Chain[TableRow[text]]): Chain[text] =
      rows match
        case row #:: tail => Grid.rowLines(style, widths, row).to(Chain) #::: recur(widths, tail)
        case _            => Chain()

    sections match
      case first :: _ =>
        val topLine =
          if style.topLine.absent then Chain() else
            Chain(Grid.rule(style, first.widths, above = false, below = true))

        val midRule = Grid.rule(style, first.widths, above = true, below = true)

        val bottomLine =
          if style.bottomLine.absent then Chain() else
            Chain(Grid.rule(style, first.widths, above = true, below = false))

        val body =
          sections.to[Chain].bind: section =>
            (midRule #:: recur(section.widths, section.rows)): Chain[text]

        // Every section's block starts with a `midRule`; the first one is dropped because the
        // grid's top edge is `topLine`'s responsibility.
        val trunk = body match
          case _ #:: rest => rest
          case _          => Chain()

        topLine #::: trunk #::: bottomLine

      case _ =>
        Chain()
