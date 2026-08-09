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

import anticipation.*
import fulminate.*
import gossamer.*
import hieroglyph.*
import proscenium.compat.*
import rudiments.*
import spectacular.*
import symbolism.*
import vacuous.*

object Grid:
  given printable: [text: {Textual, Printable as printable}] => (Text is Measurable)
  =>  Grid[text] is Printable =

    (layout, termcap) => layout.render.map(printable.print(_, termcap)).join(t"\n")

case class Grid[text](sections: List[TableSection[text]], style: TableStyle):
  def render(using metrics: Text is Measurable, textual: text is Textual): Chain[text] =
    val pad = t" "*style.padding
    val leftEdge = Textual(t"${style.charset(top = style.sideLines, bottom = style.sideLines)}$pad")

    val rightEdge =
      Textual(t"$pad${style.charset(top = style.sideLines, bottom = style.sideLines)}")

    val midEdge =
      Textual(t"$pad${style.charset(top = style.innerLines, bottom = style.innerLines)}$pad")

    def recur(widths: Array[Int]^{}, rows: Chain[TableRow[text]]): Chain[text] =
      rows match
        case row #:: tail =>
          val lines = (0 until row.height).map: lineNumber =>
            widths.indices.map: index =>
              val cell = row(index)

              if cell.minHeight > lineNumber
              then
                cell.textAlign.pad
                  ( cell(lineNumber), widths.readUnchecked(index), lineNumber == cell.minHeight - 1 )

              else
                Textual((t" "*widths.readUnchecked(index)))

            . join(leftEdge, midEdge, rightEdge)

          lines.to(Chain) #::: recur(widths, tail)

        case _ =>
          Chain()

    // Every rule adjoins at least one row of columns, whose shared `widths` it takes
    // unconditionally; `above`/`below` say which side(s) those columns are on.
    def rule(widths: Array[Int]^{}, above: Boolean, below: Boolean): text =
      val width = widths.sum + style.cost(widths.length)

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

    val topLine =
      if style.topLine.absent then Chain() else
        Chain(rule(sections.stdlib.head.widths, above = false, below = true))

    val midRule = rule(sections.stdlib.head.widths, above = true, below = true)

    val bottomLine =
      if style.bottomLine.absent then Chain() else
        Chain(rule(sections.stdlib.head.widths, above = true, below = false))

    val body =
      sections.stdlib.to(Chain).bind: section =>
        (midRule #:: recur(section.widths, section.rows)): Chain[text]

    topLine #::: body.tail #::: bottomLine
