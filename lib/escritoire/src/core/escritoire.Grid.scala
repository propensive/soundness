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

import language.experimental.pureFunctions

import scala.collection.immutable as sci

import anticipation.*
import fulminate.*
import gossamer.*
import hieroglyph.*
import rudiments.*
import spectacular.*
import symbolism.*
import vacuous.*

object Grid:
  given printable: [text: {Textual, Printable as printable}] => (Text is Measurable)
  =>  Grid[text] is Printable =

    (layout, termcap) =>
      layout.render.map(printable.print(_, termcap)).join(t"\n")

case class Grid[text](sections: List[TableSection[text]], style: TableStyle):
  def render(using metrics: Text is Measurable, textual: text is Textual): LazyList[text] =
    val pad = t" "*style.padding
    val leftEdge = Textual(t"${style.charset(top = style.sideLines, bottom = style.sideLines)}$pad")

    val rightEdge =
      Textual(t"$pad${style.charset(top = style.sideLines, bottom = style.sideLines)}")

    val midEdge =
      Textual(t"$pad${style.charset(top = style.innerLines, bottom = style.innerLines)}$pad")

    def recur(widths: IArray[Int], rows: LazyList[TableRow[text]]): LazyList[text] =
      rows match
        case row #:: tail =>
          val lines = (0 until row.height).map: lineNumber =>
            widths.indices.map: index =>
              val cell = row(index)

              if cell.minHeight > lineNumber
              then
                cell.textAlign.pad
                  ( cell(lineNumber), widths(index), lineNumber == cell.minHeight - 1 )

              else
                Textual((t" "*widths(index)))

            . join(leftEdge, midEdge, rightEdge)

          lines.to(LazyList) #::: recur(widths, tail)

        case _ =>
          LazyList()

    def rule(above: Optional[IArray[Int]], below: Optional[IArray[Int]]): text =
      val width = above.or(below).vouch.pipe: widths =>
        widths.sum + style.cost(widths.length)

      val ascenders =
        above.let(_.scan(0)(_ + _ + style.padding*2 + 1).to(sci.BitSet)).or(sci.BitSet())

      val descenders =
        below.let(_.scan(0)(_ + _ + style.padding*2 + 1).to(sci.BitSet)).or(sci.BitSet())

      val horizontal =
        if above.absent then style.topLine
        else if below.absent then style.bottomLine
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
      if style.topLine.absent then LazyList() else LazyList(rule(Unset, sections.head.widths))

    val midRule = rule(sections.head.widths, sections.head.widths)

    val bottomLine =
      if style.bottomLine.absent then LazyList() else LazyList(rule(sections.head.widths, Unset))

    val body =
      sections.to(LazyList).flatMap: section => midRule #:: recur(section.widths, section.rows)

    topLine #::: body.tail #::: bottomLine
