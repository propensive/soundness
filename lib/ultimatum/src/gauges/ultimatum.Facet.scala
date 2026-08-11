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
package ultimatum

import anticipation.*
import escapade.*
import gossamer.*
import symbolism.*

object Facet:
  // A part whose width is fixed by its own content.
  def fixed(shed: Int, content: Teletype)(using gauging: Gauging): Facet =
    Facet(shed, gauging.cells(content.plain), false, _ => content)

  // The part that absorbs whatever width is left. At most one facet in a row may be flexible; it
  // is never shed, because a row that has dropped its gauge is not worth drawing.
  def flexible(minWidth: Int)(render: Int -> Teletype): Facet =
    Facet(Int.MinValue, minWidth, true, render)

  // Lay `facets` out into exactly `width` cells, separated by `gap` spaces: drop parts in
  // descending `shed` order until what remains fits, then hand the surplus to the flexible part.
  // This is escritoire's column-negotiation idea — a declared minimum, a droppable part, a
  // flexible remainder — without its binary search on slack: a gauge's parts have exact widths and
  // never reflow, so one greedy pass is exact, and there is no gas budget to run out of.
  // Total by construction: below every minimum, the result is `width` spaces.
  def solve(facets: List[Facet], width: Int, gap: Int = 1)(using gauging: Gauging): Teletype =
    def extent(kept: scala.List[Facet]): Int =
      kept.map(_.minWidth).sum + gap*(kept.length - 1).max(0)

    // Shed the most expendable part first; ties keep source order, so a row degrades the same way
    // every time rather than flickering between two arrangements at a boundary.
    def shed(kept: scala.List[Facet]): scala.List[Facet] =
      if extent(kept) <= width || kept.length <= 1 then kept else
        val worst = kept.filter(!_.flexible).map(_.shed).maxOption

        worst.map: value =>
          val index = kept.indexWhere: facet => !facet.flexible && facet.shed == value

          if index < 0 then kept else shed(kept.patch(index, scala.Nil, 1))

        . getOrElse(kept)

    val kept = shed(facets.stdlib.toList)

    if kept.isEmpty || extent(kept) > width then Teletype(t" "*width.max(0)) else
      val surplus = width - extent(kept)

      val parts = kept.map: facet =>
        facet.render(facet.minWidth + (if facet.flexible then surplus else 0))

      val joined = parts.reduceLeft: (left, right) =>
        e"$left${t" "*gap}$right"

      val used = gauging.cells(joined.plain)

      if used >= width then joined else e"$joined${t" "*(width - used)}"

// One horizontal part of a composite gauge row — a caption, the bar itself, a percentage, a rate,
// an estimate. `shed` orders the parts by how readily they are given up when the row will not fit:
// the highest value goes first. `render` is handed the width the part actually got, which for the
// flexible part is more than its minimum.
// `render` is a *pure* function of the width the part got: a design captures nothing, so that the
// pane tree it ends up inside stays pure under capture checking.
case class Facet(shed: Int, minWidth: Int, flexible: Boolean, render: Int -> Teletype)
