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
package tasseomancy

import anticipation.*
import gossamer.*
import prepositional.*
import rudiments.*
import savagery.*
import spectacular.*
import vacuous.*

object Chart:
  enum Legend:
    case Hidden, Right, Bottom

  object Style:
    given standard: Style = Style()

  // Everything about a chart's appearance that is not the data: its size, its lines and markers,
  // its type, how densely its axes are graduated (`pitch` is the least spacing between
  // gradations, in pixels), and where its legend goes. Colours are the `ChartPalette`'s.
  case class Style
    ( width:         Double         = 640.0,
      height:        Double         = 400.0,
      inset:         Double         = 12.0,
      strokeWidth:   Double         = 2.0,
      markerRadius:  Double         = 3.5,
      markers:       Boolean        = false,
      fontFamily:    Text           = t"sans-serif",
      fontSize:      Double         = 12.0,
      tickLength:    Double         = 5.0,
      pitch:         Double         = 60.0,
      grid:          Boolean        = true,
      legend:        Legend         = Legend.Right,
      barGap:        Double         = 0.25,
      abscissaTitle: Optional[Text] = Unset,
      ordinateTitle: Optional[Text] = Unset )

  // A rendered chart as its parts, each under a stable identifier — `grid`, `abscissa`,
  // `ordinate`, `legend`, `series-0`, `series-1`, … — in drawing order. The identifiers are what
  // a revision names.
  case class Drawing
    ( width: Double, height: Double, defs: List[Svg.Def], parts: Ledger[Svg.Id, Figure] ):
    def svg: Svg = Svg(width.toFloat, height.toFloat, defs, parts.values)

  // What changed between one drawing of a chart and the next: either the whole chart, because
  // the axes moved to fit the new data, or only the identified parts that differ. A page holding
  // the SVG needs only to replace an element by its identifier, or the whole element.
  enum Revision:
    case Redraw(svg: Svg)
    case Replace(id: Svg.Id, figure: Figure)

// Data, a chart kind that can draw it, and the fit of the one to the other. Immutable: `revise`
// yields a new chart for new data along with what has to change on the page, keeping the fit
// whenever it still accommodates the data so that the axes hold still.
class Chart[data, form, fit](val data: data, val form: form, val fit: fit)
  ( using val plottable: data is Plottable in form to fit ):

  def drawing(using Chart.Style, ChartPalette, FontMetric): Chart.Drawing =
    plottable.draw(form, data, fit)

  def svg(using Chart.Style, ChartPalette, FontMetric): Svg = drawing.svg

  def revise(data2: data)(using Chart.Style, ChartPalette, FontMetric)
  :   (Chart[data, form, fit], List[Chart.Revision]) =

    if plottable.accommodates(form, fit, data2) then
      val next = Chart(data2, form, fit)

      val before: List[(Svg.Id, Text)] = drawing.parts.fold(List[(Svg.Id, Text)]()): (acc, pair) =>
        (pair(0), pair(1).xml.show) :: acc

      val revisions = next.drawing.parts.fold(List[Chart.Revision]()): (acc, pair) =>
        val rendered = pair(1).xml.show
        val unchanged = before.exists: (id, text) => id == pair(0) && text == rendered
        if unchanged then acc else Chart.Revision.Replace(pair(0), pair(1)) :: acc

      (next, revisions.reverse)
    else
      val next = Chart(data2, form, plottable.fit(form, data2))
      (next, List(Chart.Revision.Redraw(next.svg)))
