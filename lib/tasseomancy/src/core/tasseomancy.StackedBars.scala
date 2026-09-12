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

import Cartesian.*
import murmuration.Traversable
import prepositional.*
import rudiments.*
import savagery.*
import symbolism.*
import vacuous.*

object StackedBars:
  case class Fit(bands: Bands, ordinate: Scale)

  // The running totals of a category's positive and negative values, stacked apart.
  private case class Stack(positive: Double, negative: Double)

  given traversable: [collection, x, y]
  =>  ( traversable: collection is Traversable by Series[x, y] )
  =>  ( categorical: x is Categorical, continuous: y is Continuous, calibration: y is Calibration )
  =>  collection is Plottable in StackedBars to StackedBars.Fit =
    plottable[collection, y]: collection =>
      List.from(traversable.traverse(collection).map(columnOf(_)))

  private def empty(count: Int): Sequence[Stack] =
    var stacks: Sequence[Stack] = Sequence.empty
    var index = 0

    while index < count do
      stacks = Sequence.append(stacks, Stack(0.0, 0.0))
      index += 1

    stacks

  private def totals(all: List[Column], bands: Bands): Sequence[Stack] =
    var stacks = empty(bands.count)

    all.foreach: column =>
      column.marks.foreach: mark =>
        bands.index(mark.category).let: band =>
          val stack = Sequence.at(stacks, band)

          val updated =
            if mark.y >= 0.0 then Stack(stack.positive + mark.y, stack.negative)
            else Stack(stack.positive, stack.negative + mark.y)

          stacks = Sequence.define(stacks, band, updated)

    stacks

  private def plottable[data, y: {Continuous as continuous, Calibration as calibration}]
    ( columns: data -> List[Column] )
  :   data is Plottable in StackedBars to StackedBars.Fit =

    new Plottable:
      type Self = data
      type Form = StackedBars
      type Result = StackedBars.Fit

      def fit(form: StackedBars, data: data): StackedBars.Fit =
        val all = columns(data)
        val bands = Bands(categories(all))

        val extent = totals(all, bands).fold(Extent.empty): (acc, stack) =>
          acc.include(stack.positive).include(stack.negative)

        val scale =
          form.ordinate.or(calibration)
          . scale(extent.lowerOr0, extent.upperOr1, true, continuous.spacing, continuous.labelling)

        StackedBars.Fit(bands, scale)

      def accommodates(form: StackedBars, fit: StackedBars.Fit, data: data): Boolean =
        val all = columns(data)

        val known = all.all: column =>
          column.marks.all: mark => fit.bands.index(mark.category).present

        val within = totals(all, fit.bands).all: stack =>
          fit.ordinate.accommodates(stack.positive) && fit.ordinate.accommodates(stack.negative)

        known && within

      def draw(form: StackedBars, data: data, fit: StackedBars.Fit)
        ( using style: Chart.Style, palette: ChartPalette, metric: FontMetric )
      :   Chart.Drawing =

        val all = columns(data)
        val names = all.map(_.name)
        val layout = Cartesian.layout(fit.ordinate, names)
        val frame = layout.frame
        val barWidth = frame.width*fit.bands.width*(1.0 - style.barGap)
        var stacks = empty(fit.bands.count)
        var index = 0

        val seriesParts = all.map: column =>
          val color = filled(palette.color(index))

          val figures = column.marks.fold(List[Figure]()): (acc, mark) =>
            fit.bands.index(mark.category).lay(acc): band =>
              val stack = Sequence.at(stacks, band)
              val from = if mark.y >= 0.0 then stack.positive else stack.negative
              val to = from + mark.y

              val updated =
                if mark.y >= 0.0 then Stack(to, stack.negative) else Stack(stack.positive, to)

              stacks = Sequence.define(stacks, band, updated)
              val x0 = frame.x(fit.bands.centre(band)) - barWidth/2.0
              val y0 = frame.y(fit.ordinate.unit(from))
              val y1 = frame.y(fit.ordinate.unit(to))
              val corner = point(x0, y0.min(y1))
              Rectangle(corner, barWidth.toFloat, (y1 - y0).abs.toFloat, style = color) :: acc

          val part = seriesId(index) -> Group(figures.reverse, id = seriesId(index))
          index += 1
          part

        val legend = legendPart(layout.legend, names)
        Cartesian.drawing(axes(frame, fit.bands, fit.ordinate) + seriesParts + legend)

// Bars stacked by category: each series' value sits on the total of those before it, so that a
// bar's full height is the category's total. Positive and negative values stack apart, in
// opposite directions from zero.
case class StackedBars(ordinate: Optional[Calibration] = Unset)
