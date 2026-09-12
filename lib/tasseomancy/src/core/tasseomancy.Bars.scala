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

object Bars:
  case class Fit(bands: Bands, ordinate: Scale)

  given series: [x: Categorical, y: {Continuous, Calibration}]
  =>  Series[x, y] is Plottable in Bars to Bars.Fit =
    plottable[Series[x, y], y]: series => List(columnOf(series))

  // The traversal evidence comes first: it is what determines `x` and `y`, which the evidence
  // after it is then resolved for.
  given traversable: [collection, x, y]
  =>  ( traversable: collection is Traversable by Series[x, y] )
  =>  ( categorical: x is Categorical, continuous: y is Continuous, calibration: y is Calibration )
  =>  collection is Plottable in Bars to Bars.Fit =
    plottable[collection, y]: collection =>
      List.from(traversable.traverse(collection).map(columnOf(_)))

  private def plottable[data, y: {Continuous as continuous, Calibration as calibration}]
    ( columns: data -> List[Column] )
  :   data is Plottable in Bars to Bars.Fit =

    new Plottable:
      type Self = data
      type Form = Bars
      type Result = Bars.Fit

      def fit(form: Bars, data: data): Bars.Fit =
        val all = columns(data)

        val extent = all.fold(Extent.empty): (acc, column) =>
          column.marks.fold(acc): (acc2, mark) => acc2.include(mark.y).include(mark.bounds)

        val scale =
          form.ordinate.or(calibration)
          . scale(extent.lowerOr0, extent.upperOr1, true, continuous.spacing, continuous.labelling)

        Bars.Fit(Bands(categories(all)), scale)

      def accommodates(form: Bars, fit: Bars.Fit, data: data): Boolean =
        columns(data).all: column =>
          column.marks.all: mark =>
            val within = mark.bounds.lay(true): (low, high) =>
              fit.ordinate.accommodates(low) && fit.ordinate.accommodates(high)

            fit.bands.index(mark.category).present && fit.ordinate.accommodates(mark.y) && within

      def draw(form: Bars, data: data, fit: Bars.Fit)
        ( using style: Chart.Style, palette: ChartPalette, metric: FontMetric )
      :   Chart.Drawing =

        val all = columns(data)
        val names = all.map(_.name)
        val layout = Cartesian.layout(fit.ordinate, names)
        val frame = layout.frame
        val total = countOf(all).max(1)
        val groupWidth = frame.width*fit.bands.width*(1.0 - style.barGap)
        val barWidth = groupWidth/total
        val zero = frame.y(fit.ordinate.unit(0.0))
        var index = 0

        val seriesParts = all.map: column =>
          val color = filled(palette.color(index))

          val figures = column.marks.fold(List[Figure]()): (acc, mark) =>
            fit.bands.index(mark.category).lay(acc): band =>
              val x0 = frame.x(fit.bands.centre(band)) - groupWidth/2.0 + index*barWidth
              val y = frame.y(fit.ordinate.unit(mark.y))
              val corner = point(x0, y.min(zero))
              val bar = Rectangle(corner, barWidth.toFloat, (y - zero).abs.toFloat, style = color)

              val errors = mark.bounds.lay(Nil): (low, high) =>
                val top = frame.y(fit.ordinate.unit(high))
                val bottom = frame.y(fit.ordinate.unit(low))
                errorBar(x0 + barWidth/2.0, top, bottom, barWidth/4.0)

              errors.reverse + (bar :: acc)

          val part = seriesId(index) -> Group(figures.reverse, id = seriesId(index))
          index += 1
          part

        val legend = legendPart(layout.legend, names)
        Cartesian.drawing(axes(frame, fit.bands, fit.ordinate) + seriesParts + legend)

// Bars grouped by category: one bar per series within each category's band, from zero to the
// value, with error bars where the value carries an interval. The ordinate is anchored at zero,
// since a bar's length is its meaning.
case class Bars(ordinate: Optional[Calibration] = Unset)
