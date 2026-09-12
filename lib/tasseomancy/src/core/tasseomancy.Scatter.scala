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

object Scatter:
  case class Fit(abscissa: Scale, ordinate: Scale)

  given series: [x: {Continuous, Calibration}, y: {Continuous, Calibration}]
  =>  Series[x, y] is Plottable in Scatter to Scatter.Fit =
    plottable[Series[x, y], x, y]: series => List(traceOf(series))

  given traversable: [collection, x, y]
  =>  ( traversable: collection is Traversable by Series[x, y] )
  =>  ( continuousX: x is Continuous, calibrationX: x is Calibration )
  =>  ( continuousY: y is Continuous, calibrationY: y is Calibration )
  =>  collection is Plottable in Scatter to Scatter.Fit =
    plottable[collection, x, y]: collection =>
      List.from(traversable.traverse(collection).map(traceOf(_)))

  private def plottable[data, x: {Continuous, Calibration}, y: {Continuous, Calibration}]
    ( traces: data -> List[Trace] )
  :   data is Plottable in Scatter to Scatter.Fit =

    new Plottable:
      type Self = data
      type Form = Scatter
      type Result = Scatter.Fit

      def fit(form: Scatter, data: data): Scatter.Fit =
        val (abscissa, ordinate) =
          Lines.fitTraces[x, y](traces(data), form.abscissa, form.ordinate)

        Scatter.Fit(abscissa, ordinate)

      def accommodates(form: Scatter, fit: Scatter.Fit, data: data): Boolean =
        Lines.accommodatesTraces(traces(data), fit.abscissa, fit.ordinate)

      def draw(form: Scatter, data: data, fit: Scatter.Fit)
        ( using style: Chart.Style, palette: ChartPalette, metric: FontMetric )
      :   Chart.Drawing =

        val all = traces(data)
        val names = all.map(_.name)
        val layout = Cartesian.layout(fit.abscissa, fit.ordinate, names)
        val frame = layout.frame
        var index = 0

        val seriesParts = all.map: trace =>
          val color = palette.color(index)

          val figures = trace.data.fold(List[Figure]()): (acc, datum) =>
            val x = frame.x(fit.abscissa.unit(datum.x))
            val y = frame.y(fit.ordinate.unit(datum.y))
            val marker = Circle(point(x, y), style.markerRadius.toFloat, style = filled(color))

            val errors = datum.bounds.lay(Nil): (low, high) =>
              val top = frame.y(fit.ordinate.unit(high))
              val bottom = frame.y(fit.ordinate.unit(low))
              errorBar(x, top, bottom, style.markerRadius)

            errors.reverse + (marker :: acc)

          val part = seriesId(index) -> Group(figures.reverse, id = seriesId(index))
          index += 1
          part

        val legend = legendPart(layout.legend, names)
        Cartesian.drawing(axes(frame, fit.abscissa, fit.ordinate) + seriesParts + legend)

// A marker per point, in no particular order, with error bars where the values carry intervals.
// The same fit as a line chart; only the drawing differs.
case class Scatter
  ( abscissa: Optional[Calibration] = Unset, ordinate: Optional[Calibration] = Unset )
