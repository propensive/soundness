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
import denominative.*
import murmuration.Traversable
import murmuration.sortingAlgorithms.timsort
import prepositional.*
import rudiments.*
import savagery.*
import symbolism.*
import vacuous.*

object Lines:
  case class Fit(abscissa: Scale, ordinate: Scale)

  given series: [x: {Continuous, Calibration}, y: {Continuous, Calibration}]
  =>  Series[x, y] is Plottable in Lines to Lines.Fit =
    plottable[Series[x, y], x, y]: series => List(traceOf(series))

  given traversable: [collection, x, y]
  =>  ( traversable: collection is Traversable by Series[x, y] )
  =>  ( continuousX: x is Continuous, calibrationX: x is Calibration )
  =>  ( continuousY: y is Continuous, calibrationY: y is Calibration )
  =>  collection is Plottable in Lines to Lines.Fit =
    plottable[collection, x, y]: collection =>
      List.from(traversable.traverse(collection).map(traceOf(_)))

  // The fit shared with a scatter plot: each axis over the extent of its positions (and, on the
  // ordinate, of their intervals), neither anchored at zero.
  private[tasseomancy] def fitTraces
    [ x: {Continuous as cx, Calibration as kx}, y: {Continuous as cy, Calibration as ky} ]
    ( traces: List[Trace], abscissa: Optional[Calibration], ordinate: Optional[Calibration] )
  :   (Scale, Scale) =

    val (xs, ys) = traces.fold((Extent.empty, Extent.empty)): (acc, trace) =>
      trace.data.fold(acc): (acc2, datum) =>
        (acc2(0).include(datum.x), acc2(1).include(datum.y).include(datum.bounds))

    val xScale = abscissa.or(kx).scale(xs.lowerOr0, xs.upperOr1, false, cx.spacing, cx.labelling)
    val yScale = ordinate.or(ky).scale(ys.lowerOr0, ys.upperOr1, false, cy.spacing, cy.labelling)
    (xScale, yScale)

  private[tasseomancy] def accommodatesTraces
    ( traces: List[Trace], abscissa: Scale, ordinate: Scale )
  :   Boolean =

    traces.all: trace =>
      trace.data.all: datum =>
        val within = datum.bounds.lay(true): (low, high) =>
          ordinate.accommodates(low) && ordinate.accommodates(high)

        abscissa.accommodates(datum.x) && ordinate.accommodates(datum.y) && within

  private def plottable[data, x: {Continuous, Calibration}, y: {Continuous, Calibration}]
    ( traces: data -> List[Trace] )
  :   data is Plottable in Lines to Lines.Fit =

    new Plottable:
      type Self = data
      type Form = Lines
      type Result = Lines.Fit

      def fit(form: Lines, data: data): Lines.Fit =
        val (abscissa, ordinate) = fitTraces[x, y](traces(data), form.abscissa, form.ordinate)
        Lines.Fit(abscissa, ordinate)

      def accommodates(form: Lines, fit: Lines.Fit, data: data): Boolean =
        accommodatesTraces(traces(data), fit.abscissa, fit.ordinate)

      def draw(form: Lines, data: data, fit: Lines.Fit)
        ( using style: Chart.Style, palette: ChartPalette, metric: FontMetric )
      :   Chart.Drawing =

        val all = traces(data)
        val names = all.map(_.name)
        val layout = Cartesian.layout(fit.ordinate, names)
        val frame = layout.frame
        var index = 0

        def at(datum: Datum, value: Double): Point =
          point(frame.x(fit.abscissa.unit(datum.x)), frame.y(fit.ordinate.unit(value)))

        val seriesParts = all.map: trace =>
          val sorted: Sequence[Datum] = trace.data.order(_.x)
          val color = palette.color(index)

          val points: List[Point] =
            sorted.fold(List[Point]()) { (acc, datum) => at(datum, datum.y) :: acc }.reverse

          val (highs, lows) = sorted.fold((List[Point](), List[Point]())): (acc, datum) =>
            datum.bounds.lay(acc): (low, high) =>
              (at(datum, high) :: acc(0), at(datum, low) :: acc(1))

          val band: List[Figure] =
            if highs.nil then Nil
            else List(Polyline(highs.reverse + lows, closed = true, style = filled(color, 0.2)))

          val line = Polyline(points, style = stroked(color, style.strokeWidth))

          val radius = style.markerRadius.toFloat

          val markers: List[Figure] =
            if !style.markers then Nil
            else points.map: point => Circle(point, radius, style = filled(color))

          val part = seriesId(index) -> Group(band + (line :: markers), id = seriesId(index))
          index += 1
          part

        val legend = legendPart(layout.legend, names)
        Cartesian.drawing(axes(frame, fit.abscissa, fit.ordinate) + seriesParts + legend)

// A line per series through its points in abscissa order, with a translucent band where the
// values carry intervals, and markers at the points if the style asks for them. Neither axis is
// anchored at zero: a line shows change, not magnitude.
case class Lines(abscissa: Optional[Calibration] = Unset, ordinate: Optional[Calibration] = Unset)
