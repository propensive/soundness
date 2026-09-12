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

import Framing.*
import denominative.*
import iridescence.*
import murmuration.Traversable
import murmuration.sortingAlgorithms.timsort
import prepositional.*
import rudiments.*
import savagery.*
import symbolism.*
import vacuous.*

object Lines:
  case class Fit(abscissa: Scale, ordinate: Scale)

  // The components of a line chart beyond the axes: the line through a series' points, the band
  // between their bounds, and a marker at each point if `markers` asks for one.
  trait Style extends Chart.Cartesian:
    def markers: Boolean

    def line(points: List[Point], color: Color in Srgb, series: Int): List[Figure] =
      List(Polyline(points, style = stroked(color, strokeWidth)))

    def band(points: List[Point], color: Color in Srgb, series: Int): List[Figure] =
      List(Polyline(points, closed = true, style = filled(color, 0.2)))

    def lineMarker(at: Point, color: Color in Srgb, series: Int): List[Figure] =
      if markers then marker(at, color, series) else Nil

  given series: [x: {Continuous, Calibration}, y: {Continuous, Calibration}]
  =>  Series[x, y] is Plottable in Lines to Lines.Fit by Lines.Style =
    plottable[Series[x, y], x, y]: series => List(traceOf(series))

  given traversable: [collection, x, y]
  =>  ( traversable: collection is Traversable by Series[x, y] )
  =>  ( continuousX: x is Continuous, calibrationX: x is Calibration )
  =>  ( continuousY: y is Continuous, calibrationY: y is Calibration )
  =>  collection is Plottable in Lines to Lines.Fit by Lines.Style =
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

    val xScale = abscissa.or(kx).scale(xs.lowerOr0, xs.upperOr1, false, cx.notation)
    val yScale = ordinate.or(ky).scale(ys.lowerOr0, ys.upperOr1, false, cy.notation)
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
  :   data is Plottable in Lines to Lines.Fit by Lines.Style =

    new Plottable:
      type Self = data
      type Form = Lines
      type Result = Lines.Fit
      type Operand = Lines.Style

      def fit(form: Lines, data: data): Lines.Fit =
        val (abscissa, ordinate) = fitTraces[x, y](traces(data), form.abscissa, form.ordinate)
        Lines.Fit(abscissa, ordinate)

      def accommodates(form: Lines, fit: Lines.Fit, data: data): Boolean =
        accommodatesTraces(traces(data), fit.abscissa, fit.ordinate)

      def draw(form: Lines, data: data, fit: Lines.Fit)
        ( using style: Lines.Style, palette: ChartPalette, metric: FontMetric )
      :   Chart.Drawing =

        val all = traces(data)
        val names = all.map(_.name)
        val layout = Framing.layout(fit.abscissa, fit.ordinate, names)
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
            if highs.nil then Nil else style.band(highs.reverse + lows, color, index)

          val line = style.line(points, color, index)

          val decorations: List[Figure] = sorted.fold(List[Figure]()): (acc, datum) =>
            val position = at(datum, datum.y)
            val marker = style.lineMarker(position, color, index)
            val note = datum.note.lay(Nil): text => style.pointLabel(position, text, palette.text)
            note.reverse + (marker.reverse + acc)

          val figures = band + line + decorations.reverse
          val part = seriesId(index) -> Group(figures, id = seriesId(index))
          index += 1
          part

        val legend = legendPart(layout.legend, names)
        Framing.drawing(axes(frame, fit.abscissa, fit.ordinate) + seriesParts + legend)

// A line per series through its points in abscissa order, with a translucent band where the
// values carry intervals, and markers at the points if the style asks for them. Neither axis is
// anchored at zero: a line shows change, not magnitude.
case class Lines(abscissa: Optional[Calibration] = Unset, ordinate: Optional[Calibration] = Unset)
