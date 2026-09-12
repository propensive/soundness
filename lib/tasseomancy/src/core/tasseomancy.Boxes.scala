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
import anticipation.*
import denominative.*
import hypotenuse.*
import iridescence.*
import murmuration.Traversable
import murmuration.sortingAlgorithms.timsort
import prepositional.*
import rudiments.*
import savagery.*
import symbolism.*
import vacuous.*

object Boxes:
  object Summary:
    // Quartiles by linear interpolation between the order statistics either side.
    def of(values: Sequence[Double]): Summary =
      val sorted: Sequence[Double] = values.sort
      val size = sorted.size

      def quantile(fraction: Double): Double =
        if size == 0 then 0.0 else
          val position = fraction*(size - 1)
          val low = position.floor.toInt
          val high = position.ceiling.toInt
          val below = Sequence.at(sorted, low)
          val above = Sequence.at(sorted, high)
          below + (above - below)*(position - low)

      Summary(quantile(0.0), quantile(0.25), quantile(0.5), quantile(0.75), quantile(1.0))

  // The five-number summary of one set of samples.
  case class Summary
    ( minimum:       Double,
      lowerQuartile: Double,
      median:        Double,
      upperQuartile: Double,
      maximum:       Double )

  case class Fit(bands: Bands, ordinate: Scale, summaries: Sequence[Summary])

  // The components of a box plot beyond the axes: the box between the quartiles, the median
  // across it, and the whiskers to the extremes with their caps.
  trait Style extends Chart.Cartesian:
    def boxGap: Double

    def box(corner: Point, width: Double, height: Double, color: Color in Srgb, series: Int)
    :   List[Figure] =

      List(Rectangle(corner, width.toFloat, height.toFloat, style = filled(color)))

    def whisker(from: Point, to: Point, color: Color in Srgb): List[Figure] =
      List(Polyline(List(from, to), style = stroked(color, 1.0)))

    def whiskerCap(from: Point, to: Point, color: Color in Srgb): List[Figure] =
      List(Polyline(List(from, to), style = stroked(color, 1.0)))

    def median(from: Point, to: Point, color: Color in Srgb): List[Figure] =
      List(Polyline(List(from, to), style = stroked(color, strokeWidth)))

  given samples: [y: {Continuous, Calibration}]
  =>  Samples[y] is Plottable in Boxes to Boxes.Fit by Boxes.Style =
    plottable[Samples[y], y]: samples => List(Histogram.positions(samples))

  given traversable: [collection, y]
  =>  ( traversable: collection is Traversable by Samples[y] )
  =>  ( continuous: y is Continuous, calibration: y is Calibration )
  =>  collection is Plottable in Boxes to Boxes.Fit by Boxes.Style =
    plottable[collection, y]: collection =>
      List.from(traversable.traverse(collection).map(Histogram.positions(_)))

  private def plottable[data, y: {Continuous as continuous, Calibration as calibration}]
    ( extract: data -> List[(Text, Sequence[Double])] )
  :   data is Plottable in Boxes to Boxes.Fit by Boxes.Style =

    new Plottable:
      type Self = data
      type Form = Boxes
      type Result = Boxes.Fit
      type Operand = Boxes.Style

      def fit(form: Boxes, data: data): Boxes.Fit =
        val all = extract(data)
        val names = all.map(_(0)).to[Sequence]
        val summaries = all.map { entry => Summary.of(entry(1)) }.to[Sequence]

        val extent = summaries.fold(Extent.empty): (acc, summary) =>
          acc.include(summary.minimum).include(summary.maximum)

        val scale =
          form.ordinate.or(calibration)
          . scale(extent.lowerOr0, extent.upperOr1, false, continuous.notation)

        Boxes.Fit(Bands(names), scale, summaries)

      def accommodates(form: Boxes, fit: Boxes.Fit, data: data): Boolean =
        val all = extract(data)

        val known = all.all: entry =>
          fit.bands.index(entry(0)).present && entry(1).all(fit.ordinate.accommodates(_))

        countOf(all) == fit.bands.count && known

      def draw(form: Boxes, data: data, fit: Boxes.Fit)
        ( using style: Boxes.Style, palette: ChartPalette, metric: FontMetric )
      :   Chart.Drawing =

        val all = extract(data)
        val names = all.map(_(0))
        val layout = Framing.layout(fit.bands, fit.ordinate, names)
        val frame = layout.frame
        val boxWidth = frame.width*fit.bands.width*(1.0 - style.boxGap)
        var index = 0

        val seriesParts = all.map: entry =>
          val summary = Summary.of(entry(1))
          val color = palette.color(index)
          val x = frame.x(fit.bands.centre(index))
          val x0 = x - boxWidth/2.0
          val x1 = x + boxWidth/2.0
          val cap = boxWidth/4.0

          def y(value: Double): Double = frame.y(fit.ordinate.unit(value))

          val box =
            style.box
              ( point(x0, y(summary.upperQuartile)), boxWidth,
                (y(summary.lowerQuartile) - y(summary.upperQuartile)).abs, color, index )

          val top = y(summary.maximum)
          val bottom = y(summary.minimum)
          val axis = palette.axis

          val parts: List[List[Figure]] =
            List
              ( style.whisker(point(x, top), point(x, y(summary.upperQuartile)), axis),
                style.whisker(point(x, y(summary.lowerQuartile)), point(x, bottom), axis),
                style.whiskerCap(point(x - cap, top), point(x + cap, top), axis),
                style.whiskerCap(point(x - cap, bottom), point(x + cap, bottom), axis),
                box,
                style.median(point(x0, y(summary.median)), point(x1, y(summary.median)), axis) )

          val figures: List[Figure] = parts.fold(List[Figure]())(_ + _)

          val part = seriesId(index) -> Group(figures, id = seriesId(index))
          index += 1
          part

        val legend = legendPart(layout.legend, names)
        Framing.drawing(axes(frame, fit.bands, fit.ordinate) + seriesParts + legend)

// A box per set of samples: the box spans the quartiles with the median across it, and the
// whiskers reach the least and greatest sample. Each set is a category on the abscissa.
case class Boxes(ordinate: Optional[Calibration] = Unset)
