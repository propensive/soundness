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
import anticipation.*
import denominative.*
import gossamer.*
import hypotenuse.*
import murmuration.Traversable
import prepositional.*
import rudiments.*
import savagery.*
import symbolism.*
import vacuous.*

object Histogram:
  case class Fit(edges: Sequence[Double], abscissa: Scale, ordinate: Scale)

  given samples: [y: {Continuous, Calibration}]
  =>  Samples[y] is Plottable in Histogram to Histogram.Fit =
    plottable[Samples[y], y]: samples => List(positions(samples))

  given traversable: [collection, y]
  =>  ( traversable: collection is Traversable by Samples[y] )
  =>  ( continuous: y is Continuous, calibration: y is Calibration )
  =>  collection is Plottable in Histogram to Histogram.Fit =
    plottable[collection, y]: collection =>
      List.from(traversable.traverse(collection).map(positions(_)))

  private[tasseomancy] def positions[y: Continuous as continuous](samples: Samples[y])
  :   (Text, Sequence[Double]) =

    (samples.name, samples.values.map(continuous.position(_)))

  private def bin(fit: Histogram.Fit, value: Double): Int =
    val bins = fit.edges.size - 1
    val width = (fit.abscissa.upper - fit.abscissa.lower)/bins
    ((value - fit.abscissa.lower)/width).floor.toInt.max(0).min(bins - 1)

  private def counts(fit: Histogram.Fit, values: Sequence[Double]): Sequence[Int] =
    var tally: Sequence[Int] = Sequence.empty
    var index = 0

    while index < fit.edges.size - 1 do
      tally = Sequence.append(tally, 0)
      index += 1

    values.foreach: value =>
      val slot = bin(fit, value)
      tally = Sequence.define(tally, slot, Sequence.at(tally, slot) + 1)

    tally

  private def mostCounted(fit: Histogram.Fit, all: List[(Text, Sequence[Double])]): Int =
    all.fold(0): (acc, entry) => counts(fit, entry(1)).fold(acc)(_.max(_))

  private def plottable[data, y: {Continuous as continuous, Calibration as calibration}]
    ( extract: data -> List[(Text, Sequence[Double])] )
  :   data is Plottable in Histogram to Histogram.Fit =

    new Plottable:
      type Self = data
      type Form = Histogram
      type Result = Histogram.Fit

      def fit(form: Histogram, data: data): Histogram.Fit =
        val all = extract(data)

        val (extent, total) = all.fold((Extent.empty, 0)): (acc, entry) =>
          entry(1).fold(acc): (acc2, value) => (acc2(0).include(value), acc2(1) + 1)

        // Sturges' rule where no bin count is given: one more than the base-two logarithm of the
        // sample count.
        val sturges = (ln(total.max(1).toDouble).double/ln(2.0).double).ceiling.toInt + 1
        val bins = form.bins.or(sturges).max(1)
        val span0 = extent.upperOr1 - extent.lowerOr0
        val span = if span0 > 0.0 then span0 else extent.upperOr1.abs.max(1.0)

        val width = continuous.spacing match
          case Scale.Spacing.Decimal     => Scale.step(span/bins)
          case Scale.Spacing.Sexagesimal => Scale.sexagesimalStep(span/bins)

        val first = (extent.lowerOr0/width + Scale.tolerance).floor*width
        var edges: List[Double] = List(first)
        var edge = first
        var count = 1

        while edge < extent.upperOr1 - width*Scale.tolerance || count < 2 do
          edge += width
          edges = edge :: edges
          count += 1

        val abscissa =
          Scale(first, edge, Scale.Transform.Linear, continuous.spacing, continuous.labelling)

        val provisional = Histogram.Fit(edges.reverse.to[Sequence], abscissa, abscissa)
        val most = mostCounted(provisional, all)
        val number = Scale.Labelling.Number(t"")

        val ordinate =
          form.ordinate.or(Calibration[Int](Calibration.Policy.Linear))
          . scale(0.0, most.toDouble, true, Scale.Spacing.Decimal, number)

        Histogram.Fit(provisional.edges, abscissa, ordinate)

      def accommodates(form: Histogram, fit: Histogram.Fit, data: data): Boolean =
        val all = extract(data)
        val within = all.all: entry => entry(1).all(fit.abscissa.accommodates(_))
        within && mostCounted(fit, all).toDouble <= fit.ordinate.upper

      def draw(form: Histogram, data: data, fit: Histogram.Fit)
        ( using style: Chart.Style, palette: ChartPalette, metric: FontMetric )
      :   Chart.Drawing =

        val all = extract(data)
        val names = all.map(_(0))
        val layout = Cartesian.layout(fit.ordinate, names)
        val frame = layout.frame
        val total = countOf(all).max(1)
        val bins = fit.edges.size - 1
        val binWidth = frame.width/bins.max(1)
        val groupWidth = binWidth*(1.0 - style.barGap)
        val barWidth = groupWidth/total
        val zero = frame.y(fit.ordinate.unit(0.0))
        var index = 0

        val seriesParts = all.map: entry =>
          val tally = counts(fit, entry(1))
          val color = filled(palette.color(index))
          var bin = 0
          var figures: List[Figure] = Nil

          while bin < bins do
            val height = Sequence.at(tally, bin)

            if height > 0 then
              val x0 = frame.left + bin*binWidth + (binWidth - groupWidth)/2.0 + index*barWidth
              val y = frame.y(fit.ordinate.unit(height.toDouble))
              val bar = Rectangle(point(x0, y), barWidth.toFloat, (zero - y).toFloat, style = color)
              figures = bar :: figures

            bin += 1

          val part = seriesId(index) -> Group(figures.reverse, id = seriesId(index))
          index += 1
          part

        val legend = legendPart(layout.legend, names)
        Cartesian.drawing(axes(frame, fit.abscissa, fit.ordinate) + seriesParts + legend)

// The distribution of samples: the range is cut into equal bins, and a bar per series in each
// bin counts the samples that fall in it. Without a bin count, Sturges' rule chooses one.
case class Histogram(bins: Optional[Int] = Unset, ordinate: Optional[Calibration] = Unset)
