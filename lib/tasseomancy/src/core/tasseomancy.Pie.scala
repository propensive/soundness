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
import cataclysm.Css
import denominative.*
import geodesy.*
import gossamer.*
import hypotenuse.*
import iridescence.*
import prepositional.*
import rudiments.*
import savagery.*
import symbolism.*
import vacuous.*

object Pie:
  case class Fit(labels: Sequence[Text], total: Double)

  // The components of a pie: its wedges, given as the path operations of their outlines, and the
  // percentage set on any wedge at least `labelThreshold` of the whole. `hole` hollows the
  // centre to that fraction of the radius.
  trait Style extends Chart.Style:
    def hole: Double
    def labelThreshold: Double = 0.04

    def wedge(ops: List[Stroke], color: Color in Srgb, index: Int): List[Figure] =
      List(Outline(ops, style = filled(color) + Css.Style.of(List(t"fill-rule" -> t"evenodd"))))

    def wedgeLabel(at: Point, text: Text, color: Color in Srgb): List[Figure] =
      List(lettering(at, text, Lettering.Anchor.Middle, Lettering.Baseline.Middle, color))

  given series: [x: Categorical, y: Continuous as continuous]
  =>  Series[x, y] is Plottable in Pie to Pie.Fit by Pie.Style =

    new Plottable:
      type Self = Series[x, y]
      type Form = Pie
      type Result = Pie.Fit
      type Operand = Pie.Style

      def fit(form: Pie, data: Series[x, y]): Pie.Fit =
        val marks = columnOf(data).marks
        val total = marks.fold(0.0): (acc, mark) => acc + mark.y.max(0.0)
        Pie.Fit(marks.map(_.category), total)

      // A pie has no axes to hold still: any change in a value moves every wedge, so a fit only
      // survives while the labels are the same, and then it is the one `wedges` part that
      // changes.
      def accommodates(form: Pie, fit: Pie.Fit, data: Series[x, y]): Boolean =
        val labels = columnOf(data).marks.map(_.category)

        val (same, _) = labels.fold((labels.size == fit.labels.size, 0)): (acc, label) =>
          (acc(0) && Sequence.at(fit.labels, acc(1)) == label, acc(1) + 1)

        same

      def draw(form: Pie, data: Series[x, y], fit: Pie.Fit)
        ( using style: Pie.Style, palette: ChartPalette, metric: FontMetric )
      :   Chart.Drawing =

        val marks = columnOf(data).marks
        val names = fit.labels.to[List]
        val frame = pieFrame(names)
        val radius = (frame.width.min(frame.height)/2.0 - style.fontSize).max(1.0)
        val hole = radius*style.hole.max(0.0).min(0.95)
        val cx = frame.left + frame.width/2.0
        val cy = frame.top + frame.height/2.0

        def rim(angle: Double, distance: Double): Point =
          point(cx + distance*sin(angle).double, cy - distance*cos(angle).double)

        def arc(distance: Double, large: Boolean, sweep: Sweep, angle: Double): Stroke =
          val size = distance.toFloat
          Stroke.ArcTo(size, size, Angle(0.0), large, sweep, rim(angle, distance))

        // A wedge's path operations, listed last to first as an `Outline` holds them.
        def wedge(start: Double, end: Double, large: Boolean): List[Stroke] =
          if hole <= 0.0 then
            List
              ( Stroke.Close,
                arc(radius, large, Sweep.Clockwise, end),
                Stroke.DrawTo(rim(start, radius)),
                Stroke.MoveTo(point(cx, cy)) )
          else
            List
              ( Stroke.Close,
                arc(hole, large, Sweep.Counterclockwise, start),
                Stroke.DrawTo(rim(end, hole)),
                arc(radius, large, Sweep.Clockwise, end),
                Stroke.MoveTo(rim(start, radius)) )

        // A whole circle cannot be one arc, whose ends would coincide: it is two half-turns,
        // and a ring is a circle with a hole cut by the even-odd rule.
        def whole: List[Stroke] =
          val outer =
            List
              ( Stroke.Close,
                arc(radius, true, Sweep.Clockwise, 0.0),
                arc(radius, true, Sweep.Clockwise, π),
                Stroke.MoveTo(rim(0.0, radius)) )

          if hole <= 0.0 then outer else
            val inner =
              List
                ( Stroke.Close,
                  arc(hole, true, Sweep.Counterclockwise, π),
                  arc(hole, true, Sweep.Counterclockwise, 0.0),
                  Stroke.MoveTo(rim(π, hole)) )

            inner + outer

        var start = 0.0
        var index = 0

        val figures = marks.fold(List[Figure]()): (acc, mark) =>
          val fraction = if fit.total <= 0.0 then 0.0 else mark.y.max(0.0)/fit.total
          val sweep = fraction*2.0*π
          val color = palette.color(index)
          val end = start + sweep
          val complete = fraction >= 1.0 - Scale.tolerance
          val ops = if complete then whole else wedge(start, end, sweep > π)
          val shape = style.wedge(ops, color, index)

          val label: List[Figure] =
            if fraction < style.labelThreshold then Nil else
              val middle = (start + end)/2.0
              val distance = if hole <= 0.0 then radius*0.65 else (radius + hole)/2.0
              val text = t"${Scale.format(fraction*100.0, 0)}%"
              style.wedgeLabel(rim(middle, distance), text, palette.text)

          start = end
          index += 1
          label.reverse + (shape.reverse + acc)

        val wedges = Svg.Id(t"wedges") -> Group(figures.reverse, id = Svg.Id(t"wedges"))
        Framing.drawing(wedges :: legendPart(legendFrame(names), names))

      // A pie has no axes, so its frame is the canvas less the insets and the legend.
      private def pieFrame(names: List[Text])
        ( using style: Pie.Style, metric: FontMetric )
      :   Frame =

        val entries = countOf(names)
        val showLegend = entries > 0 && style.legend != Chart.Legend.Hidden
        val widest = names.fold(0.0): (acc, name) => acc.max(textWidth(name))
        val legendWidth = widest + style.swatchSize + style.gap

        val right =
          if showLegend && style.legend == Chart.Legend.Right then legendWidth + style.gap*2
          else 0.0

        val bottom =
          if showLegend && style.legend == Chart.Legend.Bottom then style.legendLineHeight else 0.0

        val width = (style.width - style.inset*2 - right).max(1.0)
        val height = (style.height - style.inset*2 - bottom).max(1.0)
        Frame(style.inset, style.inset, width, height)

      private def legendFrame(names: List[Text])
        ( using style: Pie.Style, metric: FontMetric )
      :   Optional[Frame] =

        val entries = countOf(names)
        val frame = pieFrame(names)
        val widest = names.fold(0.0): (acc, name) => acc.max(textWidth(name))
        val legendWidth = widest + style.swatchSize + style.gap
        val lineHeight = style.legendLineHeight

        if entries == 0 || style.legend == Chart.Legend.Hidden then Unset
        else if style.legend == Chart.Legend.Right
        then Frame(frame.right + style.gap*2, frame.top, legendWidth, entries*lineHeight)
        else
          val row = style.height - style.inset - style.legendLineHeight
          Frame(frame.left, row, frame.width, style.legendLineHeight)

// Parts of a whole: one series, each category a wedge proportional to its value, clockwise from
// the top.
case class Pie()
