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

// The layout and axes shared by every chart kind with an abscissa and an ordinate: the plot
// rectangle after room is taken for labels, titles and the legend; the grid, the two axes with
// their gradations; the legend itself; and the normalized forms of series that the kinds fit
// and draw from.
private[tasseomancy] object Cartesian:
  case class Frame(left: Double, top: Double, width: Double, height: Double):
    def right: Double = left + width
    def bottom: Double = top + height
    def x(unit: Double): Double = left + unit*width
    def y(unit: Double): Double = top + (1.0 - unit)*height

  case class Layout(frame: Frame, legend: Optional[Frame])

  // A series with numeric axes, and one with a categorical abscissa, reduced to positions.
  case class Datum(x: Double, y: Double, bounds: Optional[(Double, Double)])
  case class Trace(name: Text, data: Sequence[Datum])
  case class Mark(category: Text, y: Double, bounds: Optional[(Double, Double)])
  case class Column(name: Text, marks: Sequence[Mark])

  def traceOf[x: Continuous as cx, y: Continuous as cy](series: Series[x, y]): Trace =
    val data = series.points.map: (abscissa, ordinate) =>
      Datum(cx.position(abscissa), cy.position(ordinate), cy.bounds(ordinate))

    Trace(series.name, data)

  def columnOf[x: Categorical as cx, y: Continuous as cy](series: Series[x, y]): Column =
    val marks = series.points.map: (abscissa, ordinate) =>
      Mark(cx.label(abscissa), cy.position(ordinate), cy.bounds(ordinate))

    Column(series.name, marks)

  // The categories of some columns, in first-appearance order.
  def categories(columns: List[Column]): Sequence[Text] =
    val labels = columns.fold(List[Text]()): (acc, column) =>
      column.marks.fold(acc): (acc2, mark) =>
        if acc2.exists(_ == mark.category) then acc2 else mark.category :: acc2

    labels.reverse.to[Sequence]

  object Extent:
    val empty: Extent = Extent(0.0, 0.0, false)

  // The least and greatest of some values, growing as values are included. An empty extent is
  // reported as `[0, 1]`, so an axis over no data is still an axis.
  case class Extent(lower: Double, upper: Double, populated: Boolean):
    def include(value: Double): Extent =
      if !populated then Extent(value, value, true)
      else Extent(lower.min(value), upper.max(value), true)

    def include(bounds: Optional[(Double, Double)]): Extent =
      bounds.lay(this): (low, high) => include(low).include(high)

    def lowerOr0: Double = if populated then lower else 0.0
    def upperOr1: Double = if populated then upper else 1.0

  def countOf[element](list: List[element]): Int = list.fold(0): (n, _) => n + 1

  // Colour and style helpers: every figure's style is an inline declaration set, so an SVG is
  // self-contained.
  private val hexDigits: Text = t"0123456789abcdef"

  def hexOf(color: Color in Srgb): Text =
    val srgb = color.to[Srgb]

    def channel(value: Double): Text =
      val byte = (value*255.0).round.toInt.max(0).min(255)
      t"${hexDigits.at((byte/16).z).or('0')}${hexDigits.at((byte%16).z).or('0')}"

    t"#${channel(srgb.red)}${channel(srgb.green)}${channel(srgb.blue)}"

  def px(value: Double): Text = t"${value.toString}px"
  def css(pairs: (Text, Text)*): Css.Style = Css.Style.of(List.from(pairs))

  def filled(color: Color in Srgb, opacity: Optional[Double] = Unset): Css.Style =
    val base = List(t"fill" -> hexOf(color), t"stroke" -> t"none")
    val alpha = opacity.lay(Nil): value => List(t"fill-opacity" -> value.toString.tt)
    Css.Style.of(base + alpha)

  def stroked(color: Color in Srgb, width: Double): Css.Style =
    css
      ( t"fill" -> t"none", t"stroke" -> hexOf(color), t"stroke-width" -> px(width),
        t"stroke-linejoin" -> t"round", t"stroke-linecap" -> t"round" )

  def font(using style: Chart.Style, palette: ChartPalette): Css.Style =
    css
      ( t"font-family" -> style.fontFamily, t"font-size" -> px(style.fontSize),
        t"fill" -> hexOf(palette.text) )

  def textWidth(text: Text)(using metric: FontMetric, style: Chart.Style): Double =
    metric.width(text).value*style.fontSize

  def point(x: Double, y: Double): Point = Point(x.toFloat, y.toFloat)
  def seriesId(index: Int): Svg.Id = Svg.Id(t"series-$index")

  private def gap(using style: Chart.Style): Double = style.fontSize*0.4
  private def lineHeight(using style: Chart.Style): Double = style.fontSize*1.6
  private def swatch(using style: Chart.Style): Double = style.fontSize

  private def legendWidth(names: List[Text])(using Chart.Style, FontMetric): Double =
    names.fold(0.0) { (acc, name) => acc.max(textWidth(name)) } + swatch + gap

  // The plot rectangle: the style's size less the insets, the ordinate's labels (measured from
  // the gradations the full height would allow), the abscissa's labels, any titles and the
  // legend. A chart without an ordinate (a pie) passes none.
  def layout(ordinate: Optional[Ruler], names: List[Text])
    ( using style: Chart.Style, metric: FontMetric )
  :   Layout =

    val titleRoom = style.fontSize*1.6
    val entries = countOf(names)
    val showLegend = entries > 0 && style.legend != Chart.Legend.Hidden
    val rightLegend = showLegend && style.legend == Chart.Legend.Right
    val bottomLegend = showLegend && style.legend == Chart.Legend.Bottom

    val labelWidth = ordinate.lay(0.0): ruler =>
      val budget = (style.height/style.pitch).toInt.max(2)
      ruler.gradations(budget).fold(0.0): (acc, gradation) => acc.max(textWidth(gradation.label))

    val axisRoom = ordinate.lay(0.0) { _ => gap + style.tickLength }
    val titleWidth = style.ordinateTitle.lay(0.0) { _ => titleRoom }
    val left = style.inset + labelWidth + axisRoom + titleWidth
    val legendRoom = if rightLegend then legendWidth(names) + gap*2 else 0.0
    val right = style.inset + style.fontSize*0.6 + legendRoom
    val top = style.inset + style.fontSize*0.6
    val legendRow = if bottomLegend then lineHeight else 0.0
    val labelRow = ordinate.lay(0.0) { _ => style.fontSize + gap + style.tickLength }
    val titleHeight = style.abscissaTitle.lay(0.0) { _ => titleRoom }
    val bottom = style.inset + labelRow + titleHeight + legendRow
    val width = (style.width - left - right).max(1.0)
    val height = (style.height - top - bottom).max(1.0)
    val frame = Frame(left, top, width, height)

    val legend: Optional[Frame] =
      if !showLegend then Unset
      else if rightLegend
      then Frame(frame.right + gap*2, frame.top, legendWidth(names), entries*lineHeight)
      else Frame(frame.left, style.height - style.inset - lineHeight, frame.width, lineHeight)

    Layout(frame, legend)

  // The abscissa's gradations: as many as the pitch allows on a numeric axis, reduced until their
  // labels no longer overlap; every band of a categorical one.
  def abscissaGradations(frame: Frame, ruler: Ruler)
    ( using style: Chart.Style, metric: FontMetric )
  :   Sequence[Gradation] =

    ruler match
      case bands: Bands => bands.gradations(0)

      case scale: Scale =>
        var budget = (frame.width/style.pitch).toInt.max(1)
        var marks = scale.gradations(budget)

        def crowded: Boolean =
          val occupied = marks.fold(0.0): (acc, mark) =>
            if mark.major then acc + textWidth(mark.label) + gap else acc

          occupied > frame.width

        while budget > 1 && crowded do
          budget -= 1
          marks = scale.gradations(budget)

        marks

      case other => other.gradations((frame.width/style.pitch).toInt.max(1))

  // The grid and both axes, as identified groups.
  def axes(frame: Frame, abscissa: Ruler, ordinate: Ruler)
    ( using style: Chart.Style, palette: ChartPalette, metric: FontMetric )
  :   List[(Svg.Id, Figure)] =

    val xs = abscissaGradations(frame, abscissa)
    val ys = ordinate.gradations((frame.height/style.pitch).toInt.max(2))
    val lettering = font
    val gridStroke = stroked(palette.grid, 1.0)
    val axisStroke = stroked(palette.axis, 1.0)

    def line(x0: Double, y0: Double, x1: Double, y1: Double, style: Css.Style): Figure =
      Polyline(List(point(x0, y0), point(x1, y1)), style = style)

    val gridLines: List[Figure] =
      if !style.grid then Nil else
        val horizontal = ys.fold(List[Figure]()): (acc, mark) =>
          if !mark.major then acc else
            val y = frame.y(mark.position)
            line(frame.left, y, frame.right, y, gridStroke) :: acc

        abscissa match
          case scale: Scale =>
            xs.fold(horizontal): (acc, mark) =>
              if !mark.major then acc else
                val x = frame.x(mark.position)
                line(x, frame.top, x, frame.bottom, gridStroke) :: acc

          case _ => horizontal

    val abscissaFigures: List[Figure] =
      val baseline = line(frame.left, frame.bottom, frame.right, frame.bottom, axisStroke)

      val marks = xs.fold(List[Figure]()): (acc, mark) =>
        val x = frame.x(mark.position)
        val tick = line(x, frame.bottom, x, frame.bottom + style.tickLength, axisStroke)

        if !mark.major then tick :: acc else
          val position = point(x, frame.bottom + style.tickLength + gap)

          val label =
            Lettering
              ( position, mark.label, Lettering.Anchor.Middle, Lettering.Baseline.Hanging,
                style = lettering )

          label :: tick :: acc

      val title = style.abscissaTitle.lay(Nil): text =>
        val y = frame.bottom + style.tickLength + gap + style.fontSize + gap
        val position = point(frame.left + frame.width/2.0, y)

        List
          ( Lettering
              ( position, text, Lettering.Anchor.Middle, Lettering.Baseline.Hanging,
                style = lettering ) )

      baseline :: (marks.reverse + title)

    val ordinateFigures: List[Figure] =
      val baseline = line(frame.left, frame.top, frame.left, frame.bottom, axisStroke)

      val marks = ys.fold(List[Figure]()): (acc, mark) =>
        val y = frame.y(mark.position)
        val tick = line(frame.left - style.tickLength, y, frame.left, y, axisStroke)

        if !mark.major then tick :: acc else
          val position = point(frame.left - style.tickLength - gap, y)

          val label =
            Lettering
              ( position, mark.label, Lettering.Anchor.End, Lettering.Baseline.Middle,
                style = lettering )

          label :: tick :: acc

      val title = style.ordinateTitle.lay(Nil): text =>
        val centre = Delta(style.inset.toFloat, (frame.top + frame.height/2.0).toFloat)
        val transforms = List(Transform.Translate(centre), Transform.Rotate(Angle.degrees(-90.0)))

        List
          ( Lettering
              ( point(0.0, 0.0), text, Lettering.Anchor.Middle, Lettering.Baseline.Hanging,
                style = lettering, transforms = transforms ) )

      baseline :: (marks.reverse + title)

    List
      ( Svg.Id(t"grid") -> Group(gridLines.reverse, id = Svg.Id(t"grid")),
        Svg.Id(t"abscissa") -> Group(abscissaFigures, id = Svg.Id(t"abscissa")),
        Svg.Id(t"ordinate") -> Group(ordinateFigures, id = Svg.Id(t"ordinate")) )

  // The legend: a swatch and a name per series, stacked at the right or flowing along the bottom.
  def legend(frame: Frame, names: List[Text])
    ( using style: Chart.Style, palette: ChartPalette, metric: FontMetric )
  :   (Svg.Id, Figure) =

    val lettering = font
    val vertical = style.legend == Chart.Legend.Right
    var index = 0
    var x = frame.left
    var figures: List[Figure] = Nil

    names.foreach: name =>
      val y = if vertical then frame.top + index*lineHeight else frame.top
      val corner = point(x, y + (lineHeight - swatch)/2.0)
      val color = filled(palette.color(index))
      val box = Rectangle(corner, swatch.toFloat, swatch.toFloat, style = color)
      val position = point(x + swatch + gap, y + lineHeight/2.0)

      val label =
        Lettering
          ( position, name, Lettering.Anchor.Start, Lettering.Baseline.Middle, style = lettering )

      figures = label :: box :: figures
      if !vertical then x += swatch + gap + textWidth(name) + gap*3
      index += 1

    Svg.Id(t"legend") -> Group(figures.reverse, id = Svg.Id(t"legend"))

  // An error bar: a vertical line between two ordinate positions with a cap at each end.
  def errorBar(x: Double, low: Double, high: Double, cap: Double)(using palette: ChartPalette)
  :   List[Figure] =

    val bar = stroked(palette.axis, 1.0)

    List
      ( Polyline(List(point(x, low), point(x, high)), style = bar),
        Polyline(List(point(x - cap, low), point(x + cap, low)), style = bar),
        Polyline(List(point(x - cap, high), point(x + cap, high)), style = bar) )

  // The parts of a chart whose legend, if any, occupies the given frame.
  def legendPart(frame: Optional[Frame], names: List[Text])
    ( using Chart.Style, ChartPalette, FontMetric )
  :   List[(Svg.Id, Figure)] =

    frame.lay(Nil): frame => List(legend(frame, names))

  def drawing(parts: List[(Svg.Id, Figure)])(using style: Chart.Style): Chart.Drawing =
    Chart.Drawing(style.width, style.height, Nil, parts.to[Ledger])
