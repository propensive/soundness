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
import gossamer.*
import hypotenuse.*
import iridescence.*
import prepositional.*
import rudiments.*
import savagery.*
import symbolism.*
import vacuous.*

// The layout shared by every chart kind with an abscissa and an ordinate: the plot rectangle
// after room is taken for labels, titles and the legend; the grid, the two axes with their
// gradations; the legend; and the normalized forms of series that the kinds fit and draw from.
// Every figure is produced by a method of the style in scope, so this decides only where each
// component goes.
private[tasseomancy] object Framing:
  case class Frame(left: Double, top: Double, width: Double, height: Double):
    def right: Double = left + width
    def bottom: Double = top + height
    def x(unit: Double): Double = left + unit*width
    def y(unit: Double): Double = top + (1.0 - unit)*height

  case class Layout(frame: Frame, legend: Optional[Frame])

  // A series with numeric axes, and one with a categorical abscissa, reduced to positions.
  case class Datum(x: Double, y: Double, bounds: Optional[(Double, Double)], note: Optional[Text])
  case class Trace(name: Text, data: Sequence[Datum])
  case class Mark(category: Text, y: Double, bounds: Optional[(Double, Double)])
  case class Column(name: Text, marks: Sequence[Mark])

  def traceOf[x: Continuous as cx, y: Continuous as cy](series: Series[x, y]): Trace =
    val data = series.points.map: (abscissa, ordinate) =>
      val note = cx.annotation(abscissa).or(cy.annotation(ordinate))
      Datum(cx.position(abscissa), cy.position(ordinate), cy.bounds(ordinate), note)

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

  def textWidth(text: Text)(using metric: FontMetric, style: Chart.Style): Double =
    metric.width(text).value*style.fontSize

  def point(x: Double, y: Double): Point = Point(x.toFloat, y.toFloat)
  def seriesId(index: Int): Svg.Id = Svg.Id(t"series-$index")

  // An axis's title: the style's, if given, with the axis's unit appended; otherwise the name and
  // unit the axis's type supplies; a categorical axis has neither.
  def title(ruler: Ruler, supplied: Optional[Text]): Optional[Text] = ruler match
    case scale: Scale => scale.notation.title(supplied)
    case _            => supplied

  private def legendWidth(names: List[Text])(using style: Chart.Style, metric: FontMetric)
  :   Double =

    names.fold(0.0) { (acc, name) => acc.max(textWidth(name)) } + style.swatchSize + style.gap

  // The plot rectangle: the style's size less the insets, the room each axis's labels need
  // (which the style reports, from the gradations the full extent would allow), any titles and
  // the legend. A chart without axes (a pie) passes none.
  def layout(abscissa: Optional[Ruler], ordinate: Optional[Ruler], names: List[Text])
    ( using style: Chart.Cartesian, metric: FontMetric )
  :   Layout =

    import Chart.Axis.*

    val entries = countOf(names)
    val showLegend = entries > 0 && style.legend != Chart.Legend.Hidden
    val rightLegend = showLegend && style.legend == Chart.Legend.Right
    val bottomLegend = showLegend && style.legend == Chart.Legend.Bottom
    val ordinateTitle = ordinate.let(title(_, style.ordinateTitle)).or(style.ordinateTitle)
    val abscissaTitle = abscissa.let(title(_, style.abscissaTitle)).or(style.abscissaTitle)

    def labels(ruler: Ruler, length: Double): List[Text] =
      ruler.gradations((length/style.pitch).toInt.max(2)).filter(_.major).map(_.label).to[List]

    val ordinateRoom = ordinate.lay(0.0): ruler =>
      style.labelRoom(Ordinate, labels(ruler, style.height))

    val left = style.inset + ordinateRoom + ordinateTitle.lay(0.0) { _ => style.titleRoom }
    val legendRoom = if rightLegend then legendWidth(names) + style.gap*2 else 0.0
    val right = style.inset + style.fontSize*0.6 + legendRoom
    val top = style.inset + style.fontSize*0.6
    val width = (style.width - left - right).max(1.0)

    val abscissaRoom = abscissa.lay(0.0): ruler => style.labelRoom(Abscissa, labels(ruler, width))

    val legendRow = if bottomLegend then style.legendLineHeight else 0.0
    val titleHeight = abscissaTitle.lay(0.0) { _ => style.titleRoom }
    val bottom = style.inset + abscissaRoom + titleHeight + legendRow
    val height = (style.height - top - bottom).max(1.0)
    val frame = Frame(left, top, width, height)

    val lineHeight = style.legendLineHeight

    val legend: Optional[Frame] =
      if !showLegend then Unset
      else if rightLegend
      then Frame(frame.right + style.gap*2, frame.top, legendWidth(names), entries*lineHeight)
      else Frame(frame.left, style.height - style.inset - lineHeight, frame.width, lineHeight)

    Layout(frame, legend)

  // The abscissa's gradations: as many as the pitch allows on a numeric axis, reduced until their
  // labels no longer overlap; every band of a categorical one.
  def abscissaGradations(frame: Frame, ruler: Ruler)
    ( using style: Chart.Cartesian, metric: FontMetric )
  :   Sequence[Gradation] =

    ruler match
      case bands: Bands => bands.gradations(0)

      case scale: Scale =>
        var budget = (frame.width/style.pitch).toInt.max(1)
        var marks = scale.gradations(budget)

        def crowded: Boolean =
          val occupied = marks.fold(0.0): (acc, mark) =>
            if mark.major then acc + textWidth(mark.label) + style.gap else acc

          occupied > frame.width

        while budget > 1 && crowded do
          budget -= 1
          marks = scale.gradations(budget)

        marks

      case other => other.gradations((frame.width/style.pitch).toInt.max(1))

  // Whether a linear axis starts away from zero, which the style may mark with a break.
  private def broken(ruler: Ruler): Boolean = ruler match
    case scale: Scale =>
      scale.transform == Scale.Transform.Linear && (scale.lower > 0.0 || scale.upper < 0.0)

    case _ => false

  // The grid and both axes, as identified groups.
  def axes(frame: Frame, abscissa: Ruler, ordinate: Ruler)
    ( using style: Chart.Cartesian, palette: ChartPalette, metric: FontMetric )
  :   List[(Svg.Id, Figure)] =

    import Chart.Axis.*

    val xs = abscissaGradations(frame, abscissa)
    val ys = ordinate.gradations((frame.height/style.pitch).toInt.max(2))
    val origin = point(frame.left, frame.bottom)
    val axisColor = palette.axis

    val gridLines: List[Figure] =
      val horizontal = ys.fold(List[Figure]()): (acc, mark) =>
        if !mark.major then acc else
          val y = frame.y(mark.position)
          style.gridLine(point(frame.left, y), point(frame.right, y), Ordinate, palette.grid) + acc

      abscissa match
        case scale: Scale =>
          xs.fold(horizontal): (acc, mark) =>
            if !mark.major then acc else
              val x = frame.x(mark.position)
              val top = point(x, frame.top)
              style.gridLine(top, point(x, frame.bottom), Abscissa, palette.grid) + acc

        case _ => horizontal

    // The gradations of one axis: each tick and, for a major gradation, its label.
    def gradations(marks: Sequence[Gradation], axis: Chart.Axis, at: Gradation => Point)
    :   List[Figure] =

      val figures = marks.fold(List[Figure]()): (acc, mark) =>
        val position = at(mark)
        val tick = style.tick(position, axis, mark.major, axisColor)

        val label =
          if mark.major then style.tickLabel(position, mark.label, axis, palette.text) else Nil

        label.reverse + (tick.reverse + acc)

      figures.reverse

    // The far-end arrowhead and the origin-end break mark of one axis, if the style draws them.
    def ends(ruler: Ruler, axis: Chart.Axis, tip: Point): List[Figure] =
      val break = if broken(ruler) then style.axisBreak(origin, axis, axisColor) else Nil
      style.arrowhead(tip, axis, axisColor) + break

    val abscissaFigures: List[Figure] =
      val line = style.axisLine(origin, point(frame.right, frame.bottom), Abscissa, axisColor)
      val marks = gradations(xs, Abscissa, mark => point(frame.x(mark.position), frame.bottom))

      val titleFigures = title(abscissa, style.abscissaTitle).lay(Nil): text =>
        val room = style.labelRoom(Abscissa, xs.filter(_.major).map(_.label).to[List])
        val at = point(frame.left + frame.width/2.0, frame.bottom + room + style.gap)
        style.axisTitle(at, text, Abscissa, palette.text)

      line + marks + titleFigures + ends(abscissa, Abscissa, point(frame.right, frame.bottom))

    val ordinateFigures: List[Figure] =
      val line = style.axisLine(point(frame.left, frame.top), origin, Ordinate, axisColor)
      val marks = gradations(ys, Ordinate, mark => point(frame.left, frame.y(mark.position)))

      val titleFigures = title(ordinate, style.ordinateTitle).lay(Nil): text =>
        val at = point(style.inset, frame.top + frame.height/2.0)
        style.axisTitle(at, text, Ordinate, palette.text)

      line + marks + titleFigures + ends(ordinate, Ordinate, point(frame.left, frame.top))

    List
      ( Svg.Id(t"grid") -> Group(gridLines.reverse, id = Svg.Id(t"grid")),
        Svg.Id(t"abscissa") -> Group(abscissaFigures, id = Svg.Id(t"abscissa")),
        Svg.Id(t"ordinate") -> Group(ordinateFigures, id = Svg.Id(t"ordinate")) )

  // The legend: a swatch and a name per series, stacked at the right or flowing along the bottom.
  def legend(frame: Frame, names: List[Text])
    ( using style: Chart.Style, palette: ChartPalette, metric: FontMetric )
  :   (Svg.Id, Figure) =

    val vertical = style.legend == Chart.Legend.Right
    val lineHeight = style.legendLineHeight
    var index = 0
    var x = frame.left
    var figures: List[Figure] = Nil

    names.foreach: name =>
      val y = if vertical then frame.top + index*lineHeight else frame.top
      val corner = point(x, y + (lineHeight - style.swatchSize)/2.0)
      val position = point(x + style.swatchSize + style.gap, y + lineHeight/2.0)
      val swatch = style.swatch(corner, palette.color(index), index)
      val entry = swatch + style.legendLabel(position, name, palette.text)
      figures = entry.reverse + figures
      if !vertical then x += style.swatchSize + style.gap + textWidth(name) + style.gap*3
      index += 1

    Svg.Id(t"legend") -> Group(figures.reverse, id = Svg.Id(t"legend"))

  // The parts of a chart whose legend, if any, occupies the given frame.
  def legendPart(frame: Optional[Frame], names: List[Text])
    ( using Chart.Style, ChartPalette, FontMetric )
  :   List[(Svg.Id, Figure)] =

    frame.lay(Nil): frame => List(legend(frame, names))

  // The parts behind a backdrop in the palette's background colour, so that a dark palette's
  // lettering is not set on the page's white.
  def drawing(parts: List[(Svg.Id, Figure)])(using style: Chart.Style, palette: ChartPalette)
  :   Chart.Drawing =

    val figures = style.backdrop(style.width, style.height, palette.background)
    val backdrop = Group(figures, id = Svg.Id(t"backdrop"))
    val all = (Svg.Id(t"backdrop") -> backdrop) :: parts
    Chart.Drawing(style.width, style.height, Nil, all.to[Ledger])
