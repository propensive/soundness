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
import cataclysm.{Css, Web}
import geodesy.*
import gossamer.*
import iridescence.*
import phoenicia.Font
import prepositional.*
import rudiments.*
import savagery.*
import spectacular.*
import vacuous.*

object Chart:
  enum Legend:
    case Hidden, Right, Bottom

  enum Axis:
    case Abscissa, Ordinate

  object Style:
    given standard: Standard = Standard()

  // What every kind of chart shares: the canvas, the font, the legend, and how the components
  // common to all of them — a run of text, the backdrop, a legend entry — become figures. A chart
  // kind computes the geometry (where a label sits, how tall a bar is) and calls one of these
  // methods for each component; a style overrides the methods it wants drawn differently, and
  // inherits the rest. `Standard` implements every kind's style with the defaults below.
  //
  // The font is a `Font in Web`: a face paired with a provision for its typeface, so a chart can
  // only be styled in a typeface the SVG will carry a `@font-face` for, or a generic family.
  trait Style:
    def width: Double
    def height: Double
    def inset: Double
    def font: Font in Web
    def fontSize: Double
    def legend: Legend

    // The unit of spacing between components, derived from the type size.
    def gap: Double = fontSize*0.4

    // The size and colour of a run of text; the face itself is the lettering's `font`.
    def fontStyle(color: Color in Srgb): Css.Style =
      css(t"font-size" -> px(fontSize), t"fill" -> hexOf(color))

    // Every label, title and legend entry is set through this, unless a more specific method is
    // overridden.
    def lettering
      ( position:   Point,
        text:       Text,
        anchor:     Lettering.Anchor,
        baseline:   Lettering.Baseline,
        color:      Color in Srgb,
        transforms: List[Transform] = Nil )
    :   Figure =

      Lettering
        ( position, text, anchor, baseline, style = fontStyle(color), font = font,
          transforms = transforms )

    def backdrop(width: Double, height: Double, color: Color in Srgb): List[Figure] =
      List(Rectangle(point(0.0, 0.0), width.toFloat, height.toFloat, style = filled(color)))

    def legendLineHeight: Double = fontSize*1.6
    def swatchSize: Double = fontSize

    def swatch(corner: Point, color: Color in Srgb, series: Int): List[Figure] =
      List(Rectangle(corner, swatchSize.toFloat, swatchSize.toFloat, style = filled(color)))

    def legendLabel(position: Point, name: Text, color: Color in Srgb): List[Figure] =
      List(lettering(position, name, Lettering.Anchor.Start, Lettering.Baseline.Middle, color))

  // What every chart with an abscissa and an ordinate shares: the axes and their gradations, the
  // grid, the titles, and the markers, error bars and point labels that lines and scatter plots
  // draw. Positions are given in the drawing's coordinates; the style decides the shape, the
  // stroke and where a label sits relative to its tick.
  trait Cartesian extends Style:
    def pitch: Double
    def tickLength: Double
    def grid: Boolean
    def abscissaTitle: Optional[Text]
    def ordinateTitle: Optional[Text]
    def strokeWidth: Double
    def markerRadius: Double

    def axisLine(from: Point, to: Point, axis: Axis, color: Color in Srgb): List[Figure] =
      List(Polyline(List(from, to), style = stroked(color, 1.0)))

    // Drawn at the far end of each axis; nothing by default.
    def arrowhead(tip: Point, axis: Axis, color: Color in Srgb): List[Figure] = Nil

    // Drawn at the origin end of a linear axis whose range does not include zero; nothing by
    // default.
    def axisBreak(at: Point, axis: Axis, color: Color in Srgb): List[Figure] = Nil

    def gridLine(from: Point, to: Point, axis: Axis, color: Color in Srgb): List[Figure] =
      if grid then List(Polyline(List(from, to), style = stroked(color, 1.0))) else Nil

    def tick(at: Point, axis: Axis, major: Boolean, color: Color in Srgb): List[Figure] =
      val far = axis match
        case Axis.Abscissa => point(at.x, at.y + tickLength)
        case Axis.Ordinate => point(at.x - tickLength, at.y)

      List(Polyline(List(at, far), style = stroked(color, 1.0)))

    def tickLabel(at: Point, text: Text, axis: Axis, color: Color in Srgb): List[Figure] =
      axis match
        case Axis.Abscissa =>
          val position = point(at.x, at.y + tickLength + gap)
          val anchor = Lettering.Anchor.Middle
          List(lettering(position, text, anchor, Lettering.Baseline.Hanging, color))

        case Axis.Ordinate =>
          val position = point(at.x - tickLength - gap, at.y)
          List(lettering(position, text, Lettering.Anchor.End, Lettering.Baseline.Middle, color))

    // The room an axis's ticks and labels take beyond its line, which the layout reserves: a
    // style that rotates or moves its labels overrides this to match.
    def labelRoom(axis: Axis, labels: List[Text])(using metric: FontMetric): Double = axis match
      case Axis.Abscissa => tickLength + gap + fontSize

      case Axis.Ordinate =>
        val widest = labels.fold(0.0): (acc, label) => acc.max(metric.width(label).value*fontSize)
        tickLength + gap + widest

    def titleRoom: Double = fontSize*1.6

    // `at` is the title's anchor: below the middle of the abscissa, or beside the middle of the
    // ordinate at the drawing's left edge, where the default turns the text upright.
    def axisTitle(at: Point, text: Text, axis: Axis, color: Color in Srgb): List[Figure] =
      axis match
        case Axis.Abscissa =>
          List(lettering(at, text, Lettering.Anchor.Middle, Lettering.Baseline.Hanging, color))

        case Axis.Ordinate =>
          val transforms =
            List(Transform.Translate(Delta(at.x, at.y)), Transform.Rotate(Angle.degrees(-90.0)))

          List
            ( lettering
                ( point(0.0, 0.0), text, Lettering.Anchor.Middle, Lettering.Baseline.Hanging,
                  color, transforms ) )

    def marker(at: Point, color: Color in Srgb, series: Int): List[Figure] =
      List(Circle(at, markerRadius.toFloat, style = filled(color)))

    def errorBar(x: Double, top: Double, bottom: Double, cap: Double, color: Color in Srgb)
    :   List[Figure] =

      val bar = stroked(color, 1.0)

      List
        ( Polyline(List(point(x, top), point(x, bottom)), style = bar),
          Polyline(List(point(x - cap, top), point(x + cap, top)), style = bar),
          Polyline(List(point(x - cap, bottom), point(x + cap, bottom)), style = bar) )

    // The note an `Annotated` point carries, set beside its marker.
    def pointLabel(at: Point, text: Text, color: Color in Srgb): List[Figure] =
      val position = point(at.x + markerRadius + gap/2.0, at.y)
      List(lettering(position, text, Lettering.Anchor.Start, Lettering.Baseline.Middle, color))

  // The default style of every chart kind, as a case class: its parameters are named
  // arguments, and any component's rendering is an override on an anonymous subclass.
  case class Standard
    ( width:         Double         = 640.0,
      height:        Double         = 400.0,
      inset:         Double         = 12.0,
      font:          Font in Web    = Web.sansSerifFont,
      fontSize:      Double         = 12.0,
      legend:        Legend         = Legend.Right,
      pitch:         Double         = 60.0,
      tickLength:    Double         = 5.0,
      grid:          Boolean        = true,
      abscissaTitle: Optional[Text] = Unset,
      ordinateTitle: Optional[Text] = Unset,
      strokeWidth:   Double         = 2.0,
      markerRadius:  Double         = 3.5,
      markers:       Boolean        = false,
      barGap:        Double         = 0.25,
      boxGap:        Double         = 0.25,
      hole:          Double         = 0.0 )
  extends Bars.Style, StackedBars.Style, Histogram.Style, Lines.Style, Scatter.Style, Boxes.Style,
    Pie.Style

  // A rendered chart as its parts, each under a stable identifier — `backdrop`, `grid`,
  // `abscissa`, `ordinate`, `legend`, `series-0`, `series-1`, … — in drawing order. The
  // identifiers are what a revision names.
  case class Drawing
    ( width: Double, height: Double, defs: List[Svg.Def], parts: Ledger[Svg.Id, Figure] ):
    def svg: Svg = Svg(width.toFloat, height.toFloat, defs, parts.values)

  // What changed between one drawing of a chart and the next: either the whole chart, because
  // the axes moved to fit the new data, or only the identified parts that differ. A page holding
  // the SVG needs only to replace an element by its identifier, or the whole element.
  enum Revision:
    case Redraw(svg: Svg)
    case Replace(id: Svg.Id, figure: Figure)

// Data, a chart kind that can draw it, and the fit of the one to the other. Immutable: `revise`
// yields a new chart for new data along with what has to change on the page, keeping the fit
// whenever it still accommodates the data so that the axes hold still. The style type is the
// kind's own (`Bars.Style` for bars), so a chart asks for exactly the components it draws.
class Chart[data, form, fit, style <: Chart.Style](val data: data, val form: form, val fit: fit)
  ( using val plottable: data is Plottable in form to fit by style ):

  def drawing(using style, ChartPalette, FontMetric): Chart.Drawing =
    plottable.draw(form, data, fit)

  def svg(using style, ChartPalette, FontMetric): Svg = drawing.svg

  def revise(data2: data)(using style, ChartPalette, FontMetric)
  :   (Chart[data, form, fit, style], List[Chart.Revision]) =

    if plottable.accommodates(form, fit, data2) then
      val next = Chart(data2, form, fit)

      val before: List[(Svg.Id, Text)] = drawing.parts.fold(List[(Svg.Id, Text)]()): (acc, pair) =>
        (pair(0), pair(1).xml.show) :: acc

      val revisions = next.drawing.parts.fold(List[Chart.Revision]()): (acc, pair) =>
        val rendered = pair(1).xml.show
        val unchanged = before.exists: (id, text) => id == pair(0) && text == rendered
        if unchanged then acc else Chart.Revision.Replace(pair(0), pair(1)) :: acc

      (next, revisions.reverse)
    else
      val next = Chart(data2, form, plottable.fit(form, data2))
      (next, List(Chart.Revision.Redraw(next.svg)))
