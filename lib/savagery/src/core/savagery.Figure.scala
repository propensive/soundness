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
package savagery

import scala.collection.immutable.VectorMap
import scala.collection.mutable.Builder

import anticipation.*
import cataclysm.Css
import denominative.*
import geodesy.*
import gossamer.*
import rudiments.*
import spectacular.*
import vacuous.*
import xylophone.*

object Figure:
  private def fields(pairs: (Text, Text)*): Text =
    pairs.map { (label, value) => s"${label.s}:${value.s}" }.mkString(" ╱ ").tt

  private def optional[value: Inspectable](value: Optional[value]): Text =
    value.lay(t"○"): value => t"｢${value.inspect}｣"

  // The attributes any figure may carry besides its geometry — identifier, transform list and
  // inline style — appended in that order, and only when present, so a figure without them
  // serializes exactly as it did before they existed.
  private[savagery] def decorate
    ( attrs:      Builder[(Text, Text), VectorMap[Text, Text]],
      id:         Optional[Svg.Id],
      transforms: List[Transform],
      style:      Optional[Css.Style] )
  :   Unit =

    id.let: svgId => attrs += t"id" -> svgId.text
    if !transforms.nil then attrs += t"transform" -> transforms.map(_.encode).join(t" ")
    style.let: css => attrs += t"style" -> css.text

  // A figure's `xml` is its serialized form: multi-line, and (for an `Outline`) with every path
  // operation compressed into one `d` attribute. Inspection names the case and labels each field
  // instead, so the strokes and the transform list — the state a misplaced figure is debugged
  // from — stay individually legible. A `Css.Style` has no inspection of its own, so its property
  // text is rendered here rather than borrowed.
  given inspectable: [figure <: Figure] => figure is Inspectable = figure =>
    given styleInspectable: Css.Style is Inspectable = _.text.inspect

    figure.absolve match
      case Rectangle(position, width, height, transforms, style, id) =>
        val body =
          fields
            ( t"position"   -> position.inspect,
              t"width"      -> width.inspect,
              t"height"     -> height.inspect,
              t"transforms" -> transforms.inspect,
              t"style"      -> optional(style),
              t"id"         -> optional(id) )

        t"Rectangle($body)"

      case Outline(ops, style, id, transforms) =>
        val body =
          fields
            ( t"ops"        -> ops.inspect,
              t"style"      -> optional(style),
              t"id"         -> optional(id),
              t"transforms" -> transforms.inspect )

        t"Outline($body)"

      case Ellipse(center, xRadius, yRadius, angle, transforms, style, id) =>
        val body =
          fields
            ( t"center"     -> center.inspect,
              t"xRadius"    -> xRadius.inspect,
              t"yRadius"    -> yRadius.inspect,
              t"angle"      -> angle.inspect,
              t"transforms" -> transforms.inspect,
              t"style"      -> optional(style),
              t"id"         -> optional(id) )

        t"Ellipse($body)"

      case Group(figures, id, style, transforms) =>
        val body =
          fields
            ( t"figures"    -> figures.inspect,
              t"id"         -> optional(id),
              t"style"      -> optional(style),
              t"transforms" -> transforms.inspect )

        t"Group($body)"

      case Polyline(points, closed, id, style, transforms) =>
        val body =
          fields
            ( t"points"     -> points.inspect,
              t"closed"     -> closed.inspect,
              t"id"         -> optional(id),
              t"style"      -> optional(style),
              t"transforms" -> transforms.inspect )

        t"Polyline($body)"

      case Lettering(position, text, anchor, baseline, id, style, transforms) =>
        val body =
          fields
            ( t"position"   -> position.inspect,
              t"text"       -> text.inspect,
              t"anchor"     -> anchor.inspect,
              t"baseline"   -> optional(baseline),
              t"id"         -> optional(id),
              t"style"      -> optional(style),
              t"transforms" -> transforms.inspect )

        t"Lettering($body)"

sealed trait Figure:
  def xml: Xml

case class Rectangle
  ( position:   Point,
    width:      Float,
    height:     Float,
    transforms: List[Transform]     = Nil,
    style:      Optional[Css.Style] = Unset,
    id:         Optional[Svg.Id]    = Unset )
extends Figure:

  def xml: Xml =
    given showable: Float is Showable = _.toString.tt
    val attrs = VectorMap.newBuilder[Text, Text]
    attrs += t"x" -> position.x.show
    attrs += t"y" -> position.y.show
    attrs += t"width" -> width.show
    attrs += t"height" -> height.show
    Figure.decorate(attrs, id, transforms, style)

    Element(t"rect", Attributes.from(attrs.result().to(Map)), Array())

case class Outline
  ( ops:        List[Stroke]        = Nil,
    style:      Optional[Css.Style] = Unset,
    id:         Optional[Svg.Id]    = Unset,
    transforms: List[Transform]     = Nil )
extends Figure:

  import Stroke.*

  def xml: Xml =
    val d: Text = ops.reverse.map(_.encode).join(t" ")
    val attrs = VectorMap.newBuilder[Text, Text]
    attrs += t"d" -> d
    Figure.decorate(attrs, id, transforms, style)

    Element(t"path", Attributes.from(attrs.result().to(Map)), Array())

  def moveTo(point: Point): Outline = copy(ops = MoveTo(point) :: ops)
  def lineTo(point: Point): Outline = copy(ops = DrawTo(point) :: ops)
  def move(vector: Delta): Outline = copy(ops = Move(vector) :: ops)
  def line(vector: Delta): Outline = copy(ops = Draw(vector) :: ops)

  def curve(ctrl1: Delta, ctrl2: Delta, point: Delta): Outline =
    copy(ops = Cubic(ctrl1, ctrl2, point) :: ops)

  def curveTo(ctrl1: Point, ctrl2: Point, point: Point): Outline =
    copy(ops = CubicTo(ctrl1, ctrl2, point) :: ops)

  def curve(ctrl2: Delta, vector: Delta): Outline = copy(ops = Cubic(Unset, ctrl2, vector) :: ops)
  def curveTo(ctrl2: Point, point: Point): Outline = copy(ops = CubicTo(Unset, ctrl2, point) :: ops)
  def quadCurve(ctrl1: Delta, vector: Delta): Outline = copy(ops = Quadratic(ctrl1, vector) :: ops)

  def quadCurveTo(ctrl1: Point, point: Point): Outline =
    copy(ops = QuadraticTo(ctrl1, point) :: ops)

  def quadCurve(vector: Delta): Outline = copy(ops = Quadratic(Unset, vector) :: ops)
  def quadCurveTo(point: Point): Outline = copy(ops = QuadraticTo(Unset, point) :: ops)
  def moveUp(value: Float): Outline = copy(ops = Move(Delta(value, 0.0)) :: ops)
  def moveDown(value: Float): Outline = copy(ops = Move(Delta(-value, 0.0)) :: ops)
  def moveLeft(value: Float): Outline = copy(ops = Move(Delta(0.0, -value)) :: ops)
  def moveRight(value: Float): Outline = copy(ops = Move(Delta(0.0, value)) :: ops)
  def lineUp(value: Float): Outline = copy(ops = Draw(Delta(value, 0.0)) :: ops)
  def lineDown(value: Float): Outline = copy(ops = Draw(Delta(-value, 0.0)) :: ops)
  def lineLeft(value: Float): Outline = copy(ops = Draw(Delta(0.0, -value)) :: ops)
  def lineRight(value: Float): Outline = copy(ops = Draw(Delta(0.0, value)) :: ops)
  def closed: Outline = copy(ops = Close :: ops)

case class Ellipse
  ( center:     Point,
    xRadius:    Float,
    yRadius:    Float,
    angle:      Angle,
    transforms: List[Transform]     = Nil,
    style:      Optional[Css.Style] = Unset,
    id:         Optional[Svg.Id]    = Unset )
extends Figure:

  def circle: Boolean = xRadius == yRadius

  def xml: Xml =
    given showable: Float is Showable = _.toString.tt
    val attrs = VectorMap.newBuilder[Text, Text]
    attrs += t"cx" -> center.x.show
    attrs += t"cy" -> center.y.show

    if circle then attrs += t"r" -> xRadius.show
    else
      attrs += t"rx" -> xRadius.show
      attrs += t"ry" -> yRadius.show

    Figure.decorate(attrs, id, transforms, style)

    val label = if circle then t"circle" else t"ellipse"
    Element(label, Attributes.from(attrs.result().to(Map)), Array())

// A `<g>` element: figures that move, style and identify together. A chart's axes or one of its
// series is a group, so that the whole part can be replaced by its identifier when it changes.
case class Group
  ( figures:    List[Figure],
    id:         Optional[Svg.Id]    = Unset,
    style:      Optional[Css.Style] = Unset,
    transforms: List[Transform]     = Nil )
extends Figure:

  def xml: Xml =
    val attrs = VectorMap.newBuilder[Text, Text]
    Figure.decorate(attrs, id, transforms, style)

    Element(t"g", Attributes.from(attrs.result().to(Map)), figures.map(_.xml).nodes)

// A `<polyline>` through absolute points, or a `<polygon>` when `closed`: the shape of a plotted
// line, an area under it, or a bar's outline, without a path operation per vertex.
case class Polyline
  ( points:     List[Point],
    closed:     Boolean             = false,
    id:         Optional[Svg.Id]    = Unset,
    style:      Optional[Css.Style] = Unset,
    transforms: List[Transform]     = Nil )
extends Figure:

  def xml: Xml =
    val attrs = VectorMap.newBuilder[Text, Text]
    def pair(point: Point): Text = t"${point.x.toString},${point.y.toString}"
    attrs += t"points" -> points.map(pair).join(t" ")
    Figure.decorate(attrs, id, transforms, style)
    val label = if closed then t"polygon" else t"polyline"

    Element(label, Attributes.from(attrs.result().to(Map)), Array())

object Lettering:
  // Where the text sits relative to its position: `text-anchor`.
  enum Anchor:
    case Start, Middle, End

    def text: Text = this match
      case Start  => t"start"
      case Middle => t"middle"
      case End    => t"end"

  // Which line of the text the position lies on: `dominant-baseline`. Unset leaves it to the
  // renderer, which is the alphabetic baseline.
  enum Baseline:
    case Alphabetic, Middle, Hanging

    def text: Text = this match
      case Alphabetic => t"alphabetic"
      case Middle     => t"middle"
      case Hanging    => t"hanging"

// A `<text>` element: a run of characters set at a position, anchored at its start, middle or end.
// Named for what it is in a drawing — lettering — since `Text` is the string type.
case class Lettering
  ( position:   Point,
    text:       Text,
    anchor:     Lettering.Anchor             = Lettering.Anchor.Start,
    baseline:   Optional[Lettering.Baseline] = Unset,
    id:         Optional[Svg.Id]             = Unset,
    style:      Optional[Css.Style]          = Unset,
    transforms: List[Transform]              = Nil )
extends Figure:

  def xml: Xml =
    given showable: Float is Showable = _.toString.tt
    val attrs = VectorMap.newBuilder[Text, Text]
    attrs += t"x" -> position.x.show
    attrs += t"y" -> position.y.show
    if anchor != Lettering.Anchor.Start then attrs += t"text-anchor" -> anchor.text
    baseline.let: baseline => attrs += t"dominant-baseline" -> baseline.text
    Figure.decorate(attrs, id, transforms, style)

    Element(t"text", Attributes.from(attrs.result().to(Map)), List[Xml](TextNode(text)).nodes)
