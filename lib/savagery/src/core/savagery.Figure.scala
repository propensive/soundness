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
┃    Soundness, version 0.54.0.                                                                    ┃
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

import scala.collection.immutable.SeqMap
import denominative.*

import anticipation.*
import cataclysm.{Float as _, *}
import geodesy.*
import gossamer.*
import spectacular.*
import vacuous.*
import xylophone.*

sealed trait Figure:
  def xml: Xml

case class Rectangle
  ( position:   Point,
    width:      Float,
    height:     Float,
    transforms: List[Transform] = Nil )
extends Figure:

  def xml: Xml =
    given showable: Float is Showable = _.toString.tt
    val attrs = SeqMap.newBuilder[Text, Text]
    attrs += t"x" -> position.x.show
    attrs += t"y" -> position.y.show
    attrs += t"width" -> width.show
    attrs += t"height" -> height.show

    if !transforms.nil
    then attrs += t"transform" -> transforms.map(_.encode).join(t" ")

    Element(t"rect", Attributes.from(Map.from(attrs.result())), IArray())

case class Outline
  ( ops:        List[Stroke]       = Nil,
    style:      Optional[CssStyle] = Unset,
    id:         Optional[SvgId]    = Unset,
    transforms: List[Transform]    = Nil )
extends Figure:

  import Stroke.*

  def xml: Xml =
    val d: Text = ops.reverse.map(_.encode).join(t" ")
    val attrs = SeqMap.newBuilder[Text, Text]
    attrs += t"d" -> d
    id.let: svgId => attrs += t"id" -> svgId.text

    if !transforms.nil
    then attrs += t"transform" -> transforms.map(_.encode).join(t" ")

    style.let: css => attrs += t"style" -> css.properties.map(_.text).join(t";")
    Element(t"path", Attributes.from(Map.from(attrs.result())), IArray())

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
    transforms: List[Transform] = Nil )
extends Figure:

  def circle: Boolean = xRadius == yRadius

  def xml: Xml =
    given showable: Float is Showable = _.toString.tt
    val attrs = SeqMap.newBuilder[Text, Text]
    attrs += t"cx" -> center.x.show
    attrs += t"cy" -> center.y.show

    if circle then attrs += t"r" -> xRadius.show
    else
      attrs += t"rx" -> xRadius.show
      attrs += t"ry" -> yRadius.show

    if !transforms.nil
    then attrs += t"transform" -> transforms.map(_.encode).join(t" ")

    Element(if circle then t"circle" else t"ellipse", Attributes.from(Map.from(attrs.result())), IArray())
