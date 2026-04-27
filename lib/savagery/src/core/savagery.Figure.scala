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

import anticipation.*
import cataclysm.{Float as _, *}
import geodesy.*
import gossamer.*
import prepositional.*
import proscenium.*
import spectacular.*
import vacuous.*
import xylophone.*

sealed trait Figure:
  def xml: Xml

case class Rectangle(position: Point, width: Float, height: Float) extends Figure:
  def xml: Xml =
    given showable: Float is Showable = _.toString.tt
    x"<rect x=${position.x.show} y=${position.y.show} width=${width.show} height=${height.show}/>"

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
    id.let { svgId => attrs += t"id" -> svgId.text }

    if transforms.nonEmpty
    then attrs += t"transform" -> transforms.map(_.encode).join(t" ")

    style.let { css => attrs += t"style" -> css.properties.map(_.text).join(t";") }
    Element(t"path", attrs.result(), IArray())

  def moveTo(point: Point): Outline = copy(ops = Move(point) :: ops)
  def lineTo(point: Point): Outline = copy(ops = Draw(point) :: ops)
  def move(vector: Shift): Outline = copy(ops = Move(vector) :: ops)
  def line(vector: Shift): Outline = copy(ops = Draw(vector) :: ops)

  def curve(ctrl1: Shift, ctrl2: Shift, point: Shift): Outline =
    copy(ops = Cubic(ctrl1, ctrl2, point) :: ops)

  def curveTo(ctrl1: Point, ctrl2: Point, point: Point): Outline =
    copy(ops = Cubic(ctrl1, ctrl2, point) :: ops)

  def curve(ctrl2: Shift, vector: Shift): Outline = copy(ops = Cubic(Unset, ctrl2, vector) :: ops)
  def curveTo(ctrl2: Point, point: Point): Outline = copy(ops = Cubic(Unset, ctrl2, point) :: ops)
  def quadCurve(ctrl1: Shift, vector: Shift): Outline = copy(ops = Quadratic(ctrl1, vector) :: ops)
  def quadCurveTo(ctrl1: Point, point: Point): Outline = copy(ops = Quadratic(ctrl1, point) :: ops)
  def quadCurve(vector: Shift): Outline = copy(ops = Quadratic(Unset, vector) :: ops)
  def quadCurveTo(point: Point): Outline = copy(ops = Quadratic(Unset, point) :: ops)
  def moveUp(value: Float): Outline = copy(ops = Move(Shift(value, 0.0)) :: ops)
  def moveDown(value: Float): Outline = copy(ops = Move(Shift(-value, 0.0)) :: ops)
  def moveLeft(value: Float): Outline = copy(ops = Move(Shift(0.0, -value)) :: ops)
  def moveRight(value: Float): Outline = copy(ops = Move(Shift(0.0, value)) :: ops)
  def lineUp(value: Float): Outline = copy(ops = Draw(Shift(value, 0.0)) :: ops)
  def lineDown(value: Float): Outline = copy(ops = Draw(Shift(-value, 0.0)) :: ops)
  def lineLeft(value: Float): Outline = copy(ops = Draw(Shift(0.0, -value)) :: ops)
  def lineRight(value: Float): Outline = copy(ops = Draw(Shift(0.0, value)) :: ops)
  def closed: Outline = copy(ops = Close :: ops)

case class Ellipse(center: Point, xRadius: Float, yRadius: Float, angle: Angle) extends Figure:
  def circle: Boolean = xRadius == yRadius

  def xml: Xml =
    given showable: Float is Showable = _.toString.tt
    if circle
    then x"<circle cx=${center.x.show} cy=${center.y.show} r=${xRadius.show}/>"
    else x"<ellipse cx=${center.x.show} cy=${center.y.show} rx=${xRadius.show} ry=${yRadius.show}/>"
