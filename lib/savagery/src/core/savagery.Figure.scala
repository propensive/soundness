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

import anticipation.*
import cataclysm.{Float as _, Length as _, *}
import contingency.*
import geodesy.*
import gossamer.*
import spectacular.*
import vacuous.*
import xylophone.*

sealed trait Figure:
  val transforms: List[Transform] = Nil

case class Rectangle(position: Point, width: Float, height: Float) extends Figure:
  def xml: Xml = unsafely:
    given showable: Float is Showable = _.toString.tt
    x"<rect x=${position.x.show} y=${position.y.show} width=${width.show} height=${height.show}/>"

case class Outline
  ( ops:       List[Stroke]       = Nil,
    style:     Optional[CssStyle] = Unset,
    id:        Optional[SvgId]    = Unset,
    transform: List[Transform]    = Nil)
extends Figure:
  import Stroke.*

  def xml: Xml =
    val d: Text = ops.reverse.map(_.encode).join(t" ")
    x"<path d=$d/>"

  def moveTo(point: Point): Outline = Outline(Move(point) :: ops)
  def lineTo(point: Point): Outline = Outline(Draw(point) :: ops)
  def move(vector: Shift): Outline = Outline(Move(vector) :: ops)
  def line(vector: Shift): Outline = Outline(Draw(vector) :: ops)

  def curve(ctrl1: Shift, ctrl2: Shift, point: Shift): Outline =
    Outline(Cubic(ctrl1, ctrl2, point) :: ops)

  def curveTo(ctrl1: Point, ctrl2: Point, point: Point): Outline =
    Outline(Cubic(ctrl1, ctrl2, point) :: ops)

  def curve(ctrl2: Shift, vector: Shift): Outline = Outline(Cubic(Unset, ctrl2, vector) :: ops)
  def curveTo(ctrl2: Point, point: Point): Outline = Outline(Cubic(Unset, ctrl2, point) :: ops)

  def quadCurve(ctrl1: Shift, vector: Shift): Outline = Outline(Quadratic(ctrl1, vector) :: ops)
  def quadCurveTo(ctrl1: Point, point: Point): Outline = Outline(Quadratic(ctrl1, point) :: ops)

  def quadCurve(vector: Shift): Outline = Outline(Quadratic(Unset, vector) :: ops)
  def quadCurveTo(point: Point): Outline = Outline(Quadratic(Unset, point) :: ops)

  def moveUp(value: Float): Outline = Outline(Move(Shift(value, 0.0)) :: ops)
  def moveDown(value: Float): Outline = Outline(Move(Shift(-value, 0.0)) :: ops)
  def moveLeft(value: Float): Outline = Outline(Move(Shift(0.0, -value)) :: ops)
  def moveRight(value: Float): Outline = Outline(Move(Shift(0.0, value)) :: ops)

  def lineUp(value: Float): Outline = Outline(Draw(Shift(value, 0.0)) :: ops)
  def lineDown(value: Float): Outline = Outline(Draw(Shift(-value, 0.0)) :: ops)
  def lineLeft(value: Float): Outline = Outline(Draw(Shift(0.0, -value)) :: ops)
  def lineRight(value: Float): Outline = Outline(Draw(Shift(0.0, value)) :: ops)

  def closed: Outline = Outline(Close :: ops)

case class Ellipse(center: Point, xRadius: Float, yRadius: Float, angle: Angle) extends Figure:
  def circle: Boolean = xRadius == yRadius

  def xml: Xml =
    given showable: Float is Showable = _.toString.tt
    if circle
    then x"<circle cx=${center.x.show} cy=${center.y.show} r=${xRadius.show}/>"
    else x"<ellipse cx=${center.x.show} cy=${center.y.show} rx=${xRadius.show} ry=${yRadius.show}/>"
