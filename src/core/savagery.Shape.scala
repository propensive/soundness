/*
    Savagery, version [unreleased]. Copyright 2025 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package savagery

import anticipation.*
import cataclysm.{Float as _, Length as _, *}
import contingency.*
import gossamer.*, decimalFormatters.java
import rudiments.*
import spectacular.*
import vacuous.*
import xylophone.*

sealed trait Shape:
  val transforms: List[Transform] = Nil

case class Rectangle(position: Point, width: Float, height: Float) extends Shape:
  def xml: Xml = unsafely(Xml.parse(t"""<rect x="${position.x.toDouble} y="${position.y.toDouble}" width="${width.toDouble}" height="${height.toDouble}"/>"""))

case class Line
   (ops:       List[PathOp]       = Nil,
    style:     Optional[CssStyle] = Unset,
    id:        Optional[SvgId]    = Unset,
    transform: List[Transform]    = Nil)
extends Shape:
  import PathOp.*

  def xml: Xml =
    val d: Text = ops.reverse.map(_.encode).join(t" ")
    // FIXME
    unsafely(Xml.parse(t"""<path d="$d"/>"""))

  def moveTo(point: Point): Line = Line(Move(Abs(point)) :: ops)
  def lineTo(point: Point): Line = Line(Draw(Abs(point)) :: ops)
  def move(vector: DxDy): Line = Line(Move(Rel(vector)) :: ops)
  def line(vector: DxDy): Line = Line(Draw(Rel(vector)) :: ops)

  def curve(ctrl1: DxDy, ctrl2: DxDy, point: DxDy): Line =
    Line(Cubic(Rel(ctrl1), Rel(ctrl2), Rel(point)) :: ops)

  def curveTo(ctrl1: Point, ctrl2: Point, point: Point): Line =
    Line(Cubic(Abs(ctrl1), Abs(ctrl2), Abs(point)) :: ops)

  def curve(ctrl2: DxDy, vector: DxDy): Line = Line(Cubic(Unset, Rel(ctrl2), Rel(vector)) :: ops)
  def curveTo(ctrl2: Point, point: Point): Line = Line(Cubic(Unset, Abs(ctrl2), Abs(point)) :: ops)

  def quadCurve(ctrl1: DxDy, vector: DxDy): Line = Line(Quadratic(Rel(ctrl1), Rel(vector)) :: ops)
  def quadCurveTo(ctrl1: Point, point: Point): Line = Line(Quadratic(Abs(ctrl1), Abs(point)) :: ops)

  def quadCurve(vector: DxDy): Line = Line(Quadratic(Unset, Rel(vector)) :: ops)
  def quadCurveTo(point: Point): Line = Line(Quadratic(Unset, Abs(point)) :: ops)

  def moveUp(value: Float): Line = Line(Move(Rel(DxDy(value, 0.0))) :: ops)
  def moveDown(value: Float): Line = Line(Move(Rel(DxDy(-value, 0.0))) :: ops)
  def moveLeft(value: Float): Line = Line(Move(Rel(DxDy(0.0, -value))) :: ops)
  def moveRight(value: Float): Line = Line(Move(Rel(DxDy(0.0, value))) :: ops)

  def lineUp(value: Float): Line = Line(Draw(Rel(DxDy(value, 0.0))) :: ops)
  def lineDown(value: Float): Line = Line(Draw(Rel(DxDy(-value, 0.0))) :: ops)
  def lineLeft(value: Float): Line = Line(Draw(Rel(DxDy(0.0, -value))) :: ops)
  def lineRight(value: Float): Line = Line(Draw(Rel(DxDy(0.0, value))) :: ops)

  def closed: Line = Line(Close :: ops)

case class Ellipse(center: Point, xRadius: Float, yRadius: Float, angle: Degrees) extends Shape:
  def circle: Boolean = xRadius == yRadius

  def xml: Xml = unsafely:
    Xml.parse:
      if circle
      then t"""<circle cx="${center.x.toDouble}" cy="${center.y.toDouble}" r="${xRadius.toDouble}"/>"""
      else t"""<ellipse cx="${center.x.toDouble}" cy="${center.y.toDouble}" rx="${xRadius.toDouble}" ry="${yRadius.toDouble}"/>"""
