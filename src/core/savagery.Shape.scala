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

case class Rectangle(position: Xy, width: Float, height: Float) extends Shape:
  def xml: Xml = unsafely(Xml.parse(t"""<rect x="${position.x.toDouble} y="${position.y.toDouble}" width="${width.toDouble}" height="${height.toDouble}"/>"""))

case class Path
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

  def moveTo(point: Xy): Path = Path(Move(Abs(point)) :: ops)
  def lineTo(point: Xy): Path = Path(Line(Abs(point)) :: ops)
  def move(vector: DxDy): Path = Path(Move(Rel(vector)) :: ops)
  def line(vector: DxDy): Path = Path(Line(Rel(vector)) :: ops)

  def curve(ctrl1: DxDy, ctrl2: DxDy, point: DxDy): Path =
    Path(Cubic(Rel(ctrl1), Rel(ctrl2), Rel(point)) :: ops)

  def curveTo(ctrl1: Xy, ctrl2: Xy, point: Xy): Path =
    Path(Cubic(Abs(ctrl1), Abs(ctrl2), Abs(point)) :: ops)

  def curve(ctrl2: DxDy, vector: DxDy): Path = Path(Cubic(Unset, Rel(ctrl2), Rel(vector)) :: ops)
  def curveTo(ctrl2: Xy, point: Xy): Path = Path(Cubic(Unset, Abs(ctrl2), Abs(point)) :: ops)

  def quadCurve(ctrl1: DxDy, vector: DxDy): Path = Path(Quadratic(Rel(ctrl1), Rel(vector)) :: ops)
  def quadCurveTo(ctrl1: Xy, point: Xy): Path = Path(Quadratic(Abs(ctrl1), Abs(point)) :: ops)

  def quadCurve(vector: DxDy): Path = Path(Quadratic(Unset, Rel(vector)) :: ops)
  def quadCurveTo(point: Xy): Path = Path(Quadratic(Unset, Abs(point)) :: ops)

  def moveUp(value: Float): Path = Path(Move(Rel(DxDy(value, 0.0))) :: ops)
  def moveDown(value: Float): Path = Path(Move(Rel(DxDy(-value, 0.0))) :: ops)
  def moveLeft(value: Float): Path = Path(Move(Rel(DxDy(0.0, -value))) :: ops)
  def moveRight(value: Float): Path = Path(Move(Rel(DxDy(0.0, value))) :: ops)

  def lineUp(value: Float): Path = Path(Line(Rel(DxDy(value, 0.0))) :: ops)
  def lineDown(value: Float): Path = Path(Line(Rel(DxDy(-value, 0.0))) :: ops)
  def lineLeft(value: Float): Path = Path(Line(Rel(DxDy(0.0, -value))) :: ops)
  def lineRight(value: Float): Path = Path(Line(Rel(DxDy(0.0, value))) :: ops)

  def closed: Path = Path(Close :: ops)

case class Ellipse(center: Xy, xRadius: Float, yRadius: Float, angle: Degrees) extends Shape:
  def circle: Boolean = xRadius == yRadius

  def xml: Xml = unsafely:
    Xml.parse:
      if circle
      then t"""<circle cx="${center.x.toDouble}" cy="${center.y.toDouble}" r="${xRadius.toDouble}"/>"""
      else t"""<ellipse cx="${center.x.toDouble}" cy="${center.y.toDouble}" rx="${xRadius.toDouble}" ry="${yRadius.toDouble}"/>"""
