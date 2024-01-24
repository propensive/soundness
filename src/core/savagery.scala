/*
    Savagery, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

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

import rudiments.*
import gossamer.*, decimalFormatting.javaDouble
import cardinality.*
import cataclysm.{Float as _, Length as _, *}
import perforate.*
import quantitative.*
import anticipation.*
import spectacular.*
import hieroglyph.*
import xylophone.*
import vacuous.*

extension (left: Float)
  @targetName("makeCoordinates")
  def !(right: Float) = Xy(left, right)

case class Xy(x: Float, y: Float)
case class DxDy(dx: Float, dy: Float)

object DxDy:
  given show: Show[DxDy] = value => t"${value.dx.toString} ${value.dy.toString}"

object Xy:
  given show: Show[Xy] = value => t"${value.x.toString} ${value.y.toString}"
  
object Savagery:
  opaque type Degrees = Double
  opaque type SvgId = Text

  object SvgId:
    def apply(id: Text): SvgId = id

  object Degrees:
    def apply(degrees: Double): Degrees = degrees

    given Show[Degrees] = _.toString.show
    
  extension (point: Xy)
    @targetName("plus")
    def +(vector: DxDy): Xy = Xy(point.x + vector.dx, point.y + vector.dy)
    
    @targetName("asVector")
    def unary_~ : DxDy = DxDy(point.x, point.y)
  
  extension (vector: DxDy)
    @targetName("plus2")
    def +(right: DxDy): DxDy = DxDy(vector.dx + right.dx, vector.dy + right.dy)

export Savagery.{Degrees, SvgId}

object Coords:
  given Show[Coords] =
    case Rel(vector) => vector.show
    case Abs(point)  => point.show

enum Coords:
  case Rel(vector: DxDy)
  case Abs(point: Xy)

  def key(char: Char): Text = this match
    case Rel(_) => char.show.lower
    case Abs(_) => char.show.upper
  
export Coords.{Rel, Abs}

enum PathOp:
  case Move(coords: Coords)
  case Line(coords: Coords)
  case Close
  case Cubic[CoordsType <: (Rel | Abs)](ctrl1: Optional[CoordsType], ctrl2: CoordsType, point: CoordsType)
  case Quadratic[CoordsType <: (Rel | Abs)](ctrl1: Optional[CoordsType], point: CoordsType)
  case Arc(rx: Float, ry: Float, angle: Degrees, largeArc: Boolean, sweep: Boolean, coords: Coords)

object PathOp:
  private def bit(value: Boolean): Text = if value then t"1" else t"0"
  
  given Show[PathOp] =
    case Move(coords)                => t"${coords.key('m')} $coords"
    case Line(Rel(DxDy(0.0f, v)))    => t"v ${v.toDouble}"
    case Line(Rel(DxDy(h, 0.0f)))    => t"h ${h.toDouble}"
    case Line(coords)                => t"${coords.key('l')} $coords"
    case Close                       => t"Z"
    case Cubic(Unset, ctrl2, coords) => t"${coords.key('s')} $ctrl2, $coords"
    case Cubic(ctrl1, ctrl2, coords) => t"${coords.key('c')} ${ctrl1.option.get}, $ctrl2, $coords"
    case Quadratic(Unset, coords)    => t"${coords.key('t')} $coords"
    case Quadratic(ctrl1, coords)    => t"${coords.key('q')} ${ctrl1.option.get}, $coords"
    
    case Arc(rx, ry, angle, largeArc, sweep, coords) =>
      t"${coords.key('a')} ${rx.toDouble} ${ry.toDouble} $angle ${bit(largeArc)} ${bit(sweep)} $coords"

case class Path
    (ops: List[PathOp] = Nil, style: Optional[CssStyle] = Unset, id: Optional[SvgId] = Unset,
        transform: List[Transform] = Nil)
extends Shape:
  import PathOp.*
  
  def xml: Xml =
    val d: Text = ops.reverse.map(_.show).join(t" ")
    // FIXME
    unsafely(Xml.parse(t"""<path d="$d"/>"""))
  
  def moveTo(point: Xy): Path = Path(Move(Abs(point)) :: ops)
  def lineTo(point: Xy): Path = Path(Line(Abs(point)) :: ops)
  def move(vector: DxDy): Path = Path(Move(Rel(vector)) :: ops)
  def line(vector: DxDy): Path = Path(Line(Rel(vector)) :: ops)

  def curve(ctrl1: DxDy, ctrl2: DxDy, point: DxDy): Path = Path(Cubic(Rel(ctrl1), Rel(ctrl2), Rel(point)) :: ops)
  def curveTo(ctrl1: Xy, ctrl2: Xy, point: Xy): Path = Path(Cubic(Abs(ctrl1), Abs(ctrl2), Abs(point)) :: ops)
  
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

object Circle:
  def apply(center: Xy, radius: Float): Ellipse = Ellipse(center, radius, radius, Degrees(0.0))

case class Rectangle(position: Xy, width: Float, height: Float) extends Shape:
  def xml: Xml = unsafely(Xml.parse(t"""<rect x="${position.x.toDouble} y="${position.y.toDouble}" width="${width.toDouble}" height="${height.toDouble}"/>"""))

case class Ellipse(center: Xy, xRadius: Float, yRadius: Float, angle: Degrees) extends Shape:
  def circle: Boolean = xRadius == yRadius
  
  def xml: Xml = unsafely:
    Xml.parse:
      if circle then t"""<circle cx="${center.x.toDouble}" cy="${center.y.toDouble}" r="${xRadius.toDouble}"/>"""
      else t"""<ellipse cx="${center.x.toDouble}" cy="${center.y.toDouble}" rx="${xRadius.toDouble}" ry="${yRadius.toDouble}"/>"""

case class Svg(width: Quantity[Units[1, Length]], height: Quantity[Units[1, Length]], viewWidth: Float, viewHeight: Float, defs: List[SvgDef], shapes: List[Shape])

sealed trait Shape:
  val transforms: List[Transform] = Nil

enum Transform:
  case Translate(vector: DxDy)
  case Scale(x: Float, y: Optional[Float])
  case Matrix()
  case Skew()
  case Rotate(angle: Degrees)

sealed trait SvgDef

case class LinearGradient[ColorType: RgbColor](stops: Stop[ColorType]*) extends SvgDef

case class Stop[ColorType: RgbColor](offset: 0.0 ~ 1.0, color: ColorType)

case class SvgDoc(svg: Svg, encoding: Encoding)


// extension (elem: Shape)
//   def translate(vector: DxDy) 
//   def scale(xScale: Float, yScale: Optional[Float])
//   def skew()
//   def rotate(angle: 0.0 ~ 360.0)
//   def matrix(x1: Float, x2: Float, x3: Float, y1: Float, y2: Float, y3: Float)
