package savagery

import rudiments.*
import gossamer.*, decimalFormats.exact
import cardinality.*
import cataclysm.{Float as _, Length as _, *}
import turbulence.*
import quantify.*
import xylophone.*
import iridescence.*

import java.lang as jl

import unsafeExceptions.canThrowAny

object SavageryInternals:
  opaque type Xy = Long
  opaque type DxDy = Long
  opaque type Degrees = Double
  opaque type SvgId = Text

  object SvgId:
    def apply(id: Text): SvgId = id

  object DxDy:
    def apply(dx: Float, dy: Float): DxDy =
      (jl.Float.floatToRawIntBits(dx).toLong << 32) + jl.Float.floatToRawIntBits(dy)
    
    given show: Show[DxDy] = value => t"${value.dx.toString} ${value.dy.toString}"

  object Xy:
    def apply(x: Float, y: Float): Xy =
      (jl.Float.floatToRawIntBits(x).toLong << 32) + jl.Float.floatToRawIntBits(y)

    given show: Show[Xy] = value => t"${value.x.toString} ${value.y.toString}"
  
  object Degrees:
    def apply(degrees: Double): Degrees = degrees

    given Show[Degrees] = _.toString.show
    
  extension (point: Xy)
    def x: Float = jl.Float.intBitsToFloat((point >> 32).toInt)
    def y: Float = jl.Float.intBitsToFloat(point.toInt)
    def unary_~ : DxDy = DxDy(x, y)
  
  extension (vector: DxDy)
    def dx: Float = jl.Float.intBitsToFloat((vector >> 32).toInt)
    def dy: Float = jl.Float.intBitsToFloat(vector.toInt)

export SavageryInternals.{Xy, DxDy, Degrees, SvgId}

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
  case Cubic[CoordsType <: (Rel | Abs)](ctrl1: Maybe[CoordsType], ctrl2: CoordsType, point: CoordsType)
  case Quadratic[CoordsType <: (Rel | Abs)](ctrl1: Maybe[CoordsType], point: CoordsType)
  case Arc(rx: Float, ry: Float, angle: Degrees, largeArc: Boolean, sweep: Boolean, coords: Coords)

object PathOp:
  private def bit(value: Boolean): Text = if value then t"1" else t"0"
  
  given Show[PathOp] =
    case Move(coords)                => t"${coords.key('m')} $coords"
    case Line(coords)                => t"${coords.key('l')} $coords"
    case Close                       => t"Z"
    case Cubic(Unset, ctrl2, coords) => t"${coords.key('s')} $ctrl2, $coords"
    case Cubic(ctrl1, ctrl2, coords) => t"${coords.key('c')} ${ctrl1.option.get}, $ctrl2, $coords"
    case Quadratic(Unset, coords)    => t"${coords.key('t')} $coords"
    case Quadratic(ctrl1, coords)    => t"${coords.key('q')} ${ctrl1.option.get}, $coords"
    
    case Arc(rx, ry, angle, largeArc, sweep, coords) =>
      t"${coords.key('a')} $rx $ry $angle ${bit(largeArc)} ${bit(sweep)} $coords"

case class Path
    (ops: List[PathOp], style: Maybe[CssStyle] = Unset, id: Maybe[SvgId] = Unset,
        transform: List[Transform] = Nil)
extends Shape:
  import PathOp.*
  
  def xml: Xml =
    val d: Text = ops.reverse.map(_.show).join(t" ")
    // FIXME
    Xml.parse(t"""<path d="$d"/>""")
  
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

case class Rectangle() extends Shape:
  def xml: Xml = Xml.parse(t"<rect/>")

case class Ellipse(center: Xy, xRadius: Float, yRadius: Float, angle: Degrees) extends Shape:
  def circle: Boolean = xRadius == yRadius
  def xml: Xml = Xml.parse(if circle then t"<circle/>" else t"<ellipse/>")

case class Svg(width: Quantity[Units[1, Length]], height: Quantity[Units[1, Length]], viewWidth: Float, viewHeight: Float, defs: List[SvgDef], shapes: List[Shape])

sealed trait Shape:
  val transforms: List[Transform] = Nil

enum Transform:
  case Translate(vector: DxDy)
  case Scale(x: Float, y: Maybe[Float])
  case Matrix()
  case Skew()
  case Rotate(angle: Degrees)

sealed trait SvgDef

case class LinearGradient(stops: Stop*) extends SvgDef
case class Stop(offset: 0.0 ~ 1.0, color: Color)


case class SvgDoc(svg: Svg, encoding: Encoding)


// extension (elem: Shape)
//   def translate(vector: DxDy) 
//   def scale(xScale: Float, yScale: Maybe[Float])
//   def skew()
//   def rotate(angle: 0.0 ~ 360.0)
//   def matrix(x1: Float, x2: Float, x3: Float, y1: Float, y2: Float, y3: Float)
