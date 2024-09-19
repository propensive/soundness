package geodesy

import gossamer.*
import rudiments.*
import spectacular.*
import symbolism.*
import prepositional.*

import scala.compiletime.*

export Geodesy.{Location, Radians, Degrees}

object Geodesy:
  private val range = math.pow(2, 32) - 2
  private given Decimalizer = Decimalizer(decimalPlaces = 6)

  opaque type Location = Long
  opaque type Radians = Double
  opaque type Degrees = Double

  object Location:
    given Location is Showable = location =>
      t"${location.latitude.degrees},${location.longitude.degrees}"

    private def fromRadians(latitude: Radians, longitude: Radians): Location =
      val latitude2 = ((latitude + math.Pi/2)*range/math.Pi).toLong
      val longitude2 = (longitude*range/(2*math.Pi)).toLong
      (latitude2 << 32) | longitude2
    
    def apply(latitude: Radians, longitude: Radians): Location = fromRadians(latitude, longitude)
    
    @targetName("applyDegrees")
    def apply(latitude: Degrees, longitude: Degrees): Location =
      fromRadians(latitude.radians, longitude.radians)

  object Radians:
    def apply(value: Double): Radians = value

    given Radians is Addable by Radians into Radians as addable =
      (left, right) => (left + right)%(2*math.Pi)
    
    given Radians is Subtractable by Radians into Radians as subtractable =
      (left, right) => (2*math.Pi + left - right)%(2*math.Pi)
  
  extension (radians: Radians)
    def degrees: Degrees = radians*180/math.Pi
    def value: Double = radians

  object Degrees:
    given Degrees is Showable = degrees => t"$degrees°"
    def apply(value: Double): Degrees = value

  extension (degrees: Degrees)
    def radians: Radians = degrees*math.Pi/180
    
    @targetName("degreesValue")
    def value: Double = degrees

  extension (left: Location)
    def latitude: Radians = (left >>> 32).toDouble/range*math.Pi - math.Pi/2
    def longitude: Radians = (left & 0xffffffffL).toDouble/range*2*math.Pi
    def pair: (Radians, Radians) = (latitude, longitude)

    def surfaceDistance(right: Location): Radians =
      val dLat = math.abs(left.latitude - right.latitude)
      val dLng = math.abs(left.longitude - right.longitude)

      val a = math.pow(math.sin(dLat/2), 2) +
          math.cos(left.latitude)*math.cos(right.latitude)*math.pow(math.sin(dLng/2), 2)
      
      2*math.atan2(math.sqrt(a), math.sqrt(1 - a))

    def bearing[CompassType](right: Location)(using compass: Bearing[CompassType]): CompassType =
      val dLng = math.abs(left.longitude - right.longitude)

      val result: Double =
        math.atan2
         (math.sin(dLng)*math.cos(right.latitude),
          math.cos(left.latitude)*math.sin(right.latitude) -
              math.sin(left.latitude)*math.cos(right.latitude)*math.cos(dLng))
      
      compass.from(result.rad)

extension (double: Double)
  def rad: Radians = Radians(double)
  def deg: Degrees = Degrees(double)

enum CardinalWind:
  case North, East, South, West

enum IntercardinalWind:
  case Northeast, Southeast, Southwest, Northwest

enum HalfWind:
  case NorthNortheast, EastNortheast, EastSoutheast, SouthSoutheast, SouthSouthwest, WestSouthwest,
      WestNorthwest, NorthNorthwest

export CardinalWind.*, IntercardinalWind.*, HalfWind.*

object Compass:
  val points4: IArray[CardinalWind] =
    IArray(North, East, South, West)
  
  val points8: IArray[CardinalWind | IntercardinalWind] =
    IArray(North, Northeast, East, Southeast, South, Southwest, West, Southwest)
  
  val points16: IArray[CardinalWind | IntercardinalWind | HalfWind] =
    IArray
     (North, NorthNortheast, Northeast, EastNortheast, East, EastSoutheast, Southeast,
      SouthSoutheast, South, SouthSouthwest, Southwest, WestSouthwest, West, WestNorthwest,
      Northwest, NorthNorthwest)

  inline def apply[PointsType <: 4 | 8 | 16](angle: Radians): Compass[PointsType] =
    inline erasedValue[PointsType] match
      case _: 4  => points4((0.5 + 2*angle.value/math.Pi).toInt%4)
      case _: 8  => points8((0.5 + 4*angle.value/math.Pi).toInt%8)
      case _: 16 => points16((0.5 + 8*angle.value/math.Pi).toInt%16)

type Compass[PointsType <: 4 | 8 | 16] = PointsType match
  case 4  => CardinalWind
  case 8  => CardinalWind | IntercardinalWind
  case 16 => CardinalWind | IntercardinalWind | HalfWind

trait Bearing[BearingType]:
  def from(radians: Radians): BearingType

package compassBearings:
  given Bearing[Compass[4]] as fourPointCompass = Compass[4](_)
  given Bearing[Compass[8]] as eightPointCompass = Compass[8](_)
  given Bearing[Compass[16]] as sixteenPointCompass = Compass[16](_)
  given Bearing[Degrees] as degreesFromNorth = _.degrees
  given Bearing[Radians] as radiansFromNorth = identity(_)
  given Bearing[Degrees] as degreesFromEast = radians => (radians - Radians(math.Pi/2)).degrees
  given Bearing[Radians] as radiansFromEast = radians => radians - Radians(math.Pi/2)