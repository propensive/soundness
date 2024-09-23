package geodesy

import gossamer.*
import rudiments.*
import spectacular.*
import symbolism.*
import prepositional.*

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
