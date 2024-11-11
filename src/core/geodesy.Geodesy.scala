package geodesy

import gossamer.*
import rudiments.*
import anticipation.*
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
      (encodeLatitude(latitude).toLong << 32) | (encodeLongitude(longitude) & 0xffffffffL)

    private def encodeLatitude(latitude: Radians): Int = (latitude*2*Int.MaxValue/math.Pi).toInt

    private def encodeLongitude(longitude: Radians): Int =
      ((longitude - math.Pi)*Int.MaxValue/math.Pi).toInt

    def apply(latitude: Radians, longitude: Radians): Location = fromRadians(latitude, longitude)

    @targetName("applyDegrees")
    def apply(latitude: Degrees, longitude: Degrees): Location =
      fromRadians(latitude.radians, longitude.radians)

  object Radians:
    private val c = math.Pi*2
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
    def latitude: Radians = ((left >>> 32) & 0xffffffffL).toInt.toDouble/2/Int.MaxValue*math.Pi
    def longitude: Radians = (left & 0xffffffffL).toInt.toDouble/Int.MaxValue*math.Pi + math.Pi
    def pair: (Radians, Radians) = (latitude, longitude)

    def geohash(length: Int): Text =

      val bits = length*5
      val lat: Int = ((left >>> 32)&0xffffffffL).toInt

      val long: Int =
        val long0 = left&0xffffffffL
        if long0 < 0 then (long0 + Int.MaxValue).toInt else (long0 - Int.MaxValue).toInt

      def recur
         (value:   Long,
          latMin:  Long,
          latMax:  Long,
          longMin: Long,
          longMax: Long,
          count: Int)
              : Long =

        if count >= bits then value else (count%2: @unchecked) match
          case 0 =>
            val midpoint = (longMin + longMax)/2
            if long < midpoint
            then recur(value << 1, latMin, latMax, longMin, midpoint, count + 1)
            else recur((value << 1) | 1L, latMin, latMax, midpoint, longMax, count + 1)

          case 1 =>
            val midpoint = (latMin + latMax)/2
            if lat < midpoint
            then recur(value << 1, latMin, midpoint, longMin, longMax, count + 1)
            else recur((value << 1) | 1L, midpoint, latMax, longMin, longMax, count + 1)

      val binary = recur(0L, Int.MinValue + 1, Int.MaxValue, Int.MinValue + 1, Int.MaxValue, 0)

      Text:
        IArray.tabulate[Char](length): index =>
          "0123456789bcdefghjkmnpqrstuvwxyz".charAt((binary >> ((length - index - 1)*5)&31).toInt)

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
