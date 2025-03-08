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
┃    Soundness, version 0.27.0.                                                                    ┃
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
package geodesy

import anticipation.*
import gossamer.*
import prepositional.*
import proscenium.*
import spectacular.*
import symbolism.*

object Geodesy:
  private val range = math.pow(2, 32) - 2
  private given Decimalizer = Decimalizer(decimalPlaces = 6)

  opaque type Location = Long
  opaque type Angle = Double

  object Location:
    given Location is Showable = location =>
      t"${location.latitude.degrees},${location.longitude.degrees}"

    private def fromAngle(latitude: Angle, longitude: Angle): Location =
      (encodeLatitude(latitude).toLong << 32) | (encodeLongitude(longitude) & 0xffffffffL)

    private def encodeLatitude(latitude: Angle): Int = (latitude*2*Int.MaxValue/math.Pi).toInt

    private def encodeLongitude(longitude: Angle): Int =
      ((longitude - math.Pi)*Int.MaxValue/math.Pi).toInt

    def apply(latitude: Angle, longitude: Angle): Location = fromAngle(latitude, longitude)

    def apply(north: Int, east: Int): Location =
      fromAngle(Degree*north/1000000.0, Degree*((360.0 + east/1000000.0)%360.0))

  object Angle:
    private val c = math.Pi*2
    def apply(value: Double): Angle = value

    given addable: Angle is Addable by Angle into Angle =
      (left, right) => (left + right)%(2*math.Pi)

    given subtractable: Angle is Subtractable by Angle into Angle =
      (left, right) => (2*math.Pi + left - right)%(2*math.Pi)

    given multiplicable: Angle is Multiplicable by Double into Angle =
      (left, right) => (left*right)%(2*math.Pi)

    given divisible: Angle is Divisible by Double into Angle =
      (left, right) => (left/right)%(2*math.Pi)

    given multiplicable2: Double is Multiplicable by Angle into Angle =
      (left, right) => (left*right)%(2*math.Pi)

  extension (angle: Angle)
    def degrees: Double = angle*180/math.Pi
    def radians: Double = angle

  extension (left: Location)
    def latitude: Angle = ((left >>> 32) & 0xffffffffL).toInt.toDouble/2/Int.MaxValue*math.Pi
    def longitude: Angle = (left & 0xffffffffL).toInt.toDouble/Int.MaxValue*math.Pi + math.Pi
    def pair: (Angle, Angle) = (latitude, longitude)

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
      :     Long =

        if count >= bits then value else (count%2).absolve match
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

    def surfaceDistance(right: Location): Angle =
      val dLat = math.abs(left.latitude - right.latitude)
      val dLng = math.abs(left.longitude - right.longitude)

      val a =
        math.pow(math.sin(dLat/2), 2)
        + math.cos(left.latitude)*math.cos(right.latitude)*math.pow(math.sin(dLng/2), 2)

      2*math.atan2(math.sqrt(a), math.sqrt(1 - a))

    def bearing[CompassType: Directional](right: Location): CompassType =
      val dLng = math.abs(left.longitude - right.longitude)

      val result: Double =
        math.atan2
         (math.sin(dLng)*math.cos(right.latitude),
          math.cos(left.latitude)*math.sin(right.latitude) -
              math.sin(left.latitude)*math.cos(right.latitude)*math.cos(dLng))

      CompassType.direction(Angle(result))
