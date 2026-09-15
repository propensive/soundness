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
┃    Soundness, version 0.64.0.                                                                    ┃
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

import scala.math

import soundness.*

import strategies.throwUnsafely
import errorDiagnostics.stackTracesDiagnostics

object Tests extends Suite(m"Geodesy tests"):
  def run(): Unit =
    // Latitude and longitude are each packed into 32 bits, so a location only returns the nearest
    // representable angles: about 8×10⁻⁸ degrees apart for longitude, half that for latitude.
    def near(left: Double, right: Double, tolerance: Double = 1e-9): Boolean =
      math.abs(left - right) < tolerance

    def nearDegrees(left: Angle, right: Double): Boolean = near(left.degrees, right, 1e-6)

    val london = Location(51.5074.deg, Angle.degrees(-0.1278))
    val paris = Location(48.8566.deg, 2.3522.deg)
    val sydney = Location(Angle.degrees(-33.8688), 151.2093.deg)
    val melbourne = Location(Angle.degrees(-37.8136), 144.9631.deg)
    val newYork = Location(40.7128.deg, Angle.degrees(-74.0060))
    val aalborg = Location(57.64911.deg, 10.40744.deg)
    val earthRadius = 6371.0

    suite(m"Angle rendering"):
      test(m"a whole-number literal widens to an angle in degrees"):
        val angle = 45.deg
        angle.show

      . assert(_ == t"45.0°")

      test(m"render a simple angle"):
        val angle = Angle.degrees(45)
        angle.show

      . assert(_ == t"45.0°")

      test(m"render an angle to 1 decimal place"):
        val angle = Angle.degrees(7.25)
        angle.show

      . assert(_ == t"7.3°")

      test(m"render zero degrees"):
        val angle = Angle.degrees(0)
        angle.show

      . assert(_ == t"0.0°")

      // Inspection keeps the precision which `show` rounds away.
      test(m"inspect an angle at full precision"):
        val angle = Angle.degrees(7.25)
        angle.inspect

      . assert(_ == t"7.25°")

      // A missing `Inspectable` never fails to compile — `derived` substitutes a marked
      // `toString`, `Showable` or `Encodable` rendering — so coverage is held in place by
      // asserting on the renderings themselves.
      test(m"geodesy's types inspect natively"):
        Inspectable.fallbacks
         ( Angle.degrees(90).inspect,
           CardinalWind.North.inspect,
           IntercardinalWind.Northeast.inspect,
           HalfWind.NorthNortheast.inspect,
           Location(Angle.degrees(51.5), Angle.degrees(0.126)).inspect )

      . assert(_ == Nil)

    suite(m"Angle conversions"):
      test(m"half a turn is 180 degrees"):
        Angle.turns(0.5).degrees
      . assert(near(_, 180.0))

      test(m"π radians is 180 degrees"):
        math.Pi.rad.degrees
      . assert(near(_, 180.0))

      test(m"90 degrees is π/2 radians"):
        90.deg.radians
      . assert(near(_, math.Pi/2))

      test(m"90 degrees is a quarter turn"):
        Angle.degrees(90).turns
      . assert(near(_, 0.25))

      test(m"a degree is sixty arcminutes"):
        (ArcMinute*60.0).radians
      . assert(near(_, Degree.radians))

      test(m"a degree is 3600 arcseconds"):
        (ArcSecond*3600.0).radians
      . assert(near(_, Degree.radians))

      test(m"render principal angle"):
        val angle = Angle.degrees(375)
        angle.principal.show

      . assert(_ == t"15.0°")

      test(m"the principal value of a negative angle is positive"):
        Angle.degrees(-30).principal.show
      . assert(_ == t"330.0°")

      test(m"render canonical angle"):
        val angle = Angle.degrees(355)
        angle.canonical.show

      . assert(_ == t"-5.0°")

      test(m"the canonical value of 190 degrees is -170 degrees"):
        Angle.degrees(190).canonical.show
      . assert(_ == t"-170.0°")

    suite(m"Angle arithmetic"):
      test(m"addition wraps past a full turn"):
        (350.deg + 20.deg).degrees
      . assert(near(_, 10.0))

      test(m"subtraction wraps below zero"):
        (10.deg - 20.deg).show
      . assert(_ == t"350.0°")

      test(m"multiplication by a scalar wraps"):
        (Angle.degrees(100)*4.0).show
      . assert(_ == t"40.0°")

      test(m"a scalar may multiply an angle from the left"):
        (2.0*Angle.degrees(90)).show
      . assert(_ == t"180.0°")

      test(m"division by a scalar"):
        (Angle.degrees(90)/2.0).show
      . assert(_ == t"45.0°")

    suite(m"Winds"):
      test(m"cardinal winds render as arrows"):
        List(North, East, South, West).map(_.show)
      . assert(_ == List(t"↑", t"→", t"↓", t"←"))

      test(m"intercardinal winds render as diagonal arrows"):
        List(Northeast, Southeast, Southwest, Northwest).map(_.show)
      . assert(_ == List(t"↗", t"↘", t"↙", t"↖"))

    suite(m"Compass.points4"):
      test(m"lists the cardinal winds clockwise from north"):
        Compass.points4.readable.to(List)
      . assert(_ == List(North, East, South, West))

    suite(m"Compass.points8"):
      test(m"contains eight points"):
        Compass.points8.readable.length
      . assert(_ == 8)

      test(m"index 7 is Northwest"):
        Compass.points8.readable(7)
      . assert(_ == Northwest)

      test(m"contains no duplicates"):
        Compass.points8.readable.toSet.size
      . assert(_ == 8)

      test(m"315 degrees maps to Northwest"):
        Compass[8](Angle.degrees(315))
      . assert(_ == Northwest)

    suite(m"Compass.points16"):
      test(m"contains sixteen distinct points"):
        Compass.points16.readable.toSet.size
      . assert(_ == 16)

      test(m"every fourth point is a cardinal wind"):
        List(0, 4, 8, 12).map(Compass.points16.readable(_))
      . assert(_ == List(North, East, South, West))

      test(m"intercardinal winds lie between the cardinal ones"):
        List(2, 6, 10, 14).map(Compass.points16.readable(_))
      . assert(_ == List(Northeast, Southeast, Southwest, Northwest))

    suite(m"Compass directions"):
      test(m"zero degrees is north on every compass"):
        (Compass[4](0.deg), Compass[8](0.deg), Compass[16](0.deg))
      . assert(_ == (North, North, North))

      test(m"a four-point compass turns east after 45 degrees"):
        (Compass[4](44.deg), Compass[4](46.deg))
      . assert(_ == (North, East))

      test(m"an eight-point compass turns northeast after 22.5 degrees"):
        (Compass[8](22.deg), Compass[8](23.deg))
      . assert(_ == (North, Northeast))

      test(m"a sixteen-point compass turns NNE after 11.25 degrees"):
        (Compass[16](11.deg), Compass[16](12.deg))
      . assert(_ == (North, NorthNortheast))

      test(m"a sixteen-point compass finds west-southwest"):
        Compass[16](Angle.degrees(247.5))
      . assert(_ == WestSouthwest)

      test(m"just short of a full turn is north again"):
        (Compass[4](359.deg), Compass[8](359.deg), Compass[16](359.deg))
      . assert(_ == (North, North, North))

      test(m"a negative angle is measured anticlockwise from north"):
        Compass[8](Angle.degrees(-90))
      . assert(_ == West)

    suite(m"Locations"):
      test(m"a location keeps its latitude"):
        london.latitude
      . assert(nearDegrees(_, 51.5074))

      test(m"a location keeps its longitude"):
        paris.longitude
      . assert(nearDegrees(_, 2.3522))

      test(m"a southern latitude is negative"):
        sydney.latitude
      . assert(nearDegrees(_, -33.8688))

      test(m"a western longitude is negative"):
        newYork.longitude
      . assert(nearDegrees(_, -74.006))

      test(m"the north pole has latitude 90 degrees"):
        Location(90.deg, 0.deg).latitude
      . assert(nearDegrees(_, 90.0))

      test(m"the south pole has latitude -90 degrees"):
        Location(Angle.degrees(-90), 0.deg).latitude
      . assert(nearDegrees(_, -90.0))

      test(m"pair gives latitude then longitude"):
        val (latitude, longitude) = sydney.pair
        (latitude.degrees, longitude.degrees)
      . assert: (latitude, longitude) =>
          near(latitude, -33.8688, 1e-6) && near(longitude, 151.2093, 1e-6)

      test(m"a location from microdegrees matches one from angles"):
        val location = Location(-33868800, 151209300)
        (location.latitude.degrees, location.longitude.degrees)
      . assert: (latitude, longitude) =>
          near(latitude, -33.8688, 1e-6) && near(longitude, 151.2093, 1e-6)

      test(m"a western location from microdegrees is negative"):
        Location(40712800, -74006000).longitude
      . assert(nearDegrees(_, -74.006))

      test(m"a location encodes as latitude and longitude"):
        Location(51.5.deg, 0.25.deg).encode
      . assert(_ == t"51.500000,0.250000")

      test(m"inspect a location as a pair of angles"):
        Location(Angle.degrees(51.5), Angle.degrees(0.126)).inspect

      . assert(_ == t"⌖51.4999999718275°,0.12599995365645733°")

    // Expected hashes are from the reference algorithm (Niemeyer's geohash.org).
    suite(m"Geohashing"):
      test(m"the origin hashes to s0000"):
        Location(0.deg, 0.deg).geohash(5)
      . assert(_ == t"s0000")

      test(m"the canonical geohash.org example"):
        aalborg.geohash(11)
      . assert(_ == t"u4pruydqqvj")

      test(m"a location in the southern hemisphere"):
        sydney.geohash(8)
      . assert(_ == t"r3gx2f77")

      test(m"a location with a western longitude"):
        Location(42.6.deg, Angle.degrees(-5.6)).geohash(5)
      . assert(_ == t"ezs42")

      test(m"New York hashes to dr5regw"):
        newYork.geohash(7)
      . assert(_ == t"dr5regw")

      test(m"a shorter geohash is a prefix of a longer one"):
        (aalborg.geohash(4), aalborg.geohash(9))
      . assert: (short, long) => long.starts(short)

    suite(m"Surface distance"):
      test(m"a location is no distance from itself"):
        paris.surfaceDistance(paris).radians
      . assert(near(_, 0.0))

      test(m"the poles are half a turn apart"):
        Location(90.deg, 0.deg).surfaceDistance(Location(Angle.degrees(-90), 0.deg)).radians
      . assert(near(_, math.Pi, 1e-6))

      test(m"a quarter of the equator is a quarter turn"):
        Location(0.deg, 0.deg).surfaceDistance(Location(0.deg, 90.deg)).radians
      . assert(near(_, math.Pi/2, 1e-6))

      test(m"distance is symmetric"):
        (sydney.surfaceDistance(melbourne).radians, melbourne.surfaceDistance(sydney).radians)
      . assert(near(_, _))

      test(m"Sydney to Melbourne is about 713 km"):
        sydney.surfaceDistance(melbourne).radians*earthRadius
      . assert(near(_, 713.43, 0.1))

      test(m"London to Paris is about 344 km"):
        london.surfaceDistance(paris).radians*earthRadius
      . assert(near(_, 343.56, 0.1))

    suite(m"Bearings"):
      val origin = Location(0.deg, 20.deg)

      test(m"a location due north has a bearing of zero"):
        import compassBearings.fromNorthBearing
        origin.bearing[Angle](Location(10.deg, 20.deg)).radians
      . assert(near(_, 0.0, 1e-6))

      test(m"a location due east has a bearing of 90 degrees"):
        import compassBearings.fromNorthBearing
        origin.bearing[Angle](Location(0.deg, 30.deg)).degrees
      . assert(near(_, 90.0, 1e-6))

      test(m"a location due north is north"):
        import compassBearings.eightPointCompassBearing
        origin.bearing[Compass[8]](Location(10.deg, 20.deg))
      . assert(_ == North)

      test(m"a location due east is east"):
        import compassBearings.fourPointCompassBearing
        origin.bearing[Compass[4]](Location(0.deg, 30.deg))
      . assert(_ == East)

      test(m"a location due south is south"):
        import compassBearings.sixteenPointCompassBearing
        origin.bearing[Compass[16]](Location(Angle.degrees(-10), 20.deg))
      . assert(_ == South)

      test(m"a location to the northeast is northeast"):
        import compassBearings.eightPointCompassBearing
        origin.bearing[Compass[8]](Location(10.deg, 30.deg))
      . assert(_ == Northeast)

      test(m"a location due west is west"):
        import compassBearings.fourPointCompassBearing
        origin.bearing[Compass[4]](Location(0.deg, 10.deg))
      . assert(_ == West)

    suite(m"Geo URI decoding"):
      test(m"decode a latitude and longitude"):
        t"geo:13.4125,103.8667".as[Geolocation]
      . assert(_ == Geolocation(Location(13.4125.deg, 103.8667.deg)))

      test(m"decode negative coordinates"):
        t"geo:-33.8688,151.2093".as[Geolocation].location.latitude
      . assert(nearDegrees(_, -33.8688))

      test(m"decode integer coordinates"):
        t"geo:48,16".as[Geolocation]
      . assert(_ == Geolocation(Location(48.0.deg, 16.0.deg)))

      test(m"decode an altitude"):
        t"geo:48.2010,16.3695,183".as[Geolocation].altitude
      . assert(_ == 183.0)

      test(m"a URI without an altitude has none"):
        t"geo:48.2010,16.3695".as[Geolocation].altitude
      . assert(_ == Unset)

      test(m"decode an uncertainty"):
        t"geo:48.2010,16.3695,183;u=35".as[Geolocation].uncertainty
      . assert(_ == 35.0)

      test(m"decode a coordinate reference system"):
        t"geo:48.2010,16.3695,183;crs=wgs84;u=35".as[Geolocation]
      . assert: geolocation =>
          geolocation.crs == t"wgs84" && geolocation.uncertainty == 35.0

      test(m"further parameters are kept"):
        t"geo:48.2010,16.3695,183;u=35;name=Vienna".as[Geolocation].parameters
      . assert(_ == Map(t"name" -> t"Vienna"))

      // RFC 5870: `geo-uri = "geo:" coordinates p` with the altitude optional in `coordinates`
      test(m"parameters may follow coordinates without an altitude"):
        t"geo:48.2010,16.3695;u=35".as[Geolocation].uncertainty
      . assert(_ == 35.0)

    suite(m"Geo URI errors"):
      import Geolocation.Error.Reason

      test(m"a different scheme is rejected"):
        capture[Geolocation.Error](t"http://example.com/".as[Geolocation]).reason
      . assert(_ == Reason.BadScheme)

      test(m"a geo URI needs coordinates"):
        capture[Geolocation.Error](t"geo:here".as[Geolocation]).reason
      . assert(_ == Reason.ExpectedCoordinates)

      test(m"junk after the longitude is rejected"):
        capture[Geolocation.Error](t"geo:1,2x".as[Geolocation]).reason
      . assert(_ == Reason.UnexpectedSuffix)

      test(m"a non-numeric altitude is rejected"):
        capture[Geolocation.Error](t"geo:1,2,high".as[Geolocation]).reason
      . assert(_ == Reason.UnexpectedSuffix)

      test(m"junk after the altitude is rejected"):
        capture[Geolocation.Error](t"geo:1,2,3x".as[Geolocation]).reason
      . assert(_ == Reason.ExpectedSemicolon)

      test(m"a parameter without a value is rejected"):
        capture[Geolocation.Error](t"geo:1,2,3;u".as[Geolocation]).reason
      . assert(_ == Reason.MissingEquals)

      test(m"a parameter with two equals signs is rejected"):
        capture[Geolocation.Error](t"geo:1,2,3;u=1=2".as[Geolocation]).reason
      . assert(_ == Reason.MultipleEquals)

      test(m"a non-numeric uncertainty is rejected"):
        capture[Geolocation.Error](t"geo:1,2,3;u=far".as[Geolocation]).reason
      . assert(_ == Reason.BadUncertainty)

    suite(m"Geo URI encoding"):
      test(m"encode a location alone"):
        Geolocation(Location(51.5.deg, 0.25.deg)).encode
      . assert(_ == t"geo:51.500000,0.250000")

      test(m"encode a location with altitude"):
        Geolocation(Location(51.5.deg, 0.25.deg), 12.5).encode
      . assert(_ == t"geo:51.500000,0.250000,12.500000")

      test(m"encode a location with altitude and uncertainty"):
        Geolocation(Location(51.5.deg, 0.25.deg), 12.5, uncertainty = 3.0).encode
      . assert(_ == t"geo:51.500000,0.250000,12.500000;u=3.000000")

      test(m"an encoded geolocation decodes to the same values"):
        val geolocation = Geolocation(sydney, 58.0, uncertainty = 10.0)
        geolocation.encode.as[Geolocation]
      . assert: decoded =>
          nearDegrees(decoded.location.latitude, -33.8688)
          && nearDegrees(decoded.location.longitude, 151.2093)
          && decoded.altitude == 58.0
          && decoded.uncertainty == 10.0

      test(m"an encoded geolocation keeps its CRS"):
        Geolocation(sydney, 58.0, crs = t"wgs84").encode.as[Geolocation].crs
      . assert(_ == t"wgs84")
