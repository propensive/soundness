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

import soundness.*

object Tests extends Suite(m"Geodesy tests"):
  def run(): Unit =
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

    // Latitude and longitude are packed into 32 bits each, so the angles which come back out are
    // the nearest representable ones; inspection shows them as they are, without rounding.
    test(m"inspect a location as a pair of angles"):
      Location(Angle.degrees(51.5), Angle.degrees(0.126)).inspect

    . assert(_ == t"⌖51.4999999718275°,0.12600003747548583°")

    test(m"render principal angle"):
      val angle = Angle.degrees(375)
      angle.principal.show

    . assert(_ == t"15.0°")

    test(m"render canonical angle"):
      val angle = Angle.degrees(355)
      angle.canonical.show

    . assert(_ == t"-5.0°")

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
