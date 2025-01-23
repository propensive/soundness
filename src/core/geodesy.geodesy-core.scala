/*
    Geodesy, version 0.26.0. Copyright 2025 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package geodesy

import prepositional.*
import rudiments.*
import symbolism.*

export Geodesy.{Location, Radians, Degrees}

extension (double: Double)
  def rad: Radians = Radians(double)
  def deg: Degrees = Degrees(double)

export CardinalWind.{North, South, East, West}
export IntercardinalWind.{Northeast, Southeast, Southwest, Northwest}

export HalfWind
. { NorthNortheast, EastNortheast, EastSoutheast, SouthSoutheast, SouthSouthwest, WestSouthwest,
    WestNorthwest, NorthNorthwest }

package compassBearings:
  given fourPointCompass: Bearing[Compass[4]] = Compass[4](_)
  given eightPointCompass: Bearing[Compass[8]] = Compass[8](_)
  given sixteenPointCompass: Bearing[Compass[16]] = Compass[16](_)
  given degreesFromNorth: Bearing[Degrees] = _.degrees
  given radiansFromNorth: Bearing[Radians] = identity(_)
  given degreesFromEast: Bearing[Degrees] = radians => (radians - Radians(math.Pi/2)).degrees
  given radiansFromEast: Bearing[Radians] = radians => radians - Radians(math.Pi/2)
