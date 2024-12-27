/*
    Geodesy, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

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

export CardinalWind.*, IntercardinalWind.*, HalfWind.*

package compassBearings:
  given Bearing[Compass[4]] as fourPointCompass = Compass[4](_)
  given Bearing[Compass[8]] as eightPointCompass = Compass[8](_)
  given Bearing[Compass[16]] as sixteenPointCompass = Compass[16](_)
  given Bearing[Degrees] as degreesFromNorth = _.degrees
  given Bearing[Radians] as radiansFromNorth = identity(_)
  given Bearing[Degrees] as degreesFromEast = radians => (radians - Radians(math.Pi/2)).degrees
  given Bearing[Radians] as radiansFromEast = radians => radians - Radians(math.Pi/2)
