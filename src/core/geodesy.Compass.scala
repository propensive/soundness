/*
    Geodesy, version 0.24.0. Copyright 2025 Jon Pretty, Propensive OÜ.

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

import scala.compiletime.*

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
