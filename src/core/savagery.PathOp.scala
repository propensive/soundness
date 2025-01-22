/*
    Savagery, version [unreleased]. Copyright 2025 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package savagery

import anticipation.*
import gossamer.*, decimalFormatters.java
import prepositional.*
import spectacular.*
import vacuous.*

enum PathOp:
  case Move(coords: Coords)
  case Line(coords: Coords)
  case Close

  case Cubic[CoordsType <: (Rel | Abs)](ctrl1: Optional[CoordsType],
                                        ctrl2: CoordsType,
                                        point: CoordsType)

  case Quadratic[CoordsType <: (Rel | Abs)](ctrl1: Optional[CoordsType], point: CoordsType)
  case Arc(rx: Float, ry: Float, angle: Degrees, largeArc: Boolean, sweep: Sweep, coords: Coords)

object PathOp:
  private def bit(value: Boolean): Text = if value then t"1" else t"0"

  given PathOp is Encodable in Text as encodable =
    case Move(coords)                => t"${coords.key('m')} $coords"
    case Line(Rel(DxDy(0.0f, v)))    => t"v ${v.toDouble}"
    case Line(Rel(DxDy(h, 0.0f)))    => t"h ${h.toDouble}"
    case Line(coords)                => t"${coords.key('l')} $coords"
    case Close                       => t"Z"
    case Cubic(Unset, ctrl2, coords) => t"${coords.key('s')} $ctrl2, $coords"
    case Cubic(ctrl1, ctrl2, coords) => t"${coords.key('c')} ${ctrl1.option.get}, $ctrl2, $coords"
    case Quadratic(Unset, coords)    => t"${coords.key('t')} $coords"
    case Quadratic(ctrl1, coords)    => t"${coords.key('q')} ${ctrl1.option.get}, $coords"

    case Arc(rx, ry, angle, largeArc, sweep, coords) =>
      t"${coords.key('a')} ${rx.toDouble} ${ry.toDouble} ${angle.encode} ${bit(largeArc)} ${bit(sweep == Sweep.Clockwise)} ${coords.encode}"
