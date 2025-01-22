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

enum Stroke:
  case Move(coords: Shift | Point)
  case Draw(coords: Shift | Point)
  case Close

  case Cubic[PointType <: (Shift | Point)]
     (ctrl1: Optional[PointType], ctrl2: PointType, point: PointType)

  case Quadratic[PointType <: (Shift | Point)](ctrl1: Optional[PointType], point: PointType)

  case Arc
     (rx: Float, ry: Float, angle: Degrees, largeArc: Boolean, sweep: Sweep, coords: Point | Shift)

object Stroke:
  private def bit(value: Boolean): Text = if value then t"1" else t"0"

  given encodable: Stroke is Encodable in Text =
    case Move(shift: Shift)                              => t"m $shift"
    case Move(point: Point)                              => t"M $point"
    case Draw(Shift(0.0f, v))                            => t"v ${v.toDouble}"
    case Draw(Shift(h, 0.0f))                            => t"h ${h.toDouble}"
    case Draw(shift: Shift)                              => t"l $shift"
    case Draw(point: Point)                              => t"L $point"
    case Close                                           => t"Z"
    case Cubic(Unset, ctrl2: Point, point: Point)        => t"S $ctrl2, $point"
    case Cubic(Unset, ctrl2: Shift, shift: Shift)        => t"s $ctrl2, $shift"
    case Cubic(ctrl1: Point, ctrl2: Point, point: Point) => t"C $ctrl1, $ctrl2, $point"
    case Cubic(ctrl1: Shift, ctrl2: Shift, shift: Shift) => t"c $ctrl1, $ctrl2, $shift"
    case Quadratic(Unset, point: Point)                  => t"T $point"
    case Quadratic(Unset, shift: Shift)                  => t"t $shift"
    case Quadratic(ctrl1: Point, point: Point)           => t"Q $ctrl1, $point"
    case Quadratic(ctrl1: Point, shift: Shift)           => t"q $ctrl1, $shift"

    case Arc(rx, ry, angle, largeArc, sweep, point: Point) =>
      val clockwise = sweep == Sweep.Clockwise
      t"A ${rx.toDouble} ${ry.toDouble} ${angle.encode} ${bit(largeArc)} ${bit(clockwise)} $point"

    case Arc(rx, ry, angle, largeArc, sweep, shift: Shift) =>
      val clockwise = sweep == Sweep.Clockwise
      t"A ${rx.toDouble} ${ry.toDouble} ${angle.encode} ${bit(largeArc)} ${bit(clockwise)} $shift"
