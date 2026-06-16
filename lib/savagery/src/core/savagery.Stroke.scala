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
┃    Soundness, version 0.54.0.                                                                    ┃
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
package savagery

import anticipation.*
import geodesy.*
import gossamer.*
import prepositional.*
import spectacular.*
import vacuous.*

import decimalConverters.javaDecimalConverter

object Stroke:
  private def bit(value: Boolean): Text = if value then t"1" else t"0"

  given encodable: Stroke is Encodable in Text =
    _.absolve match
      case Move(shift)                          => t"m $shift"
      case MoveTo(point)                        => t"M $point"
      case Draw(delta) if delta.dx == 0.0f      => t"v ${delta.dy.toDouble}"
      case Draw(delta) if delta.dy == 0.0f      => t"h ${delta.dx.toDouble}"
      case Draw(shift)                          => t"l $shift"
      case DrawTo(point)                        => t"L $point"
      case Close                                => t"Z"
      case CubicTo(Unset, ctrl2, point)         => t"S $ctrl2, $point"
      case Cubic(Unset, ctrl2, shift)           => t"s $ctrl2, $shift"
      case CubicTo(ctrl1: Point, ctrl2, point)  => t"C $ctrl1, $ctrl2, $point"
      case Cubic(ctrl1: Delta, ctrl2, shift)    => t"c $ctrl1, $ctrl2, $shift"
      case QuadraticTo(Unset, point)            => t"T $point"
      case Quadratic(Unset, shift)              => t"t $shift"
      case QuadraticTo(ctrl1: Point, point)     => t"Q $ctrl1, $point"
      case Quadratic(ctrl1: Delta, shift)       => t"q $ctrl1, $shift"

      case ArcTo(rx, ry, angle, largeArc, sweep, point) =>
        val clockwise = sweep == Sweep.Clockwise
        val degrees = angle.degrees.show
        t"A ${rx.toDouble} ${ry.toDouble} $degrees ${bit(largeArc)} ${bit(clockwise)} $point"

      case Arc(rx, ry, angle, largeArc, sweep, shift) =>
        val clockwise = sweep == Sweep.Clockwise
        val degrees = angle.degrees.show
        t"A ${rx.toDouble} ${ry.toDouble} $degrees ${bit(largeArc)} ${bit(clockwise)} $shift"

enum Stroke:
  case MoveTo(point: Point)
  case Move(shift: Delta)
  case DrawTo(point: Point)
  case Draw(shift: Delta)
  case Close
  case CubicTo(ctrl1: Optional[Point], ctrl2: Point, point: Point)
  case Cubic(ctrl1: Optional[Delta], ctrl2: Delta, shift: Delta)
  case QuadraticTo(ctrl1: Optional[Point], point: Point)
  case Quadratic(ctrl1: Optional[Delta], shift: Delta)

  case ArcTo
    ( rx: Float, ry: Float, angle: Angle, largeArc: Boolean, sweep: Sweep, point: Point )

  case Arc
    ( rx: Float, ry: Float, angle: Angle, largeArc: Boolean, sweep: Sweep, shift: Delta )
