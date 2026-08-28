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
package savagery

import anticipation.*
import geodesy.*
import gossamer.*
import prepositional.*
import spectacular.*
import vacuous.*

import decimalConverters.javaDecimalConverter


object Transform:
  private def form(name: Text, parts: Text*): Text =
    parts.map(_.s).mkString(s"${name.s}(", " ╱ ", ")").tt

  // Written out case by case, rather than left to structural derivation, because `Angle`'s
  // instance is a top-level given in geodesy (its companion sits below the text stack) and so is
  // only in scope where it has been imported by name — here.
  given inspectable: [transform <: Transform] => transform is Inspectable =
    _.absolve match
      case Translate(delta)         => form(t"Translate", delta.inspect)
      case Scale(x, y)              => form(t"Scale", x.inspect, y.inspect)
      case Rotate(angle)            => form(t"Rotate", angle.inspect)
      case Skew(angle, orientation) => form(t"Skew", angle.inspect, orientation.inspect)
      case Matrix(affine)           => form(t"Matrix", affine.inspect)

  private given floatShowable: Float is Showable = _.toString.tt

  given encodable: Transform is Encodable in Text =
    _.absolve match
      case Translate(delta)                    => t"translate(${delta.dx},${delta.dy})"
      case Scale(x, Unset)                     => t"scale($x)"
      case Scale(x, y: Float)                  => t"scale($x,$y)"
      case Rotate(angle)                       => t"rotate(${angle.degrees})"
      case Skew(angle, Orientation.Horizontal) => t"skewX(${angle.degrees})"
      case Skew(angle, Orientation.Vertical)   => t"skewY(${angle.degrees})"

      case Matrix(m) =>
        t"matrix(${m.a},${m.b},${m.c},${m.d},${m.e},${m.f})"

enum Transform:
  case Translate(vector: Delta)
  case Scale(x: Float, y: Optional[Float])
  case Rotate(angle: Angle)
  case Skew(angle: Angle, orientation: Orientation = Orientation.Horizontal)
  case Matrix(affine: Affine[Float])
