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
package iridescence

// Blending works coordinate by coordinate, so a space needs only to say how a pointwise operation
// over its coordinates rebuilds a color. `Hsl` and `Hsv` have no instance: their hue is an angle on
// a circle, and averaging two hues coordinatewise would take the long way round whenever the pair
// straddles zero — mixing two reds would give cyan.
object Blendable:
  given srgb: Srgb is Blendable = (left, right, operation) =>
    Srgb
      ( operation(left.red, right.red),
        operation(left.green, right.green),
        operation(left.blue, right.blue) )

  given cmy: Cmy is Blendable = (left, right, operation) =>
    Cmy
      ( operation(left.cyan, right.cyan),
        operation(left.magenta, right.magenta),
        operation(left.yellow, right.yellow) )

  given cmyk: Cmyk is Blendable = (left, right, operation) =>
    Cmyk
      ( operation(left.cyan, right.cyan),
        operation(left.magenta, right.magenta),
        operation(left.yellow, right.yellow),
        operation(left.key, right.key) )

  given cielab: Cielab is Blendable = (left, right, operation) =>
    Cielab
      ( operation(left.lightness, right.lightness),
        operation(left.blueYellow, right.blueYellow),
        operation(left.greenRed, right.greenRed) )

  given xyz: Xyz is Blendable = (left, right, operation) =>
    Xyz(operation(left.x, right.x), operation(left.y, right.y), operation(left.z, right.z))

trait Blendable:
  type Self <: Color

  def zip(left: Self, right: Self, operation: (Double, Double) => Double): Self
