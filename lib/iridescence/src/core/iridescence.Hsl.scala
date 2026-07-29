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

import geodesy.*
import hypotenuse.*
import prepositional.*
import symbolism.*

object Hsl:
  given perceptual: Hsl is Perceptual in Srgb =
    color =>
      if color.saturation == 0 then Srgb(color.lightness, color.lightness, color.lightness) else
        val v2 =
          if color.lightness < 0.5 then color.lightness*(1 + color.saturation)
          else (color.lightness + color.saturation - color.saturation*color.lightness)

        val v1 = 2*color.lightness - v2

        def convert(hue: Angle): Double =
          val vh = hue.principal.turns

          if 6*vh < 1 then v1 + (v2 - v1)*6*vh
          else if 2*vh < 1 then v2
          else if 3*vh < 2 then v1 + (v2 - v1)*((2.0/3) - vh)*6
          else v1

        val third = Angle.turns(1.0/3.0)

        Srgb(convert(color.hue + third), convert(color.hue), convert(color.hue - third))

case class Hsl(hue: Angle, saturation: Double, lightness: Double) extends Color:
  type Form = Hsl

  def saturate: Hsl               = Hsl(hue, 1, lightness)
  def desaturate: Hsl             = Hsl(hue, 0, lightness)
  def rotate(angle: Angle): Hsl   = Hsl((hue + angle).principal, saturation, lightness)
  def complement: Hsl             = rotate(Angle(π))
  def pure: Hsl                   = Hsl(hue, 1, 0.5)

  def lighten(amount: Double): Hsl = Hsl(hue, saturation, lightness + (1 - lightness)*amount)
  def darken(amount: Double): Hsl  = Hsl(hue, saturation, lightness*(1 - amount))
