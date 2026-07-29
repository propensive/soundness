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

object Hsv:
  given perceptual: Hsv is Perceptual in Srgb =
    color =>
      val hue = color.hue.principal.turns
      val i = (hue*6).toInt%6
      val a1 = color.value*(1 - color.saturation)
      val a2 = color.value*(1 - color.saturation*(hue*6 - i))
      val a3 = color.value*(1 - color.saturation*(1 - (hue*6 - i)))

      val red = if i == 1 then a2 else if i/2 == 1 then a1 else if i == 4 then a3 else color.value
      val green = if i/2 == 2 then a1 else if i == 3 then a2 else if i == 0 then a3 else color.value
      val blue = if i/2 == 0 then a1 else if i == 2 then a3 else if i == 5 then a2 else color.value

      Srgb(red, green, blue)

case class Hsv(hue: Angle, saturation: Double, value: Double) extends Color:
  type Form = Hsv

  def saturate: Hsv             = Hsv(hue, 1, value)
  def desaturate: Hsv           = Hsv(hue, 0, value)
  def rotate(angle: Angle): Hsv = Hsv((hue + angle).principal, saturation, value)
  def complement: Hsv           = rotate(Angle(π))
  def pure: Hsv                 = Hsv(hue, 1, 1)

  def shade(black: Double = 0): Hsv = Hsv(hue, saturation, value*(1 - black))
  def tint(white: Double = 0): Hsv  = Hsv(hue, saturation*(1 - white), value)
  def tone(black: Double = 0, white: Double = 0): Hsv = shade(black).tint(white)
