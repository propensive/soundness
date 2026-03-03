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
package iridescence

import anticipation.*
import prepositional.*

object Hsv:
  given perceptual: Hsv is Perceptual in Srgb =
    color =>
      val i = (color.hue*6).toInt%6
      val a1 = color.value*(1 - color.saturation)
      val a2 = color.value*(1 - color.saturation*(color.hue*6 - i))
      val a3 = color.value*(1 - color.saturation*(1 - (color.hue*6 - i)))

      val red = if i == 1 then a2 else if i/2 == 1 then a1 else if i == 4 then a3 else color.value
      val green = if i/2 == 2 then a1 else if i == 3 then a2 else if i == 0 then a3 else color.value
      val blue = if i/2 == 0 then a1 else if i == 2 then a3 else if i == 5 then a2 else color.value

      Srgb(red, green, blue)

case class Hsv(hue: Double, saturation: Double, value: Double) extends Color:
  type Form = Hsv

//   def saturate: Hsv = Hsv(hue, 1, value)
//   def desaturate: Hsv = Hsv(hue, 0, value)
//   def rotate(degrees: Double): Hsv = Hsv(unitary(hue + degrees/360), saturation, value)
//   def pure: Hsv = Hsv(hue, 1, 0)
//   def tone(black: Double = 0, white: Double = 0) = shade(black).tint(white)
//   def shade(black: Double = 0): Hsv = Hsv(hue, saturation, value*(1 - black) + (1 - value)*black)
//   def tint(white: Double = 0): Hsv = Hsv(hue, saturation*(1 - white) + (1 - saturation)*white, value)
