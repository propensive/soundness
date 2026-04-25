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

import hypotenuse.*
import prepositional.*

object Srgb:
  given xyz: Colorimetry => Srgb is Perceptual in Xyz =
    color =>
      def clamp(v: Double): Double = if v > 0.04045 then ((v + 0.055)/1.055)**2.4 else v/12.92

      val List(r, g, b) = List(color.red, color.green, color.blue).map(clamp(_)*100)
      val x = r*0.4124 + g*0.3576 + b*0.1805
      val y = r*0.2126 + g*0.7152 + b*0.0722
      val z = r*0.0193 + g*0.1192 + b*0.9505

      Xyz(x, y, z)

  given cielab: Colorimetry => Srgb is Perceptual in Cielab = _.in[Xyz].in[Cielab]

  given cmy: Srgb is Perceptual in Cmy =
    color => Cmy(1 - color.red, 1 - color.green, 1 - color.blue)

  given cmyk: Srgb is Perceptual in Cmyk = _.in[Cmy].in[Cmyk]

  given hsl: Srgb is Perceptual in Hsl =
    color =>
      val min = color.red min color.green min color.blue
      val max = color.red max color.green max color.blue
      val delta = max - min
      val lightness = (max + min)/2

      if delta == 0 then Hsl(0, 0, lightness) else
        val saturation = if lightness < 0.5 then delta/(max + min) else delta/(2 - max - min)
        val dRed = ((max - color.red)/6 + delta/2)/delta
        val dGreen = ((max - color.green)/6 + delta/2)/delta
        val dBlue = ((max - color.blue)/6 + delta/2)/delta

        val hue =
          if max == color.red then dBlue - dGreen
          else if max == color.green then 1.0/3 + dRed - dBlue
          else 2.0/3 + dGreen - dRed

        Hsl(unitary(hue), saturation, lightness)

  given hsv: Srgb is Perceptual in Hsv =
    color =>
      val min = color.red min color.green min color.blue
      val value = color.red max color.green max color.blue
      val delta = value - min

      if delta == 0 then Hsv(0, 0, value)
      else
        val saturation = delta/value
        val dr = ((value - color.red)/6) + (delta/2)/delta
        val dg = ((value - color.green)/6) + (delta/2)/delta
        val db = ((value - color.blue)/6) + (delta/2)/delta

        val hue =
          if value == color.red then db - dg
          else if value == color.green then 1.0/3 + dr - db
          else 2.0/3 + dg - dr

        Hsv(unitary(hue), saturation, value)

case class Srgb(red: Double, green: Double, blue: Double) extends Color:
  type Form = Srgb
