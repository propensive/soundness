/*
    Iridescence, version 0.26.0. Copyright 2025 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package iridescence

import language.experimental.captureChecking

import scala.util.chaining.*

import anticipation.*
import hypotenuse.*


object Srgb:
  given Srgb is Chromatic = _.rgb24.asInt

case class Srgb(red: Double, green: Double, blue: Double):
  def css: Text = Text(s"rgb(${(red*255).toInt}, ${(green*255).toInt}, ${(blue*255).toInt})")

  def rgb24: Rgb24 = Rgb24((red*255).toInt, (green*255).toInt, (blue*255).toInt)
  def srgb: Srgb = this
  def highContrast(using ColorProfile): Srgb = if xyz.y >= 0.5 then Srgb(0, 0, 0) else Srgb(1, 1, 1)

  def xyz(using ColorProfile): Xyz =
    def limit(v: Double): Double = if v > 0.04045 then ((v + 0.055)/1.055)**2.4 else v/12.92

    val List(r, g, b) = List(red, green, blue).map(limit(_)*100)

    val x = r*0.4124 + g*0.3576 + b*0.1805
    val y = r*0.2126 + g*0.7152 + b*0.0722
    val z = r*0.0193 + g*0.1192 + b*0.9505

    Xyz(x, y, z)

  def cielab(using ColorProfile): Cielab = xyz.cielab
  def cmy: Cmy = Cmy(1 - red, 1 - green, 1 - blue)
  def cmyk: Cmyk = cmy.cmyk

  def hsl: Hsl =
    val min = red min green min blue
    val max = red max green max blue
    val delta = max - min
    val lightness = (max + min)/2

    if delta == 0 then Hsl(0, 0, lightness)
    else
      val saturation = if lightness < 0.5 then delta/(max + min) else delta/(2 - max - min)
      val dRed = ((max - red)/6 + delta/2)/delta
      val dGreen = ((max - green)/6 + delta/2)/delta
      val dBlue = ((max - blue)/6 + delta/2)/delta

      val hue =
        if max == red then dBlue - dGreen
        else if max == green then 1.0/3 + dRed - dBlue
        else 2.0/3 + dGreen - dRed

      Hsl(unitary(hue), saturation, lightness)

  def hsv: Hsv =
    val min = red min green min blue
    val value = red max green max blue
    val delta = value - min

    if delta == 0 then Hsv(0, 0, value)
    else
      val saturation = delta/value
      val dr = ((value - red)/6) + (delta/2)/delta
      val dg = ((value - green)/6) + (delta/2)/delta
      val db = ((value - blue)/6) + (delta/2)/delta

      val hue =
        if value == red then db - dg
        else if value == green then 1.0/3 + dr - db
        else 2.0/3 + dg - dr

      Hsv(unitary(hue), saturation, value)
