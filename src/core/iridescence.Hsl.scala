/*
    Iridescence, version [unreleased]. Copyright 2025 Jon Pretty, Propensive OÜ.

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

object Hsl:
  given Hsl is Chromatic = _.srgb.rgb24.asInt

case class Hsl(hue: Double, saturation: Double, lightness: Double):
  def saturate: Hsv = Hsv(hue, 1, lightness)
  def desaturate: Hsv = Hsv(hue, 0, lightness)
  def rotate(degrees: Double): Hsv = Hsv(unitary(hue + degrees/360), saturation, lightness)
  def pure: Hsv = Hsv(hue, 1, 0)

  def srgb: Srgb =
    if saturation == 0 then Srgb(lightness, lightness, lightness)
    else
      val v2 =
        if lightness < 0.5 then lightness*(1 + saturation)
        else (lightness + saturation - saturation*lightness)

      val v1 = 2*lightness - v2

      def convert(h: Double): Double =
        val vh = unitary(h)
        if 6*vh < 1 then v1 + (v2 - v1)*6*vh
        else if 2*vh < 1 then v2
        else if 3*vh < 2 then v1 + (v2 - v1)*((2.0/3) - vh)*6
        else v1

      Srgb(convert(hue + (1.0/3.0)), convert(hue), convert(hue - (1.0/3.0)))

  def css: Text =
    Text(s"hsl(${(hue*360).toInt}, ${(saturation*100).toInt}%, ${(lightness*100).toInt}%)")
