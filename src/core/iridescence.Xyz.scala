/*
    Iridescence, version 0.24.0. Copyright 2025 Jon Pretty, Propensive OÜ.

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

object Xyz:
  given Xyz is Chromatic = _.srgb.rgb24.asInt

case class Xyz(x: Double, y: Double, z: Double):
  def luminescence: Double = y

  def srgb: Srgb =
    def limit(v: Double): Double =
      if v > 0.0031308 then 1.055*(v**(1/2.4)) - 0.055 else 12.92*v

    val red = limit(x*0.032406994 - y*0.0153738318 - z*0.0049861076)
    val green = limit(-x*0.0096924364 + y*0.01878675 + z*0.0004155506)
    val blue = limit(x*0.0005563008 - y*0.0020397696 + z*0.0105697151)

    Srgb(red, green, blue)

  def rgb24: Rgb24 = srgb.rgb24

  def cielab(using profile: ColorProfile): Cielab =
    def limit(v: Double): Double = if v > 0.008856 then v**(1.0/3) else 7.787*v + 0.13793

    val l: Double = 116*limit(y/profile.y2) - 16
    val a: Double = 500*(limit(x/profile.x2) - limit(y/profile.y2))
    val b: Double = 200*(limit(y/profile.y2) - limit(z/profile.z2))

    Cielab(l, a, b)
