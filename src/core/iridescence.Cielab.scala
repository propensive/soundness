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

object Cielab:
  given (using ColorProfile) => Cielab is Chromatic = _.srgb.rgb24.asInt

case class Cielab(l: Double, a: Double, b: Double):
  def srgb(using ColorProfile): Srgb = xyz.srgb

  def xyz(using profile: ColorProfile): Xyz =
    def limit(v: Double): Double = if v*v*v > 0.008856 then v*v*v else (v - 16.0/116)/7.787

    val y = limit((l + 16)/116)*profile.y2
    val x = limit(a/500 + (l + 16)/116)*profile.x2
    val z = limit((l + 16)/116 - b/200)*profile.z2

    Xyz(x, y, z)

  def mix(that: Cielab, ratio: Double = 0.5): Cielab =
    Cielab(l*(1 - ratio) + ratio*that.l, a*(1 - ratio) + ratio*that.a, b*(1 - ratio) + ratio*that.b)

  def delta(that: Cielab): Double = (hyp(F64(that.a), F64(that.b)) - hyp(F64(a), F64(b))).double
