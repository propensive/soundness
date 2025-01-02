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

import anticipation.*
import rudiments.*

object Rgb12Opaque:
  opaque type Rgb12 = Int

  object Rgb12:
    given underlying: Underlying[Rgb12, Int] = ###
    given Rgb12 is Chromatic = _.srgb.rgb24.asInt

    def apply(red: Int, green: Int, blue: Int): Rgb12 = ((red&15) << 8) + ((green&15) << 4) + (blue&15)

  extension (color: Rgb12)
    def red: Int = (color >> 8)&15
    def green: Int = (color >> 4)&15
    def blue: Int = color&15
    def hex: Text = Text("#"+List(red, green, blue).map(_.hex).mkString)
    def srgb: Srgb = Srgb(red/15.0, green/15.0, blue/15.0)

export Rgb12Opaque.Rgb12
