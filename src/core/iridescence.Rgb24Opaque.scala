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

import anticipation.*
import rudiments.*

object Rgb24Opaque:
  opaque type Rgb24 = Int

  object Rgb24:
    given underlying: Underlying[Rgb24, Int] = ###
    given Rgb24 is Chromatic = _.asInt

    def apply(red: Int, green: Int, blue: Int): Rgb24 = ((red&255) << 16) + ((green&255) << 8) + (blue&255)
    def apply(packedInt: Int): Rgb24 = packedInt & 0x00ffffff

  extension (color: Rgb24)
    def red: Int = (color >> 16) & 255
    def green: Int = (color >> 8) & 255
    def blue: Int = color&255

    def srgb: Srgb = Srgb(red/255.0, green/255.0, blue/255.0)
    def asInt: Int = color

    def hex: Text =
      List(red, green, blue).foldLeft("#"): (acc, c) =>
        acc+(c.hex.pipe { s => if s.s.length < 2 then "0"+s else s })

      . tt

export Rgb24Opaque.Rgb24
