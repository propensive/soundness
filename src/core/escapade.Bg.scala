/*
    Escapade, version [unreleased]. Copyright 2025 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package escapade

import anticipation.*
import contextual.*
import gossamer.*
import spectacular.*

import language.experimental.pureFunctions

object Bg:
  def apply[ColorType: Chromatic](color: ColorType): Bg = Bg(color.asRgb24Int)

case class Bg(color: Int):
  def fg: Fg = Fg(color)

  def highContrast: Fg =
    Fg(if ((color&255)*0.07 + ((color >> 8)&255)*0.72 + ((color >> 16)&255)*0.21) > 128 then 0 else 16777215)

  def ansi(colorDepth: ColorDepth): Text =
    val red = (color >> 16)&255
    val green = (color >> 8)&255
    val blue = color&255

    colorDepth match
      case ColorDepth.TrueColor =>
        t"\e[48;2;$red;$green;${blue}m"

      case _ =>
        val n = 16 +
          (if blue == red && red == green then 216 + red*23/255 else red*5/255*36 + green*5/255*6 + blue*5/255)

        t"\e[48;5;${n}m"
