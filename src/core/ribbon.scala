/*
    Escapade, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

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

import rudiments.*
import vacuous.*
import gossamer.*
import anticipation.*

//import language.experimental.captureChecking

object Ribbon:
  def apply[ColorType: RgbColor](colors: ColorType*): Ribbon = Ribbon(colors.map(Bg(_))*)

case class Ribbon(colors: Bg*):
  def fill(parts: Display*): Display =
    import escapes.*
    IArray.from(colors.zip(parts)).curse:
      val (background, text) = cursor
      
      val i = background.color
      val fg = Fg(if ((i&255)*2 + ((i >> 8)&255)*5 + ((i >> 16)&255))*3 > 3839 then 0 else 16777215)
      
      val arrow = postcursor.lay(e"$Reset${background.fg}()"): (color, _) =>
        e"${background.fg}($color())"
      
      e"$background( $fg($text) )$arrow"
    .join
    
