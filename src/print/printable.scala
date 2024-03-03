/*
    Anticipation, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package anticipation

import scala.annotation.*

import language.experimental.captureChecking

object Printable:
  given text: Printable[Text] = (text, termcap) => text
  given string: Printable[String] = (string, termcap) => string.tt
  given char: Printable[Char] = (char, termcap) => char.toString.tt
  //given message: Printable[Message] = (message, termcap) => message.text

object ColorCapability:
  def apply(colors: Int): ColorCapability = colors match
    case 8               => ColorCapability.Indexed8
    case 15 | 16         => ColorCapability.Indexed16
    case 52 | 64 | 88    => ColorCapability.Cube4
    case 256             => ColorCapability.Cube6
    case 6536 | 16777216 => ColorCapability.TrueColor
    case _               => ColorCapability.NoColor

enum ColorCapability:
  case NoColor, Indexed8, Indexed16, Cube4, Cube6, TrueColor

package termcapDefinitions:
  given basic: Termcap with
    def ansi: Boolean = false
    def color: ColorCapability = ColorCapability.NoColor
  
  given xterm256: Termcap with
    def ansi: Boolean = true
    def color: ColorCapability = ColorCapability.Cube6
  
  given xtermTrueColor: Termcap with
    def ansi: Boolean = true
    def color: ColorCapability = ColorCapability.TrueColor

trait Termcap:
  def ansi: Boolean
  def color: ColorCapability
  def width: Int = Int.MaxValue

@capability
trait Printable[-TextType]:
  def print(text: TextType, termcap: Termcap): Text
