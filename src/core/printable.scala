/*
    Rudiments, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package rudiments

import anticipation.*
import fulminate.*

import language.experimental.captureChecking

object Printable:
  given text: Printable[Text] = (text, termcap) => text
  given string: Printable[String] = (string, termcap) => string.tt
  given char: Printable[Char] = (char, termcap) => char.toString.tt
  given message: Printable[Message] = (message, termcap) => message.text

enum ColorCapability:
  case NoColor, Color8, Color16, Color256, TrueColor

object Termcap:
  val basic: Termcap = new Termcap:
    def ansi: Boolean = false
    def color: ColorCapability = ColorCapability.NoColor
  
  val xterm256: Termcap = new Termcap:
    def ansi: Boolean = true
    def color: ColorCapability = ColorCapability.Color256
  
  val xtermTrueColor: Termcap = new Termcap:
    def ansi: Boolean = true
    def color: ColorCapability = ColorCapability.TrueColor

trait Termcap:
  def ansi: Boolean
  def color: ColorCapability

@capability
trait Printable[-TextType]:
  def print(text: TextType, termcap: Termcap): Text

