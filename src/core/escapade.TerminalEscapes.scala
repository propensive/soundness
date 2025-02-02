/*
    Escapade, version 0.26.0. Copyright 2025 Jon Pretty, Propensive OÜ.

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
import gossamer.*
import iridescence.*
import prepositional.*
import spectacular.*

import language.experimental.captureChecking

object escapes:
  object foreground:
    val Black = Escape(t"[30m", t"[39m")
    val Red = Escape(t"[31m", t"[39m")
    val Green = Escape(t"[32m", t"[39m")
    val Yellow = Escape(t"[33m", t"[39m")
    val Blue = Escape(t"[34m", t"[39m")
    val Magenta = Escape(t"[35m", t"[39m")
    val Cyan = Escape(t"[36m", t"[39m")
    val White = Escape(t"[37m", t"[39m")

    val BrightBlack = Escape(t"[90m", t"[39m")
    val BrightRed = Escape(t"[91m", t"[39m")
    val BrightGreen = Escape(t"[92m", t"[39m")
    val BrightYellow = Escape(t"[93m", t"[39m")
    val BrightBlue = Escape(t"[94m", t"[39m")
    val BrightMagenta = Escape(t"[95m", t"[39m")
    val BrightCyan = Escape(t"[96m", t"[39m")
    val BrightWhite = Escape(t"[97m", t"[49m")

  object background:
    val Black = Escape(t"[40m", t"[49m")
    val Red = Escape(t"[41m", t"[49m")
    val Green = Escape(t"[42m", t"[49m")
    val Yellow = Escape(t"[43m", t"[49m")
    val Blue = Escape(t"[44m", t"[49m")
    val Magenta = Escape(t"[45m", t"[49m")
    val Cyan = Escape(t"[46m", t"[49m")
    val White = Escape(t"[47m", t"[49m")

    val BrightBlack = Escape(t"[100m", t"[49m")
    val BrightRed = Escape(t"[101m", t"[49m")
    val BrightGreen = Escape(t"[102m", t"[49m")
    val BrightYellow = Escape(t"[103m", t"[49m")
    val BrightBlue = Escape(t"[104m", t"[49m")
    val BrightMagenta = Escape(t"[105m", t"[49m")
    val BrightCyan = Escape(t"[106m", t"[49m")
    val BrightWhite = Escape(t"[107m", t"[49m")

  object styles:
    val Bold: Escape = Escape(t"[1m", t"[22m")
    val Light: Escape = Escape(t"[2m", t"[22m")
    val Italic: Escape = Escape(t"[3m", t"[23m")
    val Underline: Escape = Escape(t"[4m", t"[24m")
    val SlowBlink: Escape = Escape(t"[5m", t"[25m")
    val FastBlink: Escape = Escape(t"[6m", t"[25m")
    val Reverse: Escape = Escape(t"[7m", t"[27m")
    val Conceal: Escape = Escape(t"[8m", t"[28m")
    val Strike: Escape = Escape(t"[9m", t"[29m")

  val Reset: Escape = Escape(t"[0m", t"[0m")
  val EraseLine: Escape = Escape(t"[0K", t"[0m")

  def title(name: Text) = Escape(t"]0;$name\e\\")

  def link[UrlType: Abstractable across Urls into Text](url: UrlType, text: Text): Text =
    t"\e]8;;${url.generic}\e\\$text\e]8;;\e\\"

trait TerminalEscapes:
  def bold(state: Boolean): Text
  def italic(state: Boolean): Text
  def underline(state: Boolean): Text
  def reverse(state: Boolean): Text
  def conceal(state: Boolean): Text
  def strike(state: Boolean): Text
  def foreground(color: Rgb24): Text
  def background(color: Rgb24): Text

object StandardEscapes extends TerminalEscapes:
  def bold(state: Boolean): Text = if state then t"\e[1m" else t"\e[22m"
  def italic(state: Boolean): Text = if state then t"\e[3m" else t"\e[23m"
  def underline(state: Boolean): Text = if state then t"\e[4m" else t"\e[24m"
  def reverse(state: Boolean): Text = if state then t"\e[7m" else t"\e[27m"
  def conceal(state: Boolean): Text = if state then t"\e[8m" else t"\e[28m"
  def strike(state: Boolean): Text = if state then t"\e[9m" else t"\e[29m"
  def foreground(color: Rgb24): Text = t"\e[38;2;${color.red};${color.green};${color.blue}m"
  def background(color: Rgb24): Text = t"\e[48;2;${color.red};${color.green};${color.blue}m"
