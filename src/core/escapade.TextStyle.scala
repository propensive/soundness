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
import contextual.*
import gossamer.*
import spectacular.*
import vacuous.*

import language.experimental.pureFunctions

object TextStyle:
  val esc: Char = 27.toChar

case class TextStyle
   (fg:     Optional[Int] = Unset,
    bg:     Optional[Int] = Unset,
    italic:    Boolean       = false,
    bold:     Boolean       = false,
    reverse:   Boolean       = false,
    underline: Boolean       = false,
    conceal:   Boolean       = false,
    strike:    Boolean       = false):

  import escapes.*
  import TextStyle.esc

  private def italicEsc: Text = if italic then styles.Italic.on else styles.Italic.off
  private def boldEsc: Text = if bold then styles.Bold.on else styles.Bold.off
  private def reverseEsc: Text = if reverse then styles.Reverse.on else styles.Reverse.off
  private def underlineEsc: Text = if underline then styles.Underline.on else styles.Underline.off
  private def concealEsc: Text = if conceal then styles.Conceal.on else styles.Conceal.off
  private def strikeEsc: Text = if strike then styles.Strike.on else styles.Strike.off

  def addChanges(buf: StringBuilder, next: TextStyle, colorDepth: ColorDepth): Unit =
    if fg != next.fg then buf.add(next.fg.let(Fg(_).ansi(colorDepth)).or(t"$esc[39m"))
    if bg != next.bg then buf.add(next.bg.let(Bg(_).ansi(colorDepth)).or(t"$esc[49m"))
    if italic != next.italic then buf.add(t"${esc}${next.italicEsc}")
    if bold != next.bold then buf.add(t"${esc}${next.boldEsc}")
    if reverse != next.reverse then buf.add(t"${esc}${next.reverseEsc}")
    if underline != next.underline then buf.add(t"${esc}${next.underlineEsc}")
    if conceal != next.conceal then buf.add(t"${esc}${next.concealEsc}")
    if strike != next.strike then buf.add(t"${esc}${next.strikeEsc}")
