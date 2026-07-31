                                                                                                  /*
┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓
┃                                                                                                  ┃
┃                                                   ╭───╮                                          ┃
┃                                                   │   │                                          ┃
┃                                                   │   │                                          ┃
┃   ╭───────╮╭─────────╮╭───╮ ╭───╮╭───╮╌────╮╭────╌┤   │╭───╮╌────╮╭────────╮╭───────╮╭───────╮   ┃
┃   │   ╭───╯│   ╭─╮   ││   │ │   ││   ╭─╮   ││   ╭─╮   ││   ╭─╮   ││   ╭─╮  ││   ╭───╯│   ╭───╯   ┃
┃   │   ╰───╮│   │ │   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   ╰─╯  ││   ╰───╮│   ╰───╮   ┃
┃   ╰───╮   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   ╭────╯╰───╮   │╰───╮   │   ┃
┃   ╭───╯   ││   ╰─╯   ││   ╰─╯   ││   │ │   ││   ╰─╯   ││   │ │   ││   ╰────╮╭───╯   │╭───╯   │   ┃
┃   ╰───────╯╰─────────╯╰────╌╰───╯╰───╯ ╰───╯╰────╌╰───╯╰───╯ ╰───╯╰────────╯╰───────╯╰───────╯   ┃
┃                                                                                                  ┃
┃    Soundness, version 0.64.0.                                                                    ┃
┃    © Copyright 2021-25 Jon Pretty, Propensive OÜ.                                                ┃
┃                                                                                                  ┃
┃    The primary distribution site is:                                                             ┃
┃                                                                                                  ┃
┃        https://soundness.dev/                                                                    ┃
┃                                                                                                  ┃
┃    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file     ┃
┃    except in compliance with the License. You may obtain a copy of the License at                ┃
┃                                                                                                  ┃
┃        https://www.apache.org/licenses/LICENSE-2.0                                               ┃
┃                                                                                                  ┃
┃    Unless required by applicable law or agreed to in writing,  software distributed under the    ┃
┃    License is distributed on an "AS IS" BASIS,  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,    ┃
┃    either express or implied. See the License for the specific language governing permissions    ┃
┃    and limitations under the License.                                                            ┃
┃                                                                                                  ┃
┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛
                                                                                                  */
package escapade

import scala.language.experimental.pureFunctions

import anticipation.*
import vacuous.*

object TextStyle:
  val esc: Char = 27.toChar

case class TextStyle
  ( fg:              Optional[Chroma] = Unset,
    bg:              Optional[Chroma] = Unset,
    italic:          Boolean          = false,
    bold:            Boolean          = false,
    reverse:         Boolean          = false,
    underline:       Boolean          = false,
    conceal:         Boolean          = false,
    strike:          Boolean          = false,
    faint:           Boolean          = false,
    doubleUnderline: Boolean          = false,
    blinkSlow:       Boolean          = false,
    blinkFast:       Boolean          = false,
    overline:        Boolean          = false ):

  def styleWord: Long =
    var s: Long = 0L
    if italic          then s |= StyleWord.Italic
    if bold            then s |= StyleWord.Bold
    if reverse         then s |= StyleWord.Reverse
    if underline       then s |= StyleWord.Underline
    if conceal         then s |= StyleWord.Conceal
    if strike          then s |= StyleWord.Strike
    if faint           then s |= StyleWord.Faint
    if doubleUnderline then s |= StyleWord.DoubleUnderline
    if blinkSlow       then s |= StyleWord.BlinkSlow
    if blinkFast       then s |= StyleWord.BlinkFast
    if overline        then s |= StyleWord.Overline

    fg.let: c => s = s | (c.underlying.toLong & 0xffffffL) | StyleWord.FgSet

    bg.let: c => s = s | ((c.underlying.toLong & 0xffffffL) << 24) | StyleWord.BgSet

    s

  def addChanges(buffer: StringBuilder, next: TextStyle, colorDepth: ColorDepth): Unit =
    StyleWord.emitDiff(buffer, styleWord, next.styleWord, colorDepth)
