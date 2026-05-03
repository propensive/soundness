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
┃    Soundness, version 0.54.0.                                                                    ┃
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

import language.experimental.pureFunctions

import anticipation.*
import gossamer.*

opaque type StyleWord = Long

object StyleWord:
  final val FgMask:           Long = 0x0000000000ffffffL
  final val BgMask:           Long = 0x0000ffffff000000L
  final val FgSet:            Long = 1L << 48
  final val BgSet:            Long = 1L << 49
  final val Bold:             Long = 1L << 50
  final val Faint:            Long = 1L << 51
  final val Italic:           Long = 1L << 52
  final val Underline:        Long = 1L << 53
  final val DoubleUnderline:  Long = 1L << 54
  final val BlinkSlow:        Long = 1L << 55
  final val BlinkFast:        Long = 1L << 56
  final val Reverse:          Long = 1L << 57
  final val Conceal:          Long = 1L << 58
  final val Strike:           Long = 1L << 59
  final val Overline:         Long = 1L << 60
  final val HyperlinkChange:  Long = 1L << 61

  final val FlagsMask: Long =
    FgSet | BgSet | Bold | Faint | Italic | Underline | DoubleUnderline | BlinkSlow | BlinkFast
    | Reverse | Conceal | Strike | Overline | HyperlinkChange

  val Default: StyleWord = 0L

  inline def apply(raw: Long): StyleWord = raw

  def emitDiff(buffer: StringBuilder, prev: Long, next: Long, depth: ColorDepth): Unit =
    val diff = prev^next

    if (diff & (FgMask | FgSet)) != 0 then
      if (next & FgSet) == 0 then buffer.add(t"\e[39m")
      else buffer.add(Fg(Chroma((next & FgMask).toInt)).ansi(depth))

    if (diff & (BgMask | BgSet)) != 0 then
      if (next & BgSet) == 0 then buffer.add(t"\e[49m")
      else buffer.add(Bg(Chroma(((next & BgMask) >>> 24).toInt)).ansi(depth))

    val flagDiff = diff & (FlagsMask & ~(FgSet | BgSet | HyperlinkChange))
    if flagDiff != 0 then
      if (flagDiff & Bold)            != 0 then buffer.add(if (next & Bold)            != 0 then t"\e[1m"  else t"\e[22m")
      if (flagDiff & Faint)           != 0 then buffer.add(if (next & Faint)           != 0 then t"\e[2m"  else t"\e[22m")
      if (flagDiff & Italic)          != 0 then buffer.add(if (next & Italic)          != 0 then t"\e[3m"  else t"\e[23m")
      if (flagDiff & Underline)       != 0 then buffer.add(if (next & Underline)       != 0 then t"\e[4m"  else t"\e[24m")
      if (flagDiff & DoubleUnderline) != 0 then buffer.add(if (next & DoubleUnderline) != 0 then t"\e[21m" else t"\e[24m")
      if (flagDiff & BlinkSlow)       != 0 then buffer.add(if (next & BlinkSlow)       != 0 then t"\e[5m"  else t"\e[25m")
      if (flagDiff & BlinkFast)       != 0 then buffer.add(if (next & BlinkFast)       != 0 then t"\e[6m"  else t"\e[25m")
      if (flagDiff & Reverse)         != 0 then buffer.add(if (next & Reverse)         != 0 then t"\e[7m"  else t"\e[27m")
      if (flagDiff & Conceal)         != 0 then buffer.add(if (next & Conceal)         != 0 then t"\e[8m"  else t"\e[28m")
      if (flagDiff & Strike)          != 0 then buffer.add(if (next & Strike)          != 0 then t"\e[9m"  else t"\e[29m")
      if (flagDiff & Overline)        != 0 then buffer.add(if (next & Overline)        != 0 then t"\e[53m" else t"\e[55m")

extension (style: StyleWord)
  inline def raw: Long = style

  inline def fgRgb: Int = (style & StyleWord.FgMask).toInt
  inline def bgRgb: Int = ((style & StyleWord.BgMask) >>> 24).toInt
  inline def hasFg: Boolean = (style & StyleWord.FgSet) != 0
  inline def hasBg: Boolean = (style & StyleWord.BgSet) != 0

  inline def isBold:            Boolean = (style & StyleWord.Bold)            != 0
  inline def isFaint:           Boolean = (style & StyleWord.Faint)           != 0
  inline def isItalic:          Boolean = (style & StyleWord.Italic)          != 0
  inline def isUnderline:       Boolean = (style & StyleWord.Underline)       != 0
  inline def isDoubleUnderline: Boolean = (style & StyleWord.DoubleUnderline) != 0
  inline def isBlinkSlow:       Boolean = (style & StyleWord.BlinkSlow)       != 0
  inline def isBlinkFast:       Boolean = (style & StyleWord.BlinkFast)       != 0
  inline def isReverse:         Boolean = (style & StyleWord.Reverse)         != 0
  inline def isConceal:         Boolean = (style & StyleWord.Conceal)         != 0
  inline def isStrike:          Boolean = (style & StyleWord.Strike)          != 0
  inline def isOverline:        Boolean = (style & StyleWord.Overline)        != 0
  inline def hasHyperlinkChange: Boolean = (style & StyleWord.HyperlinkChange) != 0

  inline def withFg(rgb: Int): StyleWord =
    (style & ~StyleWord.FgMask) | (rgb.toLong & 0xffffffL) | StyleWord.FgSet

  inline def withBg(rgb: Int): StyleWord =
    (style & ~StyleWord.BgMask) | ((rgb.toLong & 0xffffffL) << 24) | StyleWord.BgSet

  inline def clearFg: StyleWord = style & ~(StyleWord.FgMask | StyleWord.FgSet)
  inline def clearBg: StyleWord = style & ~(StyleWord.BgMask | StyleWord.BgSet)

  inline def withBit(bit: Long): StyleWord = style | bit
  inline def withoutBit(bit: Long): StyleWord = style & ~bit

  inline def applyTransform(mask: Long, bits: Long): StyleWord = (style & ~mask) | bits
