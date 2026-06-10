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

import scala.reflect.*

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

  // A `StyleWord` is a `Long`, so an `Array[StyleWord]` is an unboxed `long[]`; the
  // opaque type hides that from callers, so expose the `ClassTag` (cells stored in a
  // grid need it to allocate without boxing).
  given classTag: ClassTag[StyleWord] = summon[ClassTag[Long]].asInstanceOf[ClassTag[StyleWord]]

  inline def apply(raw: Long): StyleWord = raw

  def combine(outer: Long, inner: Long): Long =
    val flagsOnly = ~(FgMask | BgMask | FgSet | BgSet)
    val combinedFlags = (outer | inner) & flagsOnly

    val fgBits =
      if (inner & FgSet) != 0 then inner & (FgMask | FgSet)
      else if (outer & FgSet) != 0 then outer & (FgMask | FgSet)
      else 0L

    val bgBits =
      if (inner & BgSet) != 0 then inner & (BgMask | BgSet)
      else if (outer & BgSet) != 0 then outer & (BgMask | BgSet)
      else 0L
    combinedFlags | fgBits | bgBits

  def emitDiff(buffer: StringBuilder, prev: Long, next: Long, depth: ColorDepth): Unit =
    val diff = prev^next
    var open = false

    def sep(): Unit =
      if !open then
        buffer.add(t"\e[")
        open = true
      else
        buffer.append(';')

    def emitColor(prefix: Int, rgb: Int): Unit =
      val r = (rgb >> 16)&255
      val g = (rgb >> 8)&255
      val b = rgb&255
      depth match
        case ColorDepth.TrueColor =>
          sep()
          buffer.append(prefix); buffer.append(';'); buffer.append('2')
          buffer.append(';'); buffer.append(r)
          buffer.append(';'); buffer.append(g)
          buffer.append(';'); buffer.append(b)

        case _ =>
          val n =
            if r == 0 && g == 0 && b == 0 then 16
            else if r == 255 && g == 255 && b == 255 then 231
            else if b == r && r == g then 232 + r*23/255
            else 16 + r*5/255*36 + g*5/255*6 + b*5/255

          sep()
          buffer.append(prefix); buffer.append(';'); buffer.append('5')
          buffer.append(';'); buffer.append(n)

    def toggle(bit: Long, flagDiff: Long, on: Int, off: Int): Unit =
      if (flagDiff & bit) != 0 then
        sep()
        buffer.append(if (next & bit) != 0 then on else off)

    if (diff & (FgMask | FgSet)) != 0 then
      if (next & FgSet) == 0 then { sep(); buffer.append(39) }
      else emitColor(38, (next & FgMask).toInt)

    if (diff & (BgMask | BgSet)) != 0 then
      if (next & BgSet) == 0 then { sep(); buffer.append(49) }
      else emitColor(48, ((next & BgMask) >>> 24).toInt)

    val flagDiff = diff & (FlagsMask & ~(FgSet | BgSet | HyperlinkChange))
    if flagDiff != 0 then
      toggle(Italic,          flagDiff, 3,  23)
      toggle(Bold,            flagDiff, 1,  22)
      toggle(Reverse,         flagDiff, 7,  27)
      toggle(Underline,       flagDiff, 4,  24)
      toggle(Conceal,         flagDiff, 8,  28)
      toggle(Strike,          flagDiff, 9,  29)
      toggle(Faint,           flagDiff, 2,  22)
      toggle(DoubleUnderline, flagDiff, 21, 24)
      toggle(BlinkSlow,       flagDiff, 5,  25)
      toggle(BlinkFast,       flagDiff, 6,  25)
      toggle(Overline,        flagDiff, 53, 55)

    if open then buffer.append('m')

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
