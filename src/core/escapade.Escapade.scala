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

import anticipation.*
import denominative.*
import gossamer.*
import hypotenuse.*
import iridescence.*
import rudiments.*
import vacuous.*

import language.experimental.pureFunctions

object Escapade:
  opaque type CharSpan = Long

  object CharSpan:
    def apply(start: Int, end: Int): CharSpan = (start.toLong << 32) + (Int.MaxValue - end)
    given Ordering[CharSpan] = Ordering.Long.on[CharSpan](identity(_))
    val Nowhere: CharSpan = CharSpan(Int.MaxValue, Int.MaxValue)

  extension (span: CharSpan)
    def start: Int = (span >> 32).toInt
    def end: Int = Int.MaxValue - span.toInt
    def isEmpty: Boolean = start == end

    def trimLeft(n: Int): CharSpan =
      if n >= end then CharSpan.Nowhere else if n <= start then CharSpan(start - n, end - n)
      else CharSpan(0, end - n)

    def takeLeft(n: Int): CharSpan =
      if n <= start then CharSpan.Nowhere else if n >= end then span else CharSpan(start, n)

    def shift(n: Int): CharSpan = CharSpan(start + n, end + n)

  opaque type AnsiStyle = B64

  object indexes:
    val SetBold = Prim
    val Bold = SetBold.next
    val SetItalic = Ter
    val Italic = SetItalic.next
    val SetUnderline = Quin
    val Underline = SetUnderline.next
    val SetStrike = Sept
    val Strike = SetStrike.next
    val SetConcealed = Non
    val Concealed = SetConcealed.next
    val SetForeground = Concealed.next
    val Foreground: Interval = SetForeground.subsequent(24)
    val SetBackground = Foreground.next
    val Background: Interval = SetBackground.subsequent(24)

    val Changes: B64 = B64.set(SetBold) | B64.set(SetItalic) | B64.set(SetUnderline) |
        B64.set(Strike) | B64.set(Concealed) | B64.set(SetForeground) | B64.set(SetBackground)

    val FlagChanges: B64 = B64.set(SetBold) | B64.set(SetItalic) | B64.set(SetUnderline) |
        B64.set(Strike) | B64.set(Concealed)

    val Mask: B64 = B64.set(Bold) | B64.set(Italic) | B64.set(Underline) | B64.set(Strike) |
        B64.set(Concealed)

    val FgMask: B64 = B64.set(Foreground)
    val BgMask: B64 = B64.set(Background)

  import indexes.*

  extension (style: AnsiStyle)
    def setBold: Boolean = (style: B64)(SetBold)
    def bold: Boolean = style(Bold)
    def setItalic: Boolean = style(SetItalic)
    def italic: Boolean = style(Italic)
    def setUnderline: Boolean = style(SetUnderline)
    def underline: Boolean = style(Underline)
    def setStrike: Boolean = style(SetStrike)
    def strike: Boolean = style(Strike)
    def setConcealed: Boolean = style(SetConcealed)
    def concealed: Boolean = style(Concealed)
    def setForeground: Boolean = style(SetForeground)
    def foreground: Rgb24 = Rgb24(style(Foreground).s64.long.toInt)
    def setBackground: Boolean = style(SetBackground)
    def background: Rgb24 = Rgb24(style(Background).s64.long.toInt)
    def changed: Boolean = (style & Changes) != B64(0)

    def update(changes: AnsiStyle): AnsiStyle =
      val flagsChanged: B64 = changes & FlagChanges
      val changed: B64 = flagsChanged << 1
      val unchanged: B64 = ~changed
      val newFlags: B64 = (changes & changed) | (style & unchanged)
      val newFlags2 = if setForeground then (newFlags & ~FgMask) | (changes & FgMask) else newFlags

      if setBackground then (newFlags2 & ~BgMask) | (changes & BgMask) else newFlags2

case class Teletype2(plain: Text, ansi: IArray[Escapade.AnsiStyle]):

  import Escapade.AnsiStyle

  @targetName("concat")
  def + (that: Teletype2): Teletype2 = Teletype2(plain+that.plain, ansi ++ that.ansi)

  def render(using escapes: TerminalEscapes): Text =
    import Escapade.indexes.*
    Text.construct:
      def recur(current: AnsiStyle, index: Ordinal): Unit =
        if index <= plain.ult.or(Prim - 1) then
          val style = ansi(index.n0)

          if style.changed then
            if style.setBold then append(escapes.bold(style.bold))
            if style.setItalic then append(escapes.italic(style.italic))
            if style.setUnderline then append(escapes.underline(style.underline))
            if style.setConcealed then append(escapes.conceal(style.concealed))
            if style.setStrike then append(escapes.strike(style.strike))
            if style.setForeground then append(escapes.foreground(style.foreground))
            if style.setBackground then append(escapes.background(style.background))

            val current2 = current.update(style)

            append(plain.s.charAt(index.n0))
            recur(current2, index + 1)
          else recur(current, index + 1)
