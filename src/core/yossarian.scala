/*
    Yossarian, version [unreleased]. Copyright 2023 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package yossarian

import rudiments.*
import vacuous.*
import iridescence.*
import spectacular.*
import anticipation.*
import gossamer.*

object Yossarian:
  opaque type Style = Long
  opaque type ScreenBuffer = (Int, Array[Style], Array[Char], Array[Text])

  extension (buffer: ScreenBuffer)
    def width: Int = buffer(0)
    def styleBuffer: Array[Style] = buffer(1)
    def charBuffer: Array[Char] = buffer(2)
    def linkBuffer: Array[Text] = buffer(3)
    def capacity: Int = charBuffer.length
    def height: Int = capacity/width
    def offset(x: Int, y: Int): Int = y*width + x
    def style(x: Int, y: Int): Style = styleBuffer(offset(x, y))
    def char(x: Int, y: Int): Char = charBuffer(offset(x, y))
    def link(x: Int, y: Int): Text = linkBuffer(offset(x, y))
    def line: ScreenBuffer = (charBuffer.length, styleBuffer, charBuffer, linkBuffer)
    def render: Text = String(charBuffer).grouped(width).to(List).map(_.tt).join(t"\n")

    def find(text: Text): Optional[ScreenBuffer] = line.render.s.indexOf(text.s) match
      case -1 =>
        Unset
      
      case index =>
        (text.length, styleBuffer.slice(index, index + text.length),
            charBuffer.slice(index, index + text.length), linkBuffer.slice(index, index + text.length))

    def styles: IArray[Style] = styleBuffer.clone().immutable(using Unsafe)

    def scroll(n: Int): Unit =
      val offset = math.abs(width*n)
      val length = capacity - offset
      val source = if n < 0 then 0 else offset
      val destination = if n < 0 then offset else 0
      
      System.arraycopy(styleBuffer, source, styleBuffer, destination, length)
      System.arraycopy(charBuffer, source, charBuffer, destination, length)
      System.arraycopy(linkBuffer, source, linkBuffer, destination, length)
      
      val start = if n < 0 then 0 else length
      
      for i <- 0 until offset do
        styleBuffer(start + i) = Style()
        charBuffer(start + i) = ' '
        linkBuffer(start + i) = t""
    
    def set(x: Int, y: Int, char: Char, style: Style, link: Text): Unit =
      styleBuffer(offset(x, y)) = style
      charBuffer(offset(x, y)) = char
      linkBuffer(offset(x, y)) = link

    def set(cursor: Int, char: Char, style: Style, link: Text): Unit =
      styleBuffer(cursor) = style
      charBuffer(cursor) = char
      linkBuffer(cursor) = link
    
    def copy(): ScreenBuffer =
      val styles = new Array[Style](capacity)
      System.arraycopy(styleBuffer, 0, styles, 0, styles.length)
      val chars = new Array[Char](capacity)
      System.arraycopy(charBuffer, 0, chars, 0, chars.length)
      val links = new Array[Text](capacity)
      System.arraycopy(linkBuffer, 0, links, 0, links.length)
      (width, styles, chars, links)
    
  object ScreenBuffer:
    def apply(width: Int, height: Int): ScreenBuffer =
      val chars = Array.fill[Char](width*height)(' ')
      val styles = Array.fill[Style](width*height)(Style())
      val links = Array.fill[Text](width*height)(t"")
      (width, styles, chars, links)

  extension (style: Style)
    def bold: Boolean = Style.Bit.Bold(style)
    def italic: Boolean = Style.Bit.Italic(style)
    def blink: Boolean = Style.Bit.Blink(style)
    def faint: Boolean = Style.Bit.Faint(style)
    def conceal: Boolean = Style.Bit.Conceal(style)
    def strike: Boolean = Style.Bit.Strike(style)
    def underline: Boolean = Style.Bit.Underline(style)
    def reverse: Boolean = Style.Bit.Reverse(style)
    def foreground: Rgb24 = Style.Foreground(style)
    def background: Rgb24 = Style.Background(style)

  object Style:
    def apply(): Style = Foreground(0L) = Rgb24(255, 255, 255)

    class Bits(offset: Int, mask: Long):
      def apply(style: Style): Rgb24 = Rgb24(((style >>> offset) & 0xffffff).toInt)
      def update(style: Style, color: Rgb24): Style = (color.asRgb24Int.toLong << offset) + (style & mask)
    
    val Foreground = Bits(40, 0x000000ffffffffffL)
    val Background = Bits(16, 0xffffff000000ffffL)

    given Debug[Style] = style =>
      Map(
        t"Bo" -> Bit.Bold(style),
        t"F"  -> Bit.Faint(style),
        t"I"  -> Bit.Italic(style),
        t"S"  -> Bit.Strike(style),
        t"Bl" -> Bit.Blink(style),
        t"U"  -> Bit.Underline(style),
        t"C"  -> Bit.Conceal(style),
        t"R"  -> Bit.Reverse(style),
      )
        .map { (key, value) => if value then key else t"!$key" }
        .join(t"[", t" ", t" ${Foreground(style).debug} ${Background(style).debug}]")

    enum Bit:
      case Bold, Faint, Italic, Strike, Blink, Underline, Conceal, Reverse
  
      def bit: Int = 15 - ordinal
      def apply(style: Style): Boolean = ((style >>> bit) & 1L) == 1L
      
      def update(style: Style, boolean: Boolean): Style =
        if boolean then (style | (1L << bit)) else (style & ~(1L << bit))

export Yossarian.*