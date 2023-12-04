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
import iridescence.*
import anticipation.*
import gossamer.*

object Yossarian:
  opaque type Style = Long
  opaque type ScreenBuffer = (Int, Array[Style], Array[Char])

  extension (buffer: ScreenBuffer)
    def width: Int = buffer(0)
    def styleBuffer: Array[Style] = buffer(1)
    def charBuffer: Array[Char] = buffer(2)
    def capacity: Int = charBuffer.length
    def height: Int = capacity/width
    def offset(x: Int, y: Int): Int = y*width + x
    def style(x: Int, y: Int): Style = styleBuffer(offset(x, y))
    def char(x: Int, y: Int): Style = charBuffer(offset(x, y))
    def line: ScreenBuffer = (charBuffer.length, styleBuffer, charBuffer)
    def render: Text = String(charBuffer).grouped(width).to(List).map(_.tt).join(t"\n")

    def find(text: Text): Maybe[ScreenBuffer] = line.render.s.indexOf(text.s) match
      case -1 =>
        Unset
      
      case index =>
        (text.length, styleBuffer.slice(index, index + text.length), charBuffer.slice(index, index + text.length))

    def styles: IArray[Style] = styleBuffer.clone().immutable(using Unsafe)

    def scroll(n: Int): Unit =
      val offset = math.abs(width*n)
      val length = capacity - offset
      val source = if n < 0 then 0 else offset
      val destination = if n < 0 then offset else 0
      
      System.arraycopy(styleBuffer, source, styleBuffer, destination, length)
      System.arraycopy(charBuffer, source, charBuffer, destination, length)
      
      val start = if n < 0 then 0 else length
      
      for i <- 0 until offset do
        styleBuffer(start + i) = Style()
        charBuffer(start + i) = ' '
    
    def set(x: Int, y: Int, char: Char, style: Style): Unit =
      styleBuffer(offset(x, y)) = style
      charBuffer(offset(x, y)) = char

    def set(cursor: Int, char: Char, style: Style): Unit =
      styleBuffer(cursor) = style
      charBuffer(cursor) = char
    
    def copy(): ScreenBuffer =
      val style = new Array[Style](capacity)
      System.arraycopy(styleBuffer, 0, style, 0, style.length)
      val chars = new Array[Char](capacity)
      System.arraycopy(charBuffer, 0, chars, 0, chars.length)
      (width, style, chars)
    
  object ScreenBuffer:
    def apply(width: Int, height: Int): ScreenBuffer =
      val chars = Array.fill[Char](width*height)(' ')
      val style = Array.fill[Style](width*height)(Style())
      (width, style, chars)

  object Style:
    def apply(): Style = Foreground(0L) = Rgb24(255, 255, 255)

    class Bits(offset: Int, mask: Long):
      def apply(style: Style): Rgb24 = Rgb24(((style >>> offset) & 0xffffff).toInt)
      def update(style: Style, color: Rgb24): Style = (color.asRgb24Int.toLong << offset) + (style & mask)
    
    val Foreground = Bits(40, 0x000000ffffffffffL)
    val Background = Bits(16, 0xffffff000000ffffL)

    enum Bit:
      case Bold, Faint, Italic, Strike, Blink, Underline, Conceal, Reverse
  
      def bit: Int = 15 - ordinal
      def apply(style: Style): Boolean = ((style >>> bit) & 1L) == 1L
      
      def update(style: Style, boolean: Boolean): Style =
        if boolean then (style | (1L << bit)) else (style & (1L << bit))

export Yossarian.*