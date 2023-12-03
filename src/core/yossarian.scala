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
    def offset(x: Int, y: Int): Int = y*buffer(0) + x
    def style(x: Int, y: Int): Style = buffer(1)(offset(x, y))
    def char(x: Int, y: Int): Style = buffer(2)(offset(x, y))
    def width: Int = buffer(0)
    def height: Int = buffer(1).length/buffer(0)
    def capacity: Int = buffer(1).length

    def render: Text = buffer(2).grouped(width).map(String(_).tt).to(List).join(t"\n")

    def scroll(n: Int): Unit =
      val offset = math.abs(width*n)
      val length = capacity - offset
      val source = if n < 0 then 0 else offset
      val destination = if n < 0 then offset else 0
      
      System.arraycopy(buffer(1), source, buffer(1), destination, length)
      System.arraycopy(buffer(2), source, buffer(2), destination, length)
      
      val start = if n < 0 then 0 else length
      
      for i <- 0 until offset do
        buffer(1)(start + i) = Style()
        buffer(2)(start + i) = ' '
    
    def set(x: Int, y: Int, char: Char, style: Style): Unit =
      buffer(1)(offset(x, y)) = style
      buffer(2)(offset(x, y)) = char

    def set(cursor: Int, char: Char, style: Style): Unit =
      buffer(1)(cursor) = style
      buffer(2)(cursor) = char
    
    def copy(): ScreenBuffer =
      val style = new Array[Style](buffer(0)*height)
      System.arraycopy(buffer(1), 0, style, 0, style.length)
      val chars = new Array[Char](buffer(0)*height)
      System.arraycopy(buffer(2), 0, chars, 0, chars.length)
      (buffer(0), style, chars)

  object ScreenBuffer:
    def apply(width: Int, height: Int): ScreenBuffer =
      val chars = Array.fill[Char](width*height)(' ')
      val style = Array.fill[Style](width*height)(Style())
      (width, style, chars)

  extension (style: Style)
    def foreground: Rgb24 = Rgb24(((style >> 40) & 0xffffff).toInt)
    def foreground_=(color: Rgb24): Style = (color.asRgb24Int << 40) + (style & 0x000000ffffffffffL)

    def background: Rgb24 = Rgb24(((style >> 16) & 0xffffff).toInt)
    def background_=(color: Rgb24): Style = (color.asRgb24Int << 16) + (style & 0xffffff000000ffffL)
    
    def bold: Boolean = ((style >> 15) & 1L) == 1L
    def bold_=(state: Boolean): Style = if state then (style | (1L << 15)) else (style & (1L << 15))
    
    def faint: Boolean = ((style >> 14) & 1L) == 1L
    def faint_=(state: Boolean): Style = if state then (style | (1L << 14)) else (style & (1L << 14))
    
    def italic: Boolean = ((style >> 13) & 1L) == 1L
    def italic_=(state: Boolean): Style = if state then (style | (1L << 13)) else (style & (1L << 13))
    
    def strike: Boolean = ((style >> 12) & 1L) == 1L
    def strike_=(state: Boolean): Style = if state then (style | (1L << 12)) else (style & (1L << 12))
    
    def blink: Boolean = ((style >> 11) & 1L) == 1L
    def blink_=(state: Boolean): Style = if state then (style | (1L << 11)) else (style & (1L << 11))
    
    def underline: Boolean = ((style >> 10) & 1L) == 1L
    def underline_=(state: Boolean): Style = if state then (style | (1L << 10)) else (style & (1L << 10))
    
    def conceal: Boolean = ((style >> 9) & 1L) == 1L
    def conceal_=(state: Boolean): Style = if state then (style | (1L << 9)) else (style & (1L << 9))
    
    def reverse: Boolean = ((style >> 8) & 1L) == 1L
    def reverse_=(state: Boolean): Style = if state then (style | (1L << 8)) else (style & (1L << 8))

  object Style:
    def apply(): Style = 0L.foreground = Rgb24(255, 255, 255)

export Yossarian.*