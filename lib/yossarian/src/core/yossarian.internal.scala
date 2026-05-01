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
package yossarian

import anticipation.*
import denominative.*
import gossamer.*
import rudiments.*
import spectacular.*
import vacuous.*

object internal:
  opaque type Style = Long


  class Screen
       (val width: Int,
        styleBuffer: Array[Style],
        charBuffer: Array[Char],
        linkBuffer: Array[Text]):

    def capacity: Int = charBuffer.length
    def height: Int = capacity/width
    def offset(x: Ordinal, y: Ordinal): Int = y.n0*width + x.n0
    def style(x: Ordinal, y: Ordinal): Style = styleBuffer(offset(x, y))
    def char(x: Ordinal, y: Ordinal): Char = charBuffer(offset(x, y))
    def link(x: Ordinal, y: Ordinal): Text = linkBuffer(offset(x, y))
    def line: Screen = new Screen(charBuffer.length, styleBuffer, charBuffer, linkBuffer)
    def render: Text = String(charBuffer).grouped(width).to(List).map(_.tt).join(t"\n")

    def find(text: Text): Optional[Screen] = line.render.s.indexOf(text.s) match
      case -1 => Unset

      case index =>
        new Screen
         (text.length,
          styleBuffer.slice(index, index + text.length),
          charBuffer.slice(index, index + text.length),
          linkBuffer.slice(index, index + text.length))

    def styles: IArray[Style] = styleBuffer.clone().immutable(using Unsafe)

    def scroll(n: Int): Unit = scroll(n, 0, height - 1)

    def scroll(n: Int, top: Int, bottom: Int): Unit =
      val regionRows = bottom - top + 1
      val absN = math.abs(n).min(regionRows)
      val offset = width*absN
      val length = width*(regionRows - absN)
      val regionStart = top*width

      val source = if n < 0 then regionStart else regionStart + offset
      val destination = if n < 0 then regionStart + offset else regionStart

      System.arraycopy(styleBuffer, source, styleBuffer, destination, length)
      System.arraycopy(charBuffer, source, charBuffer, destination, length)
      System.arraycopy(linkBuffer, source, linkBuffer, destination, length)

      val fillStart = if n < 0 then regionStart else regionStart + length

      for i <- 0 until offset do
        styleBuffer(fillStart + i) = Style()
        charBuffer(fillStart + i) = ' '
        linkBuffer(fillStart + i) = t""

    def set(x: Ordinal, y: Ordinal, char: Char, style: Style, link: Text): Unit =
      styleBuffer(offset(x, y)) = style
      charBuffer(offset(x, y)) = char
      linkBuffer(offset(x, y)) = link

    def set(cursor: Ordinal, char: Char, style: Style, link: Text): Unit =
      styleBuffer(cursor.n0) = style
      charBuffer(cursor.n0) = char
      linkBuffer(cursor.n0) = link

    def copy(): Screen =
      val styles = new Array[Style](capacity)
      System.arraycopy(styleBuffer, 0, styles, 0, styles.length)
      val chars = new Array[Char](capacity)
      System.arraycopy(charBuffer, 0, chars, 0, chars.length)
      val links = new Array[Text](capacity)
      System.arraycopy(linkBuffer, 0, links, 0, links.length)
      new Screen(width, styles, chars, links)


  object Screen:
    def apply(width: Int, height: Int): Screen =
      val chars = Array.fill[Char](width*height)(' ')
      val styles = Array.fill[Style](width*height)(Style())
      val links = Array.fill[Text](width*height)(t"")
      new Screen(width, styles, chars, links)


  extension (style: Style)
    def bold: Boolean = Style.Bit.Bold(style)

    def italic: Boolean = Style.Bit.Italic(style)
    def blink: Boolean = Style.Bit.Blink(style)
    def faint: Boolean = Style.Bit.Faint(style)
    def conceal: Boolean = Style.Bit.Conceal(style)
    def strike: Boolean = Style.Bit.Strike(style)
    def underline: Boolean = Style.Bit.Underline(style)
    def reverse: Boolean = Style.Bit.Reverse(style)
    def foreground: Chroma = Style.Foreground(style)
    def background: Chroma = Style.Background(style)


  object Style:
    def apply(): Style = Foreground(0L) = Chroma(255, 255, 255)

    class Bits(offset: Int, mask: Long):
      def apply(style: Style): Chroma = Chroma(((style >>> offset) & 0xffffff).toInt)

      def update(style: Style, color: Chroma): Style =
        (color.underlying.toLong << offset) + (style & mask)

    val Foreground = Bits(40, 0x000000ffffffffffL)
    val Background = Bits(16, 0xffffff000000ffffL)

    given inspectable: Style is Inspectable = style =>
      Map
        ( t"Bo" -> Bit.Bold(style),
          t"F"  -> Bit.Faint(style),
          t"I"  -> Bit.Italic(style),
          t"S"  -> Bit.Strike(style),
          t"Bl" -> Bit.Blink(style),
          t"U"  -> Bit.Underline(style),
          t"C"  -> Bit.Conceal(style),
          t"R"  -> Bit.Reverse(style) )

      .   map { (key, value) => if value then key else t"!$key" }
      .   join(t"[", t" ", t" ${Foreground(style).inspect} ${Background(style).inspect}]")

    enum Bit:
      case Bold, Faint, Italic, Strike, Blink, Underline, Conceal, Reverse

      def bit: Int = 15 - ordinal
      def apply(style: Style): Boolean = ((style >>> bit) & 1L) == 1L

      def update(style: Style, boolean: Boolean): Style =
        if boolean then (style | (1L << bit)) else (style & ~(1L << bit))
