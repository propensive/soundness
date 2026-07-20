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
┃    Soundness, version 0.63.0.                                                                    ┃
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

import scala.math

import scala.reflect.*

import anticipation.*
import denominative.*
import gossamer.*
import rudiments.*
import spectacular.*
import vacuous.*

object internal:
  opaque type Style = Long

  object Screen:
    def apply[styling: ClassTag](width: Int, height: Int, blank: styling): Screen[styling] =
      val graphemes = Array.fill[Grapheme](width*height)(Grapheme(" "))
      val styles = Array.fill[styling](width*height)(blank)
      val links = Array.fill[Text](width*height)(t"")
      new Screen(width, blank, styles, graphemes, links)

    // The terminal-emulator buffer: cells are `yossarian.Style` blanked to the
    // default style.
    def apply(width: Int, height: Int): Screen[Style] = apply(width, height, Style())

  // A cell-aligned screen buffer. Each cell holds one grapheme cluster (per
  // UAX #29) — an ASCII character, an East-Asian-Wide CJK glyph, an emoji, or
  // a base + combining marks. Wide graphemes occupy two adjacent cells: the
  // leading cell holds the grapheme; the trailing cell holds an empty
  // Grapheme("") sentinel, distinguishable via isWideTrailing. The per-cell style
  // type is generic, so the same buffer backs both yossarian's terminal emulator
  // (`Screen[Style]`) and a TUI surface (`Screen[escapade.StyleWord]`); `blank` is
  // the style a vacated cell takes.
  class Screen[styling]
    ( val width:      Int,
      blank:          styling,
      styleBuffer:    Array[styling],
      graphemeBuffer: Array[Grapheme],
      linkBuffer:     Array[Text] ):

    def capacity: Int = graphemeBuffer.length
    def height: Int = capacity/width
    def offset(x: Ordinal, y: Ordinal): Int = y.n0*width + x.n0
    def style(x: Ordinal, y: Ordinal): styling = styleBuffer(offset(x, y))
    def link(x: Ordinal, y: Ordinal): Text = linkBuffer(offset(x, y))

    // The grapheme stored at this cell. An empty Grapheme("") value marks the
    // trailing half of a wide character whose leading cell is at column x-1.
    def grapheme(x: Ordinal, y: Ordinal): Grapheme = graphemeBuffer(offset(x, y))

    // First Char of the cell's grapheme, or ' ' for trailing-half cells.
    // Convenience accessor for narrow-ASCII assertions.
    def char(x: Ordinal, y: Ordinal): Char =
      val grapheme = graphemeBuffer(offset(x, y)).text
      if grapheme.nil then ' ' else grapheme.at(Prim).vouch

    // True if this cell is the trailing half of a wide grapheme stored in the
    // cell to the left.
    def isWideTrailing(x: Ordinal, y: Ordinal): Boolean =
      graphemeBuffer(offset(x, y)).text.nil

    def line: Screen[styling] =
      new Screen(graphemeBuffer.length, blank, styleBuffer, graphemeBuffer, linkBuffer)

    def render: Text =
      val sb = StringBuilder()
      var y = 0

      while y < height do
        var x = 0

        while x < width do
          val s = graphemeBuffer(y*width + x).text.s
          if s.nonEmpty then sb.append(s)
          x += 1

        if y < height - 1 then sb.append('\n')
        y += 1

      sb.text

    def find(text: Text): Optional[Screen[styling]] = line.render.offsetOf(text).let: ordinal =>
      val index = ordinal.n0

      new Screen
        ( text.length, blank, styleBuffer.slice(index, index + text.length),
          graphemeBuffer.slice(index, index + text.length),
          linkBuffer.slice(index, index + text.length) )

    def styles: IArray[styling] = styleBuffer.clone().immutable(using Unsafe)

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
      System.arraycopy(graphemeBuffer, source, graphemeBuffer, destination, length)
      System.arraycopy(linkBuffer, source, linkBuffer, destination, length)

      val fillStart = if n < 0 then regionStart else regionStart + length

      for i <- 0 until offset do
        styleBuffer(fillStart + i) = blank
        graphemeBuffer(fillStart + i) = Grapheme(" ")
        linkBuffer(fillStart + i) = t""

    def set(x: Ordinal, y: Ordinal, grapheme: Grapheme, style: styling, link: Text): Unit =
      styleBuffer(offset(x, y)) = style
      graphemeBuffer(offset(x, y)) = grapheme
      linkBuffer(offset(x, y)) = link

    def set(cursor: Ordinal, grapheme: Grapheme, style: styling, link: Text): Unit =
      styleBuffer(cursor.n0) = style
      graphemeBuffer(cursor.n0) = grapheme
      linkBuffer(cursor.n0) = link

    def copy(): Screen[styling] =
      new Screen(width, blank, styleBuffer.clone(), graphemeBuffer.clone(), linkBuffer.clone())


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

    // An explicit instance to avoid deriving `Inspectable[Chroma]`, whose derived anon class the
    // Scala.js pipeline rejects (the `text` parameter acquires a fresh capture var, unlike the JVM
    // pipeline). Hand-written SAMs like this one are unaffected. (Compiler divergence; see #1520.)
    given chromaInspectable: Chroma is Inspectable = _.underlying.inspect

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

      .   map: (key, value) => if value then key else t"!$key"
      .   join(t"[", t" ", t" ${Foreground(style).inspect} ${Background(style).inspect}]")

    enum Bit:
      case Bold, Faint, Italic, Strike, Blink, Underline, Conceal, Reverse

      def bit: Int = 15 - ordinal
      def apply(style: Style): Boolean = ((style >>> bit) & 1L) == 1L

      def update(style: Style, boolean: Boolean): Style =
        if boolean then (style | (1L << bit)) else (style & ~(1L << bit))
