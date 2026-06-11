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

import anticipation.*
import gossamer.*
import prepositional.*
import rudiments.*

object Imprintable:
  // Emit each grapheme of `text` paired with one uniform `style`.
  private def graphemeCells(text: Text, style: StyleWord)(lambda: (Grapheme, StyleWord) => Unit)
  :   Unit =

    Writing(text).graphemes.each: grapheme =>
      lambda(grapheme, style)

  given text: Text is Imprintable:
    def cells(self: Text)(lambda: (Grapheme, StyleWord) => Unit): Unit =
      graphemeCells(self, StyleWord.Default)(lambda)

  given writing: Writing is Imprintable:
    def cells(self: Writing)(lambda: (Grapheme, StyleWord) => Unit): Unit =
      self.graphemes.each: grapheme =>
        lambda(grapheme, StyleWord.Default)

  given ascii: Ascii is Imprintable:
    def cells(self: Ascii)(lambda: (Grapheme, StyleWord) => Unit): Unit =
      graphemeCells(summon[Ascii is Textual].text(self), StyleWord.Default)(lambda)

  // Each grapheme takes the style of its first character (escapade stores styles per
  // character; a grapheme is styled as a unit), so colour survives decomposition.
  given teletype: Teletype is Imprintable:
    def cells(self: Teletype)(lambda: (Grapheme, StyleWord) => Unit): Unit =
      val writing    = Writing(self.plain)
      val boundaries = writing.boundaries
      val count      = boundaries.length - 1
      var index      = 0

      while index < count do
        val start: Int  = boundaries(index)
        val piece: Text = self.plain.s.substring(start, boundaries(index + 1)).nn.tt
        lambda(Grapheme(piece.s), StyleWord(self.styleAt(start)))
        index += 1

// Content that can be imprinted onto a terminal cell grid: it decomposes into a
// sequence of grapheme cells, each paired with the `StyleWord` that styles it (the
// default style for unstyled content). Working in graphemes — not chars — makes each
// cell's terminal display width unambiguous (a wide CJK glyph or a ZWJ emoji is one
// cell, a combining mark contributes none), so a surface advances its cursor by
// `Grapheme` width rather than character count. The instances cover the text types
// seamlessly: `Text`, `Ascii` and `Writing` carry the default style; `Teletype`
// carries its own per-character styling.
trait Imprintable extends Typeclass:
  def cells(self: Self)(lambda: (Grapheme, StyleWord) => Unit): Unit
