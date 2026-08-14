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
package tessellate

import anticipation.*
import gossamer.*
import gossamer.Textual.concatenable
import hieroglyph.*
import rudiments.*
import symbolism.*

object Alignment:
  // Vertical placement of content within a taller extent.
  enum Vertical:
    case Top, Middle, Bottom

  object Left extends Alignment:
    def pad[textual: Textual { type Result = Char }](content: textual, width: Int, last: Boolean)
      ( using Text is Measurable )
    :   textual =

      content.pad(width)

  object Right extends Alignment:
    def pad[textual: Textual { type Result = Char }](content: textual, width: Int, last: Boolean)
      ( using Text is Measurable )
    :   textual =

      content.pad(width, Rtl)

  object Center extends Alignment:
    def pad[textual: Textual { type Result = Char }](content: textual, width: Int, last: Boolean)
      ( using Text is Measurable )
    :   textual =

      content.center(width)

  object Justify extends Alignment:
    def pad[textual: Textual { type Result = Char }](content: textual, width: Int, last: Boolean)
      ( using Text is Measurable )
    :   textual =

      if last then content.pad(width) else
        val words = content.cut(t" ").stdlib
        val wordCount = words.length
        val spare = width - words.sumBy(_.plain.metrics)

        def recur(spare: Int, count: Int, done: textual): textual =
          if count == 0 then done+Textual(t" "*spare) else
            val space = spare/count
            recur(spare - space, count - 1, done + Textual(t" "*space) + words(wordCount - count))

        recur(spare, wordCount - 1, words.head)

// Horizontal placement of a line of content within a wider extent, realized by `pad`, which
// extends `content` to exactly `width` cells. `last` marks the final line of a paragraph,
// which `Justify` pads at the end rather than stretching its word gaps.
trait Alignment:
  def pad[textual: Textual { type Result = Char }]
    ( content: textual, width: Int, last: Boolean = true )
    ( using Text is Measurable )
  :   textual
