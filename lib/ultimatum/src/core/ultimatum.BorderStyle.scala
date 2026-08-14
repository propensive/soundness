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
package ultimatum

import anticipation.*
import tessellate.*

object BorderStyle:
  // A view over tessellate's box-drawing table: every glyph is looked up by the weights of
  // the edges meeting at it, so a style built this way always has coherent corners.
  def apply(charset: LineCharset, line: BoxLine): BorderStyle =
    def glyph
      ( top:    BoxLine = BoxLine.Blank,
        right:  BoxLine = BoxLine.Blank,
        bottom: BoxLine = BoxLine.Blank,
        left:   BoxLine = BoxLine.Blank )
    :   Text =

      charset(top, right, bottom, left).toString.tt

    BorderStyle
      ( horizontal  = glyph(right = line, left = line),
        vertical    = glyph(top = line, bottom = line),
        topLeft     = glyph(right = line, bottom = line),
        topRight    = glyph(bottom = line, left = line),
        bottomLeft  = glyph(top = line, right = line),
        bottomRight = glyph(top = line, left = line) )

  // Light single lines with square corners (the default).
  val light: BorderStyle = BorderStyle(LineCharset.Default, BoxLine.Thin)

  // Light single lines with rounded corners.
  val rounded: BorderStyle = BorderStyle(LineCharset.Rounded, BoxLine.Thin)

  // Heavy single lines with square corners.
  val heavy: BorderStyle = BorderStyle(LineCharset.Default, BoxLine.Thick)

  // Double lines.
  val double: BorderStyle = BorderStyle(LineCharset.Default, BoxLine.Double)

  // ASCII-only, for terminals without box-drawing glyphs.
  val ascii: BorderStyle = BorderStyle(LineCharset.Ascii, BoxLine.Thin)

// The glyphs a `border` draws with: a horizontal rule, a vertical rule, and the
// four corners. An edge fills its rectangle by repeating its rule, so a single
// style serves any size; corners are drawn only where two requested sides meet.
case class BorderStyle
  ( horizontal:  Text,
    vertical:    Text,
    topLeft:     Text,
    topRight:    Text,
    bottomLeft:  Text,
    bottomRight: Text )
