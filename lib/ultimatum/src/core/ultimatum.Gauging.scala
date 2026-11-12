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
import escapade.*
import hieroglyph.*
import iridescence.*
import prepositional.*

object Gauging:
  // Assembled from three separately-importable givens, so that a caller overrides any one axis —
  // the colours, the character repertoire, the width metric — without having to name the other
  // two. This is what keeps "which design", "which palette" and "which glyphs" three orthogonal
  // imports rather than one combinatorial choice.
  given assembled: (palette0:  GaugePalette,
                    glyphs0:   Gaugeable.Glyphs,
                    metric0:   Text is Measurable)
  =>  Gauging =
    new Gauging:
      val palette = palette0
      val glyphs = glyphs0
      val metric = metric0

// The ambient rendering context a design captures when its `given` is summoned: which colours to
// draw in, which glyphs it may use, and how to measure a string's width in cells.
// A design takes this as a parameter of its `given` rather than of its `rows` method, following
// `urticose`'s `urlTeletype` and `chiaroscuro`'s `juxtapositionTeletype`: resolution then happens
// where the user summons the design, so the user's imports decide the appearance.
trait Gauging:
  def palette: GaugePalette
  def glyphs: Gaugeable.Glyphs
  def metric: Text is Measurable

  // Colour a fragment, or leave it alone. Threading every tint through here means a design never
  // writes an escape directly, and a palette that declines to colour a role costs nothing.
  def tint(color: Color in Srgb)(text: Teletype): Teletype = e"${Fg(color)}($text)"
  def wash(color: Color in Srgb)(text: Teletype): Teletype = e"${Bg(color)}($text)"

  // Whether this context permits `glyphs`, so a design can pick its best available rendering.
  // The repertoires nest: braille implies unicode, and emoji implies both.
  def permits(required: Gaugeable.Glyphs): Boolean = required match
    case Gaugeable.Glyphs.Ascii   => true
    case Gaugeable.Glyphs.Unicode => glyphs != Gaugeable.Glyphs.Ascii
    case Gaugeable.Glyphs.Emoji   => glyphs == Gaugeable.Glyphs.Emoji

    case Gaugeable.Glyphs.Braille =>
      glyphs == Gaugeable.Glyphs.Braille || glyphs == Gaugeable.Glyphs.Emoji

  // The display width of `text` in cells, under whichever metric is in scope.
  def cells(text: Text): Int = metric.width(text)
