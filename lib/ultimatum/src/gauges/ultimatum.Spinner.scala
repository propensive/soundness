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
import gossamer.*
import symbolism.*
import vacuous.*

object Spinner:
  // Build from a run of single-cell frames written as one string — `⠋⠙⠹⠸⠼⠴⠦⠧⠇⠏` — split by code
  // point, which is how nearly every design in the catalogue is declared.
  def each
    ( frames:     Text,
       interval:   Int             = 80,
       repertoire: Gaugeable.Glyphs = Gaugeable.Glyphs.Unicode,
       narrower:   Optional[Spinner] = Unset )
  :   Spinner =

    Spinner(codepoints(frames), interval, 1, repertoire, narrower)

  // Split by code point rather than by `Char`, so that an astral frame (an emoji clock face) is one
  // frame and not two halves of a surrogate pair. Frames that are themselves multi-codepoint
  // clusters have to be given explicitly, as a `Sequence`.
  private def codepoints(text: Text): Sequence[Text] =
    val builder = scala.collection.immutable.Vector.newBuilder[Text]
    val string = text.s
    var index = 0

    while index < string.length do
      val codepoint = string.codePointAt(index)
      val width = java.lang.Character.charCount(codepoint)
      builder += string.substring(index, index + width).nn.tt
      index += width

    Sequence.from(builder.result())

// A cyclic run of frames, shown one at a time. `columns` is how wide every frame is (they must
// agree, or the row would shear as it animates), `repertoire` is the least adventurous character
// set that can render it, and `narrower` is what to fall back to when this design will not fit or
// is not permitted — the chain that lets an emoji design degrade to a BMP one and a wide marquee to
// a single spinning cell.
case class Spinner
  ( frames:     Sequence[Text],
    interval:   Int               = 80,
    columns:    Int               = 1,
    repertoire: Gaugeable.Glyphs  = Gaugeable.Glyphs.Unicode,
    narrower:   Optional[Spinner] = Unset ):

  // The first design in this one's fallback chain that fits `width` and is permitted here.
  def fit(width: Int, gauging: Gauging): Optional[Spinner] =
    if columns <= width && gauging.permits(repertoire) then this
    else narrower.lay(Unset: Optional[Spinner])(_.fit(width, gauging))

  // The narrowest design in the chain, which is what the layout is told this gauge needs.
  def leastColumns(gauging: Gauging): Int =
    narrower.lay(columns)(_.leastColumns(gauging).min(columns))

  // A spinner says the work is alive, not how far along it is, so it draws the same whether or not
  // a fraction is known — and the same again when there is none, which is what makes it the design
  // to import for indeterminate work.
  def gaugeable(using gauging: Gauging): Fraction is Gaugeable = new Gaugeable:
    type Self = Fraction
    override def period: Optional[Int] = interval
    override def elastic: Boolean = false
    override def minWidth(status: Fraction): Int = leastColumns(gauging)
    override def columns(status: Fraction): Int = leastColumns(gauging)
    override def absentColumns: Int = leastColumns(gauging)
    override def absent(tick: Tick, width: Int): List[Teletype] = frame(tick, width)

    def rows(status: Fraction, tick: Tick, width: Int): List[Teletype] = frame(tick, width)

    private def frame(tick: Tick, width: Int): List[Teletype] =
      val chosen = fit(width, gauging)

      val frame = chosen.lay(Teletype(t" "*width.max(0))): spinner =>
        val count = spinner.frames.stdlib.length

        val index = tick.index.abs
        val glyph = if count == 0 then t" " else spinner.frames.stdlib(index%count)

        val padding = width - spinner.columns
        val body = gauging.tint(gauging.palette.fill)(Teletype(glyph))
        if padding > 0 then e"$body${t" "*padding}" else body

      List(frame)
