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
import denominative.*
import escapade.*
import gossamer.*
import rudiments.*
import spectacular.*
import symbolism.*
import vacuous.*

object Sparkline:
  // Ascending block heights: the standard eight-level ramp.
  private val blocks: Text = t"▁▂▃▄▅▆▇█"
  private val dots: Text = t"⣀⠤⠒⠉"
  private val ascii: Text = t"_.-^"

  // Reduce `samples` to exactly `width` values by taking the maximum of each group. A sparkline
  // narrower than its series must drop information; taking the maximum keeps the peaks, whereas
  // truncating would silently show only the oldest samples and misrepresent the shape.
  def decimate(samples: Sequence[Fraction], width: Int): Sequence[Fraction] =
    val count = samples.stdlib.length

    if count <= width || width <= 0 then samples else
      Sequence.from:
        (0 until width).map: cell =>
          val from = cell*count/width
          val to = (((cell + 1)*count/width).max(from + 1)).min(count)
          Fraction(samples.stdlib.slice(from, to).map(_.value).max)

// How a run of samples is drawn. `Blocks` gives eight levels in one row; `Tall` stacks two rows for
// sixteen; `Dots` and `Ascii` trade resolution for a narrower character repertoire.
enum Sparkline:
  case Blocks, Tall, Dots, Ascii

  def rowCount: Int = this match
    case Tall => 2
    case _    => 1

  // Keyed on a plain `Sequence[Double]`, and scaled to the samples it is given.
  def gaugeable(using gauging: Gauging): Sequence[Double] is Gaugeable = scaled(Unset, Unset)

  // The same, with the scale fixed. A fixed scale is a presentation decision, not part of the data:
  // a sparkline that rescales itself every frame makes a steady signal look erratic, and only the
  // caller knows which reading is wanted.
  def scaled(floor: Optional[Double], ceiling: Optional[Double])(using gauging: Gauging)
  :   Sequence[Double] is Gaugeable =

    new Gaugeable:
      type Self = Sequence[Double]
      override def minWidth(status: Sequence[Double]): Int = 1
      override def columns(status: Sequence[Double]): Int = status.stdlib.length.max(1)
      override def height(status: Sequence[Double], width: Int): Int = rowCount

      def rows(status: Sequence[Double], tick: Tick, width: Int): List[Teletype] =
        Sparkline.this.draw(status, floor, ceiling, width, gauging)

  def draw
    ( samples: Sequence[Double],
      floor:   Optional[Double],
      ceiling: Optional[Double],
      width:   Int,
      gauging: Gauging )
  :   List[Teletype] =

    val lower = floor.or(samples.stdlib.minOption.getOrElse(0.0))
    val upper = ceiling.or(samples.stdlib.maxOption.getOrElse(1.0))
    val span = upper - lower

    val normalized = Sequence.from:
      samples.stdlib.map: sample =>
        if span <= 0 then Fraction(0.0) else Fraction((sample - lower)/span)

    val values = Sparkline.decimate(normalized, width)
    val ascii = !gauging.permits(Gaugeable.Glyphs.Unicode)

    // A design whose glyphs are unavailable falls back to the ASCII ramp rather than to nothing.
    val ramp = this match
      case Ascii             => Sparkline.ascii
      case _ if ascii        => Sparkline.ascii
      case Dots              => Sparkline.dots
      case Blocks | Tall     => Sparkline.blocks

    def cell(fraction: Fraction, ramp: Text): Teletype =
      val index = (fraction.value*ramp.length).toInt.min(ramp.length - 1).max(0)
      val glyph = ramp.at(index.z).let(_.show).or(t" ")
      gauging.tint(gauging.palette.lengthwise(fraction.value))(Teletype(glyph))

    def row(pick: Fraction -> Fraction): Teletype =
      val drawn = values.stdlib.map: value => cell(pick(value), ramp)
      val body = if drawn.isEmpty then e"" else drawn.reduceLeft: (l, r) => e"$l$r"
      val used = gauging.cells(body.plain)
      if used >= width then body else e"$body${t" "*(width - used)}"

    if this != Tall || ascii then List(row { value => value }) else
      // Two rows give sixteen levels: the upper row shows the top half of the range and the lower
      // row the bottom, so a tall sparkline resolves detail a single row would flatten.
      val upper = row: value => Fraction((value.value*2 - 1).max(0.0))
      val lower = row: value => Fraction((value.value*2).min(1.0))
      List(upper, lower)
