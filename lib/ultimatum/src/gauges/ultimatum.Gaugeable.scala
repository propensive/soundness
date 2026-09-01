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

import denominative.*
import escapade.*
import gossamer.*
import prepositional.*
import profanity.*
import symbolism.*
import vacuous.*

object Gaugeable:
  // One generic lifting of any design to the same status made optional, so that optionality is
  // handled here rather than by a parallel design for every family.
  // The `Mandatable` constraint identifies the mandatory type `inner`, so this applies only to
  // genuine optionals and never competes with `inner`'s own instance — which is what keeps
  // `Captioned(Fraction(0.5), …)` unambiguous when both are in scope.
  given optional: [inner <: value, value >: Unset.type: Mandatable to inner]
  =>  ( design: inner is Gaugeable )
  =>  value is Gaugeable =

    new Gaugeable:
      type Self = value

      // The lifted design may have to sweep, which the definite one never does, so it animates
      // whether or not this particular value happens to be present.
      override def period: Optional[Int] = design.period.or(80)
      override def elastic: Boolean = design.elastic

      override def minWidth(status: value): Int =
        status.lay(1): value => design.minWidth(value.asInstanceOf[inner])

      override def columns(status: value): Int =
        status.lay(design.absentColumns): value => design.columns(value.asInstanceOf[inner])

      override def height(status: value, width: Int): Int =
        status.lay(1): value => design.height(value.asInstanceOf[inner], width)

      def rows(status: value, tick: Tick, width: Int): List[Teletype] =
        status.lay(design.absent(tick, width)): value =>
          design.rows(value.asInstanceOf[inner], tick, width)

  // How adventurous a design may be with its character repertoire. Every design in the catalogue
  // carries an ASCII rendering as well as its preferred one, so importing `asciiGlyphs` degrades
  // the whole catalogue at once — the single import a caller writing to a dumb terminal, a log
  // capture or a CI transcript needs.
  // `Emoji` is separate from `Unicode` because emoji are the only glyphs that occupy two cells,
  // and a terminal that renders them at one cell will shear every row that uses them.
  object Glyphs:
    // The no-import default: the BMP box-drawing and block glyphs, which every modern terminal
    // has, and no emoji.
    given default: Glyphs = Unicode

  enum Glyphs:
    case Ascii, Unicode, Braille, Emoji

  // Binds a pure design to a live `Reading` and to the form's repaint machinery — the adapter
  // through which a gauge becomes something the layout can host.
  // The design is summoned where the pane is built, so the palette, glyph repertoire and metric it
  // captured are the ones the *user's* imports chose, not the driver's.
  class Fixture[status: Gaugeable as design](reading: Reading[status]) extends ultimatum.Fixture:
    // Monotonic, so that a spinner does not jump when the system clock is stepped; and per-gauge,
    // so that two spinners started at different moments stay independently phased.
    @scala.caps.unsafe.untrackedCaptures
    private val started: Long = System.nanoTime

    private def tick: Tick =
      Tick.at((System.nanoTime - started)/1000000L, design.period.or(1000))

    override def period: Optional[Int] = design.period

    private[ultimatum] override def bindWake(wake: () => Unit): Unit = reading.bindWake(wake)

    // The width reported is the design's preferred width, which the solver treats as a minimum;
    // an elastic design will be given more if the layout has it, and `rows` is told what it got.
    def measure(width: Int): (Int, Int) =
      val status = reading()
      (design.columns(status), design.height(status, width.max(1)))

    def render(canvas: Board^, focused: Boolean): Unit =
      val status = reading()
      val width = canvas.width

      if width >= design.minWidth(status) then
        var row = 0
        // `.stdlib.iterator`: the paint loop pulls rows one at a time from a stdlib `Iterator`.
        val lines = design.rows(status, tick, width).stdlib.iterator

        while lines.hasNext && row < canvas.height do
          canvas.move(Prim, row.z)
          canvas.put(lines.next())
          row += 1

      canvas.flush()

  // A design occupying exactly one row, which is nearly all of them.
  abstract class Row[status] extends Gaugeable:
    type Self = status
    def row(status: status, tick: Tick, width: Int): Teletype

    def rows(status: status, tick: Tick, width: Int): List[Teletype] =
      List(row(status, tick, width))

// A design for rendering a value of type `Self` as a terminal gauge: a one-cell spinner, a
// full-width progress bar, a multi-row checklist. The *status type is the key*, so importing a
// different choice package substitutes a different design for the same application code, with no
// other edit.
// Rendering is pure — a status, a clock reading and a width map to styled rows — so a design
// captures nothing, sits safely inside a pane tree (which must stay pure under capture checking),
// and is testable by calling it. Everything effectful lives in the two adapters,
// `Gaugeable.Fixture` and `Inlay`.
trait Gaugeable extends Typeclass:
  // How long, in milliseconds, until the design wants redrawing irrespective of its status;
  // `Unset` for a design that changes only when its status does. A spinner returns its frame
  // interval. `Form` takes the minimum over the live gauges and arms a single timer.
  def period: Optional[Int] = Unset

  // The narrowest width at which this design renders something meaningful. Given less, the driver
  // draws nothing rather than something broken.
  def minWidth(status: Self): Int = 1

  // The width the design would like. A bar is happy with whatever it is given, so this is its
  // preferred size; a spinner is exactly this wide and no wider.
  def columns(status: Self): Int = minWidth(status)

  // The rows the design occupies at `width` cells: one for a spinner or a bar, one per step for a
  // checklist, two for a dial. This is what lets a gauge push the rest of the layout around.
  def height(status: Self, width: Int): Int = 1

  // Whether the design should take all the width it is offered (a bar) or be held at `columns` (a
  // spinner, a status glyph, a counter).
  def elastic: Boolean = true

  // Render at exactly `width` cells. Every row must measure exactly `width` under the ambient
  // metric, and there must be exactly `height(status, width)` of them.
  def rows(status: Self, tick: Tick, width: Int): List[Teletype]

  // How to draw when there is no status at all — the `Unset` of an `Optional`. "Not measured" is a
  // different claim from "no progress", so a bar sweeps here rather than sitting empty, and a
  // spinner draws its frames. The default is blank, which is right for a design with nothing to
  // say without a value.
  def absent(tick: Tick, width: Int): List[Teletype] = List(Teletype(t" "*width.max(0)))

  // The width to claim when there is no status to measure.
  def absentColumns: Int = 1
