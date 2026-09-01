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

import scala.caps

import anticipation.*
import denominative.*
import denominative.dysasymptotics.linearSize
import escapade.*
import escritoire.*
import hieroglyph.*
import polysyllabic.*
import profanity.*
import rudiments.*
import vacuous.*

object TableFixture:
  // The supplier genuinely captures the caller's data source and escapes into the long-lived
  // fixture — the same growing capture set `Reading` reconciles, sound for the same reasons:
  // it is only invoked from measure/render while the hosting form is live. Hence the single,
  // localised `unsafeAssumePure`.
  def apply(content: => Tabulation[Teletype])
    ( using TableStyle, Text is Measurable, Hyphenation )
  :   TableFixture =

    new TableFixture(caps.unsafe.unsafeAssumePure { () => content })

// A live table pane: each measure lays the table out afresh at the offered width, so its
// columns renegotiate — wrapping, shrinking or collapsing — as the terminal resizes or the
// data changes, and rendering replays the lines the measure settled on. Call `refresh()`
// after changing the underlying data to wake the form and re-render.
class TableFixture(content: () -> Tabulation[Teletype])
  ( using style: TableStyle, metric: Text is Measurable, hyphenation: Hyphenation )
extends Fixture:

  // A no-op until the fixture is bound into a running form; see `bindWake`.
  @scala.caps.unsafe.untrackedCaptures
  private var wakeForm: () -> Unit = () => ()

  @scala.caps.unsafe.untrackedCaptures
  private var measuredWidth: Int = -1

  @scala.caps.unsafe.untrackedCaptures
  private var lines: List[Teletype] = Nil

  def refresh(): Unit = wakeForm()

  // As in `Panes.bindWake`: the callback captures the running form's event loop and escapes
  // into this longer-lived fixture, re-bound on every run — hence the localised assumption.
  override private[ultimatum] def bindWake(wake: () => Unit): Unit =
    wakeForm = caps.unsafe.unsafeAssumePure(wake)

  // A fixture cannot raise, so an unsatisfiable width renders overflowing rather than failing.
  private def layout(width: Int): List[Teletype] =
    given Attenuation = escritoire.columnAttenuation.ignoreAttenuation
    content().grid(width).render.to[List]

  // Counting the rendered rows is O(n) on a `List` (hence `linearSize`), and is paid once here.
  def measure(width: Int): (Int, Int) =
    lines = layout(width)
    measuredWidth = width
    val widest = lines.map(_.plain.metrics).most.or(0)
    (widest, lines.size)

  def render(canvas: Board^, focused: Boolean): Unit =
    val rows = if canvas.width == measuredWidth then lines else layout(canvas.width)

    rows.each: line =>
      if ordinal.n0 < canvas.height then
        canvas.move(Prim, ordinal.n0.z)
        canvas.put(line)
