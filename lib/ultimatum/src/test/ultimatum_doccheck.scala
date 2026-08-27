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
// Deliberately NOT under `package ultimatum`: an example compiled there would not be testing what a
// user of the library sees, which is exactly how the status types' abstraction leak went unnoticed.
package gaugedocs

import soundness.*

import bars.arrowheadBar
import gaugeGlyphs.asciiGlyphs
import palettes.solarizedDarkGaugePalette
import processions.checklistProcession
import textMetrics.uniformMetric

// Every code example in `doc/modules/gauges.md`, compiled. Prose drifts from an API silently, and a
// documented example that does not compile is worse than no example at all.
object Examples:
  def defaults(using Stdio): Unit =
    Out.println(gaugeLine(Fraction(0.42), 40))
    Out.println(gaugeLine(Reckoning(17, 120), 7))
    Out.println(e"${gaugeLine(Standing.Succeeded, 1)} built")

  def inALayout(using Terminal, Monitor, Probate): Unit =
    val progress = Reading(Fraction(0.0))
    progress() = Fraction.of(3, 10)
    conduct(Occupancy.Inline)(stack(gauge(progress)))

  def standalone(using Stdio, Monitor, Probate): Unit =
    whilst(Reading(Fraction.indeterminate)):
      ()

  // A spinner and a bar are two designs for one status, so only one can be imported at a time.
  // Where a layout needs both, the other is passed explicitly.
  def both(using Stdio): Unit =
    Out.println(gaugeLine(Fraction(0.5), 20))
    Out.println(gaugeLine(Fraction(0.0), 1)(using spinners.brailleDotsSpinner))

  def oneFrame(done: Int, total: Int)(using Stdio): Unit =
    Out.print(e"\r${gaugeLine(Fraction.of(done, total), 40)} $done/$total${csi.el()}")

  def captioned: Pane = gauge(Reading(Captioned(Fraction.indeterminate, t"resolving dependencies")))

  def procession: Pane =
    val steps =
      (Sequence
        ( Step(t"resolve", Standing.Succeeded),
          Step(t"compile", Standing.Running),
          Step(t"publish", Standing.Pending) ): Sequence[Step])

    gauge(Reading(steps))
