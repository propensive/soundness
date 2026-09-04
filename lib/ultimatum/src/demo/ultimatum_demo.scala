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

import soundness.*

import backstops.silentBackstop
import bars.smoothBar
import counters.paddedCounter
import executives.completionsExecutive
import gaugeGlyphs.unicodeGlyphs
import interpreters.posixInterpreter
import palettes.emberGaugePalette
import probates.cancelProbate
import processions.checklistProcession
import sparklines.blockSparkline
import strategies.throwUnsafely
import textMetrics.eastAsianScriptsMetric
import threading.platformThreading

// A medium-complexity fullscreen layout demonstrating the framework: a title bar
// and a status bar each pinned to a single row; a fixed-width sidebar menu; and a
// main column with a section heading, a line editor, and an activity panel. TAB
// moves focus between the menu and the editor; Escape quits.
//
// Run with `mill ultimatum.demo.run` from a real terminal.
@main
def demo(): Unit = cli:
  execute:
    supervise:
      interactive: terminal ?=>
        conduct(Occupancy.Inline)(demoLayout)
        Exit.Ok

// stack(title, strip(sidebar, stack(heading, compose, activity)), status), with the
// sidebar, compose box and activity panel each wrapped in a `border` and the
// heading underlined by a bottom-only border:
//
//   ┌──────────────────────── title ────────────────────────┐
//   │ ╭ sidebar ╮ │ heading                                  │
//   │ │  (menu) │ │ ─────────                                │
//   │ ╰─────────╯ │ ┏━ compose ━┓                            │
//   │             │ ┗━━━━━━━━━━━┛                            │
//   │             │ ┌─ activity ┐                            │
//   │             │ └───────────┘                            │
//   └─────────────────────── status ────────────────────────┘
// A pipeline part-way through: one step done, one running (which is what the checklist animates),
// and two still to come.
private def steps: Sequence[Step] =
  Sequence
    ( Step(t"resolve", Standing.Succeeded),
      Step(t"compile", Standing.Running),
      Step(t"test", Standing.Pending),
      Step(t"publish", Standing.Pending) )

private def demoLayout: Pane =
  // A rounded border around the menu; the menu itself is 20 wide, so the bordered
  // sidebar is 22.
  val sidebar = border(BorderStyle.rounded):
    menu(List(t"Overview", t"Compose", t"Activity", t"Settings"), t"Overview",
        minWidth = 20, maxWidth = 20)

  // A column of gauges, each keyed on a different status type and each picking up its design from
  // the imports at the top of this file. The spinner and the checklist animate; the bar, counter
  // and sparkline change only when their `Reading` does.
  // A spinner and a bar are two designs for one status, so only one can be imported at a time; a
  // local `given` selects the spinner for this gauge alone, leaving the imported bar for the rest.
  val resolving =
    given spinner: (Fraction is Gaugeable) = spinners.brailleDotsSpinner

    gauge(Reading(Captioned(Fraction.indeterminate, t"resolving")), minHeight = 1, maxHeight = 1)

  val activity = border():
    stack
      ( panel(minHeight = 1, maxHeight = 1)(Out.print(t"  Activity")),
        resolving,
        gauge(Reading(Captioned(Fraction(0.62), t"compiling")), minHeight = 1, maxHeight = 1),
        gauge(Reading(Reckoning(17, 120)), minHeight = 1, maxHeight = 1),
        gauge(Reading(Sequence(2.0, 5.0, 3.0, 8.0, 6.0, 9.0, 4.0)), minHeight = 1,
            maxHeight = 1),
        gauge(Reading(steps)) )

  // A bottom-only border draws a single rule under the heading, a separator with
  // no corners or sides.
  val heading = border(top = false, left = false, right = false):
    panel(minHeight = 1, maxHeight = 1)(Out.print(t"  Compose"))

  val title = panel(minHeight = 1, maxHeight = 1)(Out.print(t"  ULTIMATUM · fullscreen demo"))
  val status = panel(minHeight = 1, maxHeight = 1)(Out.print(t"  [Tab] focus    [Esc] quit"))

  // A multiline compose box: Enter inserts a newline (it never submits, so the
  // arrow keys can move the cursor up and down between lines).
  val compose = border(BorderStyle.heavy):
    editor(LineEditor(mode = LineEditor.Mode.Multiline(_ => false)))

  stack(title, strip(sidebar, stack(heading, compose, activity)), status)
