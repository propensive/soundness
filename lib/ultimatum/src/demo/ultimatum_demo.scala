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
┃    Soundness, version 0.63.0.                                                                    ┃
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
import probates.cancelProbate
import executives.completions
import interpreters.posixInterpreter
import strategies.throwUnsafely
import supervisors.globalSupervisor
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
    interactive: terminal ?=>
      form(Mode.Inline)(demoLayout)
      Exit.Ok

// rank(title, file(sidebar, rank(heading, compose, activity)), status), with the
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
private def demoLayout: Pane =
  // A rounded border around the menu; the menu itself is 20 wide, so the bordered
  // sidebar is 22.
  val sidebar = border(BorderStyle.rounded):
    menu(List(t"Overview", t"Compose", t"Activity", t"Settings"), t"Overview",
        minWidth = 20, maxWidth = 20)

  val activity = border():
    panel(minHeight = 6):
      Out.println(t"Recent activity")
      Out.println(t"")
      Out.println(t"  • Demo started")
      Out.println(t"  • Four panes tiled")
      Out.println(t"  • Tab moves focus, Esc quits")

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

  rank(title, file(sidebar, rank(heading, compose, activity)), status)
