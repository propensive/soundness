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
┃    Soundness, version 0.54.0.                                                                    ┃
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

import backstops.silent
import codicils.cancel
import executives.completions
import interpreters.posix
import strategies.throwUnsafely
import supervisors.global
import threading.platform

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

// rank(title, file(sidebar, rank(heading, editor, activity)), status):
//
//   ┌──────────────────────── title ────────────────────────┐
//   │ sidebar (menu) │ heading                               │
//   │                │ editor                                │
//   │                │ activity                              │
//   └─────────────────────── status ────────────────────────┘
private def demoLayout: Pane =
  val sidebar = menu(List(t"Overview", t"Compose", t"Activity", t"Settings"), t"Overview",
      minWidth = 22, maxWidth = 22)

  val activity = panel(minHeight = 6):
    Out.println(t"Recent activity")
    Out.println(t"")
    Out.println(t"  • Demo started")
    Out.println(t"  • Four panes tiled")
    Out.println(t"  • Tab moves focus, Esc quits")

  val heading = panel(minHeight = 1, maxHeight = 1)(Out.print(t"  Compose"))
  val title = panel(minHeight = 1, maxHeight = 1)(Out.print(t"  ULTIMATUM · fullscreen demo"))
  val status = panel(minHeight = 1, maxHeight = 1)(Out.print(t"  [Tab] focus    [Esc] quit"))

  // A multiline compose box: Enter inserts a newline (it never submits, so the
  // arrow keys can move the cursor up and down between lines).
  val compose = editor(LineEditor(mode = LineEditor.Mode.Multiline(_ => false)))

  rank(title, file(sidebar, rank(heading, compose, activity)), status)
