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

import anticipation.*
import gossamer.*
import profanity.*
import spectacular.*
import vacuous.*

// A focusable element bound into an interactive layout. It owns mutable widget
// state, renders itself onto whichever `Canvas` the driver gives it (so the
// driver can move it to a new rectangle on re-layout), folds an event into its
// state, and reports the intrinsic size its current content needs — the hook the
// reactive layout uses to grow or shrink a panel.
trait Focus:
  def render(canvas: Canvas, focused: Boolean): Unit
  def handle(event: TerminalEvent): Unit
  def measure(width: Int): (Int, Int)

// A focusable wrapping a `LineEditor`. Its intrinsic height is the number of
// wrapped rows its current value occupies, so an editor that grows past one line
// pushes the rest of the layout down.
class EditorField(initial: LineEditor = LineEditor()) extends Focus:
  private var editor: LineEditor = initial

  def value: Text = editor.value

  def render(canvas: Canvas, focused: Boolean): Unit =
    given Canvas = canvas
    summon[Interaction[Text, LineEditor]].render(Unset, editor)

  def handle(event: TerminalEvent): Unit = editor = editor.apply(event)

  def measure(width: Int): (Int, Int) =
    val rows = LineEditor.cursorPosition(editor.value, editor.value.length, width.max(1))._1 + 1
    (0, rows)

// A focusable wrapping a `SelectMenu`. Its intrinsic height is one row per
// option.
class MenuField[item: Showable](initial: SelectMenu[item]) extends Focus:
  private var menu: SelectMenu[item] = initial

  def value: item = menu.current

  def render(canvas: Canvas, focused: Boolean): Unit =
    given Canvas = canvas
    summon[Interaction[item, SelectMenu[item]]].render(Unset, menu)

  def handle(event: TerminalEvent): Unit = menu = menu.apply(event)

  def measure(width: Int): (Int, Int) = (0, menu.options.length)
