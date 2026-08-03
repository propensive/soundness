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
import gossamer.*
import profanity.*
import rudiments.*
import spectacular.*
import vacuous.*

// A focusable element bound into an interactive layout. It owns mutable widget
// state, renders itself onto whichever `Board` the driver gives it (so the
// driver can move it to a new rectangle on re-layout), folds an event into its
// state, and reports the intrinsic size its current content needs — the hook the
// reactive layout uses to grow or shrink a panel.
trait Focus:
  def render(canvas: Board^, focused: Boolean): Unit
  def handle(event: TerminalEvent): Unit
  def measure(width: Int): (Int, Int)

// A focusable wrapping a `LineEditor`. Its intrinsic height is the number of
// wrapped rows its current value occupies, so an editor that grows past one line
// pushes the rest of the layout down.
class EditorField(initial: LineEditor = LineEditor()) extends Focus:
  @scala.caps.unsafe.untrackedCaptures
  private var editor: LineEditor = initial

  def value: Text = editor.value

  // The shared `Interaction` draws the editor and positions the caret; the
  // hardware cursor is then shown only when this field has focus, so the caret
  // appears in the focused editor and is hidden elsewhere.
  def render(canvas: Board^, focused: Boolean): Unit =
    given canvas0: (Board^{canvas}) = canvas
    summon[Interaction[Text, LineEditor]].render(Unset, editor)
    canvas.cursor(focused)

  def handle(event: TerminalEvent): Unit = editor = editor.apply(event)

  def measure(width: Int): (Int, Int) =
    val rows = LineEditor.cursorPosition(editor.value, editor.value.length, width.max(1))._1 + 1
    (0, rows)

// A focusable wrapping a `SelectMenu`. Its intrinsic height is one row per
// option.
class MenuField[item: Showable](initial: SelectMenu[item]) extends Focus:
  @scala.caps.unsafe.untrackedCaptures
  private var menu: SelectMenu[item] = initial

  def value: item = menu.current

  // Renders the options with a focus-aware marker on the current selection: a
  // pointer (`>`) when this field has focus, a dot (`·`) when it does not, so the
  // selection is always visible but the focused menu is distinguished. The
  // hardware cursor is kept hidden — a menu has no text caret.
  def render(canvas: Board^, focused: Boolean): Unit =
    given canvas0: (Board^{canvas}) = canvas
    val cols = canvas.width.max(1)
    canvas.move(Prim, Prim)
    canvas.clear()
    var row = 0

    menu.options.each: option =>
      canvas.move(Prim, row.z)

      val marker =
        if option != menu.current then t"   " else if focused then t" > " else t" · "

      val line = t"$marker${option.show}"
      canvas.put(line)
      row += (line.length - 1)/cols + 1

    canvas.flush()
    canvas.cursor(false)

  def handle(event: TerminalEvent): Unit = menu = menu.apply(event)

  def measure(width: Int): (Int, Int) = (0, menu.options.stdlib.length)
