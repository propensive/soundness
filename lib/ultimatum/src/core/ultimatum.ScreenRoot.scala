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
import profanity.*
import turbulence.*

object ScreenRoot:
  // Build a fullscreen root covering the whole terminal, reading its size live (so a
  // resize is reflected the next time the layout is solved) and writing through its
  // `Stdio`. The size thunks retain the terminal, so the root honestly captures it.
  def apply(terminal: Terminal): ScreenRoot^{terminal} =
    // Both thunks only read the same terminal's dimensions; no aliased writer.
    scala.caps.unsafe.unsafeAssumeSeparate:
      new ScreenRoot(() => terminal.knownColumns, () => terminal.knownRows)(using terminal.stdio)

  def apply(width: Int, height: Int)(using Stdio): ScreenRoot =
    new ScreenRoot(() => width, () => height)

// The root `Canvas` for FULLSCREEN mode: panels composite into its character grid
// (inherited from `GridSurface`), and `flush` presents by diffing the grid against
// the snapshot of what the last present drew, overprinting only the damaged runs —
// an unchanged cell is a no-op and an identical frame emits nothing at all. Every
// present is ONE write, so partial frames never flash and the SIGWINCH size-probe
// reply cannot interleave mid-frame. The first present, and any present after
// `invalidate` (a resize, when the terminal has reflowed whatever was on screen),
// redraws every row. It replaces the unbuffered `TerminalCanvas` as the fullscreen
// root, whose immediate per-`put` writes re-emitted every cell a repaint touched.
class ScreenRoot(widthFn: () => Int, heightFn: () => Int)(using stdio: Stdio)
extends GridSurface(widthFn(), heightFn()):

  // The live terminal size, for the layout solver; the grid is re-fitted to it by
  // `reframe` before each solve, so the two only ever disagree mid-resize.
  override def width: Int = widthFn()
  override def height: Int = heightFn()

  // Re-fit the grid to the live terminal size. A real change blanks the grid and
  // invalidates the snapshot: the terminal has reflowed whatever was on screen, so
  // the next present must redraw everything.
  def reframe(): Unit =
    if widthFn() != gridWidth || heightFn() != gridHeight then
      reshape(widthFn(), heightFn())
      invalidate()

  // Cursor visibility is deferred like the caret: recorded now, applied by `flush`,
  // so a focused editor shows it and a focused menu keeps it hidden.
  def cursor(visible: Boolean): Unit = caretVisible = visible

  // Record the caret's target cell; `flush` positions the hardware cursor there.
  def showCaret(column: Ordinal, row2: Ordinal): Unit =
    caretColumn = column.n0
    caretRow = row2.n0

  def flush(): Unit =
    val columns = gridWidth
    val h       = gridHeight

    if !invalidated && snapshotValid(1, columns, h) then presentDiff(1, columns, h)
    else
      invalidated = false

      val frame = StringBuilder()
      def emit(text: Text): Unit = frame.append(text.s)
      val termcap = stdio.termcap

      emit(csi.dectcem(false))

      // Draw every row at its absolute position (`cup` per row, never a newline, so
      // the bottom row can never scroll the screen), cleared and reset like a docked
      // inline row so no colour bleeds between rows.
      var r = 0

      while r < h do
        emit(csi.cup(r + 1, 1))
        emit(csi.el(2))
        val rendered = rowContent(r, columns).render(termcap)
        emit(rendered)
        if rendered.contains(t"\e") then emit(csi.sgr(0))
        r += 1

      if caretVisible then
        emit(csi.cup((1 + caretRow.min(h - 1)).max(1), caretColumn.min(columns - 1).max(0) + 1))
        emit(csi.dectcem(true))

      // What was just drawn IS the screen now: record it (and the caret) so the next
      // present can diff against it.
      recordSnapshot(1, columns)
      recordCaret(1, columns, h)

      Out.print(frame.toString.tt)

  // On exit, re-show the hardware cursor; the alternate-screen wrapper around the
  // session restores the previous screen contents.
  def finish(): Unit = Out.print(csi.dectcem(true))
