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
import denominative.*
import escapade.*
import gossamer.*
import profanity.*
import turbulence.*

object InlineRoot:
  // The root for inline mode over a real terminal: width and the height clamp are
  // read live, so a resize is reflected on the next frame.
  def apply(terminal: Terminal): InlineRoot =
    new InlineRoot(() => terminal.knownColumns, () => terminal.knownRows)(using terminal.stdio)

  def apply(width: Int, height: Int)(using Stdio): InlineRoot =
    new InlineRoot(() => width, () => height)

// The root `Canvas` for INLINE mode: panels composite into its character grid
// (inherited from `GridSurface`), and `flush` presents the whole grid at the
// terminal's current cursor using relative motion plus CR/LF. Because a `\n` on
// the bottom line scrolls a fresh row in (whereas a relative `cud` clamps), the
// block grows downward without needing the alternate screen buffer or any
// pre-reserved space; on shrink the freed rows are cleared. The cursor is tracked
// relative to its own resting row within the block, never an absolute screen row,
// so the present stays correct after the terminal scrolls. `widthFn`/`heightFn`
// supply the live terminal columns and the height clamp (an oversize block
// degrades to a bottom-anchored window).
class InlineRoot(widthFn: () => Int, heightFn: () => Int)(using Stdio)
extends GridSurface(widthFn(), 0):
  private var presentedRows: Int = 0
  private var cursorRow: Int = 0
  private var caretColumn: Int = 0
  private var caretRow: Int = 0
  private var caretVisible: Boolean = true

  override def width: Int = widthFn()

  // Resize the grid to the measured block height, clamped to the live terminal
  // height; called by the driver before compositing each frame.
  def reframe(width: Int, height: Int): Unit = reshape(width, height.min(heightFn()))

  // Cursor visibility is deferred like the caret: recorded now, applied by `flush`
  // (which hides the cursor while it redraws), so a focused editor shows it and a
  // focused menu keeps it hidden.
  def cursor(visible: Boolean): Unit = caretVisible = visible

  // Inline carets are deferred: record the block-local target and let `flush`
  // position it relative to the block, since mid-composite the cursor is wherever
  // the last panel left it.
  override def showCaret(column: Ordinal, row2: Ordinal): Unit =
    caretColumn = column.n0
    caretRow = row2.n0

  def flush(): Unit =
    Out.print(csi.dectcem(false))
    if cursorRow > 0 then Out.print(csi.cuu(cursorRow))
    Out.print(t"\r")

    var r = 0

    while r < height do
      Out.print(csi.el(2))
      Out.print(rowText(r))
      Out.print(t"\r")
      if r < height - 1 then Out.print(t"\n")
      r += 1

    if presentedRows > height then
      var k = height

      while k < presentedRows do
        Out.print(t"\n")
        Out.print(csi.el(2))
        k += 1

      Out.print(csi.cuu(presentedRows - height))

    val up = (height - 1) - caretRow
    if up > 0 then Out.print(csi.cuu(up))
    Out.print(csi.cha(caretColumn + 1))

    presentedRows = height
    cursorRow = caretRow
    Out.print(csi.dectcem(caretVisible))

  // On exit, drop the cursor onto a fresh line below the block and re-show it, so
  // subsequent output continues after the rendered block (like a submitted prompt).
  def finish(): Unit =
    val down = (height - 1) - cursorRow
    if down > 0 then Out.print(csi.cud(down))
    Out.print(t"\r\n")
    Out.print(csi.dectcem(true))
