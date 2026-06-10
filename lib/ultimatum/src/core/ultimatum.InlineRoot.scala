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
import symbolism.*
import turbulence.*

object InlineRoot:
  // The root for inline mode over a real terminal: width and the height clamp are
  // read live, so a resize is reflected on the next frame.
  def apply(terminal: Terminal): InlineRoot =
    new InlineRoot(() => terminal.knownColumns, () => terminal.knownRows)(using terminal.stdio)

  def apply(width: Int, height: Int)(using Stdio): InlineRoot =
    new InlineRoot(() => width, () => height)

// The root `Canvas` for INLINE mode: panels composite into its character grid
// (inherited from `GridSurface`), and `flush` presents the grid in the BOTTOM
// `height` rows of the terminal, addressed by ABSOLUTE screen row (`cup`). Pinning
// the block to the bottom and drawing each row at an absolute position means a redraw
// always lands in the same place and never drifts — the property a relative,
// cursor-tracking present loses the moment a resize reflows the lines already on
// screen. The block grows by scrolling content up into scrollback to free rows at
// the bottom, and shrinks by clearing the rows it vacates above. A resize clears the
// block's rows (sized for the terminal re-wrapping our wider lines) before redrawing,
// keeping the content above. `widthFn`/`heightFn` supply the live terminal columns
// and rows, re-read every frame, so the dock follows a resize; an oversize block is
// clamped to the terminal height. (Because it is bottom-docked, a resize that makes
// the terminal taller leaves a gap above the block — the cost of robust inline
// positioning when the terminal can't report the cursor position.)
class InlineRoot(widthFn: () => Int, heightFn: () => Int)(using Stdio)
extends GridSurface(widthFn(), 0):
  private var presentedRows: Int = 0
  private var presentedColumns: Int = 0
  private var invalidated: Boolean = false
  private var caretColumn: Int = 0
  private var caretRow: Int = 0
  private var caretVisible: Boolean = true

  override def width: Int = widthFn()

  // Resize the grid to the measured block height, clamped to the live terminal
  // height; called by the driver before compositing each frame.
  def reframe(width: Int, height: Int): Unit = reshape(width, height.min(heightFn()))

  // Cursor visibility is deferred like the caret: recorded now, applied by `flush`,
  // so a focused editor shows it and a focused menu keeps it hidden.
  def cursor(visible: Boolean): Unit = caretVisible = visible

  // Mark the next present as a resize repaint: it clears the old block's rows before
  // redrawing, since a resize can move it. The driver calls this on every
  // `WindowSize` event (a width-only resize counts too).
  def invalidate(): Unit = invalidated = true

  // Record the caret's block-local target; `flush` positions the hardware cursor at
  // the corresponding absolute screen cell.
  override def showCaret(column: Ordinal, row2: Ordinal): Unit =
    caretColumn = column.n0
    caretRow = row2.n0

  def flush(): Unit =
    val rows    = heightFn()
    val columns = gridWidth.min(widthFn())
    val h       = height
    val resized = invalidated
    invalidated = false

    // The whole frame is built up and emitted as ONE write, so the SIGWINCH handler's
    // size-probe sequence (which jumps the cursor to the corner and back) cannot
    // interleave between our escapes and corrupt the redraw.
    val frame = StringBuilder()
    def emit(text: Text): Unit = frame.append(text.s)

    emit(csi.dectcem(false))

    if resized then
      // The terminal was resized. The block was docked to the bottom, so clear only
      // its rows there (not the whole screen), preserving the content above. When the
      // width shrank, the terminal re-wrapped our previous, wider lines, so the old
      // block spans more physical rows; size the cleared region for that re-wrap.
      val narrowed = presentedColumns > columns && columns > 0
      val wrap = if narrowed then (presentedColumns + columns - 1)/columns else 1
      val cleared = (presentedRows*wrap).max(h)
      emit(csi.cup((rows - cleared + 1).max(1), 1))
      emit(csi.ed(0))
    else if h > presentedRows then
      // Grow in place: scroll the screen up so the extra rows are free at the bottom,
      // pushing the content above into scrollback rather than overwriting it. `cud` to
      // the bottom first, since only a `\n` on the bottom line scrolls.
      emit(csi.cud(9999))
      emit(t"\r")
      emit(t"\n"*(h - presentedRows))

    // Draw the block into the bottom `h` rows from an ABSOLUTE top, clipping each row
    // to the live width so it can never wrap (a wrapped row would scroll the screen
    // and break the addressing).
    emit(csi.cup((rows - h + 1).max(1), 1))

    var r = 0

    while r < h do
      emit(csi.el(2))
      emit(rowText(r).keep(columns))
      emit(t"\r")
      if r < h - 1 then emit(t"\n")
      r += 1

    // Shrink in place: clear the rows a taller previous block used above the new top
    // (a resize already cleared the block's rows).
    if !resized then
      var j = 0

      while j < presentedRows - h do
        emit(csi.cup((rows - presentedRows + 1 + j).max(1), 1))
        emit(csi.el(2))
        j += 1

    presentedRows = h
    presentedColumns = columns

    // The caret is the hardware cursor, placed at an absolute cell and shown only when
    // visible; otherwise the cursor is left hidden.
    if caretVisible then
      val caretAbsRow = (rows - h + 1 + caretRow.min(h - 1)).max(1)
      emit(csi.cup(caretAbsRow, caretColumn.min(columns - 1).max(0) + 1))
      emit(csi.dectcem(true))

    Out.print(frame.toString.tt)

  // On exit, drop the cursor onto a fresh line below the block and re-show it, so
  // subsequent output continues after the rendered block (like a submitted prompt).
  def finish(): Unit =
    Out.print(csi.cup(heightFn(), 1))
    Out.print(t"\r\n")
    Out.print(csi.dectcem(true))
