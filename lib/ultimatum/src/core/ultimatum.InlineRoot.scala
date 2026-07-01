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
  // read live, so a resize is reflected on the next frame. The rendering policy is
  // read from context (`InlineAnchoring`/`InlineGrowth`/`InlineShrink`), so it is
  // fixed at the caller's site rather than inside the library.
  def apply(terminal: Terminal)
    ( using anchoring: InlineAnchoring, growth: InlineGrowth, shrink: InlineShrink )
  :   InlineRoot =

    new InlineRoot(() => terminal.knownColumns, () => terminal.knownRows)
      ( using terminal.stdio, anchoring, growth, shrink )

  def apply(width: Int, height: Int)
    ( using Stdio, InlineAnchoring, InlineGrowth, InlineShrink )
  :   InlineRoot =

    new InlineRoot(() => width, () => height)

// The root `Canvas` for INLINE mode: panels composite into its character grid
// (inherited from `GridSurface`), and `flush` presents the grid with ABSOLUTE row
// addressing (`cup`) so a redraw always lands in the same place and never drifts —
// the property a relative, cursor-tracking present loses the moment a resize reflows
// the lines already on screen. Anchoring, growth and shrink are each governed by a
// contextual policy (`InlineAnchoring`/`InlineGrowth`/`InlineShrink`); the defaults
// reproduce the historic behaviour. Under the default `TopAfterResize` anchoring the
// block is BOTTOM-docked on launch (appearing inline beneath the shell command) and
// switches to TOP-anchored — pinned to rows 1..height — on the first resize, because
// a bottom-docked block leaves a gap (and strands a ghost) when the terminal grows
// taller, whereas a top-anchored one is simply overdrawn in place. A top-anchored
// resize clears the whole screen first (the block relocates here and the terminal may
// have moved the old one); otherwise each row is cleared in place. `Fullscreen`
// additionally wraps the session in the alternate screen buffer. `widthFn`/`heightFn`
// supply the live terminal columns and rows, re-read every frame; an oversize block
// is clamped to the terminal height.
class InlineRoot(widthFn: () => Int, heightFn: () => Int)
  ( using stdio:     Stdio,
          anchoring: InlineAnchoring,
          growth:    InlineGrowth,
          shrink:    InlineShrink )
extends GridSurface(widthFn(), 0):
  private var presentedRows: Int = 0
  private var presentedColumns: Int = 0
  private var invalidated: Boolean = false

  // Start top-anchored only when the policy pins the block from the first frame;
  // `TopAfterResize` starts bottom-docked and flips on the first `invalidate`.
  private var topAnchored: Boolean = anchoring match
    case InlineAnchoring.TopAnchored | InlineAnchoring.Fullscreen      => true
    case InlineAnchoring.BottomDocked | InlineAnchoring.TopAfterResize => false

  private var started: Boolean = false
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

  // Mark the next present as a resize repaint. Under `TopAfterResize` the first resize
  // also switches to top-anchoring; the other anchorings keep their fixed reference.
  // The driver calls it on every `WindowSize` event (a width-only resize counts too).
  def invalidate(): Unit =
    invalidated = true
    if anchoring == InlineAnchoring.TopAfterResize then topAnchored = true

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

    // `Fullscreen` takes over the alternate screen buffer for the session: enter it on
    // the first present (restored by `finish`), so the block owns a blank screen.
    if anchoring == InlineAnchoring.Fullscreen && !started then emit(t"\e[?1049h")
    started = true

    emit(csi.dectcem(false))

    // The block's absolute top row, and the resize/grow clearing for each anchoring.
    val top =
      if topAnchored then
        // Pinned to the top. A resize relocates the block here (on the first one) and
        // may have moved the old block, so clear the whole screen before redrawing.
        if resized then
          emit(csi.cup(1, 1))
          emit(csi.ed(0))

        1
      else
        if resized then
          // Clear the block's rows at the bottom dock, sized for the terminal
          // re-wrapping our previous, wider lines.
          val narrowed = presentedColumns > columns && columns > 0
          val wrap = if narrowed then (presentedColumns + columns - 1)/columns else 1
          val cleared = (presentedRows*wrap).max(h)
          emit(csi.cup((rows - cleared + 1).max(1), 1))
          emit(csi.ed(0))
          (rows - h + 1).max(1)
        else if h > presentedRows then
          // Grow beyond the previous height. `ScrollIntoScrollback` scrolls the screen
          // up so the extra rows are free at the bottom, pushing the content above into
          // scrollback (`cud` to the bottom first, since only a `\n` on the bottom line
          // scrolls); `ClampToScreen` instead grows upward in place, overwriting the
          // rows above without touching scrollback.
          growth match
            case InlineGrowth.ScrollIntoScrollback =>
              emit(csi.cud(9999))
              emit(t"\r")
              emit(t"\n"*(h - presentedRows))

            case InlineGrowth.ClampToScreen =>
              ()

          (rows - h + 1).max(1)
        else if presentedRows > h && shrink == InlineShrink.KeepTop then
          // Hold the top row where the taller block began, rather than re-docking down.
          (rows - presentedRows + 1).max(1)
        else
          (rows - h + 1).max(1)

    // Draw the block from its absolute `top` down, clipping each row to the live width
    // so it can never wrap (a wrapped row would scroll the screen and break addressing).
    emit(csi.cup(top, 1))

    // Each row is rendered to SGR from its styled cells, then reset (`sgr(0)`) so the
    // next absolutely-addressed, `el(2)`-cleared row starts from a clean style and no
    // colour bleeds across the frame.
    val termcap = summon[Stdio].termcap
    var r = 0

    while r < h do
      emit(csi.el(2))
      val rendered = rowContent(r, columns).render(termcap)
      emit(rendered)
      // Reset only after a row that actually emitted SGR, so a plain row is byte-for-
      // byte as before and no colour bleeds into the next `el(2)`-cleared row.
      if rendered.contains(t"\e") then emit(csi.sgr(0))
      emit(t"\r")
      if r < h - 1 then emit(t"\n")
      r += 1

    // Shrink: clear the rows a taller previous block vacated. Clearing happens BELOW
    // the block when top-anchored or when `KeepTop` holds the top in place; when
    // bottom-docked under `RedockBottom` the block re-docks down and the vacated rows
    // above are cleared instead (a resize already cleared them).
    if !resized && presentedRows > h then
      if topAnchored || shrink == InlineShrink.KeepTop then
        var k = h

        while k < presentedRows do
          emit(t"\n")
          emit(csi.el(2))
          k += 1
      else
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
      emit(csi.cup((top + caretRow.min(h - 1)).max(1), caretColumn.min(columns - 1).max(0) + 1))
      emit(csi.dectcem(true))

    Out.print(frame.toString.tt)

  // On exit, drop the cursor onto a fresh line below the block and re-show it, so
  // subsequent output continues after the rendered block (like a submitted prompt).
  // A `Fullscreen` session leaves the alternate screen buffer, restoring what was
  // there before it started.
  def finish(): Unit =
    val below = if topAnchored then (presentedRows + 1).min(heightFn()) else heightFn()
    Out.print(csi.cup(below.max(1), 1))
    Out.print(t"\r\n")
    Out.print(csi.dectcem(true))
    if anchoring == InlineAnchoring.Fullscreen && started then Out.print(t"\e[?1049l")
