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
import symbolism.*
import turbulence.*
import vacuous.*

object InlineRoot:
  // The root for inline mode over a real terminal: width and the height clamp are
  // read live, so a resize is reflected on the next frame. The rendering policy is
  // read from context (`InlineAnchoring`/`InlineGrowth`/`InlineShrink`), so it is
  // fixed at the caller's site rather than inside the library.
  def apply(terminal: Terminal)
    ( using anchoring: InlineAnchoring, growth: InlineGrowth, shrink: InlineShrink )
  :   InlineRoot^{terminal} =

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
  private var presentedTop: Int = 1

  // For `Inline` anchoring: the row offset (from the block's top) at which the cursor was
  // left after the last frame, so the next frame can rise back to the top relatively.
  private var flowCursorRow: Int = 0

  // Start top-anchored only when the policy pins the block from the first frame;
  // `TopAfterResize` starts bottom-docked and flips on the first `invalidate`.
  private var topAnchored: Boolean = anchoring match
    case InlineAnchoring.TopAnchored | InlineAnchoring.Fullscreen      => true
    case InlineAnchoring.BottomDocked | InlineAnchoring.TopAfterResize => false
    case InlineAnchoring.Inline                                        => false

  private var started: Boolean = false

  // Where the terminal reported the parked cursor cell after the last resize's reflow
  // (1-based screen coordinates), stashed by the driver from the anchor query's reply
  // and consumed by the next resized present — so a stale anchor can never inform a
  // later resize it didn't measure.
  private var anchorCell: Optional[(Int, Int)] = Unset

  // Stash the anchor reply for the next resized present. A reflowing terminal keeps
  // the cursor attached to the logical cell it was on, and every present parks the
  // cursor at the caret cell, so this reveals where that known cell landed.
  def anchor(row: Int, column: Int): Unit = anchorCell = (row, column)

  override def width: Int = widthFn()

  // Resize the grid to the measured block height, clamped to the live terminal
  // height; called by the driver before compositing each frame.
  def reframe(width: Int, height: Int): Unit = reshape(width, height.min(heightFn()))

  // Cursor visibility is deferred like the caret: recorded now, applied by `flush`,
  // so a focused editor shows it and a focused menu keeps it hidden.
  def cursor(visible: Boolean): Unit = caretVisible = visible

  // `invalidate()` (inherited) marks the next present as a resize repaint; the driver
  // calls it on every `WindowSize` event (a width-only resize counts too). Under
  // `TopAfterResize`, the switch to top-anchoring no longer happens there: the resized
  // present first tries to recover the block's position from the anchor reply, and
  // flips (latched) only when that recovery is unavailable or fails.

  // Forget everything recorded about what is on screen, so the NEXT frame renders as a
  // fresh first frame at the current cursor rather than relative to (or docked against)
  // a prior block. Intended for use after the caller has cleared the screen — e.g. an
  // `Inline` driver that, on a terminal resize, wipes the reflowed screen and restarts
  // the flow from the top rather than trying to reconcile the old, now-garbled layout.
  def reset(): Unit =
    started = false
    presentedRows = 0
    presentedColumns = 0
    presentedTop = 1
    flowCursorRow = 0
    invalidated = false
    snapshot = Unset
    anchorCell = Unset
    presentedCaretRow = -1
    presentedCaretColumn = -1
    presentedCaretVisible = false

  // Record the caret's block-local target; `flush` positions the hardware cursor at
  // the corresponding absolute screen cell.
  override def showCaret(column: Ordinal, row2: Ordinal): Unit =
    caretColumn = column.n0
    caretRow = row2.n0

  // `Inline` anchoring renders the block RELATIVE to the cursor, so it appears wherever
  // prior output has flowed to (right after the previous result) and never docks to a
  // fixed edge. The block's top is not a fixed screen row: on the first frame it is drawn
  // at the current cursor; on later frames the cursor is risen back to the top via the
  // recorded `flowCursorRow`. Growing scrolls the screen (and the block) naturally as the
  // last row's newline hits the foot; shrinking clears the freed rows below, holding the
  // block's top so the box stays put (freed space becomes blank screen below, not a gap).
  private def flushInline(): Unit =
    val columns = gridWidth.min(widthFn())
    val h       = height
    invalidated = false

    // `Inline` anchoring addresses the screen RELATIVELY, so an absolute snapshot can
    // never describe where its cells really landed; drop it to keep the invariant that
    // a snapshot always refers to an absolutely-addressed present.
    snapshot = Unset

    val frame = StringBuilder()
    def emit(text: Text): Unit = frame.append(text.s)
    val termcap = summon[Stdio].termcap

    emit(csi.dectcem(false))

    // Rise to the block's top-left. On the first frame the cursor is already there.
    emit(t"\r")
    if started && flowCursorRow > 0 then emit(csi.cuu(flowCursorRow))
    started = true

    // Draw the block's rows from the top down; a trailing newline past the screen foot
    // scrolls the block up with the rest of the screen (relative addressing tracks it).
    var r = 0
    while r < h do
      emit(csi.el(2))
      val rendered = trimmedRowContent(r, columns).render(termcap)
      emit(rendered)
      if rendered.contains(t"\e") then emit(csi.sgr(0))
      emit(t"\r")
      if r < h - 1 then emit(t"\n")
      r += 1
    // The cursor now sits at the block's last drawn row (h - 1), column 0.

    // Shrink: clear the rows a taller previous block left below, then return to the last
    // row. The top does not move, so the box stays put and the freed rows go blank.
    if presentedRows > h then
      var k = h
      while k < presentedRows do
        emit(t"\n")
        emit(csi.el(2))
        k += 1

      emit(csi.cuu(presentedRows - h))

    presentedRows = h
    presentedColumns = columns

    // Place the caret relative to the block's last row (where the cursor is now).
    val cr = caretRow.min(h - 1).max(0)
    if h - 1 - cr > 0 then emit(csi.cuu(h - 1 - cr))
    emit(t"\r")
    val cc = caretColumn.min(columns - 1).max(0)
    if cc > 0 then emit(csi.cuf(cc))
    flowCursorRow = cr
    if caretVisible then emit(csi.dectcem(true))

    Out.print(frame.toString.tt)

  def flush(): Unit =
    if anchoring == InlineAnchoring.Inline then flushInline() else flushDocked()

  private def flushDocked(): Unit =
    val rows    = heightFn()
    val columns = gridWidth.min(widthFn())
    val h       = height

    // When the geometry is exactly as last presented (same dock row, height and width,
    // and not a resize), every grid cell maps to the same screen cell as the snapshot,
    // so the present is a pure overprint and only the cells that changed need emitting.
    // Any geometry change falls back to the full redraw, which also handles docking,
    // growth, shrink-clearing and resize-clearing.
    val dockTop = if topAnchored then 1 else (rows - h + 1).max(1)

    if !invalidated && started && h == presentedRows && columns == presentedColumns
      && dockTop == presentedTop && snapshotValid(dockTop, columns, h)
    then presentDiff(dockTop, columns, h)
    else flushDockedFull(rows, columns, h)

  private def flushDockedFull(rows: Int, columns: Int, h: Int): Unit =
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

    // The anchor is consumed by whichever present follows the resize that measured
    // it — used or not — so it can never inform a later resize.
    val anchor0 = anchorCell
    anchorCell = Unset

    // Today's resize behaviour, for when no anchor could reconcile the reflow: under
    // `TopAfterResize` the block gives up its dock — flip to top-anchoring (latched;
    // a terminal that failed to answer once won't start) and clear the whole screen;
    // otherwise clear a bottom band sized by the wrap heuristic.
    def unrecoveredResize(): Int =
      if anchoring == InlineAnchoring.TopAfterResize then
        topAnchored = true
        emit(csi.cup(1, 1))
        emit(csi.ed(0))
        1
      else
        val narrowed = presentedColumns > columns && columns > 0
        val wrap = if narrowed then (presentedColumns + columns - 1)/columns else 1
        val cleared = (presentedRows*wrap).max(h)
        emit(csi.cup((rows - cleared + 1).max(1), 1))
        emit(csi.ed(0))
        (rows - h + 1).max(1)

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
          // Recover the old block's position from the anchor: the terminal kept the
          // parked cursor attached to its cell through the reflow, so the residue's
          // top row is computable from the reply and the retained snapshot. Clear
          // exactly from there to the screen foot and re-dock — the block survives
          // the resize without wiping the scrollback history above it. When the
          // anchor is missing or cannot be reconciled, fall back to today's clears.
          val recovered: Optional[Int] = anchor0.let: (anchorRow, anchorColumn) =>
            residueTop(anchorRow, anchorColumn, columns, rows)

          recovered.lay(unrecoveredResize()): clearTop =>
            emit(csi.cup(clearTop, 1))
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

    // Remember where the block was actually drawn, so `finish` can drop the cursor onto
    // the row immediately after its last line rather than assuming it reaches the screen
    // foot — which is false when a shrink held the top (`KeepTop`) leaving blank below.
    presentedTop = top

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
      // Trailing default-styled blanks are trimmed (`el(2)` has cleared them): each
      // row becomes a hard logical line of its content width, so a resize's reflow
      // of these rows is predictable — the basis of the anchor recovery above.
      val rendered = trimmedRowContent(r, columns).render(termcap)
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

    // The caret is the hardware cursor, parked at its absolute cell UNCONDITIONALLY —
    // a hidden cursor still has a position, and a reflowing terminal drags that
    // position with the cell it is on, so the next resize can ask where the block
    // went. Only visibility is conditional.
    emit(csi.cup((top + caretRow.min(h - 1)).max(1), caretColumn.min(columns - 1).max(0) + 1))
    if caretVisible then emit(csi.dectcem(true))

    // What was just drawn IS the screen now: record it (and the caret) so the next
    // geometry-stable present can diff against it.
    recordSnapshot(top, columns)
    recordCaret(top, columns, h)

    Out.print(frame.toString.tt)

  // Predict the topmost screen row the previous block's residue can occupy after a
  // resize, from the observed anchor cell and the retained snapshot. Two models cover
  // the real terminal population: a REFLOWING terminal (VTE, iTerm2, kitty, alacritty,
  // tmux, Terminal.app, foot, wezterm) re-wraps each of the block's hard logical lines
  // at the new width and keeps the cursor attached to its logical cell; a TRUNCATING
  // terminal (xterm, st, urxvt, screen, mosh) clips lines in place and leaves the
  // cursor's row alone. Each model predicts where the parked cursor should have landed
  // — row offset AND column — so the observed column discriminates them: trust the
  // model whose column matches; when both match take the safer (upper) minimum; when
  // neither matches the terminal did something unmodellable, so return `Unset` and let
  // the caller fall back to today's clears. The result is clamped to the screen.
  private def residueTop(anchorRow: Int, anchorColumn: Int, newColumns: Int, rows: Int)
  :   Optional[Int] =

    snapshot.let: snap =>
      val oldH = snap.height
      val parkLocal = presentedCaretRow - snapshotTop     // the park cell, block-local
      val parkColumn = presentedCaretColumn - 1           // 0-based

      if parkLocal < 0 || parkLocal >= oldH || anchorRow < 1 || anchorRow > rows
        || newColumns <= 0
      then Unset
      else
        // Simulate the terminal re-wrapping snapshot row `r`'s content (trimmed, so
        // exactly what was really printed) at width `w`: the number of physical
        // sub-rows it occupies, and the sub-row and column on which cell `target`
        // lands. A wide glyph that would straddle the margin is pushed whole to the
        // next sub-row, as terminals do. A target beyond the content (the park sat in
        // the `el(2)`-cleared tail) stays on the last sub-row at its clamped column —
        // erased cells are not content and never wrap.
        def wrapProfile(r: Int, w: Int, target: Int): (Int, Int, Int) =
          val width0 = contentWidth(snap, r, snapshotColumns)
          var c = 0
          var col = 0
          var sub = 0
          var targetSub = -1
          var targetColumn = 0

          while c < width0 do
            val grapheme = snap.grapheme(c.z, r.z)

            if grapheme.text.nil then
              // A wide-trailing sentinel: its lead already advanced past this cell.
              if c == target then
                targetSub = sub
                targetColumn = (col - 1).max(0)
            else
              val cellWidth = metric.width(grapheme)
              if cellWidth > 0 && col + cellWidth > w then
                sub += 1
                col = 0
              if c == target then
                targetSub = sub
                targetColumn = col
              col += cellWidth

            c += 1

          if targetSub < 0 then
            targetSub = sub
            targetColumn = target.min(w - 1).max(0)

          (sub + 1, targetSub, targetColumn)

        // Physical rows of block content above the park cell, per model, and the
        // column each model predicts for the anchor reply.
        var reflowAbove = 0
        var r = 0

        while r < parkLocal do
          reflowAbove += wrapProfile(r, newColumns, -1)(0)
          r += 1

        val (_, parkSub, parkNewColumn) = wrapProfile(parkLocal, newColumns, parkColumn)
        reflowAbove += parkSub

        val truncateAbove = parkLocal
        val reflowColumn = parkNewColumn + 1
        val truncateColumn = parkColumn.min(newColumns - 1) + 1

        val matchReflow = anchorColumn == reflowColumn
        val matchTruncate = anchorColumn == truncateColumn

        val clearTop: Optional[Int] =
          if matchReflow && !matchTruncate then anchorRow - reflowAbove
          else if matchTruncate && !matchReflow then anchorRow - truncateAbove
          else if matchReflow && matchTruncate
          then (anchorRow - reflowAbove).min(anchorRow - truncateAbove)
          else Unset

        clearTop.let: top =>
          if top > rows then Unset else top.max(1)

  // On exit, drop the cursor onto a fresh line below the block and re-show it, so
  // subsequent output continues after the rendered block (like a submitted prompt).
  // A `Fullscreen` session leaves the alternate screen buffer, restoring what was
  // there before it started.
  def finish(): Unit =
    // `Inline` drops the cursor onto a fresh line right below the block, RELATIVELY (from
    // the caret down to the block's last row, then a newline that scrolls if at the foot),
    // so the following output — and the next inline block — continues immediately after it.
    if anchoring == InlineAnchoring.Inline then
      val down = presentedRows - 1 - flowCursorRow
      if down > 0 then Out.print(csi.cud(down))
      Out.print(t"\r\n")
    else
      // Drop the cursor just past the block's LAST drawn row (`presentedTop + presentedRows
      // - 1`) rather than at the screen foot. For a full bottom-docked block the last row IS
      // the foot, so this is unchanged; but when a `KeepTop` shrink held the block high and
      // cleared blank below it, this lands the following output right under the block instead
      // of after the vacated rows. Top-anchored keeps its historic one-line trailing gap.
      val below =
        if topAnchored then (presentedRows + 1).min(heightFn())
        else (presentedTop + presentedRows - 1).max(1).min(heightFn())

      Out.print(csi.cup(below.max(1), 1))
      Out.print(t"\r\n")
    Out.print(csi.dectcem(true))
    if anchoring == InlineAnchoring.Fullscreen && started then Out.print(t"\e[?1049l")
