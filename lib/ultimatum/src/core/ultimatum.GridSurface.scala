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

import anticipation.*
import denominative.*
import escapade.*
import gossamer.*
import hieroglyph.*, textMetrics.wideCharacterWidthMetric
import profanity.*
import turbulence.*
import vacuous.*
import yossarian.*

// The shared styled-grapheme grid behind `FlowExtent` (a clipped sub-rectangle that
// composites onto a parent) and `InlineRoot` (the inline-mode root that presents its
// grid at the cursor). Cells are stored in a `yossarian.Screen[StyleWord]`, so each
// holds one grapheme cluster with its own style: writes flow within the grid, wrap at
// the grid width (counting display columns, so a wide CJK glyph takes two cells), and
// scroll up once the last row fills — like a miniature terminal. Any `Imprintable`
// content (`Text`/`Ascii`/`Writing`/`Teletype`) can be written, so colour survives.
// Subclasses supply `cursor`, `showCaret` and `flush`.
private[ultimatum] abstract class GridSurface(initialWidth: Int, initialHeight: Int)
extends Canvas:
  protected var gridWidth: Int = initialWidth.max(1)
  protected var gridHeight: Int = initialHeight.max(1)
  protected var screen: Screen[StyleWord] = Screen(gridWidth, gridHeight, StyleWord.Default)
  protected var col: Int = 0
  protected var row: Int = 0

  // The physical-screen model: a copy of the grid as the last present actually drew it,
  // tagged with the absolute top row and column count it was drawn at, so the next
  // present can emit only the cells that differ (an unchanged overprint is a no-op).
  // It models the TERMINAL, not the compose grid: `reshape` and `clear` leave it alone
  // (they only blank what will be composed next), and only `invalidate` drops it —
  // forcing the next present to redraw everything, e.g. after a resize has reflowed
  // whatever was really on screen.
  protected var snapshot: Optional[Screen[StyleWord]] = Unset
  protected var snapshotTop: Int = 0
  protected var snapshotColumns: Int = 0
  protected var invalidated: Boolean = false

  // Mark the next present as a full repaint: the screen can no longer be assumed to
  // match the snapshot (typically after a WINCH, when the terminal has reflowed it).
  def invalidate(): Unit =
    invalidated = true
    snapshot = Unset

  // The caret (hardware cursor) target and visibility, recorded by a root's `cursor`/
  // `showCaret` and applied when the frame is presented; and where the last present
  // left them, so a diffed present whose caret hasn't moved can omit placing it.
  protected var caretColumn: Int = 0
  protected var caretRow: Int = 0
  protected var caretVisible: Boolean = true
  protected var presentedCaretRow: Int = -1
  protected var presentedCaretColumn: Int = -1
  protected var presentedCaretVisible: Boolean = false

  private val metric: Grapheme is Measurable = summon[Grapheme is Measurable]

  def width: Int = gridWidth
  def height: Int = gridHeight

  // Reallocate the grid to a new size, blanking it and homing the cursor. Used by
  // `InlineRoot` to resize to the measured block height each frame.
  protected def reshape(width2: Int, height2: Int): Unit =
    gridWidth = width2.max(1)
    gridHeight = height2.max(1)
    screen = Screen(gridWidth, gridHeight, StyleWord.Default)
    col = 0
    row = 0

  protected def scrollUp(): Unit = screen.scroll(1)

  protected def newline(): Unit =
    col = 0
    if row < gridHeight - 1 then row += 1 else scrollUp()

  // Write one styled grapheme cell, advancing the cursor by the grapheme's display
  // width. A zero-width grapheme is dropped (clustering already folded it into its
  // base); a width-2 grapheme writes an empty trailing sentinel into the next cell and
  // wraps to a new row if it would straddle the right edge.
  protected def putCell(grapheme: Grapheme, style: StyleWord): Unit =
    if grapheme.text == t"\n" then newline()
    else
      val cellWidth = metric.width(grapheme)

      if cellWidth > 0 then
        // Wrap before writing (never eagerly after), so the final cell of a full row
        // doesn't scroll a row off the top until the next cell actually arrives.
        if col + cellWidth > gridWidth then newline()

        if row < gridHeight && col < gridWidth then
          screen.set(col.z, row.z, grapheme, style, t"")

          if cellWidth >= 2 && col + 1 < gridWidth then
            screen.set((col + 1).z, row.z, Grapheme(""), style, t"")

        col += cellWidth

  // Imprint any styled content cell-by-cell (the seamless entry for all text types).
  protected def imprint[content: Imprintable](content: content): Unit =
    summon[content is Imprintable].cells(content): (grapheme, style) =>
      putCell(grapheme, style)

  def move(column: Ordinal, row2: Ordinal): Unit =
    col = column.n0.min(gridWidth - 1).max(0)
    row = row2.n0.min(gridHeight - 1).max(0)

  def put(text: Text): Unit = imprint(text)
  def put(text: Teletype): Unit = imprint(text)

  def clear(): Unit =
    screen = Screen(gridWidth, gridHeight, StyleWord.Default)
    col = 0
    row = 0

  def clearLine(): Unit =
    var c = col

    while c < gridWidth do
      screen.set(c.z, row.z, Grapheme(" "), StyleWord.Default, t"")
      c += 1

  // The plain text of row `r` (graphemes concatenated, wide-trailing sentinels
  // skipped). Styling is read directly off `screen` by the flush path.
  protected def rowText(r: Int): Text =
    val builder = StringBuilder()
    var c = 0

    while c < gridWidth do
      val grapheme = screen.grapheme(c.z, r.z).text
      if !grapheme.nil then builder.append(grapheme)
      c += 1

    builder.text

  // A styled `Teletype` of row `r`, up to `columns` cells (wide-trailing sentinels
  // skipped), so the row's colour can be composited onto a parent surface or rendered
  // to SGR by the inline presenter.
  protected def rowContent(r: Int, columns: Int): Teletype = runContent(r, 0, columns)

  // A styled `Teletype` of the cells `[from, until)` of row `r` (wide-trailing
  // sentinels skipped): a single damaged run of the row, ready to render to SGR.
  protected def runContent(r: Int, from: Int, until: Int): Teletype =
    var content = Teletype.empty
    val limit = until.min(gridWidth)
    var c = from

    while c < limit do
      val grapheme = screen.grapheme(c.z, r.z).text
      if !grapheme.nil then content = content.append(styledCell(grapheme, screen.style(c.z, r.z)))
      c += 1

    content

  // Record the grid as what is now really on screen, drawn at absolute row `top` and
  // clipped to `columns`. Called by a presenter after every present, on both the full
  // and the diffed path.
  protected def recordSnapshot(top: Int, columns: Int): Unit =
    snapshot = screen.copy()
    snapshotTop = top
    snapshotColumns = columns

  // Whether the snapshot really describes the screen region about to be presented: the
  // same absolute top, the same column clip and the same grid dimensions. Any geometry
  // change means the diff would be against the wrong cells, so the caller must fall
  // back to a full redraw.
  protected def snapshotValid(top: Int, columns: Int, h: Int): Boolean =
    snapshot.lay(false): snap =>
      snapshotTop == top && snapshotColumns == columns
        && snap.height == h && snap.width == gridWidth

  // Whether the cell at `(c, r)` differs from the snapshot's. A cell is one grapheme
  // with one style; links never reach the grid (`putCell` always writes `t""`), so the
  // pair is the whole identity.
  private def cellChanged(snap: Screen[StyleWord], c: Int, r: Int): Boolean =
    screen.grapheme(c.z, r.z).text != snap.grapheme(c.z, r.z).text
      || screen.style(c.z, r.z).raw != snap.style(c.z, r.z).raw

  // Diff the grid (drawn at absolute rows `top..top + h - 1`, clipped to `columns`)
  // against the snapshot, appending one absolutely-addressed run per contiguous patch
  // of changed cells: `cup` to the patch, its cells as SGR, and a style reset after any
  // run that emitted SGR (each run renders self-contained, like a full row today).
  // Returns the number of runs, so a caller can skip the write when nothing changed.
  // A wide glyph never straddles a run boundary: its trailing sentinel carries the
  // leading cell's style, so the two cells always change (or not) together; backing a
  // run up off a sentinel start is a defensive backstop.
  protected def emitDiffRuns
    ( frame: StringBuilder, top: Int, columns: Int, h: Int, termcap: Termcap )
  :   Int =

    val snap = snapshot.vouch
    var runs = 0
    var r = 0

    while r < h do
      var c = 0

      while c < columns do
        if cellChanged(snap, c, r) then
          var start = c
          if screen.grapheme(start.z, r.z).text.nil && start > 0 then start -= 1
          while c < columns && cellChanged(snap, c, r) do c += 1
          frame.append(csi.cup(top + r, start + 1).s)
          val rendered = runContent(r, start, c).render(termcap)
          frame.append(rendered.s)
          if rendered.contains(t"\e") then frame.append(csi.sgr(0).s)
          runs += 1
        else c += 1

      r += 1

    runs

  // Present by overprinting only the damaged cells (the geometry is unchanged, so a
  // cell equal to the snapshot's is already on screen), the grid's rows mapping to the
  // absolute screen rows `top..top + h - 1`. When no cell and no caret changed,
  // NOTHING is written — an identical frame is a true no-op. Otherwise the patches
  // land in one single write with the cursor hidden, so it never flashes across the
  // screen between runs (and nothing can interleave mid-frame).
  protected def presentDiff(top: Int, columns: Int, h: Int)(using Stdio): Unit =
    val termcap = summon[Stdio].termcap
    val body = StringBuilder()
    val runs = emitDiffRuns(body, top, columns, h, termcap)

    val caretRow2 = (top + caretRow.min(h - 1)).max(1)
    val caretColumn2 = caretColumn.min(columns - 1).max(0) + 1

    val caretSame = caretVisible == presentedCaretVisible
      && (!caretVisible || (caretRow2 == presentedCaretRow && caretColumn2 == presentedCaretColumn))

    if runs > 0 || !caretSame then
      val frame = StringBuilder()
      frame.append(csi.dectcem(false).s)
      frame.append(body.toString)

      if caretVisible then
        frame.append(csi.cup(caretRow2, caretColumn2).s)
        frame.append(csi.dectcem(true).s)

      recordSnapshot(top, columns)
      presentedCaretRow = caretRow2
      presentedCaretColumn = caretColumn2
      presentedCaretVisible = caretVisible
      Out.print(frame.toString.tt)

  // Record the caret state the present just applied, alongside `recordSnapshot`, so
  // the next diffed present can recognise an unmoved caret. Called on the full path
  // (the diffed path records inline, only when it writes).
  protected def recordCaret(top: Int, columns: Int, h: Int): Unit =
    presentedCaretRow = (top + caretRow.min(h - 1)).max(1)
    presentedCaretColumn = caretColumn.min(columns - 1).max(0) + 1
    presentedCaretVisible = caretVisible

  // A `Teletype` of `text` in one uniform style (sparse single-run form).
  private def styledCell(text: Text, style: StyleWord): Teletype =
    Teletype(text, IArray(style.raw, 0L), boundaries = IArray(0))

  // A plain-text snapshot of the grid (rows joined by newlines), for testing and for
  // content-change detection.
  def render: Text = screen.render
