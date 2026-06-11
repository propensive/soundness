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
import hieroglyph.*, textMetrics.wideCharacterWidth
import profanity.*
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
  protected def rowContent(r: Int, columns: Int): Teletype =
    var content = Teletype.empty
    val limit = columns.min(gridWidth)
    var c = 0

    while c < limit do
      val grapheme = screen.grapheme(c.z, r.z).text
      if !grapheme.nil then content = content.append(styledCell(grapheme, screen.style(c.z, r.z)))
      c += 1

    content

  // A `Teletype` of `text` in one uniform style (sparse single-run form).
  private def styledCell(text: Text, style: StyleWord): Teletype =
    Teletype(text, IArray(style.raw, 0L), boundaries = IArray(0))

  // A plain-text snapshot of the grid (rows joined by newlines), for testing and for
  // content-change detection.
  def render: Text = screen.render
