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
import profanity.*

// The shared character-grid mechanics behind `FlowExtent` (a clipped sub-rectangle
// that composites onto a parent) and `InlineRoot` (the inline-mode root that
// presents its grid at the cursor). Writes flow within the grid: text wraps at
// the grid width and, once the last row fills, the grid scrolls up — like a
// miniature terminal. Subclasses supply `cursor`, `showCaret` and `flush`.
private[ultimatum] abstract class GridSurface(initialWidth: Int, initialHeight: Int)
extends Canvas:
  protected var gridWidth: Int = initialWidth
  protected var gridHeight: Int = initialHeight
  protected var cells: Array[Char] = Array.fill(gridWidth*gridHeight)(' ')
  protected var col: Int = 0
  protected var row: Int = 0

  def width: Int = gridWidth
  def height: Int = gridHeight

  // Reallocate the grid to a new size, blanking it and homing the cursor. Used by
  // `InlineRoot` to resize to the measured block height each frame.
  protected def reshape(width2: Int, height2: Int): Unit =
    gridWidth = width2
    gridHeight = height2
    cells = Array.fill(gridWidth*gridHeight)(' ')
    col = 0
    row = 0

  protected def scrollUp(): Unit =
    System.arraycopy(cells, gridWidth, cells, 0, gridWidth*(gridHeight - 1))
    var i = gridWidth*(gridHeight - 1)

    while i < gridWidth*gridHeight do
      cells(i) = ' '
      i += 1

  protected def newline(): Unit =
    col = 0
    if row < gridHeight - 1 then row += 1 else scrollUp()

  protected def putChar(char: Char): Unit =
    if char == '\n' then newline()
    else
      if col >= gridWidth then newline()
      cells(row*gridWidth + col) = char
      col += 1

  protected def rowText(r: Int): Text = String(cells, r*gridWidth, gridWidth).tt

  def move(column: Ordinal, row2: Ordinal): Unit =
    col = column.n0.min(gridWidth - 1).max(0)
    row = row2.n0.min(gridHeight - 1).max(0)

  def put(text: Text): Unit =
    val string = text.s
    var i = 0

    while i < string.length do
      putChar(string.charAt(i))
      i += 1

  def put(text: Teletype): Unit = put(text.plain)

  def clear(): Unit =
    var i = 0

    while i < cells.length do
      cells(i) = ' '
      i += 1

    col = 0
    row = 0

  def clearLine(): Unit =
    var c = col

    while c < gridWidth do
      cells(row*gridWidth + c) = ' '
      c += 1

  // A plain-text snapshot of the grid (rows joined by newlines), for testing and
  // for content-change detection.
  def render: Text =
    val builder = StringBuilder()
    var r = 0

    while r < gridHeight do
      builder.append(String(cells, r*gridWidth, gridWidth))
      if r < gridHeight - 1 then builder.append('\n')
      r += 1

    builder.toString.tt
