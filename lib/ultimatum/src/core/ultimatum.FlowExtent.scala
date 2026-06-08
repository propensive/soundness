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
import turbulence.*

// A `Canvas` confined to a `Rect` of a parent surface, backed by a
// hand-rolled character grid. Writes flow within the rectangle — text wraps at
// the rectangle's width and, once the last row fills, the grid scrolls up —
// exactly like a miniature terminal. Mutations (`put`, `move`, `clear`) update
// the grid and the local cursor; `flush` paints the grid onto the parent
// surface, one row at a time, via the parent's `move`/`put` (so positioning is
// always expressed through the `Canvas` interface, never as inline escapes).
//
// Input is assumed to be plain text: the routing `Stdio` and `put(Text)` lay out
// characters cell-by-cell, so styled output should arrive through `put(Teletype)`
// (whose styling is, for now, flattened to its plain text). Wide graphemes are
// likewise treated as single cells for the moment.
class FlowExtent(parent: Canvas, val rect: Rect) extends Extent:
  def width: Int = rect.width
  def height: Int = rect.height

  private val cells: Array[Char] = Array.fill(width*height)(' ')
  private var col: Int = 0
  private var row: Int = 0

  private def scrollUp(): Unit =
    System.arraycopy(cells, width, cells, 0, width*(height - 1))
    var i = width*(height - 1)

    while i < width*height do
      cells(i) = ' '
      i += 1

  private def newline(): Unit =
    col = 0
    if row < height - 1 then row += 1 else scrollUp()

  private def putChar(char: Char): Unit =
    if char == '\n' then newline()
    else
      if col >= width then newline()
      cells(row*width + col) = char
      col += 1

  private def rowText(r: Int): Text = String(cells, r*width, width).tt

  def move(column: Ordinal, row2: Ordinal): Unit =
    col = column.n0.min(width - 1).max(0)
    row = row2.n0.min(height - 1).max(0)

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

    while c < width do
      cells(row*width + c) = ' '
      c += 1

  def cursor(visible: Boolean): Unit = parent.cursor(visible)

  def showCaret(column: Ordinal, row2: Ordinal): Unit =
    parent.showCaret((rect.left + column.n0).z, (rect.top + row2.n0).z)

  // Paint the whole grid onto the parent surface, one row at a time.
  def flush(): Unit =
    var r = 0

    while r < height do
      parent.move(rect.left.z, (rect.top + r).z)
      parent.put(rowText(r))
      r += 1

    parent.flush()

  // A plain-text snapshot of the grid (rows joined by newlines), for testing and
  // for content-change detection.
  def render: Text =
    val builder = StringBuilder()
    var r = 0

    while r < height do
      builder.append(String(cells, r*width, width))
      if r < height - 1 then builder.append('\n')
      r += 1

    builder.toString.tt

  // `Stdio` members: routing `Out` output (and other `Stdio` writes) into this
  // extent. `print` lays text out through the same cursor model as `put`; the
  // underlying streams are muted because all rendering goes via `flush`.
  val termcap: Termcap = new Termcap:
    def ansi: Boolean = true
    def color: ColorDepth = ColorDepth.TrueColor
    override def width: Int = rect.width

  val out = Stdio.MutePrintStream
  val err = Stdio.MutePrintStream
  val in = Stdio.MuteInputStream

  override def print(text: Text): Unit = put(text)
