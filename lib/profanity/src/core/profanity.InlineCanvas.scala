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
package profanity

import anticipation.*
import denominative.*
import escapade.*
import gossamer.*
import turbulence.*

object InlineCanvas:
  def apply(terminal: Terminal): InlineCanvas =
    new InlineCanvas(terminal.knownColumns, terminal.knownRows)(using terminal.stdio)

// A `Canvas` that renders a widget "inline" at the terminal's current cursor
// position, using relative cursor motion. Local coordinates are interpreted
// relative to the line where rendering began (local row 0) and absolute column 0,
// so a standalone widget redraws in place — exactly as profanity's widgets did
// with hand-written escape codes — while being driven through the same move/put
// `Canvas` API as a panel's `Extent`. The surface tracks its own cursor so that
// each `move` can be expressed as relative vertical motion plus an absolute
// column, which is what keeps the widget anchored to the prompt rather than the
// top-left of the screen.
class InlineCanvas(val width: Int, val height: Int)(using Stdio) extends Canvas:
  @scala.caps.unsafe.untrackedCaptures
  private var row: Int = 0
  @scala.caps.unsafe.untrackedCaptures
  private var column: Int = 0

  def move(column2: Ordinal, row2: Ordinal): Unit =
    val targetRow = row2.n0
    val targetColumn = column2.n0

    if targetRow < row then Out.print(csi.cuu(row - targetRow))
    else if targetRow > row then Out.print(csi.cud(targetRow - row))

    Out.print(csi.cha(targetColumn + 1))
    row = targetRow
    column = targetColumn

  def put(text: Text): Unit =
    val string = text.s
    var i = 0

    while i < string.length do
      val char = string.charAt(i)

      if char == '\n' then
        Out.print(t"\r\n")
        row += 1
        column = 0
      else
        Out.print(t"$char")
        column += 1

        if column >= width then
          column = 0
          row += 1

      i += 1

  def put(text: Teletype): Unit = put(text.plain)
  def clear(): Unit = Out.print(csi.ed(0))
  def clearLine(): Unit = Out.print(csi.el(0))
  def cursor(visible: Boolean): Unit = Out.print(csi.dectcem(visible))
  def showCaret(column2: Ordinal, row2: Ordinal): Unit = move(column2, row2)
  def flush(): Unit = ()
