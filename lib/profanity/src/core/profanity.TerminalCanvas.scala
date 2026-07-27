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
import turbulence.*

object TerminalCanvas:
  def apply(width: Int, height: Int)(using Stdio): TerminalCanvas =
    new TerminalCanvas(() => width, () => height)

  // Build a surface covering the whole terminal, reading its size live (so a
  // resize is reflected the next time the layout is solved) and writing through
  // its `Stdio`. The size thunks retain the terminal, so the canvas honestly
  // captures it.
  def apply(terminal: Terminal): TerminalCanvas^{terminal} =
    // Both thunks only read the same terminal's dimensions; no aliased writer.
    scala.caps.unsafe.unsafeAssumeSeparate:
      new TerminalCanvas(() => terminal.knownColumns, () => terminal.knownRows)
        (using terminal.stdio)

// A `Canvas` over a real terminal: every positioning operation maps to an
// escapade `csi` sequence, written through the in-scope `Stdio`. It keeps no
// cursor of its own — `put` lets the terminal advance the hardware cursor
// naturally, while `move`/`showCaret` set absolute positions. `width`/`height`
// are read on demand, so a terminal-backed canvas tracks the live size.
class TerminalCanvas(widthFn: () => Int, heightFn: () => Int)(using Stdio) extends Canvas:
  def width: Int = widthFn()
  def height: Int = heightFn()

  // `csi.cup` takes a 1-based (row, column); our coordinates are 0-based
  // `Ordinal`s, so `.n1` converts each.
  def move(column: Ordinal, row: Ordinal): Unit = Out.print(csi.cup(row.n1, column.n1))

  def put(text: Text): Unit = Out.print(text)
  def put(text: Teletype): Unit = Out.print(text)
  def clear(): Unit = Out.print(csi.ed(2))
  def clearLine(): Unit = Out.print(csi.el(0))
  def cursor(visible: Boolean): Unit = Out.print(csi.dectcem(visible))
  def showCaret(column: Ordinal, row: Ordinal): Unit = move(column, row)
  def flush(): Unit = ()
