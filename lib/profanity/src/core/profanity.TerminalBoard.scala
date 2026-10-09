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

object TerminalBoard:
  def apply(width: Int, height: Int)(using Stdio): TerminalBoard^ =
    new TerminalBoard(() => width, () => height)

  // Build a surface covering the whole terminal, reading its size live (so a
  // resize is reflected the next time the layout is solved) and writing through
  // its `Stdio`. The size thunks only read the terminal, so the canvas captures it
  // read-only; the canvas itself is a fresh exclusive capability (the `caps.any`).
  def apply(terminal: Terminal): TerminalBoard^{terminal.rd, scala.caps.any} =
    new TerminalBoard(() => terminal.knownColumns, () => terminal.knownRows)(using terminal.stdio)

// A `Board` over a real terminal: every positioning operation maps to an
// escapade `csi` sequence, written through the in-scope `Stdio`. It keeps no
// cursor of its own — `put` lets the terminal advance the hardware cursor
// naturally, while `move`/`showCaret` set absolute positions. `width`/`height`
// are read on demand, so a terminal-backed canvas tracks the live size. The size thunks may
// capture only read-only capabilities: `width`/`height` are read-only methods of a stateful
// board, which may not reach an exclusive capability.
class TerminalBoard(widthFn: () ->{scala.caps.any.rd} Int, heightFn: () ->{scala.caps.any.rd} Int)
  ( using Stdio )
extends Board:
  def width: Int = widthFn()
  def height: Int = heightFn()

  // `csi.cup` takes a 1-based (row, column); our coordinates are 0-based
  // `Ordinal`s, so `.n1` converts each.
  update def move(column: Ordinal, row: Ordinal): Unit = Out.print(csi.cup(row.n1, column.n1))

  update def put(text: Text): Unit = Out.print(text)
  update def put(text: Teletype): Unit = Out.print(text)
  update def clear(): Unit = Out.print(csi.ed(2))
  update def clearLine(): Unit = Out.print(csi.el(0))
  update def cursor(visible: Boolean): Unit = Out.print(csi.dectcem(visible))
  update def showCaret(column: Ordinal, row: Ordinal): Unit = move(column, row)
  update def flush(): Unit = ()
