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

object Board:
  // The default surface for a terminal is an inline one: widgets summoned with a
  // `Terminal` in scope render in place at the cursor. A panel supplies its own
  // `Extent` (a `Board`) in a nearer scope, which shadows this default.
  given inlineBoard: (terminal: Terminal) => (Board^) = InlineBoard(terminal)

// The single abstraction every renderer draws through. All positioning happens
// by calling `move` (never by printing escape codes inline), so the same widget
// code can target the real terminal or a clipped sub-rectangle (an `Extent`)
// without change. Coordinates are surface-local, top-origin `Ordinal`s: `column`
// is the horizontal cell (x) and `row` the vertical cell (y), both zero-based.
// A board is a stateful exclusive capability: drawing mutates it, so every drawing operation
// is an `update` method, callable only through an exclusive reference (`Board^`), and an
// implementation's own state needs no `untrackedCaptures`. `Stateful` rather than `Mutable`
// because a board captures its terminal or `Stdio`, which `Mutable` would not allow.
trait Board extends scala.caps.ExclusiveCapability, scala.caps.Stateful:
  def width: Int
  def height: Int

  // Position the cursor at a surface-local cell.
  update def move(column: Ordinal, row: Ordinal): Unit

  // Write at the current cursor, advancing it.
  update def put(text: Text): Unit
  update def put(text: Teletype): Unit

  // Erase the whole surface, or the current row from the cursor onwards.
  update def clear(): Unit
  update def clearLine(): Unit

  // Show or hide the hardware cursor.
  update def cursor(visible: Boolean): Unit

  // Leave the visible caret at a surface-local cell (after a frame is drawn).
  update def showCaret(column: Ordinal, row: Ordinal): Unit

  // Commit a frame; a no-op on an unbuffered surface.
  update def flush(): Unit
