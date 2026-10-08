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
import profanity.*
import turbulence.*

// A `Board` confined to a `Rect` of a parent surface, backed by the shared
// character grid (`GridSurface`). Writes flow and wrap within the rectangle;
// `flush` paints the grid onto the parent surface, one row at a time, via the
// parent's `move`/`put` (so positioning is always expressed through the `Board`
// interface, never as inline escapes). It is also an `Stdio`, so bare
// `Out.println` in a panel body flows into it.
class FlowExtent(parent: Board^, val rect: Rect)
extends GridSurface(rect.width, rect.height), Extent:
  update def cursor(visible: Boolean): Unit = parent.cursor(visible)

  update def showCaret(column: Ordinal, row2: Ordinal): Unit =
    parent.showCaret((rect.left + column.n0).z, (rect.top + row2.n0).z)

  // Paint the whole grid onto the parent surface, one row at a time. The parent
  // is not itself flushed here — the caller presents the root once after every
  // panel has composited, so an inline root emits a complete frame rather than
  // once per panel.
  update def flush(): Unit =
    var r = 0

    while r < height do
      parent.move(rect.left.z, (rect.top + r).z)
      parent.put(rowContent(r, gridWidth))
      r += 1

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

  // `Stdio.print` is a read-only method by its interface, yet this extent's `print` draws on
  // the grid — an update, which a read-only method of a stateful class may not perform. Until
  // `Stdio` is itself stateful, the write is delegated to a plain (non-stateful) writer that
  // owns an exclusive reference to this surface, held as a pure field so that `print` is not an
  // access to that reference. The writer never leaves the object. [stdio-readonly]
  private val writer: FlowExtent.Writer = scala.caps.unsafe.unsafeAssumePure(FlowExtent.Writer(this))

  override def print(text: Text): Unit = writer.write(text)

object FlowExtent:
  private[ultimatum] class Writer(board: Board^):
    def write(text: Text): Unit = board.put(text)
