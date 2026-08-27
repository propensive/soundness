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

import scala.caps

import anticipation.*
import clavichord.*
import denominative.*
import escapade.*
import gossamer.*
import profanity.*
import rudiments.*
import vacuous.*

object ScrollFixture:
  // The supplier genuinely captures the caller's data source and escapes into the
  // long-lived fixture, exactly as `TableFixture.apply`'s content thunk does — sound for
  // the same reason (it is only invoked from `render` while the host is live), hence the
  // single, localised `unsafeAssumePure`.
  def apply
    ( track: Text = BorderStyle.light.vertical,
      thumb: Text = BorderStyle.heavy.vertical )
    ( content: => scala.List[Teletype] )
  :   ScrollFixture =

    new ScrollFixture(caps.unsafe.unsafeAssumePure { () => content }, track, thumb)

// A VIEWPORT over content taller than the space offered to render it: the fixture holds
// the scroll position, shows the window of the content that position selects, and draws a
// scrollbar in its rightmost column — a track of `track` glyphs with a `thumb` whose height
// is proportional to the visible fraction of the content, positioned proportionally to the
// scroll offset. When the content fits, no scrollbar is drawn and the full width is used.
//
// The position is either DOCKED TO THE BOTTOM (the initial state), where the viewport
// tracks the end of the content as it grows — a live log follows its tail — or an explicit
// offset, which holds still while content is appended. Scrolling down to the bottom
// re-docks. Under a `form`, the Up and Down keys scroll when the fixture is focused; a
// `paint`-driven host calls `scroll` directly.
class ScrollFixture(content: () -> scala.List[Teletype], track: Text, thumb: Text)
extends Focus:

  // A no-op until the fixture is bound into a running form; see `bindWake`.
  @scala.caps.unsafe.untrackedCaptures
  private var wakeForm: () -> Unit = () => ()

  // `Unset` = docked to the bottom, tracking growth.
  @scala.caps.unsafe.untrackedCaptures
  private var offset0: Optional[Int] = Unset

  // The content length and viewport height most recently rendered, for clamping scroll
  // requests that arrive between paints.
  @scala.caps.unsafe.untrackedCaptures
  private var lastLength: Int = 0
  @scala.caps.unsafe.untrackedCaptures
  private var lastViewport: Int = 0

  // As in `TableFixture.bindWake`: the callback captures the running form's event loop and
  // escapes into this longer-lived fixture, re-bound on every run.
  override private[ultimatum] def bindWake(wake: () => Unit): Unit =
    wakeForm = caps.unsafe.unsafeAssumePure(wake)

  def refresh(): Unit = wakeForm()

  def atBottom: Boolean = offset0.absent

  def toBottom(): Unit =
    offset0 = Unset
    wakeForm()

  // Moves the viewport by `delta` lines (negative = up). Scrolling up from the docked
  // state materializes the current bottom offset first; reaching the bottom re-docks, so
  // subsequent growth is tracked again.
  def scroll(delta: Int): Unit =
    val limit = (lastLength - lastViewport).max(0)
    val next = (offset0.or(limit) + delta).min(limit).max(0)
    offset0 = if next >= limit then Unset else next
    wakeForm()

  def handle(event: Terminal.Event): Unit = event match
    case Keypress.Up   => scroll(-1)
    case Keypress.Down => scroll(1)
    case _             => ()

  // The pane's `Sizing` distributes the height; the fixture claims no intrinsic size of
  // its own — whatever rect it receives becomes the viewport.
  def measure(width: Int): (Int, Int) = (0, 1)

  def render(canvas: Board^, focused: Boolean): Unit =
    val lines: scala.List[Teletype] = content()
    val length: Int = lines.length
    val viewport: Int = canvas.height
    lastLength = length
    lastViewport = viewport

    val fits: Boolean = length <= viewport
    val limit: Int = (length - viewport).max(0)

    // Clamped at render: the content may have shrunk since the offset was chosen.
    val offset: Int = offset0.lay(limit)(_.min(limit).max(0))

    // The rightmost column belongs to the scrollbar whenever one is needed; content is
    // clipped to the remaining width so a long line can never wrap into it. (`keep` counts
    // characters rather than display cells, so a wide glyph at the edge may spill one cell;
    // the bar is drawn afterwards, so its own column always wins.)
    val width: Int = if fits then canvas.width else canvas.width - 1

    canvas.clear()

    var row: Int = 0

    lines.drop(offset).foreach: line =>
      if row < viewport then
        canvas.move(Prim, row.z)
        canvas.put(line.keep(width))
        row += 1

    if !fits then
      val thumbHeight: Int = ((viewport*viewport + length - 1)/length).max(1).min(viewport)

      val thumbTop: Int =
        if limit == 0 then 0 else ((viewport - thumbHeight)*offset + limit/2)/limit

      var bar: Int = 0

      while bar < viewport do
        canvas.move((canvas.width - 1).z, bar.z)
        canvas.put(if bar >= thumbTop && bar < thumbTop + thumbHeight then thumb else track)
        bar += 1
