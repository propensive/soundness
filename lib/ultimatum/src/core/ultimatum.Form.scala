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

import denominative.*
import profanity.*
import rudiments.*
import vacuous.*

// Drives an interactive layout. The pane tree is re-derived from its live
// containers on every frame, so panes appended or inserted while the form runs
// are picked up and the layout re-tiles; a container mutation also wakes the
// loop (via `wake`) so background changes appear immediately. TAB cycles focus
// between widgets (tracked by identity, so it survives insertions), Escape/Ctrl-C
// exits, and other events fold into the focused widget. Fullscreen repaints only
// the changed cells; inline re-sizes the grid to the measured block height each
// frame and re-presents the whole block at the cursor.
class Form
  ( root:         Canvas,
    mode:         Mode,
    pane:         Pane,
    wake:         () => Unit   = () => (),
    throttle:     Long         = 0,
    debounce:     Long         = 0,
    scheduleWake: Long => Unit = _ => () ):
  private var leaves: IndexedSeq[Pane] = IndexedSeq()
  private var focuses: IndexedSeq[Focus] = IndexedSeq()
  private var focusLeaf: IndexedSeq[Int] = IndexedSeq()
  private var focused: Optional[Focus] = Unset
  private var rects: IndexedSeq[Rect] = IndexedSeq()
  private var lastRedraw: Long = 0
  private var lastWinch: Long = 0
  private var deferred: Optional[Set[Int]] = Unset
  private var wakePending: Boolean = false
  private var resizePending: Boolean = false

  // Bind every container to the wake function so a mutation requests a repaint.
  private def bind(node: Pane): Unit = node match
    case Pane.Branch(_, _, panes) =>
      panes.onChange = wake
      panes.contents.each(bind(_))

    case _ =>
      ()

  // Snapshot the live tree's leaves and focusables; keep focus on the same widget
  // if it still exists, else fall back to the first.
  private def rederive(): Unit =
    bind(pane)
    leaves = pane.leaves.to(IndexedSeq)
    focuses = leaves.collect { case Pane.Widget(_, focus) => focus }

    focusLeaf = leaves.indices.collect:
      case i if leaves(i).isInstanceOf[Pane.Widget] => i

    val stays = focused.lay(false): widget =>
      focuses.indexWhere(_ eq widget) >= 0

    if !stays then focused = if focuses.nil then Unset else focuses(0)

  private def focusIndex: Int = focused.lay(0): widget =>
    val index = focuses.indexWhere(_ eq widget)
    if index < 0 then 0 else index

  // Project the panes to a frame, overriding each widget's minimum with the live
  // size its content needs at the width it last occupied (the root width on the
  // first solve).
  private def liveFrame: Frame =
    val widths = if rects.nil then leaves.map(_ => root.width) else rects.map(_.width)
    var index = -1

    def project(node: Pane): Frame = node match
      case Pane.Branch(sizing, axis, panes) =>
        Frame.Split(sizing, axis, panes.contents.map(project).to(List))

      case Pane.Leaf(sizing, _) =>
        index += 1
        Frame.Cell(sizing)

      case Pane.Widget(sizing, widget) =>
        index += 1
        val (minWidth, minHeight) = widget.measure(widths(index))

        val grown = sizing.copy(minWidth = sizing.minWidth.max(minWidth),
            minHeight = sizing.minHeight.max(minHeight))

        Frame.Cell(grown)

    project(pane)

  private def solve(): IndexedSeq[Rect] =
    val frame = liveFrame

    val height = mode match
      case Mode.Fullscreen => root.height
      case Mode.Inline     => frame.measure(Axis.Rank).min

    root match
      case inline: InlineRoot => inline.reframe(root.width, height)
      case _                  => ()

    frame.arrange(Rect(0, 0, root.width, height)).cells.to(IndexedSeq)

  private def paint(index: Int): Unit =
    val extent = FlowExtent(root, rects(index))

    leaves(index) match
      case Pane.Leaf(_, content) =>
        content(extent)
        extent.flush()

      case Pane.Widget(_, widget) =>
        widget.render(extent, focusLeaf.indexOf(index) == focusIndex)

      case _ =>
        ()

  // Re-derive, re-solve and repaint, then present. A change in the number of
  // leaves (a pane was added or removed) forces a full repaint, clearing the
  // screen in fullscreen so a removed panel leaves no residue; otherwise
  // fullscreen repaints only the dirty cells and inline re-presents the block.
  private def refresh(changed: Set[Int]): Unit =
    rederive()

    if leaves.length != rects.length then
      mode match
        case Mode.Fullscreen => root.clear()
        case Mode.Inline     => ()

      rects = IndexedSeq()

    val updated = solve()

    mode match
      case Mode.Inline =>
        rects = updated
        rects.indices.each(paint(_))

      case Mode.Fullscreen =>
        val dirty = dirtyCells(rects, updated, changed)
        rects = updated
        dirty.each(paint(_))

    if focuses.nonEmpty then paint(focusLeaf(focusIndex))
    root.flush()

  // Repaint immediately, folding in any coalesced or pending-resize work. Used for
  // typing, focus changes and application redraws, which must stay responsive.
  private def requestRefresh(changed: Set[Int]): Unit =
    deferred = deferred.lay(changed)(_ ++ changed)
    flushDeferred()

  // Milliseconds until a coalesced resize may repaint: `debounce` ms after the last
  // WINCH (so a drag must pause first) but never sooner than `throttle` ms after the
  // last repaint. Zero means it may repaint now.
  private def resizeDelay: Long =
    ((lastWinch + debounce).max(lastRedraw + throttle) - System.currentTimeMillis).max(0)

  // A resize coalesces until the drag pauses: each WINCH pushes the deadline back by
  // `debounce` ms, and the single pending wake reschedules itself (in the `Redraw`
  // handler) until the deadline is reached, so the block is repainted only after the
  // drag is quiet — no mid-drag redraws to ghost or flicker. The (blocking) cursor
  // query is deferred to that repaint, so a whole drag costs one query. Typing is
  // unaffected and stays immediate.
  private def requestResizeRefresh(): Unit =
    deferred = deferred.or(Set())
    lastWinch = System.currentTimeMillis

    if resizeDelay <= 0 then flushDeferred()
    else if !wakePending then
      wakePending = true
      scheduleWake(resizeDelay)

  private def flushDeferred(): Unit = deferred.let: changed =>
    deferred = Unset
    wakePending = false
    lastRedraw = System.currentTimeMillis

    // A resize may have moved the old block; invalidate it (a width-only resize
    // counts too) so the next present clears it. Done here, once, however many
    // resizes were coalesced.
    if resizePending then
      resizePending = false

      root match
        case inline: InlineRoot => inline.invalidate()
        case _                  => ()

    refresh(changed)

  def run(events: Iterator[TerminalEvent]): Unit =
    requestRefresh(Set())
    var running = true

    while running && events.hasNext do events.next() match
      case Keypress.Tab =>
        if focuses.nonEmpty then
          // Repaint the panel losing focus too, so its focus indicator updates;
          // the panel gaining focus is repainted by `refresh` (focused last).
          val vacated = focusLeaf(focusIndex)
          focused = focuses((focusIndex + 1)%focuses.length)
          requestRefresh(Set(vacated))

      case Keypress.Escape | Keypress.Ctrl('C' | 'D') =>
        running = false

      // A resize re-tiles to the new size; reset the rects so the whole layout is
      // repainted against the live dimensions, and mark the resize so the next
      // repaint clears the moved block (with a cursor query). The repaint is debounced
      // until the drag pauses; typing is unaffected.
      case _: TerminalInfo.WindowSize =>
        rects = IndexedSeq()
        resizePending = true
        requestResizeRefresh()

      // An application redraw request (e.g. after a background layout change), or the
      // scheduled wake for a debounced resize: repaint if the drag has gone quiet,
      // else reschedule the wake for the rest of the debounce window.
      case TerminalInfo.Redraw =>
        if !resizePending then requestRefresh(Set())
        else if resizeDelay <= 0 then flushDeferred()
        else scheduleWake(resizeDelay)

      // Other signals are never widget input.
      case _: Signal =>
        ()

      case event =>
        if focuses.nonEmpty then
          focuses(focusIndex).handle(event)
          requestRefresh(Set(focusLeaf(focusIndex)))

    root match
      case inline: InlineRoot => inline.finish()
      case _                  => ()
