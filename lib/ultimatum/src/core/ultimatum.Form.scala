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
┃    Soundness, version 0.63.0.                                                                    ┃
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

import scala.collection.immutable.IndexedSeq

import proscenium.compat.*

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
  // `Canvas^`: a terminal-backed canvas retains its terminal (live size thunks), and the
  // form legitimately holds it for its whole run.
  ( root:         Canvas^,
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
  private var deferred: Optional[scala.collection.immutable.Set[Int]] = Unset
  private var wakePending: Boolean = false
  private var resizePending: Boolean = false

  // The anchor reply (the parked cursor's position after the resize's reflow),
  // stashed when it decodes and handed to the inline root at the resize repaint;
  // dropped on every new WINCH so it can only describe the latest reflow.
  private var anchor: Optional[(Int, Int)] = Unset

  // Whether the resize repaint has already been deferred once to await a late
  // anchor reply; a single grace keeps a reply-less terminal from stalling.
  private var resizeGrace: Boolean = false

  // Bind every container to the wake function so a mutation requests a repaint.
  private def bind(node: Pane): Unit = node match
    case Pane.Branch(_, _, panes) =>
      panes.bindWake(wake)
      panes.contents.each(bind(_))

    case _ =>
      ()

  // Snapshot the live tree's leaves and focusables; keep focus on the same widget
  // if it still exists, else fall back to the first.
  private def rederive(): Unit =
    bind(pane)
    leaves = pane.leaves.stdlib.toIndexedSeq
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
        Frame.Split(sizing, axis, panes.contents.map(project).to[List])

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
      case screen: ScreenRoot => screen.reframe()
      case _                  => ()

    frame.arrange(Rect(0, 0, root.width, height)).cells.stdlib.toIndexedSeq

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
  private def refresh(changed: scala.collection.immutable.Set[Int]): Unit =
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
  private def requestRefresh(changed: scala.collection.immutable.Set[Int]): Unit =
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
    deferred = deferred.or(scala.collection.immutable.Set())
    lastWinch = System.currentTimeMillis

    if resizeDelay <= 0 then flushDeferred()
    else if !wakePending then
      wakePending = true
      scheduleWake(resizeDelay)

  private def flushDeferred(): Unit = deferred.let: changed =>
    // A resize repaint whose anchor reply hasn't decoded yet defers once more, a few
    // milliseconds: the reply usually sits in the input buffer already, and waiting
    // for it is the difference between recovering the block's position and falling
    // back to a full clear. Gated on a real `debounce` (so a driver with a no-op
    // `scheduleWake` can never stall) and used at most once per resize.
    if resizePending && anchor.absent && debounce > 0 && !resizeGrace then
      resizeGrace = true
      wakePending = true
      scheduleWake(40)
    else
      deferred = Unset
      wakePending = false
      lastRedraw = System.currentTimeMillis

      // A resize may have moved the old block (and reflowed the fullscreen contents);
      // invalidate it (a width-only resize counts too) so the next present redraws in
      // full. Done here, once, however many resizes were coalesced. This is what
      // guarantees a SIGWINCH always forces a complete redraw, never a diff against a
      // stale snapshot. An inline root additionally receives the anchor, from which
      // it can recover the reflowed block's position instead of wiping the screen.
      if resizePending then
        resizePending = false
        resizeGrace = false

        root match
          case inline: InlineRoot =>
            anchor.let((row, column) => inline.anchor(row, column))
            anchor = Unset
            inline.invalidate()

          case screen: ScreenRoot =>
            screen.invalidate()

          case _ =>
            ()

      refresh(changed)

  def run(events: Iterator[TerminalEvent]): Unit =
    requestRefresh(scala.collection.immutable.Set())
    var running = true

    while running && events.hasNext do events.next() match
      case Keypress.Tab =>
        if focuses.nonEmpty then
          // Repaint the panel losing focus too, so its focus indicator updates;
          // the panel gaining focus is repainted by `refresh` (focused last).
          val vacated = focusLeaf(focusIndex)
          focused = focuses((focusIndex + 1)%focuses.length)
          requestRefresh(scala.collection.immutable.Set(vacated))

      case Keypress.Escape | Keypress.Ctrl('C' | 'D') =>
        running = false

      // The WINCH signal arrives synchronously from the trap, well before the anchor
      // and size replies decode: drop any anchor from a previous resize (it cannot
      // describe this reflow) and, when a real debounce window exists, mark the
      // resize NOW — so a keypress landing mid-resize coalesces instead of
      // presenting against stale geometry. Without a debounce (no wake scheduling),
      // suppression could stall, so the resize is only marked by `WindowSize`.
      case Signal.Winch =>
        anchor = Unset
        resizeGrace = false

        if debounce > 0 then
          resizePending = true
          requestResizeRefresh()

      // The anchor query's reply: where the terminal moved the parked cursor during
      // the reflow. Stashed for the resize repaint; never widget input, never a
      // repaint by itself.
      case TerminalInfo.CursorPosition(row, column) =>
        anchor = (row, column)

      // A resize re-tiles to the new size; reset the rects so the whole layout is
      // repainted against the live dimensions, and mark the resize so the next
      // repaint clears the moved block (using the anchor reply, when one arrived).
      // The repaint is debounced until the drag pauses; typing is unaffected.
      case _: TerminalInfo.WindowSize =>
        rects = IndexedSeq()
        resizePending = true
        requestResizeRefresh()

      // An application redraw request (e.g. after a background layout change), or the
      // scheduled wake for a debounced resize: repaint if the drag has gone quiet,
      // else reschedule the wake for the rest of the debounce window.
      case TerminalInfo.Redraw =>
        if !resizePending then requestRefresh(scala.collection.immutable.Set())
        else if resizeDelay <= 0 then flushDeferred()
        else scheduleWake(resizeDelay)

      // Other signals are never widget input.
      case _: Signal =>
        ()

      case event =>
        if focuses.nonEmpty then
          focuses(focusIndex).handle(event)
          val changed = scala.collection.immutable.Set(focusLeaf(focusIndex))

          // During a pending resize the widget's state updates immediately, but its
          // repaint coalesces into the debounced resize flush: presenting now would
          // draw against stale geometry and move the parked cursor out from under
          // the anchor recovery. (A wake is always scheduled while `resizePending`.)
          if resizePending then deferred = deferred.lay(changed)(_ ++ changed)
          else requestRefresh(changed)

    root match
      case inline: InlineRoot => inline.finish()
      case _                  => ()
