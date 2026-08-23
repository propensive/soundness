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

import clavichord.Keypress

import denominative.*
import prepositional.*
import profanity.*
import rudiments.*
import symbolism.*
import vacuous.*
import denominative.asymptotics.linearSizeComplexity

object Form:
  // One derived leaf of the live pane tree: the pane itself, the rectangle the last solve
  // assigned it, and its live element (present only for a `Pane.Widget`). Every
  // per-leaf datum lives together, so per-entry access needs no agreement between
  // parallel sequences.
  // `focus` is the same object as `fixture` whenever the fixture accepts input, and `Unset` when it
  // does not — which is precisely what keeps a display-only fixture (a gauge) out of the focus
  // cycle, since `focusables` is derived from `focus` alone. Both are recorded at `solve`, where
  // the pane is matched anyway, rather than re-tested on each access.
  private[ultimatum] case class Entry
    ( pane:    Pane,
      rect:    Rect,
      fixture: Optional[Fixture],
      focus:   Optional[Focus] )

  // An immutable snapshot of one solved layout: the entries in frame order. A `Layout` is
  // only ever constructed whole (and the `Form` re-assigns its single `layout` var
  // atomically), so an index confined to `entries` (an `Ordinal in entries.type`) is
  // proof of bounds for exactly the values it will access.
  private[ultimatum] case class Layout(entries: Sequence[Entry]):
    // The indices of the focusable entries, in leaf order, each already confined to
    // `entries` — minted once at construction, sound because `entries` is immutable.
    val focusables: Sequence[Ordinal in entries.type] =
      val builder = scala.collection.immutable.Vector.newBuilder[Ordinal in entries.type]
      entries.iterate { index => if entries(index).focus.present then builder += index }
      Sequence.of(builder.result())

// Drives an interactive layout. The pane tree is re-derived from its live
// containers on every frame, so panes appended or inserted while the form runs
// are picked up and the layout re-tiles; a container mutation also wakes the
// loop (via `wake`) so background changes appear immediately. TAB cycles focus
// between widgets (tracked by identity, so it survives insertions), Escape/Ctrl-C
// exits, and other events fold into the focused widget. Fullscreen repaints only
// the changed cells; inline re-sizes the grid to the measured block height each
// frame and re-presents the whole block at the cursor.
class Form
  // `Board^`: a terminal-backed canvas retains its terminal (live size thunks), and the
  // form legitimately holds it for its whole run.
  ( root:         Board^,
    mode:         Occupancy,
    pane:         Pane,
    wake:         () => Unit   = () => (),
    throttle:     Long         = 0,
    debounce:     Long         = 0,
    scheduleWake: Long => Unit = _ => () ):
  // The one immutable snapshot of the derived layout, re-assigned atomically by `refresh`.
  @scala.caps.unsafe.untrackedCaptures
  private var layout: Form.Layout = Form.Layout(Sequence())
  @scala.caps.unsafe.untrackedCaptures
  private var focused: Optional[Focus] = Unset

  // Whether `layout`'s geometry no longer describes the terminal (set by a resize): the
  // next refresh must re-measure at the root width and repaint in full.
  @scala.caps.unsafe.untrackedCaptures
  private var staleGeometry: Boolean = false
  @scala.caps.unsafe.untrackedCaptures
  private var lastRedraw: Long = 0
  @scala.caps.unsafe.untrackedCaptures
  private var lastWinch: Long = 0
  @scala.caps.unsafe.untrackedCaptures
  private var deferred: Optional[Set[Int]] = Unset
  @scala.caps.unsafe.untrackedCaptures
  private var wakePending: Boolean = false
  @scala.caps.unsafe.untrackedCaptures
  private var resizePending: Boolean = false

  // Whether an animation wake is already scheduled. One timer serves every animated fixture in the
  // layout (armed at the shortest period any of them asks for), and it is re-armed by each
  // repaint, so an animation costs exactly one pending wake however many spinners are on screen.
  @scala.caps.unsafe.untrackedCaptures
  private var animationPending: Boolean = false

  // The anchor reply (the parked cursor's position after the resize's reflow),
  // stashed when it decodes and handed to the inline root at the resize repaint;
  // dropped on every new WINCH so it can only describe the latest reflow.
  @scala.caps.unsafe.untrackedCaptures
  private var anchor: Optional[(Int, Int)] = Unset

  // Whether the resize repaint has already been deferred once to await a late
  // anchor reply; a single grace keeps a reply-less terminal from stalling.
  @scala.caps.unsafe.untrackedCaptures
  private var resizeGrace: Boolean = false

  // Bind every container and every live element to the wake function, so a mutation — whether of
  // the tree's shape or of an element's own state — requests a repaint.
  private def bind(node: Pane): Unit = node match
    case Pane.Branch(_, _, panes) =>
      panes.bindWake(wake)
      panes.contents.each(bind(_))

    case Pane.Widget(_, fixture) =>
      fixture.bindWake(wake)

    case _ =>
      ()

  // Snapshot the live tree's leaves; keep focus on the same widget if it still
  // exists, else fall back to the first focusable.
  private def rederive(): Sequence[Pane] =
    bind(pane)
    val panes = Sequence.from(pane.leaves.stdlib)

    val stays = focused.lay(false): widget =>
      panes.stdlib.exists:
        case Pane.Widget(_, fixture) => fixture eq widget
        case _                       => false

    if !stays then
      focused = panes.stdlib.collectFirst { case Pane.Widget(_, focus: Focus) => focus }.optional

    panes

  // The position, within `layout.focusables`, of the focused widget (matched by
  // identity, so focus survives insertions), defaulting to the first.
  private def focusPosition(layout: Form.Layout): Int = focused.lay(0): widget =>
    layout.focusables.where: ordinal =>
      layout.entries(ordinal).focus.lay(false)(_ eq widget)
    . lay(0)(_.n0)

  // Project the panes to a frame, overriding each widget's minimum with the live
  // size its content needs at the width it last occupied (the root width on the
  // first solve, or when the previous geometry is stale). The widths are threaded
  // as an iterator aligned with the leaf walk, so no leaf count has to agree with
  // any sequence: a source that runs dry (the live tree grew mid-frame) degrades
  // to the root width.
  private def liveFrame(previous: Form.Layout, stale: Boolean): Frame =
    val widths: Iterator[Int] =
      if stale then Iterator.empty else previous.entries.stdlib.iterator.map(_.rect.width)

    def nextWidth(): Int = if widths.hasNext then widths.next() else root.width

    def project(node: Pane): Frame = node match
      case Pane.Branch(sizing, arrangement, panes) =>
        Frame.Split(sizing, arrangement, panes.contents.map(project).to[List])

      case Pane.Leaf(sizing, _) =>
        nextWidth()
        Frame.Cell(sizing)

      case Pane.Widget(sizing, fixture) =>
        val (minWidth, minHeight) = fixture.measure(nextWidth())

        val grown = sizing.copy(minWidth = sizing.minWidth.max(minWidth),
            minHeight = sizing.minHeight.max(minHeight))

        Frame.Cell(grown)

    project(pane)

  // Solve the snapshot's panes against the live root, pairing each pane with its
  // solved rectangle (and its focus, for widgets) in one immutable `Layout`. The
  // `zip` makes the pairing total by construction: there are no separate sequences
  // whose lengths must silently agree.
  private def solve(panes: Sequence[Pane], previous: Form.Layout, stale: Boolean)
  :   Form.Layout =

    val frame = liveFrame(previous, stale)

    val height = mode match
      case Occupancy.Fullscreen => root.height
      case Occupancy.Inline     => frame.measure(Arrangement.Stack).min

    root match
      case inline: InlineRoot => inline.reframe(root.width, height)
      case screen: ScreenRoot => screen.reframe()
      case _                  => ()

    val rects = frame.arrange(Rect(0, 0, root.width, height)).cells

    Form.Layout:
      Sequence.from:
        panes.stdlib.zip(rects.stdlib).map: (leaf, rect) =>
          val fixture: Optional[Fixture] = leaf match
            case Pane.Widget(_, fixture) => fixture
            case _                       => Unset

          val focus: Optional[Focus] = leaf match
            case Pane.Widget(_, focus: Focus) => focus
            case _                            => Unset

          Form.Entry(leaf, rect, fixture, focus)

  // Paint one leaf of a solved layout. The index is confined to `layout.entries`, so
  // callers prove it in bounds (via `iterate`, `confine` or `focusables`) before it
  // crosses this boundary; both accesses below are then total.
  private def paint(layout: Form.Layout)(index: Ordinal in layout.entries.type): Unit =
    val entry = layout.entries(index)
    val extent = FlowExtent(root, entry.rect)

    entry.pane match
      case Pane.Leaf(_, content) =>
        content(extent)
        extent.flush()

      case Pane.Widget(_, fixture) =>
        fixture.render(extent, layout.focusables.where(_ == index).lay(-1)(_.n0) == focusPosition(layout))

      case _ =>
        ()

  // Re-derive, re-solve and repaint, then present. A change in the number of
  // leaves (a pane was added or removed) or stale geometry (a resize) forces a
  // full repaint, clearing the screen in fullscreen so a removed panel leaves no
  // residue; otherwise fullscreen repaints only the dirty cells and inline
  // re-presents the block.
  private def refresh(changed: Set[Int]): Unit =
    val panes = rederive()
    val previous = layout
    val stale = staleGeometry || panes.size != previous.entries.size
    staleGeometry = false

    if stale then
      mode match
        case Occupancy.Fullscreen => root.clear()
        case Occupancy.Inline     => ()

    val updated = solve(panes, previous, stale)
    layout = updated

    mode match
      case Occupancy.Inline =>
        updated.entries.iterate(paint(updated)(_))

      case Occupancy.Fullscreen =>
        val previousRects = if stale then Sequence() else previous.entries.map(_.rect)

        // An animated fixture is dirty by definition: its appearance depends on the clock, not on
        // its rectangle or its state, so nothing else in `dirtyCells` would notice it changing.
        val dirty = dirtyCells(previousRects, updated.entries.map(_.rect), changed + animated)
        dirty.each { index => updated.entries.confine(index.z).let(paint(updated)(_)) }

    updated.focusables.confine(focusPosition(updated).z).let: position =>
      paint(updated)(updated.focusables(position))

    root.flush()
    rearm()

  // The shortest redraw interval any live fixture asks for, or `Unset` when every fixture is
  // static — in which case no timer is armed at all and the form stays purely event-driven.
  private def animationPeriod: Optional[Int] =
    var least: Int = Int.MaxValue
    val current = layout

    current.entries.iterate: index =>
      least = least.min(current.entries(index).fixture.lay(Int.MaxValue)(_.period.or(Int.MaxValue)))

    if least == Int.MaxValue then Unset else least

  // Arm the animation timer, unless one is already armed or a resize wake already covers it (that
  // wake will repaint, and the repaint re-arms). Armed after every refresh, so the timer stops of
  // its own accord the moment the last animated fixture leaves the layout.
  private def rearm(): Unit =
    if !animationPending && !wakePending then animationPeriod.let: period =>
      animationPending = true
      scheduleWake(period.toLong)

  // The indices of the entries whose fixture animates, and which therefore have to be repainted on
  // every frame whether or not anything else about them changed.
  private def animated: Set[Int] =
    val builder = scala.collection.immutable.Set.newBuilder[Int]
    val current = layout

    current.entries.iterate: index =>
      if current.entries(index).fixture.lay(false)(_.period.present)
      then builder += (index: Ordinal).n0

    Set.of(builder.result())

  // Repaint immediately, folding in any coalesced or pending-resize work. Used for
  // typing, focus changes and application redraws, which must stay responsive.
  private def requestRefresh(changed: Set[Int]): Unit =
    deferred = deferred.lay(changed)(_ + changed)
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

  def run(events: Iterator[Terminal.Event]): Unit =
    requestRefresh(Set())
    var running = true

    while running && events.hasNext do events.next() match
      case Keypress.Tab =>
        val current = layout

        if !current.focusables.nil then
          // Repaint the panel losing focus too, so its focus indicator updates;
          // the panel gaining focus is repainted by `refresh` (focused last). The
          // wrap-around stays plain `Int` arithmetic until it is re-confined.
          val position = focusPosition(current)
          val next = (position + 1)%current.focusables.size

          current.focusables.confine(position.z).let: vacated =>
            current.focusables.confine(next.z).let: gaining =>
              focused = current.entries(current.focusables(gaining)).focus
              val vacatedEntry: Ordinal = current.focusables(vacated)
              requestRefresh(Set(vacatedEntry.n0))

      case Keypress.Escape | Keypress.Ctrl('C' | 'D') =>
        running = false

      // The WINCH signal arrives synchronously from the trap, well before the anchor
      // and size replies decode: drop any anchor from a previous resize (it cannot
      // describe this reflow) and, when a real debounce window exists, mark the
      // resize NOW — so a keypress landing mid-resize coalesces instead of
      // presenting against stale geometry. Without a debounce (no wake scheduling),
      // suppression could stall, so the resize is only marked by `WindowSize`.
      case Interrupt.Winch =>
        anchor = Unset
        resizeGrace = false

        if debounce > 0 then
          resizePending = true
          requestResizeRefresh()

      // The anchor query's reply: where the terminal moved the parked cursor during
      // the reflow. Stashed for the resize repaint; never widget input, never a
      // repaint by itself.
      case Terminal.Info.CursorPosition(row, column) =>
        anchor = (row, column)

      // A resize re-tiles to the new size; mark the geometry stale so the whole
      // layout is repainted against the live dimensions, and mark the resize so the
      // next repaint clears the moved block (using the anchor reply, when one
      // arrived). The repaint is debounced until the drag pauses; typing is
      // unaffected.
      case _: Terminal.Info.WindowSize =>
        staleGeometry = true
        resizePending = true
        requestResizeRefresh()

      // An application redraw request (e.g. after a background layout change), the scheduled wake
      // for a debounced resize, or an animation tick: repaint if the drag has gone quiet, else
      // reschedule the wake for the rest of the debounce window. An animation wake is consumed
      // here — the repaint that follows re-arms it — so a layout that stops animating stops the
      // timer of its own accord.
      case Terminal.Info.Redraw =>
        animationPending = false

        if !resizePending then requestRefresh(Set())
        else if resizeDelay <= 0 then flushDeferred()
        else scheduleWake(resizeDelay)

      // Other signals are never widget input.
      case _: Interrupt =>
        ()

      case event =>
        val current = layout

        if !current.focusables.nil then
          current.focusables.confine(focusPosition(current).z).let: position =>
            val ordinal = current.focusables(position)

            current.entries(ordinal).focus.let: widget =>
              widget.handle(event)
              val changed = Set((ordinal: Ordinal).n0)

              // During a pending resize the widget's state updates immediately, but its
              // repaint coalesces into the debounced resize flush: presenting now would
              // draw against stale geometry and move the parked cursor out from under
              // the anchor recovery. (A wake is always scheduled while `resizePending`.)
              if resizePending then deferred = deferred.lay(changed)(_ + changed)
              else requestRefresh(changed)

    root match
      case inline: InlineRoot => inline.finish()
      case _                  => ()
