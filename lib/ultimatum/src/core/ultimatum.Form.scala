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

import profanity.*
import rudiments.*

// Drives an interactive layout: solves it, paints each panel, then loops over
// terminal events — TAB cycles focus between widgets, Escape/Ctrl-C exits, and
// every other event is folded into the focused widget. After each edit (or a
// terminal resize) the layout is re-solved using each widget's live intrinsic
// size, and only the panels whose rectangle or content actually changed are
// repainted.
class Form(root: Canvas, fullScreen: Boolean, pane: Pane):
  private val leaves: IndexedSeq[Pane] = pane.leaves.to(IndexedSeq)
  private val focuses: IndexedSeq[Focus] = leaves.collect { case Pane.Widget(_, focus) => focus }

  // The leaf index hosting each focusable, in focus order.
  private val focusLeaf: IndexedSeq[Int] = leaves.indices.collect:
    case i if leaves(i).isInstanceOf[Pane.Widget] => i

  private var rects: IndexedSeq[Rect] = IndexedSeq()
  private var focus: Int = 0

  // Project the panes to a frame, overriding each widget's minimum with the live
  // size its content needs at the width it last occupied (the root width on the
  // first solve).
  private def liveFrame: Frame =
    val widths = if rects.isEmpty then leaves.map(_ => root.width) else rects.map(_.width)
    var index = -1

    def project(node: Pane): Frame = node match
      case Pane.Branch(sizing, axis, children) =>
        Frame.Split(sizing, axis, children.map(project))

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
    val height = if fullScreen then root.height else frame.measure(Axis.Rank).min
    frame.arrange(Rect(0, 0, root.width, height)).cells.to(IndexedSeq)

  private def paint(index: Int): Unit =
    val extent = FlowExtent(root, rects(index))

    leaves(index) match
      case Pane.Leaf(_, content) =>
        content(extent)
        extent.flush()

      case Pane.Widget(_, widget) =>
        widget.render(extent, focusLeaf.indexOf(index) == focus)

      case _ =>
        ()

  // Paint every panel, then repaint the focused widget last so the hardware
  // caret is left in it.
  private def paintAll(): Unit =
    rects.indices.each(paint(_))
    if focuses.nonEmpty then paint(focusLeaf(focus))

  def run(events: Iterator[TerminalEvent]): Unit =
    rects = solve()
    paintAll()
    var running = true

    while running && events.hasNext do events.next() match
      case Keypress.Tab =>
        if focuses.nonEmpty then
          focus = (focus + 1)%focuses.length
          paintAll()

      case Keypress.Escape | Keypress.Ctrl('C' | 'D') =>
        running = false

      // The terminal reports its new size after a resize (the `Winch` signal,
      // handled below, is what prompts the report). Clear the screen and re-tile
      // against the now-current dimensions, repainting every panel.
      case _: TerminalInfo.WindowSize =>
        root.clear()
        rects = solve()
        paintAll()

      // A bare resize signal arrives before the new size is known, so there is
      // nothing useful to do yet; the `WindowSize` event that follows re-tiles.
      // Swallow all signals here — they are never widget input.
      case _: Signal =>
        ()

      case event =>
        if focuses.nonEmpty then
          focuses(focus).handle(event)
          val updated = solve()
          val dirty = dirtyCells(rects, updated, Set(focusLeaf(focus)))
          rects = updated
          dirty.each(paint(_))
          paint(focusLeaf(focus))
