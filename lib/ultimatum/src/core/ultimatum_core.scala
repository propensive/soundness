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
import spectacular.*
import vacuous.*

// Construct a leaf panel: a fractional weight and optional per-axis bounds,
// plus deferred `content` that runs (with its `Extent` in context) once the
// layout has been solved and the panel's rectangle is known.
def panel
  ( fraction:  Double        = 1.0,
    minWidth:  Int           = 0,
    maxWidth:  Optional[Int] = Unset,
    minHeight: Int           = 0,
    maxHeight: Optional[Int] = Unset )
  ( content: Extent ?=> Unit )
:   Pane =

  val sizing = Sizing(fraction, minWidth, maxWidth, minHeight, maxHeight)
  Pane.Leaf(sizing, extent => content(using extent))

// A leaf panel hosting an interactive, focusable line editor.
def editor
  ( initial:   LineEditor    = LineEditor(),
    fraction:  Double        = 1.0,
    minWidth:  Int           = 0,
    maxWidth:  Optional[Int] = Unset,
    minHeight: Int           = 0,
    maxHeight: Optional[Int] = Unset )
:   Pane =

  Pane.Widget(Sizing(fraction, minWidth, maxWidth, minHeight, maxHeight), EditorField(initial))

// A leaf panel hosting an interactive, focusable selection menu.
def menu[item: Showable]
  ( options:   List[item],
    current:   item,
    fraction:  Double        = 1.0,
    minWidth:  Int           = 0,
    maxWidth:  Optional[Int] = Unset,
    minHeight: Int           = 0,
    maxHeight: Optional[Int] = Unset )
:   Pane =

  val sizing = Sizing(fraction, minWidth, maxWidth, minHeight, maxHeight)
  Pane.Widget(sizing, MenuField(SelectMenu(options, current)))

// A split whose children sit side by side as columns (distributing width).
def file(panes: Pane*): Pane = Pane.Branch(Sizing(), Axis.File, panes.to(List))

// A split whose children stack as rows (distributing height).
def rank(panes: Pane*): Pane = Pane.Branch(Sizing(), Axis.Rank, panes.to(List))

// Drive an interactive layout, looping over terminal events until the user exits.
// Used inside `interactive`. In `Fullscreen` mode the layout takes over the
// terminal via the alternate screen buffer (restored on exit) and fills its
// height; in `Inline` mode it renders a variable-height block at the cursor
// without the alternate buffer, leaving scrollback intact.
def form(mode: Mode = Mode.Fullscreen)(pane: Pane)(using terminal: Terminal): Unit =
  mode match
    case Mode.Fullscreen =>
      profanity.terminalFeatures.alternateScreen:
        val root = TerminalCanvas(terminal)
        root.cursor(false)
        try Form(root, mode, pane).run(terminal.eventIterator()) finally root.cursor(true)

    case Mode.Inline =>
      Form(InlineRoot(terminal), mode, pane).run(terminal.eventIterator())

// The leaf indices that must be repainted: any whose rectangle moved or resized
// since the last layout, plus any whose content changed this frame. A leaf whose
// rectangle and content are both unchanged is omitted, so it is left untouched on
// screen — the basis of flicker-free redraw.
def dirtyCells(previous: IndexedSeq[Rect], current: IndexedSeq[Rect], changed: Set[Int]): Set[Int] =
  val moved = current.indices.filter: i =>
    i >= previous.length || previous(i) != current(i)

  moved.to(Set) ++ changed

// Solve `pane` against `root` once and paint each leaf's content into its
// rectangle (no event loop). An `InlineRoot` is sized to the height its content
// needs and presented at the cursor; any other canvas fills its own height.
def paint(root: Canvas, pane: Pane): Unit =
  val height = root match
    case _: InlineRoot => pane.frame.measure(Axis.Rank).min
    case _             => root.height

  root match
    case inline: InlineRoot => inline.reframe(root.width, height)
    case _                  => ()

  val placement = pane.frame.arrange(Rect(0, 0, root.width, height))

  pane.leaves.zip(placement.cells).each: pair =>
    val extent = FlowExtent(root, pair._2)

    pair._1 match
      case Pane.Leaf(_, content)   => content(extent)
      case Pane.Widget(_, widget)  => widget.render(extent, false)
      case _                       => ()

    extent.flush()

  root.flush()

// Present `pane` as a static, variable-height block inline at the cursor, leaving
// the cursor on a fresh line below it.
def layout(pane: Pane)(using terminal: Terminal): Unit =
  val root = InlineRoot(terminal)
  paint(root, pane)
  root.finish()
