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

// Drive an interactive layout across the whole terminal, looping over terminal
// events until the user exits. Used inside `interactive`.
def form(fullScreen: Boolean = true)(pane: Pane)(using terminal: Terminal): Unit =
  Form(terminal, fullScreen, pane).run(terminal.eventIterator())

// The leaf indices that must be repainted: any whose rectangle moved or resized
// since the last layout, plus any whose content changed this frame. A leaf whose
// rectangle and content are both unchanged is omitted, so it is left untouched on
// screen — the basis of flicker-free redraw.
def dirtyCells(previous: IndexedSeq[Rect], current: IndexedSeq[Rect], changed: Set[Int]): Set[Int] =
  val moved = current.indices.filter: i =>
    i >= previous.length || previous(i) != current(i)

  moved.to(Set) ++ changed

// Solve `pane` against `root` and paint each leaf's content into its rectangle.
// In full-screen mode the layout fills the surface's height; otherwise it takes
// only the height its content requires.
def paint(root: Canvas, fullScreen: Boolean)(pane: Pane): Unit =
  val height = if fullScreen then root.height else pane.frame.measure(Axis.Rank).min
  val placement = pane.frame.arrange(Rect(0, 0, root.width, height))

  pane.leaves.zip(placement.cells).each: pair =>
    val extent = FlowExtent(root, pair._2)

    pair._1 match
      case Pane.Leaf(_, content)   => content(extent)
      case Pane.Widget(_, widget)  => widget.render(extent, false)
      case _                       => ()

    extent.flush()

  root.flush()

// Solve and paint `pane` across the whole terminal (the common entry point,
// used inside `interactive`).
def layout(fullScreen: Boolean = true)(pane: Pane)(using terminal: Terminal): Unit =
  paint(TerminalCanvas(terminal), fullScreen)(pane)
