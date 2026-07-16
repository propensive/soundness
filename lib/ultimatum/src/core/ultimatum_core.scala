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

import anticipation.*
import denominative.*
import parasite.*
import profanity.*
import quantitative.*
import rudiments.*
import spectacular.*
import symbolism.*
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
  ( content: (Extent^) ?-> Unit )
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
def file(panes: Pane*): Pane = Pane.Branch(Sizing(), Axis.File, Panes(panes*))

// A column split over a live container, whose children can change while running.
def file(panes: Panes): Pane = Pane.Branch(Sizing(), Axis.File, panes)

// A split whose children stack as rows (distributing height).
def rank(panes: Pane*): Pane = Pane.Branch(Sizing(), Axis.Rank, Panes(panes*))

// A row split over a live container, whose children can change while running.
def rank(panes: Panes): Pane = Pane.Branch(Sizing(), Axis.Rank, panes)

// Wrap `child` in a box-drawing border. Each requested side becomes a thin leaf
// panel whose content is regenerated from its solved size, so an edge always
// spans the bordered region exactly and re-rules itself when the layout resizes.
// A corner is drawn only where two requested sides meet, so a partial border
// (e.g. only `top`) is a single rule with no corners. The border reserves one
// extra row or column on each requested side, propagated by the solver (each
// edge's fixed bound flows up as the cross-axis limit of its band).
def border
  ( style:  BorderStyle = BorderStyle.light,
    top:    Boolean     = true,
    right:  Boolean     = true,
    bottom: Boolean     = true,
    left:   Boolean     = true )
  ( child: Pane )
:   Pane =

  // A horizontal rule filling its width: a single fixed-height row.
  def horizontalRule: Pane = panel(minHeight = 1, maxHeight = 1):
    val extent = summon[Extent]
    extent.move(Prim, Prim)
    extent.put(style.horizontal*extent.width)

  // A vertical rule filling its height: a single fixed-width column.
  def verticalRule: Pane = panel(minWidth = 1, maxWidth = 1):
    val extent = summon[Extent]
    var row = 0

    while row < extent.height do
      extent.move(Prim, row.z)
      extent.put(style.vertical)
      row += 1

  // A single cell holding one corner glyph.
  def corner(glyph: Text): Pane =
    panel(minWidth = 1, maxWidth = 1, minHeight = 1, maxHeight = 1)(summon[Extent].put(glyph))

  // The middle band: the child flanked by whichever vertical edges are requested.
  val middle =
    val edge = if left then List(verticalRule) else Nil
    file((edge ++ List(child) ++ (if right then List(verticalRule) else Nil))*)

  // A horizontal band (the top or bottom): a rule flanked by whichever corners
  // are requested (a corner appears only where a vertical edge also meets it).
  def band(leftCorner: Text, rightCorner: Text): Pane =
    val start = if left then List(corner(leftCorner)) else Nil
    file((start ++ List(horizontalRule) ++ (if right then List(corner(rightCorner)) else Nil))*)

  val head = if top then List(band(style.topLeft, style.topRight)) else Nil
  val foot = if bottom then List(band(style.bottomLeft, style.bottomRight)) else Nil
  rank((head ++ List(middle) ++ foot)*)

// Drive an interactive layout, looping over terminal events until the user exits.
// Used inside `interactive`. In `Fullscreen` mode the layout takes over the
// terminal via the alternate screen buffer (restored on exit) and fills its
// height; in `Inline` mode it renders a variable-height block at the cursor
// without the alternate buffer, leaving scrollback intact.
def form(mode: Mode = Mode.Fullscreen)(pane: Pane)
  ( using terminal: Terminal,
          monitor:   Monitor,
          probate:   Probate,
          anchoring: InlineAnchoring,
          growth:    InlineGrowth,
          shrink:    InlineShrink )
:   Unit =

  // A container mutation wakes the loop by putting a redraw event on the spool.
  val wake = () => terminal.events.put(TerminalInfo.Redraw)

  mode match
    case Mode.Fullscreen =>
      profanity.terminalFeatures.alternateScreenFeature:
        // A buffered root: panels composite into its in-memory grid and each present
        // diffs against what is already on screen, so unchanged cells are never
        // re-emitted (no flicker). `cursor(false)` is recorded now and applied by the
        // first present; `finish` re-shows the cursor on the way out.
        val root = ScreenRoot(terminal)
        root.cursor(false)

        try Form(root, mode, pane, wake).run(terminal.eventIterator())
        finally root.finish()

    case Mode.Inline =>
      // A deferred resize repaint is woken by posting a `Redraw` after the remaining
      // window (plus a small margin so it lands past it).
      val scheduleWake = (delay: Long) =>
        async:
          snooze((delay + 16).toDouble*Milli(Second))
          terminal.events.put(TerminalInfo.Redraw)

        ()

      // Resize repaints are throttled to ~10/second and debounced by 50 ms of quiet,
      // so a drag must pause before the block is redrawn; typing stays immediate.
      Form(InlineRoot(terminal), mode, pane, wake, 100, 50, scheduleWake)
      . run(terminal.eventIterator())

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
def paint(root: Canvas^, pane: Pane): Unit =
  val height = root match
    case _: InlineRoot => pane.frame.measure(Axis.Rank).min
    case _             => root.height

  root match
    case inline: InlineRoot => inline.reframe(root.width, height)
    case screen: ScreenRoot => screen.reframe()
    case _                  => ()

  val placement = pane.frame.arrange(Rect(0, 0, root.width, height))

  // An explicit iterator loop rather than `.each`: the per-cell closure constructs a `FlowExtent`
  // over the `root` canvas (a capability), and capture checking rejects that fresh capability
  // leaking out through the `.each` lambda's inferred parameter type.
  val cells = pane.leaves.zip(placement.cells).iterator
  while cells.hasNext do
    val (leaf, rect) = cells.next()
    val extent = FlowExtent(root, rect)

    leaf match
      case Pane.Leaf(_, content)   => content(extent)
      case Pane.Widget(_, widget)  => widget.render(extent, false)
      case _                       => ()

    extent.flush()

  root.flush()

// Present `pane` as a static, variable-height block inline at the cursor, leaving
// the cursor on a fresh line below it.
def layout(pane: Pane)
  ( using terminal:  Terminal,
          anchoring: InlineAnchoring,
          growth:    InlineGrowth,
          shrink:    InlineShrink )
:   Unit =

  val root = InlineRoot(terminal)
  paint(root, pane)
  root.finish()
