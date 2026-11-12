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
import scala.collection.immutable.Vector

import rudiments.*
import tessellate.*
import turbulence.Stdio
import vacuous.*

// The namespace for ultimatum's terminal-interface vocabulary. It holds the layout model: the
// content-bearing `Pane` tree that `panel`, `strip`, `stack` and `grid` build, the pure `Frame`
// it projects to for solving, and the `Placement` of `Region`s that solving produces. Nested
// here because each of these names, bare in the umbrella, would claim a far more general
// meaning than ultimatum gives it.
object Tui:
  // How a split arranges its children, and so which axis it divides. `Strip` places them side by
  // side, distributing width; `Stack` puts them one above another, distributing height; `Grid`
  // flows them row-major into `columns` columns whose widths are negotiated across every row (so
  // cells in a column align, as in a table), with `gap` blank cells between columns and between
  // rows. The names match the `strip`, `stack` and `grid` constructors that build each, so the
  // value a split carries and the call that produced it read the same.
  enum Arrangement:
    case Stack
    case Strip
    case Grid(columns: Int, gap: Int = 0)

  // How a frame is sized within its parent. `fraction` is its relative weight in
  // the parent's split direction — fractions 2, 3, 4 claim 2/9, 3/9, 4/9 of the
  // axis. The min/max values are content-driven bounds on each axis; a child whose
  // fair share would fall outside its bound on the split axis is fixed at that
  // bound and removed from the fractional pool. An `Unset` maximum is unbounded.
  case class Sizing
    ( fraction:  Double        = 1.0,
      minWidth:  Int           = 0,
      maxWidth:  Optional[Int] = Unset,
      minHeight: Int           = 0,
      maxHeight: Optional[Int] = Unset )

  // The resolved minimum and maximum size of a frame along one axis. An `Unset`
  // maximum is unbounded.
  case class Limits(min: Int, max: Optional[Int])

  // How much of the terminal an interactive layout occupies. `Fullscreen` takes over
  // the terminal via the alternate screen buffer and fills its fixed height;
  // `Inline` renders a variable-height block at the cursor (sized to its content),
  // without the alternate buffer, leaving the terminal's scrollback intact.
  enum Occupancy:
    case Inline, Fullscreen

  // A rectangular region within a surface, in top-origin coordinates. `left` and
  // `top` are zero-based offsets from the enclosing surface's top-left corner;
  // `width` and `height` are cell counts. These are internal layout numbers, so
  // plain `Int`s rather than `Ordinal`s.
  case class Region(left: Int, top: Int, width: Int, height: Int):
    def right: Int = left + width
    def bottom: Int = top + height

    def contains(column: Int, row: Int): Boolean =
      left <= column && column < right && top <= row && row < bottom

  object Frame:
    // Combine two optional maxima as a minimum, treating `Unset` as +infinity.
    private def lesser(a: Optional[Int], b: Optional[Int]): Optional[Int] =
      a.lay(b): av =>
        b.lay(av): bv => av.min(bv)

    // Min/max along the split axis: the minimum is the larger of the split's own
    // minimum and the SUM of its children's minima (a container can be no smaller
    // than its contents); the maximum is the smaller of its own maximum and the
    // sum of its children's maxima (any unbounded child makes the sum unbounded).
    private def alongLimits(own: Limits, children: List[Limits]): Limits =
      val minSum = children.fold(0): (acc, child: Limits) => acc + child.min

      val maxSum: Optional[Int] = children.fold(0: Optional[Int]): (acc, child: Limits) =>
        acc.let: total => child.max.let(total + _)

      Limits(own.min.max(minSum), lesser(own.max, maxSum))

    // Min/max across the cross axis: the minimum is the largest child minimum (the
    // cross extent must hold every child); the maximum is the smallest child
    // maximum.
    private def crossLimits(own: Limits, children: List[Limits]): Limits =
      val minMax = children.fold(0): (acc, child: Limits) => acc.max(child.min)

      val maxMin = children.fold(Unset: Optional[Int]): (acc, child: Limits) =>
        lesser(acc, child.max)

      Limits(own.min.max(minMax), lesser(own.max, maxMin))

    // Each grid column's limits, folded over the children occupying it (row-major order): a
    // column must hold its widest child minimum and may not exceed any child's cap.
    private def gridColumns(childWidths: Vector[Limits], columns: Int): Vector[Limits] =
      val cols = columns.max(1).min(childWidths.length.max(1))

      Vector.tabulate(cols): column =>
        val members = childWidths.indices.filter(_%cols == column).map(childWidths(_))
        val min = members.map(_.min).maxOption.getOrElse(0)
        val max = members.map(_.max).foldLeft(Unset: Optional[Int])(lesser)
        Limits(min, max)

    // Each grid row's height: content-sized, at the tallest minimum among the row's children.
    private def gridRows(childHeights: Vector[Limits], columns: Int): Vector[Int] =
      val cols = columns.max(1).min(childHeights.length.max(1))

      childHeights.grouped(cols).to(Vector).map: row => row.map(_.min).maxOption.getOrElse(0)

    // A grid's width limits: its columns laid side by side with gaps, like a strip of the
    // per-column folds.
    private def gridWidthLimits
      ( own: Limits, childWidths: Vector[Limits], columns: Int, gap: Int )
    :   Limits =

      val cols = gridColumns(childWidths, columns)
      val gaps = gap*(cols.length - 1).max(0)
      val minSum = cols.map(_.min).sum + gaps

      val maxSum: Optional[Int] = cols.foldLeft(gaps: Optional[Int]): (acc, column) =>
        acc.let: total => column.max.let(total + _)

      Limits(own.min.max(minSum), lesser(own.max, maxSum))

    // A grid's height limits: its rows are content-sized, so the grid is rigid at their total.
    private def gridHeightLimits
      ( own: Limits, childHeights: Vector[Limits], columns: Int, gap: Int )
    :   Limits =

      val rows = gridRows(childHeights, columns)
      val height = rows.sum + gap*(rows.length - 1).max(0)
      Limits(own.min.max(height), lesser(own.max, height))

  // A node in a layout tree: a `Cell` (a leaf panel that hosts content) or a
  // `Split` that divides its space among children along an `Arrangement`. Solving against
  // a root `Region` runs two passes: a bottom-up MEASURE computing each node's (min,
  // max) on each axis (forcing a split's minimum up to the aggregate of its
  // children), then a top-down ARRANGE distributing space by fraction, fixing any
  // child that hits a bound, and recursing.
  enum Frame:
    def sizing: Sizing

    case Cell(sizing: Sizing)
    case Split(sizing: Sizing, arrangement: Arrangement, children: List[Frame])

    // The resolved (min, max) of this frame along the axis `arrangement` selects: `Strip` for
    // width, anything else for height.
    def measure(arrangement: Arrangement): Limits =
      val own = arrangement match
        case Arrangement.Strip => Limits(sizing.minWidth, sizing.maxWidth)
        case _                 => Limits(sizing.minHeight, sizing.maxHeight)

      this match
        case Cell(_) =>
          own

        case Split(_, Arrangement.Grid(columns, gap), children) =>
          // The stdlib view gives the grid solvers below a stdlib `Vector` to index and slice.
          val childLimits = children.map(_.measure(arrangement)).stdlib.toVector

          arrangement match
            case Arrangement.Strip => Frame.gridWidthLimits(own, childLimits, columns, gap)
            case _                 => Frame.gridHeightLimits(own, childLimits, columns, gap)

        case Split(_, splitArrangement, children) =>
          val childLimits = children.map(_.measure(arrangement))

          if splitArrangement == arrangement then Frame.alongLimits(own, childLimits)
          else Frame.crossLimits(own, childLimits)

    // Solve this frame against a rectangle, producing a tree of placements.
    def arrange(region: Region): Placement = this match
      case Cell(_) =>
        Placement.Cell(region)

      case Split(_, Arrangement.Grid(columns, gap), children) =>
        // Column widths are negotiated across every row with the shared solver (each column's
        // fold of its members' limits, weighted by its members' largest fraction); rows are
        // content-sized and stacked, clamped to the grid's rectangle.
        // The stdlib view is taken once, indexed and sliced throughout the grid arrangement
        // below.
        val childList = children.stdlib.toVector
        val widthLimits = childList.map(_.measure(Arrangement.Strip))
        val cols = columns.max(1).min(childList.length.max(1))

        val tracks = Vector.tabulate(cols): column =>
          val members = childList.indices.filter(_%cols == column)
          val limits = Frame.gridColumns(widthLimits, cols)(column)
          val weight = members.map(childList(_).sizing.fraction).maxOption.getOrElse(1.0)
          Flex(Metrics(limits.min), weight, limits.max)

        // The stdlib view of the solved widths is scanned and indexed as a `Vector`.
        val colWidths = Flex.solve(Sequence.from(tracks), region.width, gap).stdlib.map(_.or(0))
        val xs = colWidths.scanLeft(region.left): (x, width) => x + width + gap
        val rowHeights = Frame.gridRows(childList.map(_.measure(Arrangement.Stack)), cols)
        val ys = rowHeights.scanLeft(region.top): (y, height) => y + height + gap

        val placements = childList.indices.map: index =>
          val row = index/cols
          val column = index%cols
          val top = ys(row)
          val height = rowHeights(row).min((region.top + region.height - top).max(0))
          childList(index).arrange(Region(xs(column), top, colWidths(column), height))

        Placement.Split(region, placements.to(List))

      case Split(_, arrangement, children) =>
        val available = arrangement match
          case Arrangement.Strip => region.width
          case _                 => region.height

        // Each child is a flex track: its measured minimum, its fraction as the weight with
        // which it claims spare space, and its measured maximum as a hard cap. No track is
        // collapsible, so every solved size is defined.
        val tracks: List[Flex] =
          children.map: child =>
            val limits = child.measure(arrangement)
            Flex(Metrics(limits.min), child.sizing.fraction, limits.max)

        // The stdlib view of the solved sizes, as above, is scanned and zipped as a `Vector`.
        val sizes = Flex.solve(tracks.to[Sequence], available).stdlib.map(_.or(0))

        val start = arrangement match
          case Arrangement.Strip => region.left
          case _                 => region.top

        val offsets = sizes.scanLeft(start)(_ + _)

        // Zip each child directly with its solved size and offset (`scanLeft` yields n + 1
        // offsets; the zip truncates to the n children), so no re-indexing is needed.
        // stdlib bridge: `lazyZip` is a stdlib-only three-way zip.
        val placements = children.stdlib.lazyZip(sizes).lazyZip(offsets).map:
          (child, size, offset) =>
            val childRegion = arrangement match
              case Arrangement.Strip => Region(offset, region.top, size, region.height)
              case _                 => Region(region.left, offset, region.width, size)

            child.arrange(childRegion)

        Placement.Split(region, placements.to(List))

  // The result of solving a layout: a tree of rectangles mirroring the `Frame`
  // tree that produced it. `cells` lists the leaf rectangles in frame order (left
  // to right for files, top to bottom for ranks).
  enum Placement:
    def region: Region

    case Cell(region: Region)
    case Split(region: Region, children: List[Placement])

    def cells: List[Region] = this match
      case Cell(region)       => List(region)
      case Split(_, children) => children.bind(_.cells)

  // The content-bearing layout tree built by the `file`/`rank`/`panel` DSL. A `Leaf` pairs a
  // `Sizing` with deferred content (run later, once its rectangle is known, with its `Extent` in
  // context); a `Branch` splits its space among children along an `Arrangement`. A pane projects to
  // a pure `Frame` for solving (via `frame`) and yields its leaves in order (via `leaves`), so a
  // solved `Placement`'s cells line up one-to-one with the leaves. A pane tree is pure: a `Leaf`'s
  // deferred `content` is a *pure* function of its `Extent` (an `Extent` is itself the
  // `Stdio`/`Board` it renders into, so content only uses what it is given and captures nothing
  // ambient), so panes can be freely collected and traversed under capture checking.
  enum Pane:
    def sizing: Sizing

    case Leaf(sizing: Sizing, content: Extent^ -> Unit)
    case Widget(sizing: Sizing, fixture: Fixture)
    case Branch(sizing: Sizing, arrangement: Arrangement, panes: Panes)

    // The pure layout structure, with content discarded, for the solver. A widget
    // contributes only its declared `Sizing` here; the interactive driver injects
    // each widget's live intrinsic size (measured at its actual width) when it
    // re-solves, so growing content can push the rest of the layout around. A
    // branch reads its container's current contents, so a mutated layout re-solves.
    def frame: Frame = this match
      case Leaf(sizing, _)   => Frame.Cell(sizing)
      case Widget(sizing, _) => Frame.Cell(sizing)

      case Branch(sizing, arrangement, panes) =>
        Frame.Split(sizing, arrangement, panes.contents.map(_.frame).to[List])

    // The leaves in frame order (left to right for files, top to bottom for ranks).
    def leaves: List[Pane] = this match
      case Branch(_, _, panes) => panes.contents.to[List].bind(_.leaves)
      case leaf                => List(leaf)

    // A copy of this pane re-weighted for use as a child of a split.
    def weight(fraction: Double): Pane = this match
      case Leaf(sizing, content)   => Leaf(sizing.copy(fraction = fraction), content)
      case Widget(sizing, fixture) => Widget(sizing.copy(fraction = fraction), fixture)

      case Branch(sizing, arrangement, panes) =>
        Branch(sizing.copy(fraction = fraction), arrangement, panes)

  // A mutable, ordered container of child panes, backed by a `Sequence` for random
  // access. Holding a reference to it lets the layout change while a `form` is
  // running: appending a pane, or inserting one before or after an existing pane,
  // re-tiles the running form. Mutations are picked up the next time the form
  // re-derives its tree; when the container is bound into a running form, a
  // mutation also wakes the event loop so the change is shown immediately (even
  // from a background task).
  // Panes themselves are pure (a pane tree captures nothing), so `Panes` need not be a capability.
  // Its one effectful field is the installed repaint callback.
  class Panes(initial: Pane*):
    // Internally a raw `Vector`: this is imperative container state, and the mutation operations
    // (`patch`, `indexWhere`, `:+`) belong to the stdlib surface. The public API exposes
    // `Sequence`. The container is mutated from background tasks, so the vector lives in an atomic
    // cell and each mutation is one compare-and-set transition: concurrent mutations cannot lose a
    // pane.
    private val vector: Atomic.Ref[Vector[Pane]] = Atomic.Ref(initial.to(Vector))

    // Installed by the running form so a mutation requests a repaint; a no-op until the container
    // is bound. Typed as a *pure* function so a pane tree (and `Panes`) captures nothing and can be
    // freely collected and traversed; the installed callback genuinely captures the running form's
    // event loop, reconciled in `bindWake`. Atomic so that a mutating task sees the callback the
    // form bound.
    private val onChange: Atomic.Ref[() -> Unit] = Atomic.Ref { () => () }

    // Install the running form's repaint trigger. The callback captures the form's event loop,
    // which outlives this assignment, so it escapes into the long-lived container — a growing
    // capture set that capture checking cannot yet track. The escape is sound by construction:
    // `onChange` is re-bound on every `run` (so it never references a finished form) and is only
    // ever called from within a mutation while that form is live. Hence the single, localised
    // `unsafeAssumePure`.
    private[ultimatum] def bindWake(wake: () => Unit): Unit =
      // [field-purity] form wake callback escapes into long-lived container
      onChange() = caps.unsafe.unsafeAssumePure(wake)

    def contents: Sequence[Pane] = Sequence.from(vector())
    def size: Int = vector().length
    def apply(index: Int): Pane = vector()(index)

    // Apply one transition atomically, then wake the form. The transition is re-run if another
    // mutation lands first, which is why each one reads the vector it is given, not the cell.
    private inline def alter(inline transition: Vector[Pane] => Vector[Pane]): Unit =
      vector.since(transition)
      onChange()()

    // Insert at a position, clamped to the vector's bounds.
    private def place(panes: Vector[Pane], index: Int, pane: Pane): Vector[Pane] =
      panes.patch(index.min(panes.length).max(0), Vector(pane), 0)

    def append(pane: Pane): Unit = alter(_ :+ pane)
    def prepend(pane: Pane): Unit = alter(pane +: _)

    // Insert at a position, clamped to the container's bounds.
    def insert(index: Int, pane: Pane): Unit = alter(place(_, index, pane))

    // Insert immediately before `reference` (by identity); appends if it is absent.
    def insertBefore(reference: Pane, pane: Pane): Unit = alter: panes =>
      val index = panes.indexWhere(_ eq reference)
      if index < 0 then panes :+ pane else place(panes, index, pane)

    // Insert immediately after `reference` (by identity); appends if it is absent.
    def insertAfter(reference: Pane, pane: Pane): Unit = alter: panes =>
      val index = panes.indexWhere(_ eq reference)
      if index < 0 then panes :+ pane else place(panes, index + 1, pane)

    // Remove `reference` (by identity), if present.
    def remove(reference: Pane): Unit = alter(_.filter(_ ne reference))

  object Block:
    import anticipation.*
    import denominative.*
    import escapade.*
    import gossamer.*
    import profanity.*
    import symbolism.*
    import turbulence.*
    import vacuous.*

    object Anchoring:
      given default: Anchoring = TopAfterResize

    // How an inline block relocates when the terminal is resized. `Block` reads
    // this from context; the default (`TopAfterResize`) is provided in the companion,
    // and a caller overrides it by importing one alternative from `inlineAnchoring`.
    enum Anchoring:
      case BottomDocked    // never switch; clear and re-dock on every resize
      case TopAnchored     // pinned to rows 1..height from the first frame
      case TopAfterResize  // bottom-docked, then top-anchored after the first resize
      case Fullscreen      // take over the alternate screen buffer, top-anchored
      case Flow            // render relative to the cursor, flowing with prior output

    object Growth:
      given default: Growth = ScrollIntoScrollback

    // What happens when a frame is taller than the last while bottom-docked. The
    // default (`ScrollIntoScrollback`) preserves the historic behaviour.
    enum Growth:
      case ScrollIntoScrollback  // scroll the screen up, pushing rows into scrollback
      case ClampToScreen         // grow upward in place, overwriting the rows above

    object Shrink:
      given default: Shrink = RedockBottom

    // What happens when a frame is shorter than the last. The default
    // (`RedockBottom`) preserves the historic behaviour.
    enum Shrink:
      case RedockBottom  // re-dock at the bottom, clearing the rows vacated above
      case KeepTop       // hold the top row and clear below (no re-dock gap)

    // The root for inline mode over a real terminal: width and the height clamp are
    // read live, so a resize is reflected on the next frame. The rendering policy is
    // read from context (`Anchoring`/`Growth`/`Shrink`), so it is fixed at the caller's site
    // rather than inside the library.
    def apply(terminal: Terminal)
      ( using anchoring: Anchoring, growth: Growth, shrink: Shrink )
    :   Block^{terminal.rd, scala.caps.any} =

      new Block(() => terminal.knownColumns, () => terminal.knownRows)
        ( using terminal.stdio, anchoring, growth, shrink )

    def apply(width: Int, height: Int)
      ( using Stdio, Anchoring, Growth, Shrink )
    :   Block^ =

      new Block(() => width, () => height)

  // The root `Board` for INLINE mode: panels composite into its character grid
  // (inherited from `GridSurface`), and `flush` presents the grid with ABSOLUTE row
  // addressing (`cup`) so a redraw always lands in the same place and never drifts —
  // the property a relative, cursor-tracking present loses the moment a resize reflows
  // the lines already on screen. Anchoring, growth and shrink are each governed by a
  // contextual policy (`Anchoring`/`Growth`/`Shrink`); the defaults
  // reproduce the historic behaviour. Under the default `TopAfterResize` anchoring the
  // block is BOTTOM-docked on launch (appearing inline beneath the shell command) and
  // switches to TOP-anchored — pinned to rows 1..height — on the first resize, because
  // a bottom-docked block leaves a gap (and strands a ghost) when the terminal grows
  // taller, whereas a top-anchored one is simply overdrawn in place. A top-anchored
  // resize clears the whole screen first (the block relocates here and the terminal may
  // have moved the old one); otherwise each row is cleared in place. `Fullscreen`
  // additionally wraps the session in the alternate screen buffer. `widthFn`/`heightFn`
  // supply the live terminal columns and rows, re-read every frame; an oversize block
  // is clamped to the terminal height.
  class Block(widthFn: () ->{scala.caps.any.rd} Int, heightFn: () ->{scala.caps.any.rd} Int)
    ( using stdio:     Stdio,
            anchoring: Block.Anchoring,
            growth:    Block.Growth,
            shrink:    Block.Shrink )
  extends GridSurface(widthFn(), 0):
    import anticipation.*
    import denominative.*
    import escapade.*
    import gossamer.*
    import profanity.*
    import symbolism.*
    import turbulence.*
    import vacuous.*

    private var presentedRows: Int = 0
    private var presentedColumns: Int = 0
    private var presentedTop: Int = 1

    // For `Inline` anchoring: the row offset (from the block's top) at which the cursor was
    // left after the last frame, so the next frame can rise back to the top relatively.
    private var flowCursorRow: Int = 0

    // Start top-anchored only when the policy pins the block from the first frame;
    // `TopAfterResize` starts bottom-docked and flips on the first `invalidate`.
    private var topAnchored: Boolean = anchoring match
      case Block.Anchoring.TopAnchored | Block.Anchoring.Fullscreen      => true
      case Block.Anchoring.BottomDocked | Block.Anchoring.TopAfterResize => false
      case Block.Anchoring.Flow                                          => false

    private var started: Boolean = false

    // Where the terminal reported the parked cursor cell after the last resize's reflow
    // (1-based screen coordinates), stashed by the driver from the anchor query's reply
    // and consumed by the next resized present — so a stale anchor can never inform a
    // later resize it didn't measure.
    private var anchorCell: Optional[(Int, Int)] = Unset

    // Stash the anchor reply for the next resized present. A reflowing terminal keeps
    // the cursor attached to the logical cell it was on, and every present parks the
    // cursor at the caret cell, so this reveals where that known cell landed.
    update def anchor(row: Int, column: Int): Unit = anchorCell = (row, column)

    override def width: Int = widthFn()

    // Resize the grid to the measured block height, clamped to the live terminal
    // height; called by the driver before compositing each frame.
    update def reframe(width: Int, height: Int): Unit = reshape(width, height.min(heightFn()))

    // Cursor visibility is deferred like the caret: recorded now, applied by `flush`,
    // so a focused editor shows it and a focused menu keeps it hidden.
    update def cursor(visible: Boolean): Unit = caretVisible = visible

    // `invalidate()` (inherited) marks the next present as a resize repaint; the driver
    // calls it on every `WindowSize` event (a width-only resize counts too). Under
    // `TopAfterResize`, the switch to top-anchoring no longer happens there: the resized
    // present first tries to recover the block's position from the anchor reply, and
    // flips (latched) only when that recovery is unavailable or fails.

    // Forget everything recorded about what is on screen, so the NEXT frame renders as a
    // fresh first frame at the current cursor rather than relative to (or docked against)
    // a prior block. Intended for use after the caller has cleared the screen — e.g. an
    // `Inline` driver that, on a terminal resize, wipes the reflowed screen and restarts
    // the flow from the top rather than trying to reconcile the old, now-garbled layout.
    update def reset(): Unit =
      started = false
      presentedRows = 0
      presentedColumns = 0
      presentedTop = 1
      flowCursorRow = 0
      invalidated = false
      snapshot = Unset
      anchorCell = Unset
      presentedCaretRow = -1
      presentedCaretColumn = -1
      presentedCaretVisible = false

    // Record the caret's block-local target; `flush` positions the hardware cursor at
    // the corresponding absolute screen cell.
    override update def showCaret(column: Ordinal, row2: Ordinal): Unit =
      caretColumn = column.n0
      caretRow = row2.n0

    // `Inline` anchoring renders the block RELATIVE to the cursor, so it appears wherever
    // prior output has flowed to (right after the previous result) and never docks to a
    // fixed edge. The block's top is not a fixed screen row: on the first frame it is drawn
    // at the current cursor; on later frames the cursor is risen back to the top via the
    // recorded `flowCursorRow`. Growing scrolls the screen (and the block) naturally as the
    // last row's newline hits the foot; shrinking clears the freed rows below, holding the
    // block's top so the box stays put (freed space becomes blank screen below, not a gap).
    private update def flushInline(): Unit =
      val columns = gridWidth.min(widthFn())
      val h       = height
      invalidated = false

      // `Inline` anchoring addresses the screen RELATIVELY, so an absolute snapshot can
      // never describe where its cells really landed; drop it to keep the invariant that
      // a snapshot always refers to an absolutely-addressed present.
      snapshot = Unset

      val frame = StringBuilder()
      def emit(text: Text): Unit = frame.append(text.s)
      val termcap = summon[Stdio].termcap

      emit(csi.dectcem(false))

      // Rise to the block's top-left. On the first frame the cursor is already there.
      emit(t"\r")
      if started && flowCursorRow > 0 then emit(csi.cuu(flowCursorRow))
      started = true

      // Draw the block's rows from the top down; a trailing newline past the screen foot
      // scrolls the block up with the rest of the screen (relative addressing tracks it).
      var r = 0

      while r < h do
        emit(csi.el(2))
        val rendered = trimmedRowContent(r, columns).render(termcap)
        emit(rendered)
        if rendered.contains(t"\e") then emit(csi.sgr(0))
        emit(t"\r")
        if r < h - 1 then emit(t"\n")
        r += 1
      // The cursor now sits at the block's last drawn row (h - 1), column 0.

      // Shrink: clear the rows a taller previous block left below, then return to the last
      // row. The top does not move, so the box stays put and the freed rows go blank.
      if presentedRows > h then
        var k = h

        while k < presentedRows do
          emit(t"\n")
          emit(csi.el(2))
          k += 1

        emit(csi.cuu(presentedRows - h))

      presentedRows = h
      presentedColumns = columns

      // Place the caret relative to the block's last row (where the cursor is now).
      val cr = caretRow.min(h - 1).max(0)
      if h - 1 - cr > 0 then emit(csi.cuu(h - 1 - cr))
      emit(t"\r")
      val cc = caretColumn.min(columns - 1).max(0)
      if cc > 0 then emit(csi.cuf(cc))
      flowCursorRow = cr
      if caretVisible then emit(csi.dectcem(true))

      Out.print(frame.toString.tt)

    update def flush(): Unit =
      if anchoring == Block.Anchoring.Flow then flushInline() else flushDocked()

    private update def flushDocked(): Unit =
      val rows    = heightFn()
      val columns = gridWidth.min(widthFn())
      val h       = height

      // When the geometry is exactly as last presented (same dock row, height and width,
      // and not a resize), every grid cell maps to the same screen cell as the snapshot,
      // so the present is a pure overprint and only the cells that changed need emitting.
      // Any geometry change falls back to the full redraw, which also handles docking,
      // growth, shrink-clearing and resize-clearing.
      val dockTop = if topAnchored then 1 else (rows - h + 1).max(1)

      val validated =
        if !invalidated && started && h == presentedRows && columns == presentedColumns &&
          dockTop == presentedTop
        then snapshotValid(dockTop, columns, h)
        else Unset

      validated.let(presentDiff(dockTop, columns, h, _)).or(flushDockedFull(rows, columns, h))

    private update def flushDockedFull(rows: Int, columns: Int, h: Int): Unit =
      val resized = invalidated
      invalidated = false

      // The whole frame is built up and emitted as ONE write, so the SIGWINCH handler's
      // size-probe sequence (which jumps the cursor to the corner and back) cannot
      // interleave between our escapes and corrupt the redraw.
      val frame = StringBuilder()
      def emit(text: Text): Unit = frame.append(text.s)

      // `Fullscreen` takes over the alternate screen buffer for the session: enter it on
      // the first present (restored by `finish`), so the block owns a blank screen.
      if anchoring == Block.Anchoring.Fullscreen && !started then emit(t"\e[?1049h")
      started = true

      emit(csi.dectcem(false))

      // The anchor is consumed by whichever present follows the resize that measured
      // it — used or not — so it can never inform a later resize.
      val anchor0 = anchorCell
      anchorCell = Unset

      // Today's resize behaviour, for when no anchor could reconcile the reflow: under
      // `TopAfterResize` the block gives up its dock — flip to top-anchoring (latched;
      // a terminal that failed to answer once won't start) and clear the whole screen;
      // otherwise clear a bottom band sized by the wrap heuristic.
      def unrecoveredResize(): Int =
        if anchoring == Block.Anchoring.TopAfterResize then
          topAnchored = true
          emit(csi.cup(1, 1))
          emit(csi.ed(0))
          1
        else
          val narrowed = presentedColumns > columns && columns > 0
          val wrap = if narrowed then (presentedColumns + columns - 1)/columns else 1
          val cleared = (presentedRows*wrap).max(h)
          emit(csi.cup((rows - cleared + 1).max(1), 1))
          emit(csi.ed(0))
          (rows - h + 1).max(1)

      // The block's absolute top row, and the resize/grow clearing for each anchoring.
      val top =
        if topAnchored then
          // Pinned to the top. A resize relocates the block here (on the first one) and
          // may have moved the old block, so clear the whole screen before redrawing.
          if resized then
            emit(csi.cup(1, 1))
            emit(csi.ed(0))

          1
        else
          if resized then
            // Recover the old block's position from the anchor: the terminal kept the
            // parked cursor attached to its cell through the reflow, so the residue's
            // top row is computable from the reply and the retained snapshot. Clear
            // exactly from there to the screen foot and re-dock — the block survives
            // the resize without wiping the scrollback history above it. When the
            // anchor is missing or cannot be reconciled, fall back to today's clears.
            val recovered: Optional[Int] = anchor0.let: (anchorRow, anchorColumn) =>
              residueTop(anchorRow, anchorColumn, columns, rows)

            recovered.lay(unrecoveredResize()): clearTop =>
              emit(csi.cup(clearTop, 1))
              emit(csi.ed(0))
              (rows - h + 1).max(1)
          else if h > presentedRows then
            // Grow beyond the previous height. `ScrollIntoScrollback` scrolls the screen
            // up so the extra rows are free at the bottom, pushing the content above into
            // scrollback (`cud` to the bottom first, since only a `\n` on the bottom line
            // scrolls); `ClampToScreen` instead grows upward in place, overwriting the
            // rows above without touching scrollback.
            growth match
              case Block.Growth.ScrollIntoScrollback =>
                emit(csi.cud(9999))
                emit(t"\r")
                emit(t"\n"*(h - presentedRows))

              case Block.Growth.ClampToScreen =>
                ()

            (rows - h + 1).max(1)
          else if presentedRows > h && shrink == Block.Shrink.KeepTop then
            // Hold the top row where the taller block began, rather than re-docking down.
            (rows - presentedRows + 1).max(1)
          else
            (rows - h + 1).max(1)

      // Remember where the block was actually drawn, so `finish` can drop the cursor onto
      // the row immediately after its last line rather than assuming it reaches the screen
      // foot — which is false when a shrink held the top (`KeepTop`) leaving blank below.
      presentedTop = top

      // Draw the block from its absolute `top` down, clipping each row to the live width
      // so it can never wrap (a wrapped row would scroll the screen and break addressing).
      emit(csi.cup(top, 1))

      // Each row is rendered to SGR from its styled cells, then reset (`sgr(0)`) so the
      // next absolutely-addressed, `el(2)`-cleared row starts from a clean style and no
      // colour bleeds across the frame.
      val termcap = summon[Stdio].termcap
      var r = 0

      while r < h do
        emit(csi.el(2))
        // Trailing default-styled blanks are trimmed (`el(2)` has cleared them): each
        // row becomes a hard logical line of its content width, so a resize's reflow
        // of these rows is predictable — the basis of the anchor recovery above.
        val rendered = trimmedRowContent(r, columns).render(termcap)
        emit(rendered)
        // Reset only after a row that actually emitted SGR, so a plain row is byte-for-
        // byte as before and no colour bleeds into the next `el(2)`-cleared row.
        if rendered.contains(t"\e") then emit(csi.sgr(0))
        emit(t"\r")
        if r < h - 1 then emit(t"\n")
        r += 1

      // Shrink: clear the rows a taller previous block vacated. Clearing happens BELOW
      // the block when top-anchored or when `KeepTop` holds the top in place; when
      // bottom-docked under `RedockBottom` the block re-docks down and the vacated rows
      // above are cleared instead (a resize already cleared them).
      if !resized && presentedRows > h then
        if topAnchored || shrink == Block.Shrink.KeepTop then
          var k = h

          while k < presentedRows do
            emit(t"\n")
            emit(csi.el(2))
            k += 1
        else
          var j = 0

          while j < presentedRows - h do
            emit(csi.cup((rows - presentedRows + 1 + j).max(1), 1))
            emit(csi.el(2))
            j += 1

      presentedRows = h
      presentedColumns = columns

      // The caret is the hardware cursor, parked at its absolute cell UNCONDITIONALLY —
      // a hidden cursor still has a position, and a reflowing terminal drags that
      // position with the cell it is on, so the next resize can ask where the block
      // went. Only visibility is conditional.
      emit(csi.cup((top + caretRow.min(h - 1)).max(1), caretColumn.min(columns - 1).max(0) + 1))
      if caretVisible then emit(csi.dectcem(true))

      // What was just drawn IS the screen now: record it (and the caret) so the next
      // geometry-stable present can diff against it.
      recordSnapshot(top, columns)
      recordCaret(top, columns, h)

      Out.print(frame.toString.tt)

    // Predict the topmost screen row the previous block's residue can occupy after a
    // resize, from the observed anchor cell and the retained snapshot. Two models cover
    // the real terminal population: a REFLOWING terminal (VTE, iTerm2, kitty, alacritty,
    // tmux, Terminal.app, foot, wezterm) re-wraps each of the block's hard logical lines
    // at the new width and keeps the cursor attached to its logical cell; a TRUNCATING
    // terminal (xterm, st, urxvt, screen, mosh) clips lines in place and leaves the
    // cursor's row alone. Each model predicts where the parked cursor should have landed
    // — row offset AND column — so the observed column discriminates them: trust the
    // model whose column matches; when both match take the safer (upper) minimum; when
    // neither matches the terminal did something unmodellable, so return `Unset` and let
    // the caller fall back to today's clears. The result is clamped to the screen.
    private update def residueTop(anchorRow: Int, anchorColumn: Int, newColumns: Int, rows: Int)
    :   Optional[Int] =

      snapshot.let: snap =>
        val oldH = snap.height
        val parkLocal = presentedCaretRow - snapshotTop     // the park cell, block-local
        val parkColumn = presentedCaretColumn - 1           // 0-based

        if parkLocal < 0 || parkLocal >= oldH || anchorRow < 1 || anchorRow > rows ||
          newColumns <= 0
        then Unset
        else
          // Simulate the terminal re-wrapping snapshot row `r`'s content (trimmed, so
          // exactly what was really printed) at width `w`: the number of physical
          // sub-rows it occupies, and the sub-row and column on which cell `target`
          // lands. A wide glyph that would straddle the margin is pushed whole to the
          // next sub-row, as terminals do. A target beyond the content (the park sat in
          // the `el(2)`-cleared tail) stays on the last sub-row at its clamped column —
          // erased cells are not content and never wrap.
          def wrapProfile(r: Int, w: Int, target: Int): (Int, Int, Int) =
            val width0 = contentWidth(snap, r, snapshotColumns)
            var c = 0
            var col = 0
            var sub = 0
            var targetSub = -1
            var targetColumn = 0

            while c < width0 do
              val grapheme = snap.grapheme(c.z, r.z)

              if grapheme.text.nil then
                // A wide-trailing sentinel: its lead already advanced past this cell.
                if c == target then
                  targetSub = sub
                  targetColumn = (col - 1).max(0)
              else
                val cellWidth = metric.width(grapheme)

                if cellWidth > 0 && col + cellWidth > w then
                  sub += 1
                  col = 0

                if c == target then
                  targetSub = sub
                  targetColumn = col

                col += cellWidth

              c += 1

            if targetSub < 0 then
              targetSub = sub
              targetColumn = target.min(w - 1).max(0)

            (sub + 1, targetSub, targetColumn)

          // Physical rows of block content above the park cell, per model, and the
          // column each model predicts for the anchor reply.
          var reflowAbove = 0
          var r = 0

          while r < parkLocal do
            reflowAbove += wrapProfile(r, newColumns, -1)(0)
            r += 1

          val (_, parkSub, parkNewColumn) = wrapProfile(parkLocal, newColumns, parkColumn)
          reflowAbove += parkSub

          val truncateAbove = parkLocal
          val reflowColumn = parkNewColumn + 1
          val truncateColumn = parkColumn.min(newColumns - 1) + 1

          val matchReflow = anchorColumn == reflowColumn
          val matchTruncate = anchorColumn == truncateColumn

          val clearTop: Optional[Int] =
            if matchReflow && !matchTruncate then anchorRow - reflowAbove
            else if matchTruncate && !matchReflow then anchorRow - truncateAbove
            else if matchReflow && matchTruncate
            then (anchorRow - reflowAbove).min(anchorRow - truncateAbove)
            else Unset

          clearTop.let: top => if top > rows then Unset else top.max(1)

    // On exit, drop the cursor onto a fresh line below the block and re-show it, so
    // subsequent output continues after the rendered block (like a submitted prompt).
    // A `Fullscreen` session leaves the alternate screen buffer, restoring what was
    // there before it started.
    update def finish(): Unit =
      // `Inline` drops the cursor onto a fresh line right below the block, RELATIVELY (from
      // the caret down to the block's last row, then a newline that scrolls if at the foot),
      // so the following output — and the next inline block — continues immediately after it.
      if anchoring == Block.Anchoring.Flow then
        val down = presentedRows - 1 - flowCursorRow
        if down > 0 then Out.print(csi.cud(down))
        Out.print(t"\r\n")
      else
        // Drop the cursor just past the block's LAST drawn row (`presentedTop + presentedRows
        // - 1`) rather than at the screen foot. For a full bottom-docked block the last row IS
        // the foot, so this is unchanged; but when a `KeepTop` shrink held the block high and
        // cleared blank below it, this lands the following output right under the block instead
        // of after the vacated rows. Top-anchored keeps its historic one-line trailing gap.
        val below =
          if topAnchored then (presentedRows + 1).min(heightFn())
          else (presentedTop + presentedRows - 1).max(1).min(heightFn())

        Out.print(csi.cup(below.max(1), 1))
        Out.print(t"\r\n")

      Out.print(csi.dectcem(true))
      if anchoring == Block.Anchoring.Fullscreen && started then Out.print(t"\e[?1049l")

  object Screen:
    import anticipation.*
    import denominative.*
    import escapade.*
    import gossamer.*
    import profanity.*
    import turbulence.*
    import vacuous.*

    // Build a fullscreen root covering the whole terminal, reading its size live (so a
    // resize is reflected the next time the layout is solved) and writing through its
    // `Stdio`. The size thunks retain the terminal, so the root honestly captures it.
    def apply(terminal: Terminal): Screen^{terminal.rd, scala.caps.any} =
      new Screen(() => terminal.knownColumns, () => terminal.knownRows)(using terminal.stdio)

    def apply(width: Int, height: Int)(using Stdio): Screen^ =
      new Screen(() => width, () => height)

  // The root `Board` for FULLSCREEN mode: panels composite into its character grid
  // (inherited from `GridSurface`), and `flush` presents by diffing the grid against
  // the snapshot of what the last present drew, overprinting only the damaged runs —
  // an unchanged cell is a no-op and an identical frame emits nothing at all. Every
  // present is ONE write, so partial frames never flash and the SIGWINCH size-probe
  // reply cannot interleave mid-frame. The first present, and any present after
  // `invalidate` (a resize, when the terminal has reflowed whatever was on screen),
  // redraws every row. It replaces the unbuffered `TerminalBoard` as the fullscreen
  // root, whose immediate per-`put` writes re-emitted every cell a repaint touched.
  class Screen(widthFn: () ->{scala.caps.any.rd} Int, heightFn: () ->{scala.caps.any.rd} Int)
    ( using stdio: Stdio )
  extends GridSurface(widthFn(), heightFn()):
    import anticipation.*
    import denominative.*
    import escapade.*
    import gossamer.*
    import profanity.*
    import turbulence.*
    import vacuous.*

    // The live terminal size, for the layout solver; the grid is re-fitted to it by
    // `reframe` before each solve, so the two only ever disagree mid-resize.
    override def width: Int = widthFn()
    override def height: Int = heightFn()

    // Re-fit the grid to the live terminal size. A real change blanks the grid and
    // invalidates the snapshot: the terminal has reflowed whatever was on screen, so
    // the next present must redraw everything.
    update def reframe(): Unit =
      if widthFn() != gridWidth || heightFn() != gridHeight then
        reshape(widthFn(), heightFn())
        invalidate()

    // Cursor visibility is deferred like the caret: recorded now, applied by `flush`,
    // so a focused editor shows it and a focused menu keeps it hidden.
    update def cursor(visible: Boolean): Unit = caretVisible = visible

    // Record the caret's target cell; `flush` positions the hardware cursor there.
    update def showCaret(column: Ordinal, row2: Ordinal): Unit =
      caretColumn = column.n0
      caretRow = row2.n0

    update def flush(): Unit =
      val columns = gridWidth
      val h       = gridHeight
      val validated = if invalidated then Unset else snapshotValid(1, columns, h)

      validated.let: snap => presentDiff(1, columns, h, snap)
      . or:
        invalidated = false

        val frame = StringBuilder()
        def emit(text: Text): Unit = frame.append(text.s)
        val termcap = stdio.termcap

        emit(csi.dectcem(false))

        // Draw every row at its absolute position (`cup` per row, never a newline, so
        // the bottom row can never scroll the screen), cleared and reset like a docked
        // inline row so no colour bleeds between rows.
        var r = 0

        while r < h do
          emit(csi.cup(r + 1, 1))
          emit(csi.el(2))
          val rendered = rowContent(r, columns).render(termcap)
          emit(rendered)
          if rendered.contains(t"\e") then emit(csi.sgr(0))
          r += 1

        if caretVisible then
          emit(csi.cup((1 + caretRow.min(h - 1)).max(1), caretColumn.min(columns - 1).max(0) + 1))
          emit(csi.dectcem(true))

        // What was just drawn IS the screen now: record it (and the caret) so the next
        // present can diff against it.
        recordSnapshot(1, columns)
        recordCaret(1, columns, h)

        Out.print(frame.toString.tt)

    // On exit, re-show the hardware cursor; the alternate-screen wrapper around the
    // session restores the previous screen contents.
    update def finish(): Unit = Out.print(csi.dectcem(true))
