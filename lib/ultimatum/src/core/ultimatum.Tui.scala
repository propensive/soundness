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

import anticipation.*
import aviation.*
import denominative.*
import denominative.dysasymptotics.linearSize
import escapade.*
import gossamer.*
import hieroglyph.*
import hypotenuse.{maximum, minimum}
import iridescence.*
import parasite.*
import prepositional.*
import quantitative.*
import rudiments.*
import spectacular.*
import symbolism.*
import tessellate.*
import turbulence.*
import vacuous.*

// The namespace for ultimatum's terminal-interface vocabulary: the layout model (the
// content-bearing `Pane` tree that `panel`, `strip`, `stack` and `grid` build, the pure `Frame` it
// projects to for solving, and the `Placement` of `Region`s that solving produces), the boards a
// form renders into (`Block` inline, `Screen` fullscreen), and the gauges (`Bar`, `Meter`,
// `Spinner`, `Checklist`, the `Reading` they draw and the statuses they draw it from). Nested here
// because each of these names, bare in the umbrella, would claim a far more general meaning than
// ultimatum gives it.
object Tui:
  export ultimatum.internal.{Countdown, Fraction}

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

  object Bar:
    // The characters a filled bar is drawn from. `partials` are the intermediate widths of the
    // boundary cell, in ascending order and excluding `full`; supplying them is what makes a bar
    // advance by a fraction of a cell rather than jumping a whole one at a time. An empty
    // `partials` gives a bar that steps cell by cell.
    case class Glyphs
      ( full:     Text,
         partials: Text           = t"",
         empty:    Text           = t" ",
         leftCap:  Optional[Text] = Unset,
         rightCap: Optional[Text] = Unset,
         tip:      Optional[Text] = Unset )

    // The intrinsic width every bar prefers, matching the width the repackager's bar has always
    // had.
    val defaultColumns: Int = 40

    // The shades a bar collapses to when it has one cell: enough to tell nearly-done from
    // barely-started, which is all one cell can honestly say.
    private val shades: Text = t"░▒▓█"
    private val asciiShades: Text = t".:*#"

  // How a proportion is drawn. Four shapes cover the catalogue: a bar that fills, a marker that
  // travels along a track, a run of discrete pips, and a bare figure.
  // Degradation is implemented here, once, rather than in each of the eighteen designs: a bar
  // re-quantizes to whatever width it is given, drops its end caps when they no longer earn their
  // cells, falls through to a percentage at four cells, and to a single shade glyph at one.
  enum Bar:
    case Filled(glyphs: Bar.Glyphs, columns: Int, gradient: Boolean)
    case Marker(track: Text, marker: Text, columns: Int)
    case Segmented(pip: Text, hollow: Text, gap: Text, pips: Int)
    case Numeric

    def columnCount: Int = this match
      case Filled(_, columns, _)      => columns
      case Marker(_, _, columns)      => columns
      case Segmented(_, _, gap, pips) => pips + (if gap == t"" then 0 else pips - 1)
      case Numeric                    => 4

    def gaugeable(using gauging: Gauging): Fraction is Gaugeable = new Gaugeable:
      type Self = Fraction
      override def minWidth(status: Fraction): Int = 1
      override def columns(status: Fraction): Int = columnCount

      def rows(status: Fraction, tick: Tick, width: Int): List[Teletype] =
        List(Bar.this.draw(status, width, gauging))

      // Where there is no figure to draw, sweep. An empty bar says "no progress", which is a
      // different claim from "not measured"; the generic lifting to `Optional[Fraction]` uses this.
      override def absent(tick: Tick, width: Int): List[Teletype] =
        List(Bar.this.sweep(tick, width, gauging))

      override def absentColumns: Int = columnCount

    // A lit window travelling back and forth along the track, its width a fifth of the whole.
    def sweep(tick: Tick, width: Int, gauging: Gauging): Teletype =
      val palette = gauging.palette
      val plain = !gauging.permits(Gaugeable.Glyphs.Unicode)
      val lit = if plain then t"#" else t"█"
      val dark = if plain then t"-" else t"░"
      val window = (width/5).max(1)
      val span = (width - window).max(1)

      // A triangle wave over `2*span`, so the window returns rather than jumping back to the start.
      val phase = tick.index%(span*2)
      val start = if phase <= span then phase else span*2 - phase

      val before = gauging.tint(palette.track)(Teletype(dark*start))
      val block = gauging.tint(palette.fill)(Teletype(lit*window.min(width - start)))
      val after = gauging.tint(palette.track)(Teletype(dark*(width - start - window).max(0)))

      e"$before$block$after"

    // Draw at exactly `width` cells.
    def draw(fraction: Fraction, width: Int, gauging: Gauging): Teletype =
      val palette = gauging.palette
      val ascii = !gauging.permits(Gaugeable.Glyphs.Unicode)

      if width <= 0 then Teletype(t"")
      else if width == 1 then
        // One cell can still carry the magnitude, as a shade.
        val shades = if ascii then Bar.asciiShades else Bar.shades
        val index = (fraction.value*shades.length).toInt.min(shades.length - 1).max(0)
        val glyph = Teletype(shades.at(index.z).let(_.show).or(t" "))

        gauging.tint(palette.lengthwise(fraction.value))(glyph)

      else if width < 4 then
        // Too narrow for a bar, wide enough for a figure: right-align the percentage into the cells
        // available, dropping its most significant digits last.
        val text = Magnitude.percentage(fraction)
        val trimmed = if text.length <= width then text else text.skip(text.length - width)
        gauging.tint(palette.caption)(Teletype(trimmed))

      else
        this match
          case Numeric =>
            val text = Magnitude.percentage(fraction)
            val padding = width - text.length
            val body = gauging.tint(palette.caption)(Teletype(text))
            if padding > 0 then e"${t" "*padding}$body" else body

          case Segmented(pip, hollow, gap, pips) =>
            // Fit as many pips as the width allows, so a segmented bar thins out rather
            // than clipping.
            val separator = if gap == t"" then 0 else 1
            val count = ((width + separator)/(1 + separator)).min(pips).max(1)
            val lit = (fraction.value*count).toInt.min(count)

            val cells = (0 until count).map: index =>
              val glyph = if index < lit then pip else hollow
              val color = if index < lit then palette.fill else palette.track
              gauging.tint(color)(Teletype(glyph))

            pad(cells.reduceLeft { (l, r) => e"$l${t" "*separator}$r" }, width, gauging)

          case Marker(track, marker, _) =>
            // A head travelling along a rail: no fill, so it reads as a position rather
            // than an amount.
            val position = (fraction.value*(width - 1)).toInt.min(width - 1).max(0)
            val before = gauging.tint(palette.track)(Teletype(track*position))
            val head = gauging.tint(palette.leadingEdge)(Teletype(marker))
            val after = gauging.tint(palette.track)(Teletype(track*(width - position - 1)))
            e"$before$head$after"

          case Filled(glyphs, _, gradient) =>
            val leftCap = glyphs.leftCap

            val caps =
              (if leftCap.present then 1 else 0) + (if glyphs.rightCap.present then 1 else 0)

            // The caps are the first thing to go: below eight cells they cost a fifth of the bar.
            val capped = width >= 8 && caps > 0
            val inner = if capped then width - caps else width
            val body = fill(fraction, inner, glyphs, gradient, gauging)

            if !capped then body else
              def cap(glyph: Optional[Text]): Teletype =
                glyph.lay(e""): text => gauging.tint(palette.track)(Teletype(text))

              e"${cap(leftCap)}$body${cap(glyphs.rightCap)}"

    // The filled region itself, at exactly `inner` cells. Cell accounting is exact — full cells,
    // plus at most one boundary cell, plus empty cells, always sums to `inner` — so the bar never
    // changes width as it fills.
    private def fill
      ( fraction: Fraction,
         inner:    Int,
         glyphs:   Bar.Glyphs,
         gradient: Boolean,
         gauging:  Gauging )
    :   Teletype =

      val palette = gauging.palette
      val steps = glyphs.partials.length + 1
      val total = (fraction.value*inner*steps).toInt.max(0).min(inner*steps)

      // A tip glyph replaces the last filled cell for as long as the bar is neither empty nor full
      // — the classic `[===>  ]`. It is a different idea from a partial: a partial says *how much*
      // of the boundary cell is filled, whereas a tip just marks where the fill has got to, and so
      // must be present at every intermediate value rather than only at the sub-cell steps.
      val (whole, boundary) = glyphs.tip.lay:
        val whole = total/steps
        val remainder = total%steps

        val partial: Optional[Text] =
          if remainder == 0 then Unset else glyphs.partials.at((remainder - 1).z).let(_.show)

        (whole, partial)

      . apply: tip =>
          val filledCells = (fraction.value*inner).toInt.max(0).min(inner)
          val complete = filledCells == 0 || filledCells == inner

          if complete then (filledCells, Unset) else (filledCells - 1, tip)

      val used = whole + (if boundary.present then 1 else 0)

      // A gradient bar colours each cell by where it sits along the bar; a plain one uses two runs.
      val filled =
        if !gradient then gauging.tint(palette.fill)(Teletype(glyphs.full*whole))
        else if whole == 0 then e"" else
          (0 until whole)
          . map: index =>
              val position = if inner <= 1 then 0.0 else index.toDouble/(inner - 1)
              gauging.tint(palette.lengthwise(position))(Teletype(glyphs.full))

          . reduceLeft: (left, right) => e"$left$right"

      // A track drawn with spaces is only visible as a background; one with its own glyph is drawn
      // in the track colour instead, so both kinds of design read correctly on any terminal.
      val washedTrack = glyphs.empty == t" "

      // The boundary cell is only partly covered by its glyph, and whatever the glyph leaves
      // uncovered is track, so it needs the same background as the cells to its right. Without
      // this, that uncovered fraction falls through to the terminal's default background and shows
      // as a sliver of the wrong colour along the leading edge. Where the track is drawn as a glyph
      // there is no background to match, and the cell is left alone.
      //
      // A tip marks where the fill has got to, so it stands out in the leading edge's colour; a
      // partial is more of the fill, so it takes the fill's colour — at its position, on a gradient
      // — lest a smooth bar seem to end in a cell of another colour.
      val edgeColor: Color in Srgb =
        if glyphs.tip.present then palette.leadingEdge
        else if !gradient then palette.fill
        else palette.lengthwise(if inner <= 1 then 0.0 else whole.toDouble/(inner - 1))

      val edge = boundary.lay(e""): glyph =>
        val tinted = gauging.tint(edgeColor)(Teletype(glyph))
        if washedTrack then gauging.wash(palette.track)(tinted) else tinted

      val blank = Teletype(glyphs.empty*(inner - used).max(0))

      val rest =
        if washedTrack then gauging.wash(palette.track)(blank)
        else gauging.tint(palette.track)(blank)

      e"$filled$edge$rest"

    private def pad(content: Teletype, width: Int, gauging: Gauging): Teletype =
      given Text is Measurable = gauging.metric
      Alignment.Left.pad(content, width)

  object CaptionLayout:
    // The no-import default: the label follows the gauge, one space away.
    given default: CaptionLayout = CaptionLayout(1, true, true)

  // Where a caption sits relative to the gauge it labels, and what happens when there is not room
  // for both. `elide` shortens the caption rather than squeezing the gauge, which is the right
  // priority: a half-drawn bar misreports the thing it is measuring, whereas a shortened label is
  // merely terser.
  case class CaptionLayout(gap: Int, trailing: Boolean, elide: Boolean):
    // How many cells the gauge itself gets. The caption is allowed at most half the row before it
    // starts costing the gauge cells — so a long label shortens rather than crowding out the thing
    // it labels, and a short one still leaves an elastic bar the rest of the row.
    def gaugeWidth(preferred: Int, caption: Text, width: Int, gauging: Gauging): Int =
      val wanted = gauging.cells(caption)
      val allowance = if elide then wanted.min(width/2) else wanted
      (width - gap - allowance).max(0).min(preferred)

    def compose(gauge: Teletype, gaugeWidth: Int, caption: Text, width: Int, gauging: Gauging)
    :   Teletype =

      val room = width - gaugeWidth - gap
      val label = if !elide then caption else shorten(caption, room, gauging)
      val text = gauging.tint(gauging.palette.caption)(Teletype(label))
      val spacer = t" "*gap.max(0)

      val composed =
        if room <= 0 then gauge
        else if trailing then e"$gauge$spacer$text"
        else e"$text$spacer$gauge"

      given Text is Measurable = gauging.metric
      Alignment.Left.pad(composed, width)

    private def shorten(caption: Text, room: Int, gauging: Gauging): Text =
      given Text is Measurable = gauging.metric
      if room <= 0 then t"" else Flow.shorten(caption, room)

  object Captioned:
    // Derived from whatever design the underlying status already has, rather than choosing one: the
    // caption's placement is the only style decision here, and that comes from `CaptionLayout`. So
    // this is structural, lives in the companion, and needs no import.
    given gaugeable: [status: Gaugeable as design]
    =>  ( layout: CaptionLayout, gauging: Gauging )
    =>  Captioned[status] is Gaugeable =

      new Gaugeable:
        type Self = Captioned[status]
        override def period: Optional[Int] = design.period
        override def minWidth(status: Self): Int = design.minWidth(status.status)

        override def columns(status: Self): Int =
          design.columns(status.status) + layout.gap + gauging.cells(status.caption)

        override def height(status: Self, width: Int): Int =
          design.height(status.status, width)

        def rows(status: Self, tick: Tick, width: Int): List[Teletype] =
          val gaugeWidth =
            layout.gaugeWidth(design.columns(status.status), status.caption, width, gauging)

          val drawn = design.rows(status.status, tick, gaugeWidth)

          // Only the first row carries the caption; a multi-row design keeps its shape below it.
          drawn.indexed.map: (row, ordinal) =>
            if ordinal.n0 > 0 then row
            else layout.compose(row, gaugeWidth, status.caption, width, gauging)

  // Any status with a label beside it: `⠹ resolving dependencies`, `████░░ copying`. Generic over
  // what it labels, so one design serves every status type — and its given derives from the
  // underlying design rather than choosing one, so it is structural, not a style, and needs no
  // import.
  case class Captioned[status](status: status, caption: Text)

  object Checklist:
    // The frames the running step's marker cycles through, so that a checklist shows which step is
    // working rather than merely which is next.
    private val working: Text = t"⠋⠙⠹⠸⠼⠴⠦⠧⠇⠏"
    private val asciiWorking: Text = t"-\\|/"

  // How a run of steps is drawn. These differ in *height* — `Rows` is one row per step, the rest
  // are a single row — which is why `Procession` has no default: the choice changes the layout
  // around it, so it is the caller's.
  enum Checklist:
    case Rows, Breadcrumb, Beads, Numbered, Ribbon

    def gaugeable(using gauging: Gauging): Sequence[Step] is Gaugeable = new Gaugeable:
      type Self = Sequence[Step]

      // Only a design that shows a working step needs the clock.
      override def period: Optional[Int] = Checklist.this match
        case Rows | Beads => 80
        case _            => Unset

      override def minWidth(status: Sequence[Step]): Int = 3

      override def height(status: Sequence[Step], width: Int): Int = Checklist.this match
        case Rows => status.size.max(1)
        case _    => 1

      def rows(status: Sequence[Step], tick: Tick, width: Int): List[Teletype] =
        Checklist.this.draw(status, tick, width, gauging)

    def draw(steps: Sequence[Step], tick: Tick, width: Int, gauging: Gauging): List[Teletype] =
      // Derived here rather than carried by a wrapper type: a run of steps is just a `Sequence`.
      val current: Optional[Step] = steps.seek(_.standing == Standing.Running)
      val started = steps.count(_.standing != Standing.Pending)
      val count = steps.size

      val palette = gauging.palette
      val plain = !gauging.permits(Gaugeable.Glyphs.Unicode)

      // The running step's marker, animated; every other standing keeps its static glyph.
      def marker(standing: Standing): Text =
        val frames = if plain then Checklist.asciiWorking else Checklist.working
        val count = frames.length

        standing match
          case Standing.Running   => frames.s.charAt(tick.index.abs%count).toString.tt
          case Standing.Succeeded => if plain then t"+" else t"✓"
          case Standing.Failed    => if plain then t"x" else t"✗"
          case Standing.Warned    => t"!"
          case Standing.Skipped   => if plain then t"-" else t"‑"
          case Standing.Pending   => if plain then t"." else t"·"

      def pad(content: Teletype): Teletype =
        given Text is Measurable = gauging.metric
        Alignment.Left.pad(content, width)

      def clip(text: Text, cells: Int): Text =
        given Text is Measurable = gauging.metric
        Flow.shorten(text, cells)

      this match
        case Rows =>
          // One row per step: the canonical multi-row widget, and the one that makes the layout
          // grow as steps are appended. Named methods rather than lambdas throughout this block: an
          // interpolation inside a lambda passed to a collection combinator runs its implicit
          // search while the combinator's element type is still uninstantiated, tripping dotc's
          // `wildApprox` assertion (scala/scala3#24824).
          def line(step: Step): Teletype =
            val glyph =
              gauging.tint(palette.colorOf(step.standing))(Teletype(marker(step.standing)))

            val faded = step.standing == Standing.Succeeded || step.standing == Standing.Skipped
            val color = if faded then palette.muted else palette.caption

            val name = gauging.tint(color)(Teletype(clip(step.name, (width - 2).max(1))))

            pad(e"$glyph $name")

          val lines = steps.map(line)

          if lines.nil then List(pad(e"")) else lines.to[List]

        case Numbered =>
          val name = current.lay(t"")(_.name)
          val position = t"[$started/$count]"
          val label = clip(name, (width - position.length - 1).max(0))

          val marker = gauging.tint(palette.muted)(Teletype(position))
          val text = gauging.tint(palette.caption)(Teletype(label))

          List(pad(e"$marker $text"))

        case Beads =>
          // A compact chain: one bead per step, joined by a rule. Fixed at `2n - 1` cells.
          val link = if plain then t"-" else t"━"

          // Named, as above (`wildApprox`).
          def bead(step: Step, ordinal: Ordinal): Teletype =
            val glyph = step.standing match
              case Standing.Pending => if plain then t"o" else t"○"
              case Standing.Running => if plain then t"*" else t"◐"
              case _                => if plain then t"@" else t"●"

            val bead = gauging.tint(palette.colorOf(step.standing))(Teletype(glyph))

            if ordinal.n0 == 0 then bead else e"${gauging.tint(palette.track)(Teletype(link))}$bead"

          def merge(left: Teletype, right: Teletype): Teletype = e"$left$right"

          val beads = steps.indexed.map: (step, ordinal) => bead(step, ordinal)

          beads.occupied.lay(List(pad(e""))): beads => List(pad(beads.reduce(merge)))

        case Breadcrumb =>
          val separator = if plain then t">" else t"›"

          val crumbs = steps.map: step =>
            val faded = step.standing == Standing.Pending
            val color = if faded then palette.track else palette.colorOf(step.standing)
            gauging.tint(color)(Teletype(step.name))

          // Named, as above (`wildApprox`).
          def join(left: Teletype, right: Teletype): Teletype =
            e"$left ${gauging.tint(palette.muted)(Teletype(separator))} $right"

          crumbs.occupied.lay(List(pad(e""))): crumbs => List(pad(crumbs.reduce(join)))

        case Ribbon =>
          // Powerline: `escapade.Ribbon` already draws the separators and picks a legible
          // foreground per segment, so this is only a matter of choosing the backgrounds.
          val names = steps.map: step => Teletype(step.name)

          if names.nil then List(pad(e"")) else
            // Qualified: this enum's own `Ribbon` case shadows the one from escapade.
            val colors = palette.steps(names.size).map(Bg(_))
            List(pad(escapade.Ribbon(colors*).fill(names*)))

  object Dial:
    private val levels: Text = t"▁▂▃▄▅▆▇█"

  // How a bounded reading is drawn. A meter is not progress — it can fall as well as rise — so
  // these designs mark the scale, and colour the reading by where it sits on it rather than by how
  // far it has come. They differ in *height* as well as width (a thermometer is a column), which is
  // why `Meter` has no default design: the choice changes the layout, so it belongs to the caller.
  enum Dial:
    case Battery, Thermometer, Needle, Bullet, Column, Ascii

    def columnCount: Int = this match
      case Battery     => 8
      case Thermometer => 1
      case Needle      => 12
      case Bullet      => 20
      case Column      => 1
      case Ascii       => 10

    def rowCount: Int = this match
      case Thermometer => 5
      case _           => 1

    def gaugeable(using gauging: Gauging): Meter is Gaugeable = new Gaugeable:
      type Self = Meter
      override def elastic: Boolean = Dial.this != Column && Dial.this != Thermometer
      override def minWidth(status: Meter): Int = if rowCount > 1 then 1 else 3
      override def columns(status: Meter): Int = columnCount
      override def height(status: Meter, width: Int): Int = rowCount

      def rows(status: Meter, tick: Tick, width: Int): List[Teletype] =
        Dial.this.draw(status, width, gauging)

    def draw(meter: Meter, width: Int, gauging: Gauging): List[Teletype] =
      val palette = gauging.palette
      val fraction = meter.fraction
      val plain = !gauging.permits(Gaugeable.Glyphs.Unicode)

      def level(fraction: Fraction): Text =
        val index = (fraction.value*Dial.levels.length).toInt.min(Dial.levels.length - 1).max(0)
        Dial.levels.at(index.z).let(_.show).or(t" ")

      def pad(content: Teletype): Teletype =
        given Text is Measurable = gauging.metric
        Alignment.Left.pad(content, width)

      this match
        case Column =>
          // One cell: the reading as a height, coloured by severity.
          List(gauging.tint(palette.severity(fraction.value))(Teletype(level(fraction))))

        case Thermometer =>
          // A column read from the bottom up, with a bulb beneath it. The top filled cell is a
          // partial, so the reading moves smoothly rather than a fifth at a time.
          val stem = rowCount - 1
          val total = fraction.value*stem
          val color = palette.severity(fraction.value)

          val cells = (0 until stem).map: row =>
            val fromTop = stem - 1 - row
            val filled = (total - fromTop).max(0.0).min(1.0)

            if filled <= 0 then gauging.tint(palette.track)(Teletype(t"░"))
            else gauging.tint(color)(Teletype(level(Fraction(filled))))

          cells.toList.to(List) :+ gauging.tint(color)(Teletype(if plain then t"o" else t"◍"))

        case Battery =>
          // Caps, cells and a terminal nub. A battery reddens as it *empties*, so the severity ramp
          // is read backwards.
          val inner = (width - 3).max(1)
          val lit = (fraction.value*inner).toInt.min(inner)
          val color = palette.severity(1 - fraction.value)
          val body = gauging.tint(color)(Teletype((if plain then t"#" else t"█")*lit))
          val empty = if plain then t"-" else t"░"
          val rest = gauging.tint(palette.track)(Teletype(empty*(inner - lit)))

          if plain then List(pad(e"[$body$rest]"))
          else
            val nub = gauging.tint(color)(Teletype(t"╸"))
            val left = gauging.tint(palette.track)(Teletype(t"▐"))
            val right = gauging.tint(palette.track)(Teletype(t"▌"))

            List(pad(e"$left$body$rest$right$nub"))

        case Needle =>
          // A tick travelling along a scale, with the ends marked: a position on a range, with no
          // suggestion that the space behind it has been filled.
          val span = (width - 2).max(1)
          val at = (fraction.value*(span - 1)).toInt.min(span - 1).max(0)
          val rail = if plain then t"-" else t"─"
          val head = if plain then t"|" else t"┃"
          val cap = if plain then t"+" else t"╷"
          val before = gauging.tint(palette.track)(Teletype(rail*at))
          val after = gauging.tint(palette.track)(Teletype(rail*(span - at - 1)))
          val marker = gauging.tint(palette.severity(fraction.value))(Teletype(head))
          val ends = gauging.tint(palette.muted)(Teletype(cap))

          List(pad(e"$ends$before$marker$after$ends"))

        case Bullet =>
          // A measure drawn over a qualitative track: the bands say what counts as low, fair and
          // high, so the reading is interpretable without a legend.
          val lit = (fraction.value*width).toInt.min(width)

          val cells = (0 until width).map: index =>
            val band = index.toDouble/width.max(1)

            if index < lit then gauging.tint(palette.severity(fraction.value))(Teletype(t"█"))
            else if plain then gauging.tint(palette.track)(Teletype(t"-"))
            else
              val shade = if band < 0.5 then t"░" else if band < 0.8 then t"▒" else t"▓"
              gauging.tint(palette.track)(Teletype(shade))

          List(pad(cells.reduceLeft { (l, r) => e"$l$r" }))

        case Ascii =>
          val inner = (width - 2).max(1)
          val lit = (fraction.value*inner).toInt.min(inner)
          val body = gauging.tint(palette.severity(fraction.value))(Teletype(t"#"*lit))
          val rest = gauging.tint(palette.track)(Teletype(t"-"*(inner - lit)))

          List(pad(e"[$body$rest]"))

  object Facet:
    // A part whose width is fixed by its own content.
    def fixed(shed: Int, content: Teletype)(using gauging: Gauging): Facet =
      Facet(shed, gauging.cells(content.plain), false, _ => content)

    // The part that absorbs whatever width is left. At most one facet in a row may be flexible; it
    // is never shed, because a row that has dropped its gauge is not worth drawing.
    def flexible(minWidth: Int)(render: Int -> Teletype): Facet =
      Facet(Int.MinValue, minWidth, true, render)

    // Lay `facets` out into exactly `width` cells, separated by `gap` spaces: drop parts in
    // descending `shed` order until what remains fits, then hand the surplus to the flexible part.
    // This is escritoire's column-negotiation idea — a declared minimum, a droppable part, a
    // flexible remainder — without its binary search on slack: a gauge's parts have exact widths
    // and never reflow, so one greedy pass is exact, and there is no gas budget to run out of.
    // Total by construction: below every minimum, the result is `width` spaces.
    def solve(facets: List[Facet], width: Int, gap: Int = 1)(using gauging: Gauging): Teletype =
      def extent(kept: scala.List[Facet]): Int =
        kept.map(_.minWidth).sum + gap*(kept.length - 1).max(0)

      // Shed the most expendable part first; ties keep source order, so a row degrades the same way
      // every time rather than flickering between two arrangements at a boundary.
      def shed(kept: scala.List[Facet]): scala.List[Facet] =
        if extent(kept) <= width || kept.length <= 1 then kept else
          val worst = kept.filter(!_.flexible).map(_.shed).maxOption

          worst.map: value =>
            val index = kept.indexWhere: facet => !facet.flexible && facet.shed == value

            if index < 0 then kept else shed(kept.patch(index, scala.Nil, 1))

          . getOrElse(kept)

      // The stdlib view feeds `shed`, which drops one facet at a time by position and needs the
      // stdlib `indexWhere`/`patch` pair.
      val kept = shed(facets.stdlib)

      if kept.isEmpty || extent(kept) > width then Teletype(t" "*width.max(0)) else
        val surplus = width - extent(kept)

        val parts = kept.map: facet =>
          facet.render(facet.minWidth + (if facet.flexible then surplus else 0))

        val joined = parts.reduceLeft: (left, right) =>
          e"$left${t" "*gap}$right"

        val used = gauging.cells(joined.plain)

        if used >= width then joined else e"$joined${t" "*(width - used)}"

  // One horizontal part of a composite gauge row — a caption, the bar itself, a percentage, a rate,
  // an estimate. `shed` orders the parts by how readily they are given up when the row will not
  // fit: the highest value goes first. `render` is handed the width the part actually got, which
  // for the flexible part is more than its minimum. `render` is a *pure* function of the width the
  // part got: a design captures nothing, so that the pane tree it ends up inside stays pure under
  // capture checking.
  case class Facet(shed: Int, minWidth: Int, flexible: Boolean, render: Int -> Teletype)

  object Gradient:
    def apply(stops: (Color in Srgb)*): Gradient = Gradient(Sequence(stops*))

  // An N-stop colour ramp over sRGB. `iridescence` supplies only a pairwise interpolation
  // (`Palette.mix`), so this builds the ramp piecewise from it: the stops are evenly spaced, and a
  // repeated stop consequently flattens that part of the ramp — which is how a severity ramp holds
  // one colour across the whole lower half before it starts to climb.
  // This is a general facility, and a candidate to move into `iridescence` alongside `Spectrum`; it
  // is local for now so that adding it does not touch a module everything else depends on.
  case class Gradient(stops: Sequence[Color in Srgb]):
    def apply(position: Double): Color in Srgb =
      val count = stops.size

      if count <= 1 then stops.head.or(Srgb(0.0, 0.0, 0.0)) else
        val scaled = position.max(0.0).min(1.0)*(count - 1)
        val index = scaled.toInt.min(count - 2)
        val balance = scaled - index
        val left = stops.at(index.z).let(_.to[Srgb]).or(Srgb(0.0, 0.0, 0.0))
        val right = stops.at((index + 1).z).let(_.to[Srgb]).or(Srgb(0.0, 0.0, 0.0))

        def channel(from: Double, to: Double): Double = from*(1 - balance) + to*balance

        val red = channel(left.red, right.red)
        val green = channel(left.green, right.green)
        val blue = channel(left.blue, right.blue)

        Srgb(red, green, blue)

  // A gauge drawn live at the cursor, with no form and no layout: the shape a command-line tool
  // wants when it has one thing to report. Each frame is redrawn in place, the animation is one
  // background task snoozing the design's own period, and `finish` erases the block. Every design
  // works here unchanged, because a design is a pure function of status, tick and width — this
  // class supplies the three and does the writing.
  class Inlay[status: Gaugeable as design]
    ( reading: Reading[status], width: Optional[Int] = Unset )
    ( using stdio: Stdio, monitor: Monitor, probate: Probate ):

    private val started: Long = System.nanoTime

    // Painting happens on two threads at once — the animation task, and whichever task updates the
    // `Reading` and so triggers its wake callback — so every write to the terminal, and the row
    // count it leaves behind, is serialised under this mutex.
    private val mutex: Mutex = Mutex()

    // How many rows the last frame occupied, so the next one knows how far to move back up. Only
    // touched under `mutex`.
    private val drawn: Atomic[Int] = Atomic(0)

    // Between `start` and `finish`. Atomic because `finish` clears it on the caller's thread while
    // the animation task polls it; `paint` re-checks it under the mutex, so a frame that was
    // already on its way when `finish` ran cannot land after the erase.
    private val live: Atomic[Boolean] = Atomic(false)

    private def columns: Int = width.or(stdio.termcap.width.max(1))

    private def tick: Tick =
      Tick.at((System.nanoTime - started)/1000000L, design.period.or(1000))

    // Draw one frame over the last one. Each row is erased to the end of the line as it is written,
    // so a frame that is shorter than its predecessor leaves no residue.
    private def paint(): Unit = mutex:
      if live() then
        // The stdlib view is taken once, indexed and counted by the paint loop below.
        val rows = design.rows(reading(), tick, columns).stdlib
        rewind()
        var index = 0

        while index < rows.length do
          Out.print(e"${rows(index)}${csi.el()}")
          if index < rows.length - 1 then Out.print(t"\n")
          index += 1

        drawn() = rows.length

    // Back to the first column of the block's first row.
    private def rewind(): Unit =
      if drawn() > 1 then Out.print(csi.cuu(drawn() - 1))
      Out.print(t"\r")

    // Paint once, and — if the design animates — keep painting until `finish`. A design with no
    // period is drawn once here and thereafter only when its `Reading` changes and calls back.
    def start(): Unit =
      live() = true
      reading.bindWake: () => paint()
      paint()

      design.period.let: period =>
        // The repaint task captures this `Inlay`, which also owns the `Monitor` the task is spawned
        // against, and separation checking rejects the overlap. They are the same single-owner
        // session: the task is started here, stopped by `finish`, and touches nothing else — so the
        // assertion is local and its scope is the object's own lifetime. (`form` makes the same
        // assertion, for the same reason, around the alternate-screen session.)
        // [by-name-receiver] async body captures Inlay owning the Monitor
        scala.caps.unsafe.unsafeAssumeSeparate:
          async:
            while live() do
              snooze(period.toDouble*Milli(Second))
              paint()

          ()

    // Erase the block and leave the cursor where it started, so whatever the caller prints next
    // begins on a clean line.
    def finish(): Unit =
      live() = false
      reading.bindWake: () => ()

      mutex:
        rewind()
        var index = 0

        while index < drawn() do
          Out.print(csi.el())
          if index < drawn() - 1 then Out.print(t"\n")
          index += 1

        if drawn() > 1 then Out.print(csi.cuu(drawn() - 1))
        Out.print(t"\r")
        drawn() = 0

  // Compact renderings of the numbers that appear beside a gauge, where every cell is contested and
  // a full-precision figure would crowd out the bar itself.
  object Magnitude:
    // A duration as the two largest units that carry information: `41s`, `2m41s`, `1h04m`, `3d02h`.
    // Never more than two, and never a leading zero unit, so the field stays narrow and stops
    // jittering once it is past a minute.
    def interval(seconds: Double): Text =
      val total = seconds.max(0.0).toLong

      if total < 60 then t"${total}s" else
        val minutes = total/60
        val hours = minutes/60
        val days = hours/24

        if minutes < 60 then t"${minutes}m${pad(total%60)}s"
        else if hours < 24 then t"${hours}h${pad(minutes%60)}m"
        else t"${days}d${pad(hours%24)}h"

    // A count, abbreviated once it stops being readable at a glance: `947`, `1.2k`, `15k`, `3.4M`.
    def count(value: Long): Text =
      if value < 1000 then value.show
      else if value < 1000000 then scaled(value, 1000.0, t"k")
      else if value < 1000000000L then scaled(value, 1000000.0, t"M")
      else scaled(value, 1000000000.0, t"G")

    // A percentage, always three cells wide (`  0%` … `100%` less its sign), so a bar's right-hand
    // figure never shifts as it fills.
    def percentage(fraction: Fraction): Text =
      val value = fraction.percentage
      if value >= 100 then t"100%" else if value >= 10 then t" $value%" else t"  $value%"

    // A byte count in the largest prefix that leaves a figure above one: `947 B`, `4.01 MiB`,
    // `1.20 GB`. `binary` selects the 1024-based prefixes (`KiB`) over the 1000-based ones (`kB`);
    // the two describe the same quantity under different conventions, and which is right depends on
    // what is being measured, so the design says which it means rather than assuming.
    def bytes(value: Double, binary: Boolean): Text =
      val step = if binary then 1024.0 else 1000.0

      val units: scala.Array[Text] =
        if binary then scala.Array(t"B", t"KiB", t"MiB", t"GiB", t"TiB")
        else scala.Array(t"B", t"kB", t"MB", t"GB", t"TB")

      var size = value.max(0.0)
      var index = 0

      while size >= step && index < units.length - 1 do
        size /= step
        index += 1

      t"${figure(size)} ${units(index)}"

    // A transfer rate, in the same prefixes.
    def rate(bytesPerSecond: Double, binary: Boolean): Text = t"${bytes(bytesPerSecond, binary)}/s"

    // Three significant figures below ten, otherwise whole units: enough precision to see movement,
    // narrow enough not to crowd the bar.
    private def figure(value: Double): Text =
      if value >= 100 then value.toLong.show
      else if value >= 10 then t"${(value*10).toLong/10}.${(value*10).toLong%10}"
      else
        val hundredths = (value*100).toLong
        t"${hundredths/100}.${pad(hundredths%100)}"

    private def pad(value: Long): Text = if value < 10 then t"0$value" else value.show

    // One decimal place below ten, none above: `1.2k` but `15k`, so the field is at most four
    // cells.
    private def scaled(value: Long, divisor: Double, suffix: Text): Text =
      val scale = value/divisor

      if scale >= 10 then t"${scale.toLong}$suffix" else
        val tenths = (scale*10).toLong
        t"${tenths/10}.${tenths%10}$suffix"

  // A reading on a bounded scale: a battery's charge, a temperature, a load average, a signal
  // level. Distinct from `Fraction` because the bounds are part of the datum — a meter shows where
  // a value sits between them, and its design may mark the scale — and because a meter is not
  // progress: it can fall as well as rise.
  object Meter:
    def apply(value: Double): Meter = Meter(value, 0.0, 1.0)

  case class Meter(value: Double, minimum: Double, maximum: Double):
    def fraction: Fraction =
      val span = maximum - minimum
      if span <= 0 then Fraction(0.0) else Fraction((value - minimum)/span)

  // A mutable cell holding a gauge's current status. Assigning to it publishes the new value and
  // wakes the running form, so a gauge updated from a background task repaints at once — the same
  // contract a `Panes` mutation has.
  // The status itself is plain data (a pane tree stays pure); the one effectful field is the
  // installed repaint callback.
  class Reading[status](initial: status):
    // Written from background tasks and read on the form's thread, and `amend` must not lose an
    // update to a concurrent one, so the status lives in an atomic cell. `Atomic.Ref[status]`, not
    // `Atomic[status]`: the match type cannot reduce for an abstract type parameter.
    private val current: Atomic.Ref[status] = Atomic.Ref(initial)

    // A no-op until the cell is bound into a running form. Atomic too, so that a task updating the
    // cell sees the callback the form bound, rather than a stale no-op that never wakes the form.
    private val onChange: Atomic.Ref[() -> Unit] = Atomic.Ref { () => () }

    // Install the running form's repaint trigger. As in `Panes.bindWake`, the callback genuinely
    // captures the form's event loop and escapes into this longer-lived cell — a growing capture
    // set that capture checking cannot yet express. It is sound by construction for the same
    // reasons: it is re-bound on every `run` (so it never references a finished form) and is only
    // ever called from a mutation while that form is live. Hence the single, localised
    // `unsafeAssumePure`.
    private[ultimatum] def bindWake(wake: () => Unit): Unit =
      // [field-purity] form wake callback escapes into Reading field
      onChange() = caps.unsafe.unsafeAssumePure(wake)

    def apply(): status = current()

    // Paired with `apply`, this gives assignment syntax: `reading() = Fraction(0.42)`.
    def update(status: status): Unit =
      current() = status
      onChange()()

    // A compare-and-set transition, so two tasks amending at once both land. `lambda` may be re-run
    // under contention, so it must be pure — the contract `Accrual`'s `combine` has.
    def amend(lambda: status => status): Unit =
      current.revise(lambda)
      onChange()()

  object Reckoning:
    // How a count is written. `Padded` right-aligns the numerator to the total's digit count so the
    // field does not jitter as it counts up — the difference between a figure that can be read
    // while it changes and one that cannot.
    enum Counter:
      case Plain, Padded, Words, Percentage, Scaled

      def write(reckoning: Reckoning): Text =
        val done = reckoning.done.show

        this match
          case Plain      => reckoning.total.lay(done): total => t"$done/$total"
          case Words      => reckoning.total.lay(done): total => t"$done of $total"
          case Percentage => reckoning.fraction.lay(t"  ?%")(Magnitude.percentage(_))

          case Scaled =>
            val scaled = Magnitude.count(reckoning.done)
            reckoning.total.lay(scaled): total => t"$scaled/${Magnitude.count(total)}"

          // The numerator is right-aligned to the total's digit count, so the field does not jitter
          // as it counts up.
          case Padded =>
            reckoning.total.lay(done): total =>
              t"${t" "*(total.show.length - done.length).max(0)}$done/$total"

      def gaugeable(using gauging: Gauging): Reckoning is Gaugeable = new Gaugeable:
        type Self = Reckoning
        override def elastic: Boolean = false
        override def minWidth(status: Reckoning): Int = 1
        override def columns(status: Reckoning): Int = gauging.cells(write(status))

        def rows(status: Reckoning, tick: Tick, width: Int): List[Teletype] =
          val text = write(status)
          val used = gauging.cells(text)
          val body = gauging.tint(gauging.palette.caption)(Teletype(text))

          // Too narrow to write the count: drop the leading characters rather than the trailing
          // ones, so the numerator (which is what changes) survives longest.
          val fitted =
            if used <= width then e"$body${t" "*(width - used)}"
            else gauging.tint(gauging.palette.caption)(Teletype(text.skip(used - width)))

          List(fitted)

    // The default: `17/120` is not a style question.
    given gaugeable: Gauging => Reckoning is Gaugeable = counters.plainCounter

  // A count of work done against a total that may not be known yet: `17/120`, or just `17` while
  // the total is still being discovered. The status a counter shows, and the one most jobs actually
  // have to hand.
  case class Reckoning(done: Long, total: Optional[Long] = Unset):
    def fraction: Optional[Fraction] = total.let: total =>
      if total <= 0 then Fraction(0.0) else Fraction(done.toDouble/total)

    def remaining: Optional[Long] = total.let: total => (total - done).max(0L)

  object Sparkline:
    // Ascending block heights: the standard eight-level ramp.
    private val blocks: Text = t"▁▂▃▄▅▆▇█"
    private val dots: Text = t"⣀⠤⠒⠉"
    private val ascii: Text = t"_.-^"

    // Reduce `samples` to exactly `width` values by taking the maximum of each group. A sparkline
    // narrower than its series must drop information; taking the maximum keeps the peaks, whereas
    // truncating would silently show only the oldest samples and misrepresent the shape.
    def decimate(samples: Sequence[Fraction], width: Int): Sequence[Fraction] =
      val count = samples.size

      if count <= width || width <= 0 then samples else

        Sequence.from:
          (0 until width).map: cell =>
            val from = cell*count/width
            val to = (((cell + 1)*count/width).max(from + 1)).min(count)
            // The excerpt always spans at least one sample, so the fallback is unreachable.
            Fraction(samples.excerpt(from, to).map(_.value).maximum.or(0.0))
  // How a run of samples is drawn. `Blocks` gives eight levels in one row; `Tall` stacks two rows
  // for sixteen; `Dots` and `Ascii` trade resolution for a narrower character repertoire.
  enum Sparkline:
    case Blocks, Tall, Dots, Ascii

    def rowCount: Int = this match
      case Tall => 2
      case _    => 1

    // Keyed on a plain `Sequence[Double]`, and scaled to the samples it is given.
    def gaugeable(using gauging: Gauging): Sequence[Double] is Gaugeable = scaled(Unset, Unset)

    // The same, with the scale fixed. A fixed scale is a presentation decision, not part of the
    // data: a sparkline that rescales itself every frame makes a steady signal look erratic, and
    // only the caller knows which reading is wanted.
    def scaled(floor: Optional[Double], ceiling: Optional[Double])(using gauging: Gauging)
    :   Sequence[Double] is Gaugeable =

      new Gaugeable:
        type Self = Sequence[Double]
        override def minWidth(status: Sequence[Double]): Int = 1
        override def columns(status: Sequence[Double]): Int = status.size.max(1)
        override def height(status: Sequence[Double], width: Int): Int = rowCount

        def rows(status: Sequence[Double], tick: Tick, width: Int): List[Teletype] =
          Sparkline.this.draw(status, floor, ceiling, width, gauging)

    def draw
      ( samples: Sequence[Double],
        floor:   Optional[Double],
        ceiling: Optional[Double],
        width:   Int,
        gauging: Gauging )
    :   List[Teletype] =

      val lower = floor.or(samples.minimum.or(0.0))
      val upper = ceiling.or(samples.maximum.or(1.0))
      val span = upper - lower

      val normalized =
        samples.map: sample => if span <= 0 then Fraction(0.0) else Fraction((sample - lower)/span)

      val values = Sparkline.decimate(normalized, width)
      val ascii = !gauging.permits(Gaugeable.Glyphs.Unicode)

      // A design whose glyphs are unavailable falls back to the ASCII ramp rather than to nothing.
      val ramp = this match
        case Ascii             => Sparkline.ascii
        case _ if ascii        => Sparkline.ascii
        case Dots              => Sparkline.dots
        case Blocks | Tall     => Sparkline.blocks

      def cell(fraction: Fraction, ramp: Text): Teletype =
        val index = (fraction.value*ramp.length).toInt.min(ramp.length - 1).max(0)
        val glyph = ramp.at(index.z).let(_.show).or(t" ")
        gauging.tint(gauging.palette.lengthwise(fraction.value))(Teletype(glyph))

      // `merge` is a named method rather than a lambda: an interpolation inside a lambda passed to
      // a collection combinator runs its implicit search while the combinator's element type is
      // still uninstantiated, tripping dotc's `wildApprox` assertion (scala/scala3#24824).
      def merge(left: Teletype, right: Teletype): Teletype = e"$left$right"

      def row(pick: Fraction -> Fraction): Teletype =
        val drawn = values.map: value => cell(pick(value), ramp)
        val body: Teletype = drawn.occupied.lay(e"")(_.reduce(merge))
        val used = gauging.cells(body.plain)
        if used >= width then body else e"$body${t" "*(width - used)}"

      if this != Tall || ascii then List(row { value => value }) else
        // Two rows give sixteen levels: the upper row shows the top half of the range and the lower
        // row the bottom, so a tall sparkline resolves detail a single row would flatten.
        val upper = row: value => Fraction((value.value*2 - 1).max(0.0))
        val lower = row: value => Fraction((value.value*2).min(1.0))
        List(upper, lower)

  object Spinner:
    // Build from a run of single-cell frames written as one string — `⠋⠙⠹⠸⠼⠴⠦⠧⠇⠏` — split by code
    // point, which is how nearly every design in the catalogue is declared.
    def each
      ( frames:     Text,
         interval:   Int             = 80,
         repertoire: Gaugeable.Glyphs = Gaugeable.Glyphs.Unicode,
         narrower:   Optional[Spinner] = Unset )
    :   Spinner =

      Spinner(codepoints(frames), interval, 1, repertoire, narrower)

    // Split by code point rather than by `Char`, so that an astral frame (an emoji clock face) is
    // one frame and not two halves of a surrogate pair. Frames that are themselves multi-codepoint
    // clusters have to be given explicitly, as a `Sequence`.
    private def codepoints(text: Text): Sequence[Text] =
      val builder = scala.collection.immutable.Vector.newBuilder[Text]
      val string = text.s
      var index = 0

      while index < string.length do
        val codepoint = string.codePointAt(index)
        val width = java.lang.Character.charCount(codepoint)
        builder += string.substring(index, index + width).nn.tt
        index += width

      Sequence.from(builder.result())

  // A cyclic run of frames, shown one at a time. `columns` is how wide every frame is (they must
  // agree, or the row would shear as it animates), `repertoire` is the least adventurous character
  // set that can render it, and `narrower` is what to fall back to when this design will not fit or
  // is not permitted — the chain that lets an emoji design degrade to a BMP one and a wide marquee
  // to a single spinning cell.
  case class Spinner
    ( frames:     Sequence[Text],
      interval:   Int               = 80,
      columns:    Int               = 1,
      repertoire: Gaugeable.Glyphs  = Gaugeable.Glyphs.Unicode,
      narrower:   Optional[Spinner] = Unset ):

    // The first design in this one's fallback chain that fits `width` and is permitted here.
    def fit(width: Int, gauging: Gauging): Optional[Spinner] =
      if columns <= width && gauging.permits(repertoire) then this
      else narrower.lay(Unset: Optional[Spinner])(_.fit(width, gauging))

    // The narrowest design in the chain, which is what the layout is told this gauge needs.
    def leastColumns(gauging: Gauging): Int =
      narrower.lay(columns)(_.leastColumns(gauging).min(columns))

    // A spinner says the work is alive, not how far along it is, so it draws the same whether or
    // not a fraction is known — and the same again when there is none, which is what makes it the
    // design to import for indeterminate work.
    def gaugeable(using gauging: Gauging): Fraction is Gaugeable = new Gaugeable:
      type Self = Fraction
      override def period: Optional[Int] = interval
      override def elastic: Boolean = false
      override def minWidth(status: Fraction): Int = leastColumns(gauging)
      override def columns(status: Fraction): Int = leastColumns(gauging)
      override def absentColumns: Int = leastColumns(gauging)
      override def absent(tick: Tick, width: Int): List[Teletype] = frame(tick, width)

      def rows(status: Fraction, tick: Tick, width: Int): List[Teletype] = frame(tick, width)

      private def frame(tick: Tick, width: Int): List[Teletype] =
        val chosen = fit(width, gauging)

        val frame = chosen.lay(Teletype(t" "*width.max(0))): spinner =>
          val count = spinner.frames.size

          val index = tick.index.abs
          val glyph = if count == 0 then t" " else spinner.frames.at((index%count).z).or(t" ")

          val padding = width - spinner.columns
          val body = gauging.tint(gauging.palette.fill)(Teletype(glyph))
          if padding > 0 then e"$body${t" "*padding}" else body

        List(frame)

  object Standing:
    // The six glyphs one design uses for the six standings, and the engine that draws them. A
    // standing is carried by its glyph first and its colour second, so that the distinction
    // survives a monochrome terminal and a redirected stream.
    case class Marks
      ( succeeded:  Text,
        failed:     Text,
        warned:     Text,
        skipped:    Text,
        running:    Text,
        pending:    Text,
        columns:    Int              = 1,
        repertoire: Gaugeable.Glyphs = Gaugeable.Glyphs.Unicode,
        narrower:   Optional[Marks]  = Unset ):

      def apply(standing: Standing): Text = standing match
        case Standing.Succeeded => succeeded
        case Standing.Failed    => failed
        case Standing.Warned    => warned
        case Standing.Skipped   => skipped
        case Standing.Running   => running
        case Standing.Pending   => pending

      // The first design in the fallback chain that fits and is permitted.
      def fit(width: Int, gauging: Gauging): Optional[Marks] =
        if columns <= width && gauging.permits(repertoire) then this
        else narrower.lay(Unset: Optional[Marks])(_.fit(width, gauging))

      def leastColumns: Int = narrower.lay(columns)(_.leastColumns.min(columns))

      // Draw one standing, padded to `width`.
      def draw(standing: Standing, width: Int, gauging: Gauging): Teletype =
        fit(width, gauging).lay(Teletype(t" "*width.max(0))): marks =>
          val glyph = gauging.tint(gauging.palette.colorOf(standing))(Teletype(marks(standing)))
          val padding = width - marks.columns
          if padding > 0 then e"$glyph${t" "*padding}" else glyph

      def gaugeable(using gauging: Gauging): Standing is Gaugeable = new Gaugeable:
        type Self = Standing
        override def elastic: Boolean = false
        override def minWidth(status: Standing): Int = leastColumns
        override def columns(status: Standing): Int = leastColumns

        def rows(status: Standing, tick: Tick, width: Int): List[Teletype] =
          List(draw(status, width, gauging))

    // The default marks, used when nothing is imported: the near-universal tick and cross.
    given gaugeable: Gauging => Standing is Gaugeable = standings.tickStanding

  // How one unit of work has turned out — or has not yet. Kept separate from the glyphs that show
  // it, so that the same vocabulary serves a one-cell status marker, a checklist row and a colour
  // lookup in a palette.
  enum Standing:
    case Pending, Running, Succeeded, Failed, Warned, Skipped

  // One named stage of a multi-stage process, with how it has turned out and an optional note (the
  // error, the artifact, the duration) shown beside it where the design has room.
  case class Step(name: Text, standing: Standing, detail: Optional[Text] = Unset)

  object Stopwatch:
    // `hh:mm:ss`, dropping the hours until there are some. Padded, so the field does not change
    // width from one second to the next.
    private def digital(seconds: Long): Text =
      val total = seconds.max(0L)
      val minutes = total/60
      val hours = minutes/60

      def pad(value: Long): Text = if value < 10 then t"0$value" else value.show

      if hours > 0 then t"$hours:${pad(minutes%60)}:${pad(total%60)}"
      else t"${pad(minutes)}:${pad(total%60)}"

  // How a duration is written. `Compact` gives the two largest units that carry information
  // (`2m41s`); `Digital` gives a clock face (`02:41`), which is wider but does not change shape as
  // it crosses a minute. The same designs serve `Elapsed` and `Countdown`, but through separate
  // givens: the two are distinct types precisely so that a duration counting down can redden as it
  // runs out while one counting up stays quiet.
  enum Stopwatch:
    case Compact, Digital

    def write(seconds: Double): Text = this match
      case Compact => Magnitude.interval(seconds)
      case Digital => Stopwatch.digital(seconds.toLong)

    def columns(seconds: Double): Int = write(seconds).length

    // Elapsed time is reference material, not a warning, so it is drawn in the muted role.
    // Keyed on `aviation.Duration` itself: elapsed time is a duration, and wrapping it bought
    // nothing. `Countdown` keeps its own type, because the two must be able to appear together.
    def elapsed(using gauging: Gauging): Duration is Gaugeable = new Gaugeable:
      type Self = Duration
      override def elastic: Boolean = false
      override def minWidth(status: Duration): Int = 1
      override def columns(status: Duration): Int = Stopwatch.this.columns(status.value)

      def rows(status: Duration, tick: Tick, width: Int): List[Teletype] =
        List(draw(status.value, gauging.palette.muted, width, gauging))

    // A countdown, optionally colouring by how little is left: `urgent` reads the severity ramp
    // backwards, so the figure passes through the warning colour and reddens as it approaches zero.
    def countdown(urgent: Boolean)(using gauging: Gauging): Countdown is Gaugeable = new Gaugeable:
      type Self = Countdown
      override def elastic: Boolean = false
      override def minWidth(status: Countdown): Int = 1
      override def columns(status: Countdown): Int = Stopwatch.this.columns(status.duration.value)

      def rows(status: Countdown, tick: Tick, width: Int): List[Teletype] =
        val seconds = status.duration.value

        // Under a minute is where a countdown starts to matter; the ramp is read over that last
        // minute, so anything longer sits at the calm end of it.
        val color =
          if !urgent then gauging.palette.caption
          else gauging.palette.severity(1.0 - (seconds/60.0).min(1.0))

        List(draw(seconds, color, width, gauging))

    private def draw(seconds: Double, color: Color in Srgb, width: Int, gauging: Gauging)
    :   Teletype =

      val text = write(seconds)
      val used = gauging.cells(text)

      // Too narrow for the figure: drop its leading characters, so the seconds — the part that is
      // actually moving — are what survives.
      if used > width then gauging.tint(color)(Teletype(text.skip(used - width)))
      else gauging.tint(color)(Teletype(t"$text${t" "*(width - used)}"))

  // Bytes moved against bytes to move, with the time it has taken. The rate and the estimate are
  // *derived*, never stored, so a caller cannot hand a design an inconsistent triple — the
  // commonest bug in hand-rolled transfer displays, where the percentage and the ETA disagree. It
  // has no default design: whether to show a rate, whether to show an estimate, and whether to
  // count in `MB` or `MiB` are three editorial decisions, and a silent default would be wrong about
  // half the time.
  case class Transfer
    ( moved:   Quantity[Bytes[1]],
      total:   Optional[Quantity[Bytes[1]]] = Unset,
      elapsed: Duration                     = 0.0*Second ):

    def rate: Quantity[Bytes[1] & Seconds[-1]] =
      if elapsed.value <= 0 then 0.0*Byte/Second else moved/elapsed

    def fraction: Optional[Fraction] = total.let: total =>
      if total.value <= 0 then Fraction(0.0) else Fraction(moved.value/total.value)

    // How long the rest should take at the rate so far; `Unset` while nothing has moved (when any
    // estimate would be a fabrication) or while the total is unknown.
    def estimate: Optional[Duration] = total.lay(Unset: Optional[Duration]): total =>
      if rate.value <= 0 then Unset else ((total - moved).value/rate.value)*Second

  object Tick:
    val zero: Tick = Tick(0, 0L)

    def at(elapsed: Long, period: Int): Tick =
      Tick(if period <= 0 then 0 else (elapsed/period).toInt, elapsed)

  // The animation clock reading handed to a design each time it is drawn. `index` counts whole
  // animation periods since the gauge started — the frame number a spinner indexes its glyph cycle
  // with — and `elapsed` is monotonic milliseconds since it started.
  // Passing the reading in, rather than letting a design read a clock, is what keeps every design a
  // pure function of its status and its tick: a test drives one by handing it a literal `Tick`
  // instead of waiting.
  case class Tick(index: Int, elapsed: Long):
    // Where this reading falls within the current period, as a fraction in `[0, 1)`; the dial a
    // design uses when it wants a continuous sweep rather than a discrete frame.
    def phase(period: Int): Double = if period <= 0 then 0.0 else (elapsed%period).toDouble/period
