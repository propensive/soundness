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
