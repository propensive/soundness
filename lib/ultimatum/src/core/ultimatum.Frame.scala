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

import scala.collection.immutable.Vector

import rudiments.*
import tessellate.*
import vacuous.*

object Frame:
  // Combine two optional maxima as a minimum, treating `Unset` as +infinity.
  private def lesser(a: Optional[Int], b: Optional[Int]): Optional[Int] =
    a.lay(b): av => b.lay(av)(bv => av.min(bv))

  // Min/max along the split axis: the minimum is the larger of the split's own
  // minimum and the SUM of its children's minima (a container can be no smaller
  // than its contents); the maximum is the smaller of its own maximum and the
  // sum of its children's maxima (any unbounded child makes the sum unbounded).
  private def alongLimits(own: Limits, children: List[Limits]): Limits =
    val minSum = children.fold(0): (acc, child: Limits) =>
      acc + child.min

    val maxSum: Optional[Int] = children.fold(0: Optional[Int]): (acc, child: Limits) =>
      acc.let: total =>
        child.max.let(total + _)

    Limits(own.min.max(minSum), lesser(own.max, maxSum))

  // Min/max across the cross axis: the minimum is the largest child minimum (the
  // cross extent must hold every child); the maximum is the smallest child
  // maximum.
  private def crossLimits(own: Limits, children: List[Limits]): Limits =
    val minMax = children.fold(0): (acc, child: Limits) =>
      acc.max(child.min)

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

    childHeights.grouped(cols).to(Vector).map: row =>
      row.map(_.min).maxOption.getOrElse(0)

  // A grid's width limits: its columns laid side by side with gaps, like a strip of the
  // per-column folds.
  private def gridWidthLimits
    ( own: Limits, childWidths: Vector[Limits], columns: Int, gap: Int )
  :   Limits =

    val cols = gridColumns(childWidths, columns)
    val gaps = gap*(cols.length - 1).max(0)
    val minSum = cols.map(_.min).sum + gaps

    val maxSum: Optional[Int] = cols.foldLeft(gaps: Optional[Int]): (acc, column) =>
      acc.let { total => column.max.let(total + _) }

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
// a root `Rect` runs two passes: a bottom-up MEASURE computing each node's (min,
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
        val childLimits = children.map(_.measure(arrangement)).stdlib.toVector

        arrangement match
          case Arrangement.Strip => Frame.gridWidthLimits(own, childLimits, columns, gap)
          case _                 => Frame.gridHeightLimits(own, childLimits, columns, gap)

      case Split(_, splitArrangement, children) =>
        val childLimits = children.map(_.measure(arrangement))

        if splitArrangement == arrangement then Frame.alongLimits(own, childLimits)
        else Frame.crossLimits(own, childLimits)

  // Solve this frame against a rectangle, producing a tree of placements.
  def arrange(rect: Rect): Placement = this match
    case Cell(_) =>
      Placement.Cell(rect)

    case Split(_, Arrangement.Grid(columns, gap), children) =>
      // Column widths are negotiated across every row with the shared solver (each column's
      // fold of its members' limits, weighted by its members' largest fraction); rows are
      // content-sized and stacked, clamped to the grid's rectangle.
      val childList = children.stdlib.toVector
      val widthLimits = childList.map(_.measure(Arrangement.Strip))
      val cols = columns.max(1).min(childList.length.max(1))

      val tracks = Vector.tabulate(cols): column =>
        val members = childList.indices.filter(_%cols == column)
        val limits = Frame.gridColumns(widthLimits, cols)(column)
        val weight = members.map(childList(_).sizing.fraction).maxOption.getOrElse(1.0)
        Flex(Metrics(limits.min), weight, limits.max)

      val colWidths = Flex.solve(Sequence.of(tracks), rect.width, gap).stdlib.map(_.or(0))
      val xs = colWidths.scanLeft(rect.left)((x, width) => x + width + gap)
      val rowHeights = Frame.gridRows(childList.map(_.measure(Arrangement.Stack)), cols)
      val ys = rowHeights.scanLeft(rect.top)((y, height) => y + height + gap)

      val placements = childList.indices.map: index =>
        val row = index/cols
        val column = index%cols
        val top = ys(row)
        val height = rowHeights(row).min((rect.top + rect.height - top).max(0))
        childList(index).arrange(Rect(xs(column), top, colWidths(column), height))

      Placement.Split(rect, List.of(placements.to(scala.List)))

    case Split(_, arrangement, children) =>
      val available = arrangement match
        case Arrangement.Strip => rect.width
        case _                 => rect.height

      // Each child is a flex track: its measured minimum, its fraction as the weight with
      // which it claims spare space, and its measured maximum as a hard cap. No track is
      // collapsible, so every solved size is defined.
      val tracks: List[Flex] =
        children.map: child =>
          val limits = child.measure(arrangement)
          Flex(Metrics(limits.min), child.sizing.fraction, limits.max)

      val sizes =
        Flex.solve(Sequence.of(tracks.stdlib.toVector), available).stdlib.map(_.or(0))

      val start = arrangement match
        case Arrangement.Strip => rect.left
        case _                 => rect.top

      val offsets = sizes.scanLeft(start)(_ + _)

      // Zip each child directly with its solved size and offset (`scanLeft` yields n + 1
      // offsets; the zip truncates to the n children), so no re-indexing is needed.
      val placements = children.stdlib.lazyZip(sizes).lazyZip(offsets).map:
        (child, size, offset) =>
          val childRect = arrangement match
            case Arrangement.Strip => Rect(offset, rect.top, size, rect.height)
            case _                 => Rect(rect.left, offset, rect.width, size)

          child.arrange(childRect)

      Placement.Split(rect, List.of(placements))
