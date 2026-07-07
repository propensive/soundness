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

import vacuous.*

object Frame:
  // Combine two optional maxima as a minimum, treating `Unset` as +infinity.
  private def lesser(a: Optional[Int], b: Optional[Int]): Optional[Int] =
    a.lay(b): av =>
      b.lay(av): bv =>
        av.min(bv)

  // Min/max along the split axis: the minimum is the larger of the split's own
  // minimum and the SUM of its children's minima (a container can be no smaller
  // than its contents); the maximum is the smaller of its own maximum and the
  // sum of its children's maxima (any unbounded child makes the sum unbounded).
  private def alongLimits(own: Limits, children: List[Limits]): Limits =
    val minSum = children.foldLeft(0): (acc, child) =>
      acc + child.min

    val maxSum: Optional[Int] = children.foldLeft(0: Optional[Int]): (acc, child) =>
      acc.let: total =>
        child.max.let(total + _)

    Limits(own.min.max(minSum), lesser(own.max, maxSum))

  // Min/max across the cross axis: the minimum is the largest child minimum (the
  // cross extent must hold every child); the maximum is the smallest child
  // maximum.
  private def crossLimits(own: Limits, children: List[Limits]): Limits =
    val minMax = children.foldLeft(0): (acc, child) =>
      acc.max(child.min)

    val maxMin = children.foldLeft(Unset: Optional[Int]): (acc, child) =>
      lesser(acc, child.max)

    Limits(own.min.max(minMax), lesser(own.max, maxMin))

  // Distribute `available` cells among children by fraction. Iteratively fix any
  // child whose fair share violates its min or max (removing it from the pool and
  // redistributing) until a fixed point, then hand the still-free children their
  // fractional shares with largest-remainder (Hamilton) rounding so the sizes sum
  // to exactly `available`.
  def distribute(fractions: List[Double], limits: List[Limits], available: Int): IndexedSeq[Int] =
    val n = fractions.length
    val frac = fractions.toIndexedSeq
    val min = limits.map(_.min).toIndexedSeq
    val max = limits.map(_.max).toIndexedSeq
    val pinned = Array.fill[Optional[Int]](n)(Unset)

    def poolAndWeight(): (Int, Double) =
      var used = 0
      var weight = 0.0
      var i = 0

      while i < n do
        if pinned(i).present then used += pinned(i).vouch else weight += frac(i)
        i += 1

      ((available - used).max(0), weight)

    var changed = true

    while changed do
      changed = false
      val (pool, weight) = poolAndWeight()
      var i = 0

      while i < n do
        if pinned(i).absent then
          val ideal = if weight <= 0.0 then 0.0 else pool*frac(i)/weight

          if ideal < min(i) then
            pinned(i) = min(i)
            changed = true
          else
            max(i).let: hi =>
              if ideal > hi then
                pinned(i) = hi
                changed = true

        i += 1

    val (pool, weight) = poolAndWeight()
    val sizes = Array.fill(n)(0)
    val remainders = Array.fill(n)(0.0)
    var floorSum = 0
    var i = 0

    while i < n do
      if pinned(i).present then sizes(i) = pinned(i).vouch
      else
        val raw = if weight <= 0.0 then 0.0 else pool*frac(i)/weight
        val floor = raw.toInt
        sizes(i) = floor
        remainders(i) = raw - floor
        floorSum += floor

      i += 1

    var remainder = pool - floorSum

    while remainder > 0 do
      var best = -1
      var bestRemainder = -1.0
      var j = 0

      while j < n do
        if pinned(j).absent && remainders(j) > bestRemainder then
          bestRemainder = remainders(j)
          best = j

        j += 1

      if best < 0 then remainder = 0
      else
        sizes(best) += 1
        remainders(best) = -1.0
        remainder -= 1

    sizes.toIndexedSeq

// A node in a layout tree: a `Cell` (a leaf panel that hosts content) or a
// `Split` that divides its space among children along an `Axis`. Solving against
// a root `Rect` runs two passes: a bottom-up MEASURE computing each node's (min,
// max) on each axis (forcing a split's minimum up to the aggregate of its
// children), then a top-down ARRANGE distributing space by fraction, fixing any
// child that hits a bound, and recursing.
enum Frame:
  def sizing: Sizing

  case Cell(sizing: Sizing)
  case Split(sizing: Sizing, axis: Axis, children: List[Frame])

  // The resolved (min, max) of this frame along `axis`.
  def measure(axis: Axis): Limits =
    val own = axis match
      case Axis.File => Limits(sizing.minWidth, sizing.maxWidth)
      case Axis.Rank => Limits(sizing.minHeight, sizing.maxHeight)

    this match
      case Cell(_) =>
        own

      case Split(_, splitAxis, children) =>
        val childLimits = children.map(_.measure(axis))

        if splitAxis == axis then Frame.alongLimits(own, childLimits)
        else Frame.crossLimits(own, childLimits)

  // Solve this frame against a rectangle, producing a tree of placements.
  def arrange(rect: Rect): Placement = this match
    case Cell(_) =>
      Placement.Cell(rect)

    case Split(_, axis, children) =>
      val available = axis match
        case Axis.File => rect.width
        case Axis.Rank => rect.height

      val limits = children.map(_.measure(axis))
      val fractions = children.map(_.sizing.fraction)
      val sizes = Frame.distribute(fractions, limits, available)

      val start = axis match
        case Axis.File => rect.left
        case Axis.Rank => rect.top

      val offsets = sizes.scanLeft(start)(_ + _)

      val placements = children.zipWithIndex.map: (child, i) =>
        val childRect = axis match
          case Axis.File => Rect(offsets(i), rect.top, sizes(i), rect.height)
          case Axis.Rank => Rect(rect.left, offsets(i), rect.width, sizes(i))

        child.arrange(childRect)

      Placement.Split(rect, placements)
