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
package tessellate

import vacuous.*

object Flex:
  // A track whose bounds derive entirely from its content: it may shrink to its min-content
  // width, never grows beyond its natural width, and competes for spare space in proportion
  // to how much it can usefully absorb.
  def content(metrics: Metrics): Flex =
    Flex(metrics, (metrics.natural - metrics.min).max(0).toDouble, metrics.natural)

  // Allot `available` cells to `tracks`, separated by `gap` cells. Two stages:
  //
  // COLLAPSE: while the tracks' minima (plus gaps) cannot fit, drop the collapsible track of
  // lowest `rank` (leftmost on ties) entirely; a dropped track yields `Unset`. If nothing
  // more can collapse, the minima overflow `available` and the caller decides what to do.
  //
  // DISTRIBUTE: among surviving tracks, iteratively fix any track whose fair share
  // (`pool*weight(i)/totalWeight`) violates its min or max bound (removing it from the pool
  // and redistributing) until a fixed point, then hand the still-free tracks their weighted
  // shares with largest-remainder (Hamilton) rounding so the sizes sum to exactly the pool.
  def solve(tracks: Sequence[Flex], available: Int, gap: Int = 0): Sequence[Optional[Int]] =
    val n = tracks.stdlib.length

    // The working state, hoisted once into index-aligned local arrays so every read in the
    // fixed-point loops below is total `Array` indexing (`i` ranges over `0 until n`).
    val minimum = scala.Array.fill(n)(0)
    val cap = scala.Array.fill[Optional[Int]](n)(Unset)
    val weight = scala.Array.fill(n)(0.0)
    val rank = scala.Array.fill(n)(0)
    val collapsible = scala.Array.fill(n)(false)
    val active = scala.Array.fill(n)(true)
    val pinned = scala.Array.fill[Optional[Int]](n)(Unset)

    var index = 0
    val trackCells = tracks.stdlib.iterator

    while index < n && trackCells.hasNext do
      val track = trackCells.next()
      minimum(index) = track.metrics.min
      cap(index) = track.max
      weight(index) = track.weight
      rank(index) = track.rank
      collapsible(index) = track.collapsible
      index += 1

    def activeCount: Int =
      var count = 0
      var i = 0

      while i < n do
        if active(i) then count += 1
        i += 1

      count

    def minTotal: Int =
      var total = 0
      var i = 0

      while i < n do
        if active(i) then total += minimum(i)
        i += 1

      total + gap*(activeCount - 1).max(0)

    var collapsing = true

    while collapsing do
      collapsing = false

      if minTotal > available then
        var best = -1
        var i = 0

        while i < n do
          if active(i) && collapsible(i) && (best < 0 || rank(i) < rank(best)) then best = i
          i += 1

        if best >= 0 then
          active(best) = false
          collapsing = true

    val pool0 = (available - gap*(activeCount - 1).max(0)).max(0)

    def poolAndWeight(): (Int, Double) =
      var used = 0
      var free = 0.0
      var i = 0

      while i < n do
        if active(i) then pinned(i).let(used += _).or(free += weight(i))
        i += 1

      ((pool0 - used).max(0), free)

    var changed = true

    while changed do
      changed = false
      val (pool, totalWeight) = poolAndWeight()
      var i = 0

      while i < n do
        if active(i) && pinned(i).absent then
          val ideal = if totalWeight <= 0.0 then 0.0 else pool*weight(i)/totalWeight

          if ideal < minimum(i) then
            pinned(i) = minimum(i)
            changed = true
          else
            cap(i).let: hi =>
              if ideal > hi then
                pinned(i) = hi
                changed = true

        i += 1

    val (pool, totalWeight) = poolAndWeight()
    val sizes = scala.Array.fill[Optional[Int]](n)(Unset)
    val remainders = scala.Array.fill(n)(0.0)
    var floorSum = 0
    var i = 0

    while i < n do
      if active(i) then
        pinned(i).let { size => sizes(i) = size }.or:
          val raw = if totalWeight <= 0.0 then 0.0 else pool*weight(i)/totalWeight
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
        if active(j) && pinned(j).absent && remainders(j) > bestRemainder then
          bestRemainder = remainders(j)
          best = j

        j += 1

      if best < 0 then remainder = 0
      else
        sizes(best) = sizes(best).or(0) + 1
        remainders(best) = -1.0
        remainder -= 1

    Sequence.from(sizes.iterator)

// One track (a column, row or pane extent) competing for space along an axis: its content's
// intrinsic `metrics`, the `weight` with which it claims spare space, an optional hard upper
// bound `max` (`Unset` = unbounded), and its collapse behavior — a `collapsible` track may be
// dropped entirely (yielding no space at all) when the axis cannot accommodate every track's
// minimum, lower `rank` collapsing first.
case class Flex
  ( metrics:     Metrics,
    weight:      Double        = 1.0,
    max:         Optional[Int] = Unset,
    rank:        Int           = 0,
    collapsible: Boolean       = false )
