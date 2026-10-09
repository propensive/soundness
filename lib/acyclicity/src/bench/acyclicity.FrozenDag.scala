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
package acyclicity

// Deliberate stdlib opt-out, as in `Dag`.
import scala.collection.immutable.{List, Map, Nil, Set, ::}

// Candidate (c): a frozen, dense form for graphs that are built once and queried often. Nodes
// are numbered in topological order and both directions of adjacency are compressed sparse rows
// (an offsets array of n + 1 and a targets array of e), so successors, predecessors, sources
// and sinks are index arithmetic. Reachability is a bit matrix of n rows of ⌈n/64⌉ words, filled
// dependencies-first in O(e·n/64) and shared by `closure`, `reduction` (Aho–Garey–Ullman over
// the rows) and the O(1) `reaches`. Nothing is editable: a bounded edit that rebuilt O(n + e)
// would be exactly the dysasymptotic cost the gates exist to name, so editing is not offered.
//
// `ascending` records whether dependencies sit at lower indices (true) or, after `invert` —
// which swaps the two CSR pairs in O(1) rather than renumbering — at higher ones.
final class FrozenDag[node] private[acyclicity]
  ( names:          scala.IArray[AnyRef],
    index:          Map[node, Int],
    offsets:        scala.IArray[Int],
    targets:        scala.IArray[Int],
    reverseOffsets: scala.IArray[Int],
    reverseTargets: scala.IArray[Int],
    ascending:      Boolean ):

  val count: Int = names.length
  private val words: Int = (count + 63) >>> 6

  private def name(id: Int): node = names(id).asInstanceOf[node]

  // The dependencies-first position of an index.
  private def rank(id: Int): Int = if ascending then id else count - 1 - id
  private def atRank(position: Int): Int = if ascending then position else count - 1 - position

  def size: Int = count
  def edgeCount: Int = targets.length
  def nodes: Set[node] = index.keySet
  def has(node: node): Boolean = index.contains(node)

  def sorted: List[node] =
    var result: List[node] = Nil
    var position = count - 1

    while position >= 0 do
      result = name(atRank(position)) :: result
      position -= 1

    result

  private def slice(starts: scala.IArray[Int], ends: scala.IArray[Int], id: Int): Set[node] =
    val builder = Set.newBuilder[node]
    var slot = starts(id)

    while slot < starts(id + 1) do
      builder += name(ends(slot))
      slot += 1

    builder.result()

  def successors(node: node): Set[node] = index.get(node) match
    case Some(id) => slice(offsets, targets, id)
    case None     => Set()

  def predecessors(node: node): Set[node] = index.get(node) match
    case Some(id) => slice(reverseOffsets, reverseTargets, id)
    case None     => Set()

  def edges: Set[(node, node)] =
    val builder = Set.newBuilder[(node, node)]
    var id = 0

    while id < count do
      var slot = offsets(id)

      while slot < offsets(id + 1) do
        builder += ((name(id), name(targets(slot))))
        slot += 1

      id += 1

    builder.result()

  private def degreeless(starts: scala.IArray[Int]): Set[node] =
    val builder = Set.newBuilder[node]
    var id = 0

    while id < count do
      if starts(id + 1) == starts(id) then builder += name(id)
      id += 1

    builder.result()

  def sources: Set[node] = degreeless(offsets)
  def sinks: Set[node] = degreeless(reverseOffsets)

  def invert: FrozenDag[node] =
    new FrozenDag(names, index, reverseOffsets, reverseTargets, offsets, targets, !ascending)

  // Depth-first over the rows, marking on push so no index is pushed twice.
  def reachable(node: node): Set[node] = index.get(node) match
    case None => Set()

    case Some(start) =>
      val seen = new scala.Array[Boolean](count)
      val stack = new scala.Array[Int](count)
      val builder = Set.newBuilder[node]
      var top = 0
      stack(top) = start
      top += 1
      seen(start) = true

      while top > 0 do
        top -= 1
        val id = stack(top)
        builder += name(id)
        var slot = offsets(id)

        while slot < offsets(id + 1) do
          val child = targets(slot)

          if !seen(child) then
            seen(child) = true
            stack(top) = child
            top += 1

          slot += 1

      builder.result()

  // Row `id` holds the indices reachable from `id`, excluding `id`. Filled dependencies-first, so
  // each row is the union of its successors' rows plus the successors' own bits.
  lazy val reach: scala.IArray[Long] =
    val bits = new scala.Array[Long](count*words)
    var position = 0

    while position < count do
      val id = atRank(position)
      val row = id*words
      var slot = offsets(id)

      while slot < offsets(id + 1) do
        val child = targets(slot)
        val childRow = child*words
        var word = 0

        while word < words do
          bits(row + word) |= bits(childRow + word)
          word += 1

        bits(row + (child >>> 6)) |= 1L << (child & 63)
        slot += 1

      position += 1

    // Frozen on the way out: a fresh array cannot sit in a field of a class that is not itself
    // a capability, and nothing writes to it again.
    bits.asInstanceOf[scala.IArray[Long]]

  private def bit(bits: scala.IArray[Long], row: Int, id: Int): Boolean =
    (bits(row + (id >>> 6)) & (1L << (id & 63))) != 0L

  def reaches(from: node, to: node): Boolean = (index.get(from), index.get(to)) match
    case (Some(source), Some(target)) => bit(reach, source*words, target)
    case _                            => false

  def closure: Map[node, Set[node]] =
    val bits = reach
    val builder = Map.newBuilder[node, Set[node]]
    var id = 0

    while id < count do
      val row = Set.newBuilder[node]
      var other = 0

      while other < count do
        if bit(bits, id*words, other) then row += name(other)
        other += 1

      builder += ((name(id), row.result()))
      id += 1

    builder.result()

  // Aho–Garey–Ullman: a dependency is redundant when a later dependency (higher rank) of the
  // same node already reaches it, so visiting the dependencies by descending rank with a
  // running union of their rows decides every edge in O(degree·n/64).
  def reduction: Map[node, Set[node]] =
    val bits = reach
    val covered = new scala.Array[Long](words)
    var largest = 0
    var id = 0

    while id < count do
      largest = largest.max(offsets(id + 1) - offsets(id))
      id += 1

    val ordered = new scala.Array[Long](largest)
    val builder = Map.newBuilder[node, Set[node]]
    id = 0

    while id < count do
      java.util.Arrays.fill(covered, 0L)
      val degree = offsets(id + 1) - offsets(id)
      var slot = 0

      while slot < degree do
        val child = targets(offsets(id) + slot)
        ordered(slot) = (rank(child).toLong << 32) | child.toLong
        slot += 1

      java.util.Arrays.sort(ordered, 0, degree)
      val kept = Set.newBuilder[node]
      slot = degree - 1

      while slot >= 0 do
        val child = (ordered(slot) & 0xffffffffL).toInt

        if !bit(covered.asInstanceOf[scala.IArray[Long]], 0, child) then
          kept += name(child)
          var word = 0

          while word < words do
            covered(word) |= bits(child*words + word)
            word += 1

        slot -= 1

      builder += ((name(id), kept.result()))
      id += 1

    builder.result()

object FrozenDag:
  // From a topological order (dependencies first) and the successor relation: two counting
  // passes and two filling passes, O(n + e).
  def apply[node](order: List[node], successors: node => Set[node]): FrozenDag[node] =
    val count = order.length
    val names = new scala.Array[AnyRef](count)
    val indexBuilder = Map.newBuilder[node, Int]
    var position = 0

    order.foreach: node =>
      names(position) = node.asInstanceOf[AnyRef]
      indexBuilder += ((node, position))
      position += 1

    val index = indexBuilder.result()
    val offsets = new scala.Array[Int](count + 1)
    val reverseOffsets = new scala.Array[Int](count + 1)
    var id = 0

    while id < count do
      offsets(id + 1) = offsets(id) + successors(names(id).asInstanceOf[node]).size
      id += 1

    val targets = new scala.Array[Int](offsets(count))
    id = 0

    while id < count do
      var slot = offsets(id)

      successors(names(id).asInstanceOf[node]).foreach: target =>
        val child = index(target)
        targets(slot) = child
        slot += 1
        reverseOffsets(child + 1) += 1

      id += 1

    id = 0

    while id < count do
      reverseOffsets(id + 1) += reverseOffsets(id)
      id += 1

    val reverseTargets = new scala.Array[Int](offsets(count))
    val fill = reverseOffsets.clone
    id = 0

    while id < count do
      var slot = offsets(id)

      while slot < offsets(id + 1) do
        val child = targets(slot)
        reverseTargets(fill(child)) = id
        fill(child) += 1
        slot += 1

      id += 1

    new FrozenDag
      ( names.asInstanceOf[scala.IArray[AnyRef]],
        index,
        offsets.asInstanceOf[scala.IArray[Int]],
        targets.asInstanceOf[scala.IArray[Int]],
        reverseOffsets.asInstanceOf[scala.IArray[Int]],
        reverseTargets.asInstanceOf[scala.IArray[Int]],
        true )

  // Consuming a workspace: the caller gives up its handle, so the frozen graph can never be
  // edited behind its back. (In core this will move the workspace's arrays; here it reads them.)
  def apply[node](consume workspace: Workspace[node]^): FrozenDag[node] =
    val order = workspace.linearized
    val adjacency = workspace.adjacency

    FrozenDag(order, adjacency.getOrElse(_, Set()))
