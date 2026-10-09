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

import scala.collection.immutable as sci

import prepositional.*

// A directed acyclic graph frozen for querying: nodes numbered in topological order, both
// directions of adjacency as compressed sparse rows (an offsets array of n + 1 and a targets
// array of e), and — on first need — a reachability matrix of n rows of ⌈n/64⌉ words, filled
// dependencies-first in O(e·n/64) and shared by `closure`, `reduction` (Aho–Garey–Ullman over
// the rows) and the O(1) `reaches`. Successors, predecessors, sources and sinks are index
// arithmetic, and `invert` swaps the two directions in O(1) rather than renumbering:
// `ascending` records whether dependencies sit at lower indices (as built) or higher (inverted).
//
// Nothing is editable. A bounded edit that rebuilt O(n + e) would be exactly the dysasymptotic
// cost the gates exist to name, so editing is not offered: `thaw` to a `Dag` or a `Topology`,
// edit, and freeze again. Reading a bit row for a single node's `reachable` would be O(n/64)
// however small the answer, so that query walks the rows instead and the matrix serves the
// whole-graph ones.
//
// The loops here are the second shape `doc/standards/loops.md` sanctions — index arithmetic
// derived from the data — and each names the invariant that bounds it.
object Frozen:
  def apply[node](dag: Dag[node]): Frozen[node] =
    of(dag.linearized, node => dag.adjacency(node).iterator)

  // The caller gives up its handle, so the frozen graph can never be edited behind its back.
  def apply[node](consume topology: Topology[node]^): Frozen[node] =
    val adjacency = topology.adjacency
    of(topology.linearized, node => adjacency(node).iterator)

  // From a topological order (dependencies first) and the successor relation, in O(n + e): one
  // pass to count each node's successors, one to place them and count predecessors, one to
  // place those.
  private[acyclicity] def of[node](order: List[node], successors: node => Iterator[node])
  :   Frozen[node] =

    val count = List.size(order)
    val names = new scala.Array[AnyRef](count)
    val indexBuilder = sci.Map.newBuilder[node, Int]
    var position = 0

    List.iterator(order).foreach: node =>
      names(position) = node.asInstanceOf[AnyRef]
      indexBuilder += ((node, position))
      position += 1

    val index = indexBuilder.result()
    val offsets = new scala.Array[Int](count + 1)
    val reverseOffsets = new scala.Array[Int](count + 1)
    var id = 0

    // `id < count` and `names.length == count`, so every read of `names` is in range.
    while id < count do
      offsets(id + 1) = offsets(id) + successors(names(id).asInstanceOf[node]).size
      id += 1

    val targets = new scala.Array[Int](offsets(count))
    id = 0

    // `slot` runs from `offsets(id)` to `offsets(id + 1)`, the successors counted above, and
    // every `child` is an index of `names`, since every successor is a node of the order.
    while id < count do
      var slot = offsets(id)

      successors(names(id).asInstanceOf[node]).foreach: target =>
        val child = index(target)
        targets(slot) = child
        slot += 1
        reverseOffsets(child + 1) += 1

      id += 1

    id = 0

    // Prefix sums over `count + 1` entries.
    while id < count do
      reverseOffsets(id + 1) += reverseOffsets(id)
      id += 1

    val reverseTargets = new scala.Array[Int](offsets(count))
    val fill = reverseOffsets.clone
    id = 0

    // `fill(child)` advances from `reverseOffsets(child)` once per predecessor of `child`, of
    // which there are exactly `reverseOffsets(child + 1) - reverseOffsets(child)`.
    while id < count do
      var slot = offsets(id)

      while slot < offsets(id + 1) do
        val child = targets(slot)
        reverseTargets(fill(child)) = id
        fill(child) += 1
        slot += 1

      id += 1

    new Frozen
      ( names.asInstanceOf[scala.IArray[AnyRef]],
        index,
        offsets.asInstanceOf[scala.IArray[Int]],
        targets.asInstanceOf[scala.IArray[Int]],
        reverseOffsets.asInstanceOf[scala.IArray[Int]],
        reverseTargets.asInstanceOf[scala.IArray[Int]],
        true )

  given nodal: [node] => (Frozen[node] is Nodal by node) = new Nodal:
    type Self = Frozen[node]
    type Operand = node
    def nodes(self: Frozen[node]): Iterator[node] = self.ordered
    def has(self: Frozen[node], node: node): Boolean = self.has(node)
    def successors(self: Frozen[node], node: node): Iterator[node] = self.successorIterator(node)

  given bidirectional: [node] => (Frozen[node] is Bidirectional by node) = new Bidirectional:
    type Self = Frozen[node]
    type Operand = node

    def predecessors(self: Frozen[node], node: node): Iterator[node] =
      self.predecessorIterator(node)

  given topological: [node] => Frozen[node] is Topological =
    new Topological { type Self = Frozen[node] }

final class Frozen[node] private[acyclicity]
  ( names:          scala.IArray[AnyRef],
    index:          sci.Map[node, Int],
    offsets:        scala.IArray[Int],
    targets:        scala.IArray[Int],
    reverseOffsets: scala.IArray[Int],
    reverseTargets: scala.IArray[Int],
    ascending:      Boolean ):

  val count: Int = names.length
  private val words: Int = (count + 63) >>> 6

  private def name(id: Int): node = names(id).asInstanceOf[node]

  // The dependencies-first position of an index, and its inverse.
  private def rank(id: Int): Int = if ascending then id else count - 1 - id
  private def atRank(position: Int): Int = if ascending then position else count - 1 - position

  def size: Int = count
  def edgeCount: Int = targets.length
  def has(node: node): Boolean = index.contains(node)
  def nodes: Set[node] = Set.from(index.keySet)

  private[acyclicity] def ordered: Iterator[node] =
    Iterator.range(0, count).map: position => name(atRank(position))

  // Every node after everything it points at.
  def linearized: List[node] = List.from(ordered)

  private def slice(starts: scala.IArray[Int], ends: scala.IArray[Int], id: Int): Iterator[node] =
    Iterator.range(starts(id), starts(id + 1)).map: slot => name(ends(slot))

  private[acyclicity] def successorIterator(node: node): Iterator[node] = index.get(node) match
    case Some(id) => slice(offsets, targets, id)
    case None     => Iterator.empty

  private[acyclicity] def predecessorIterator(node: node): Iterator[node] = index.get(node) match
    case Some(id) => slice(reverseOffsets, reverseTargets, id)
    case None     => Iterator.empty

  def successors(node: node): Set[node] = Set.from(successorIterator(node))
  def predecessors(node: node): Set[node] = Set.from(predecessorIterator(node))

  def edges: Set[(node, node)] =
    Set.from:
      Iterator.range(0, count).flatMap: id => slice(offsets, targets, id).map(name(id) -> _)

  private def degreeless(starts: scala.IArray[Int]): Set[node] =
    Set.from(Iterator.range(0, count).filter { id => starts(id + 1) == starts(id) }.map(name))

  def sources: Set[node] = degreeless(offsets)
  def sinks: Set[node] = degreeless(reverseOffsets)

  def invert: Frozen[node] =
    new Frozen(names, index, reverseOffsets, reverseTargets, offsets, targets, !ascending)

  // Depth-first over the rows, marking on push, so every index is pushed at most once and the
  // stack of `count` entries never overflows.
  def reachable(node: node): Set[node] = index.get(node) match
    case None => Set()

    case Some(start) =>
      val seen = new scala.Array[Boolean](count)
      val stack = new scala.Array[Int](count)
      val builder = sci.Set.newBuilder[node]
      var top = 0
      stack(top) = start
      top += 1
      seen(start) = true

      // Terminated by state: the stack empties once every reachable index has been popped.
      while top > 0 do
        top -= 1
        val id = stack(top)
        builder += name(id)
        var slot = offsets(id)

        // `slot` runs over the compressed row of `id`.
        while slot < offsets(id + 1) do
          val child = targets(slot)

          if !seen(child) then
            seen(child) = true
            stack(top) = child
            top += 1

          slot += 1

      Set.from(builder.result())

  // Row `id` holds the indices reachable from `id`, excluding `id`: filled dependencies-first,
  // so each row is the union of its successors' rows plus the successors' own bits.
  lazy val reach: scala.IArray[Long] =
    val bits = new scala.Array[Long](count*words)
    var position = 0

    // `id*words + word < count*words` for `id < count` and `word < words`.
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

  // Whether `from` reaches `to`, in O(1) once the matrix exists.
  def reaches(from: node, to: node): Boolean = (index.get(from), index.get(to)) match
    case (Some(source), Some(target)) => bit(reach, source*words, target)
    case _                            => false

  // Every implied edge made explicit.
  def closure: Dag[node] =
    val bits = reach

    Dag.unchecked:
      sci.VectorMap.from:
        Iterator.range(0, count).map: id =>
          val row = sci.Set.newBuilder[node]

          Iterator.range(0, count).foreach: other =>
            if bit(bits, id*words, other) then row += name(other)

          (name(id), row.result())

  // Aho–Garey–Ullman: a successor is redundant when a later successor (higher rank) of the same
  // node already reaches it, so visiting the successors by descending rank with a running union
  // of their rows decides every edge in O(degree·n/64).
  def reduction: Dag[node] =
    val bits = reach
    val covered = new scala.Array[Long](words)
    var largest = 0
    var id = 0

    // The widest row, to size the sort buffer.
    while id < count do
      largest = largest.max(offsets(id + 1) - offsets(id))
      id += 1

    val ordered = new scala.Array[Long](largest)
    val builder = sci.VectorMap.newBuilder[node, sci.Set[node]]
    id = 0

    // `degree <= largest`, so `ordered` holds every successor of `id`, each packed as its rank
    // in the high word and its index in the low.
    while id < count do
      java.util.Arrays.fill(covered, 0L)
      val degree = offsets(id + 1) - offsets(id)
      var slot = 0

      while slot < degree do
        val child = targets(offsets(id) + slot)
        ordered(slot) = (rank(child).toLong << 32) | child.toLong
        slot += 1

      java.util.Arrays.sort(ordered, 0, degree)
      val kept = sci.Set.newBuilder[node]
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

    Dag.unchecked(builder.result())

  // Back to the persistent form, in topological order.
  def thaw: Dag[node] =
    Dag.unchecked:
      sci.VectorMap.from:
        ordered.map: node => (node, sci.Set.from(successorIterator(node)))

  override def equals(other: Any): Boolean = other.asInstanceOf[Matchable] match
    case that: Frozen[?] => edges == that.edges
    case _               => false

  override def hashCode: Int = edges.hashCode
  override def toString: String = s"Frozen(${count} nodes, ${targets.length} edges)"
