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

import scala.caps
import scala.collection.immutable as sci

import contingency.*
import prepositional.*

// A directed acyclic graph edited in place, under separation checking: the exclusive handle is
// the only route to its tables, so there is nothing for the checker to be unable to express. The
// name is for what it uniquely maintains — a live topological order of the nodes, kept valid
// through every `add` by Pearce and Kelly's algorithm, which touches only the nodes between the
// two ends of an offending edge and refuses one that would close a cycle. So the graph is
// acyclic by invariant and `Topological` by construction, and `linearized` is a read of the
// order.
//
// Mutability is the capture set, as for `Array`: a `Topology[node]^` is editable, and
// `Topology.freeze` consumes it to give a `Topology[node]^{}`, on which the `update` methods are
// not callable and nothing can ever change. The frozen form is the one for querying: it carries
// the `Nodal`, `Bidirectional` and `Topological` instances, and its reachability matrix — rows
// of ⌈n/64⌉ words, built on first use in O(e·n/64) — answers `reaches` in O(1) and serves
// `closure` and `reduction` (Aho–Garey–Ullman over the rows). The matrix is cached against an
// edit revision: a frozen handle, which cannot be edited, keeps it; an editable one that has been
// edited since recomputes it for the call, since a pure reference could not replace the cache.
//
// Every table is a dense array by node id: adjacency in both directions as singly-linked edge
// lists threaded through one slot pool, in- and out-degrees, the sources and sinks as swap-remove
// bags, and the node-to-id dictionary as an open-addressing hash table. Slot and bag indices are
// stored one-based, so a zero-filled array means "none". The loops are the second shape
// `doc/standards/loops.md` sanctions — index arithmetic derived from the data — with the
// invariant that bounds each stated above it.
object Topology:
  def apply[node](): Topology[node]^ = new Topology()

  // Freezing consumes the handle: `consume` statically retires every writer, so the surviving
  // reference can drop its exclusivity without copying — the launder only forgets a capability
  // that is no longer exercisable. This is the only producer of a `Topology[node]^{}`.
  def freeze[node](consume topology: Topology[node]^): Topology[node]^{} =
    caps.unsafe.unsafeAssumePure(topology)

  // A frozen topology from an order (dependencies first) and the successor relation: nodes
  // arrive already ordered, so no edge needs reordering.
  private[acyclicity] def of[node](order: List[node], successors: node => Iterator[node])
  :   Topology[node]^{} =

    val topology: Topology[node]^ = new Topology()
    val nodes = List.iterator(order)
    while nodes.hasNext do topology.add(nodes.next())
    val again = List.iterator(order)

    while again.hasNext do
      val node = again.next()
      val children = successors(node)
      while children.hasNext do topology.attach(node, children.next())

    freeze(topology)

  // A fresh, editable copy of a frozen topology.
  def apply[node](frozen: Topology[node]^{}): Topology[node]^ =
    val topology: Topology[node]^ = new Topology()
    val nodes = List.iterator(frozen.linearized)
    while nodes.hasNext do topology.add(nodes.next())
    val again = List.iterator(frozen.linearized)

    while again.hasNext do
      val node = again.next()
      val children = Set.iterator(frozen.successors(node))
      while children.hasNext do topology.attach(node, children.next())

    topology

  given nodal: [node] => ((Topology[node]^{}) is Nodal by node) = new Nodal:
    type Self = Topology[node]^{}
    type Operand = node
    def nodes(self: Topology[node]^{}): Iterator[node] = List.iterator(self.linearized)
    def has(self: Topology[node]^{}, node: node): Boolean = self.has(node)

    def successors(self: Topology[node]^{}, node: node): Iterator[node] =
      Set.iterator(self.successors(node))

  given bidirectional: [node] => ((Topology[node]^{}) is Bidirectional by node) =
    new Bidirectional:
      type Self = Topology[node]^{}
      type Operand = node

      def predecessors(self: Topology[node]^{}, node: node): Iterator[node] =
        Set.iterator(self.predecessors(node))

  given topological: [node] => (Topology[node]^{}) is Topological =
    new Topological { type Self = Topology[node]^{} }

  // Thawing a persistent graph: nodes in topological order, so no edge needs reordering.
  def apply[node](dag: Dag[node]): Topology[node]^ =
    val topology: Topology[node]^ = new Topology()
    val order = List.iterator(dag.linearized)
    while order.hasNext do topology.add(order.next())
    val entries = dag.adjacency.iterator

    while entries.hasNext do
      val (from, targets) = entries.next()
      val children = targets.iterator
      while children.hasNext do topology.attach(from, children.next())

    topology

  // State-free helpers live here rather than on the class: passing a field of `this` to a
  // method on `this` is a separation failure, while a companion method sees only its argument.
  def grown(array: scala.Array[Int]^{caps.any.rd}, size: Int): scala.Array[Int]^ =
    val bigger: scala.Array[Int]^ = new scala.Array[Int](size)
    System.arraycopy(array, 0, bigger, 0, array.length)
    bigger

  def grownLongs(array: scala.Array[Long]^{caps.any.rd}, size: Int): scala.Array[Long]^ =
    val bigger: scala.Array[Long]^ = new scala.Array[Long](size)
    System.arraycopy(array, 0, bigger, 0, array.length)
    bigger

  def grownRefs(array: scala.Array[AnyRef]^{caps.any.rd}, size: Int): scala.Array[AnyRef]^ =
    val bigger: scala.Array[AnyRef]^ = new scala.Array[AnyRef](size)
    System.arraycopy(array, 0, bigger, 0, array.length)
    bigger

  def grownFlags(array: scala.Array[Boolean]^{caps.any.rd}, size: Int): scala.Array[Boolean]^ =
    val bigger: scala.Array[Boolean]^ = new scala.Array[Boolean](size)
    System.arraycopy(array, 0, bigger, 0, array.length)
    bigger

  // Spread the hash's high bits into the low ones the mask keeps.
  def hash(key: Any): Int =
    val raw = key.##
    raw ^ (raw >>> 16)

final class Topology[node] private[acyclicity]()
extends caps.ExclusiveCapability, caps.Stateful:
  private[acyclicity] var count: Int = 0       // ids allocated, removed ones included
  private[acyclicity] var living: Int = 0
  private[acyclicity] var edgeTotal: Int = 0
  private[acyclicity] var slots: Int = 0       // edge slots allocated, unlinked ones included
  private[acyclicity] var epoch: Int = 0
  private[acyclicity] var revision: Int = 0    // bumped by every edit that changes reachability

  // The dictionary: open addressing with linear probing, capacity a power of two, at most half
  // full. `keys(slot)` is the node whose id is `ids(slot)`, or null for an empty slot.
  private[acyclicity] var keys: scala.Array[AnyRef | Null]^ = new scala.Array[AnyRef | Null](32)
  private[acyclicity] var ids: scala.Array[Int]^ = new scala.Array[Int](32)
  private[acyclicity] var occupied: Int = 0

  // Per node, by id.
  private[acyclicity] var names: scala.Array[AnyRef]^ = new scala.Array[AnyRef](16)
  private[acyclicity] var alive: scala.Array[Boolean]^ = new scala.Array[Boolean](16)
  private[acyclicity] var outHead: scala.Array[Int]^ = new scala.Array[Int](16)
  private[acyclicity] var inHead: scala.Array[Int]^ = new scala.Array[Int](16)
  private[acyclicity] var outDegree: scala.Array[Int]^ = new scala.Array[Int](16)
  private[acyclicity] var inDegree: scala.Array[Int]^ = new scala.Array[Int](16)
  private[acyclicity] var rank: scala.Array[Int]^ = new scala.Array[Int](16)      // id -> position
  private[acyclicity] var atRank: scala.Array[Int]^ = new scala.Array[Int](16)  // rank -> 1 + id
  private[acyclicity] var stamp: scala.Array[Int]^ = new scala.Array[Int](16)
  private[acyclicity] var sourceList: scala.Array[Int]^ = new scala.Array[Int](16)
  private[acyclicity] var sourceSlot: scala.Array[Int]^ = new scala.Array[Int](16) // id -> 1 + slot
  private[acyclicity] var sinkList: scala.Array[Int]^ = new scala.Array[Int](16)
  private[acyclicity] var sinkSlot: scala.Array[Int]^ = new scala.Array[Int](16)
  private[acyclicity] var sourceCount: Int = 0
  private[acyclicity] var sinkCount: Int = 0

  // Scratch for the reordering search, sized with the node tables.
  private[acyclicity] var stack: scala.Array[Int]^ = new scala.Array[Int](16)
  private[acyclicity] var forward: scala.Array[Long]^ = new scala.Array[Long](16)
  private[acyclicity] var backward: scala.Array[Long]^ = new scala.Array[Long](16)

  // Per edge slot.
  private[acyclicity] var edgeFrom: scala.Array[Int]^ = new scala.Array[Int](16)
  private[acyclicity] var edgeTo: scala.Array[Int]^ = new scala.Array[Int](16)
  private[acyclicity] var outNext: scala.Array[Int]^ = new scala.Array[Int](16)   // 1 + next slot
  private[acyclicity] var inNext: scala.Array[Int]^ = new scala.Array[Int](16)

  def size: Int = living
  def edgeCount: Int = edgeTotal

  private[acyclicity] def words: Int = (count + 63) >>> 6

  // Row `id` holds the ids reachable from `id`, excluding `id`, filled dependencies-first over
  // the live nodes so that each row is the union of its successors' rows plus their own bits.
  private def buildReach(): scala.IArray[Long] =
    val words = this.words
    val bits = new scala.Array[Long](count*words)
    var position = 0

    // `id*words + word < count*words` for `id < count` and `word < words`; each list ends at 0.
    while position < count do
      if atRank(position) != 0 then
        val id = atRank(position) - 1
        val row = id*words
        var slot = outHead(id)

        while slot != 0 do
          val child = edgeTo(slot - 1)
          val childRow = child*words
          var word = 0

          while word < words do
            bits(row + word) |= bits(childRow + word)
            word += 1

          bits(row + (child >>> 6)) |= 1L << (child & 63)
          slot = outNext(slot - 1)

      position += 1

    // Frozen on the way out: nothing writes to it again.
    bits.asInstanceOf[scala.IArray[Long]]

  // The matrix, with the revision it was built at: initialised once, by whichever handle first
  // asks, which a pure one may do.
  private lazy val cachedReach: (Int, scala.IArray[Long]) = (revision, buildReach())

  // The matrix as of now: the cached one unless an edit has happened since it was built — never
  // the case on a frozen handle — in which case one is built for this call and not kept.
  private[acyclicity] def reach: scala.IArray[Long] =
    if cachedReach(0) == revision then cachedReach(1) else buildReach()

  private[acyclicity] def bit(bits: scala.IArray[Long], row: Int, id: Int): Boolean =
    (bits(row + (id >>> 6)) & (1L << (id & 63))) != 0L

  // ─── the dictionary ───────────────────────────────────────────────────────

  // The slot holding `node`, or -1. Terminated by state: the table is never full, so the probe
  // reaches an empty slot.
  private[acyclicity] def slotOf(node: node): Int =
    val mask = keys.length - 1
    var slot = Topology.hash(node) & mask
    var result = -1
    var probing = true

    while probing do
      val key = keys(slot)

      if key == null then probing = false
      else if key == node then
        result = slot
        probing = false
      else
        slot = (slot + 1) & mask

    result

  def has(node: node): Boolean = slotOf(node) >= 0

  private[acyclicity] update def growIndex(): Unit =
    val capacity = keys.length*2
    val bigger: scala.Array[AnyRef | Null]^ = new scala.Array[AnyRef | Null](capacity)
    val biggerIds: scala.Array[Int]^ = new scala.Array[Int](capacity)
    val mask = capacity - 1
    var from = 0

    // Every occupied slot of the old table lands in an empty slot of the larger one.
    while from < keys.length do
      val key = keys(from)

      if key != null then
        var slot = Topology.hash(key) & mask
        while bigger(slot) != null do slot = (slot + 1) & mask
        bigger(slot) = key
        biggerIds(slot) = ids(from)

      from += 1

    keys = bigger
    ids = biggerIds

  private[acyclicity] update def insertIndex(node: node, id: Int): Unit =
    if (occupied + 1)*2 > keys.length then growIndex()
    val mask = keys.length - 1
    var slot = Topology.hash(node) & mask
    // Terminated by state: at most half the slots are occupied.
    while keys(slot) != null do slot = (slot + 1) & mask
    keys(slot) = node.asInstanceOf[AnyRef]
    ids(slot) = id
    occupied += 1

  // Backward-shift deletion: later entries of the same probe run move up into the hole, so no
  // tombstones are needed.
  private[acyclicity] update def deleteIndex(slot: Int): Unit =
    val mask = keys.length - 1
    var hole = slot
    var next = (hole + 1) & mask

    // Terminated by state: the run ends at an empty slot.
    while keys(next) != null do
      val home = Topology.hash(keys(next)) & mask

      // The entry may move iff the hole lies on its probe path from `home`.
      if ((hole - home) & mask) < ((next - home) & mask) then
        keys(hole) = keys(next)
        ids(hole) = ids(next)
        hole = next

      next = (next + 1) & mask

    keys(hole) = null
    occupied -= 1

  // ─── growth ───────────────────────────────────────────────────────────────

  private[acyclicity] update def growNodes(): Unit =
    val capacity = names.length*2
    names = Topology.grownRefs(names, capacity)
    alive = Topology.grownFlags(alive, capacity)
    outHead = Topology.grown(outHead, capacity)
    inHead = Topology.grown(inHead, capacity)
    outDegree = Topology.grown(outDegree, capacity)
    inDegree = Topology.grown(inDegree, capacity)
    rank = Topology.grown(rank, capacity)
    atRank = Topology.grown(atRank, capacity)
    stamp = Topology.grown(stamp, capacity)
    sourceList = Topology.grown(sourceList, capacity)
    sourceSlot = Topology.grown(sourceSlot, capacity)
    sinkList = Topology.grown(sinkList, capacity)
    sinkSlot = Topology.grown(sinkSlot, capacity)
    stack = Topology.grown(stack, capacity)
    forward = Topology.grownLongs(forward, capacity)
    backward = Topology.grownLongs(backward, capacity)

  private[acyclicity] update def growEdges(): Unit =
    val capacity = edgeFrom.length*2
    edgeFrom = Topology.grown(edgeFrom, capacity)
    edgeTo = Topology.grown(edgeTo, capacity)
    outNext = Topology.grown(outNext, capacity)
    inNext = Topology.grown(inNext, capacity)

  // ─── the source and sink bags ─────────────────────────────────────────────

  private[acyclicity] update def enlistSource(id: Int): Unit =
    if sourceSlot(id) == 0 then
      sourceList(sourceCount) = id
      sourceSlot(id) = sourceCount + 1
      sourceCount += 1

  private[acyclicity] update def delistSource(id: Int): Unit =
    val slot = sourceSlot(id) - 1

    if slot >= 0 then
      val last = sourceList(sourceCount - 1)
      sourceList(slot) = last
      sourceSlot(last) = slot + 1
      sourceSlot(id) = 0
      sourceCount -= 1

  private[acyclicity] update def enlistSink(id: Int): Unit =
    if sinkSlot(id) == 0 then
      sinkList(sinkCount) = id
      sinkSlot(id) = sinkCount + 1
      sinkCount += 1

  private[acyclicity] update def delistSink(id: Int): Unit =
    val slot = sinkSlot(id) - 1

    if slot >= 0 then
      val last = sinkList(sinkCount - 1)
      sinkList(slot) = last
      sinkSlot(last) = slot + 1
      sinkSlot(id) = 0
      sinkCount -= 1

  // ─── editing ──────────────────────────────────────────────────────────────

  // Adds a node, if absent, at the end of the order; answers its id.
  update def add(node: node): Int =
    val slot = slotOf(node)

    if slot >= 0 then ids(slot) else
      if count == names.length then growNodes()
      val id = count
      count += 1
      living += 1
      names(id) = node.asInstanceOf[AnyRef]
      alive(id) = true
      insertIndex(node, id)
      rank(id) = id
      atRank(id) = id + 1
      enlistSource(id)
      enlistSink(id)
      revision += 1
      id

  // Terminated by state: the out-list ends at slot 0.
  private def hasEdge(from: Int, to: Int): Boolean =
    var slot = outHead(from)
    var found = false

    while slot != 0 && !found do
      if edgeTo(slot - 1) == to then found = true else slot = outNext(slot - 1)

    found

  private[acyclicity] update def link(from: Int, to: Int): Unit =
    if slots == edgeFrom.length then growEdges()
    val slot = slots
    slots += 1
    edgeFrom(slot) = from
    edgeTo(slot) = to
    outNext(slot) = outHead(from)
    outHead(from) = slot + 1
    inNext(slot) = inHead(to)
    inHead(to) = slot + 1
    outDegree(from) += 1
    inDegree(to) += 1
    edgeTotal += 1
    revision += 1
    if outDegree(from) == 1 then delistSource(from)
    if inDegree(to) == 1 then delistSink(to)

  // Unthreads a slot from its origin's out-list, which contains it.
  private[acyclicity] update def unlinkOut(slot: Int): Unit =
    val from = edgeFrom(slot)
    var current = outHead(from)
    var previous = 0

    while current - 1 != slot do
      previous = current
      current = outNext(current - 1)

    if previous == 0 then outHead(from) = outNext(slot) else outNext(previous - 1) = outNext(slot)
    outDegree(from) -= 1
    if outDegree(from) == 0 && alive(from) then enlistSource(from)

  // Unthreads a slot from its target's in-list, which contains it.
  private[acyclicity] update def unlinkIn(slot: Int): Unit =
    val to = edgeTo(slot)
    var current = inHead(to)
    var previous = 0

    while current - 1 != slot do
      previous = current
      current = inNext(current - 1)

    if previous == 0 then inHead(to) = inNext(slot) else inNext(previous - 1) = inNext(slot)
    inDegree(to) -= 1
    if inDegree(to) == 0 && alive(to) then enlistSink(to)

  // Pearce–Kelly: `from` must come to sit above `to`. The dependants of `from` ranked no higher
  // than `to` (the forward set) and the dependencies of `to` ranked no lower than `from` (the
  // backward set) are the only nodes whose ranks need to change; the backward set takes the
  // lowest of their ranks and the forward set the rest, each in its existing order. Meeting `to`
  // in the forward search means `to` already depends on `from`, so the edge would close a cycle.
  // Each search pushes a node at most once per epoch, so `stack`, `forward` and `backward`,
  // sized with the node tables, suffice.
  private[acyclicity] update def reorder(from: Int, to: Int): Boolean =
    val lower = rank(from)
    val upper = rank(to)
    epoch += 1
    var forwardCount = 0
    var top = 0
    var cyclic = false
    stack(top) = from
    top += 1
    stamp(from) = epoch

    while top > 0 && !cyclic do
      top -= 1
      val id = stack(top)
      forward(forwardCount) = (rank(id).toLong << 32) | id.toLong
      forwardCount += 1
      var slot = inHead(id)

      while slot != 0 && !cyclic do
        val dependant = edgeFrom(slot - 1)

        if dependant == to then cyclic = true
        else if stamp(dependant) != epoch && rank(dependant) <= upper then
          stamp(dependant) = epoch
          stack(top) = dependant
          top += 1

        slot = inNext(slot - 1)

    if cyclic then false else
      epoch += 1
      var backwardCount = 0
      top = 0
      stack(top) = to
      top += 1
      stamp(to) = epoch

      while top > 0 do
        top -= 1
        val id = stack(top)
        backward(backwardCount) = (rank(id).toLong << 32) | id.toLong
        backwardCount += 1
        var slot = outHead(id)

        while slot != 0 do
          val dependency = edgeTo(slot - 1)

          if stamp(dependency) != epoch && rank(dependency) >= lower then
            stamp(dependency) = epoch
            stack(top) = dependency
            top += 1

          slot = outNext(slot - 1)

      java.util.Arrays.sort(forward, 0, forwardCount)
      java.util.Arrays.sort(backward, 0, backwardCount)

      // Merge the two rank sequences ascending, handing each rank to the next node of the
      // backward set until it is exhausted, then of the forward set.
      var forwardAt = 0
      var backwardAt = 0
      var assigned = 0

      while assigned < forwardCount + backwardCount do
        val takeForward =
          backwardAt == backwardCount ||
            (forwardAt < forwardCount && forward(forwardAt) < backward(backwardAt))

        val position =
          if takeForward then
            forwardAt += 1
            (forward(forwardAt - 1) >>> 32).toInt
          else
            backwardAt += 1
            (backward(backwardAt - 1) >>> 32).toInt

        val id =
          if assigned < backwardCount then (backward(assigned) & 0xffffffffL).toInt
          else (forward(assigned - backwardCount) & 0xffffffffL).toInt

        rank(id) = position
        atRank(position) = id + 1
        assigned += 1

      true

  // Adds the edge if it keeps the graph acyclic, answering whether it did; a refused edge leaves
  // the graph unchanged.
  private[acyclicity] update def attach(from: node, to: node): Boolean =
    val source = add(from)
    val target = add(to)

    if source == target then false
    else if hasEdge(source, target) then true
    else if rank(target) < rank(source) then
      link(source, target)
      true
    else if reorder(source, target) then
      link(source, target)
      true
    else
      false

  // Adds the edge `from -> to` (`from` depends on `to`), or raises `Cyclic` and leaves the graph
  // unchanged if `to` already depends on `from`.
  update def add(from: node, to: node)(using Tactic[Dag.Error]): Unit =
    if !attach(from, to) then abort(Dag.Error(Dag.Error.Reason.Cyclic))

  // Drops a node and every edge at either end of it.
  update def remove(node: node): Unit =
    val index = slotOf(node)

    if index >= 0 then
      val id = ids(index)
      var slot = outHead(id)

      // Terminated by state: each list ends at slot 0.
      while slot != 0 do
        val next = outNext(slot - 1)
        unlinkIn(slot - 1)
        edgeTotal -= 1
        slot = next

      outHead(id) = 0
      slot = inHead(id)

      while slot != 0 do
        val next = inNext(slot - 1)
        unlinkOut(slot - 1)
        edgeTotal -= 1
        slot = next

      inHead(id) = 0
      outDegree(id) = 0
      inDegree(id) = 0
      delistSource(id)
      delistSink(id)
      alive(id) = false
      living -= 1
      atRank(rank(id)) = 0
      deleteIndex(index)
      revision += 1

  // Drops a node, rerouting each dependant to each dependency; no reordering can be needed,
  // since every dependant already ranks above every dependency.
  update def bypass(node: node): Unit =
    val index = slotOf(node)

    if index >= 0 then
      val id = ids(index)
      var inSlot = inHead(id)

      while inSlot != 0 do
        val dependant = edgeFrom(inSlot - 1)
        var outSlot = outHead(id)

        while outSlot != 0 do
          val dependency = edgeTo(outSlot - 1)
          if !hasEdge(dependant, dependency) then link(dependant, dependency)
          outSlot = outNext(outSlot - 1)

        inSlot = inNext(inSlot - 1)

      remove(node)

  // ─── reading ──────────────────────────────────────────────────────────────

  private[acyclicity] def name(id: Int): node = names(id).asInstanceOf[node]

  // Everything a node reaches, itself included: depth-first over the out-lists, marking on push,
  // so every id is pushed at most once and a stack of `count` entries suffices.
  def reachable(node: node): Set[node] =
    val start = slotOf(node)

    if start < 0 then Set() else
      val seen = new scala.Array[Boolean](count)
      val pending = new scala.Array[Int](count)
      val builder = sci.Set.newBuilder[node]
      var top = 0
      pending(top) = ids(start)
      top += 1
      seen(ids(start)) = true

      while top > 0 do
        top -= 1
        val id = pending(top)
        builder += name(id)
        var slot = outHead(id)

        while slot != 0 do
          val child = edgeTo(slot - 1)

          if !seen(child) then
            seen(child) = true
            pending(top) = child
            top += 1

          slot = outNext(slot - 1)

      Set.from(builder.result())

  def edges: Set[(node, node)] =
    val builder = sci.Set.newBuilder[(node, node)]
    var id = 0

    while id < count do
      if alive(id) then
        var slot = outHead(id)

        while slot != 0 do
          builder += ((name(id), name(edgeTo(slot - 1))))
          slot = outNext(slot - 1)

      id += 1

    Set.from(builder.result())

  // Each list reader fetches the fields itself: passing a field of `this` to a method on `this`
  // is a separation failure.
  private def outgoing(id: Int): sci.Set[node] =
    val builder = sci.Set.newBuilder[node]
    var slot = outHead(id)

    while slot != 0 do
      builder += name(edgeTo(slot - 1))
      slot = outNext(slot - 1)

    builder.result()

  private def incoming(id: Int): sci.Set[node] =
    val builder = sci.Set.newBuilder[node]
    var slot = inHead(id)

    while slot != 0 do
      builder += name(edgeFrom(slot - 1))
      slot = inNext(slot - 1)

    builder.result()

  def successors(node: node): Set[node] =
    val slot = slotOf(node)
    if slot >= 0 then Set.from(outgoing(ids(slot))) else Set()

  def predecessors(node: node): Set[node] =
    val slot = slotOf(node)
    if slot >= 0 then Set.from(incoming(ids(slot))) else Set()

  // `sourceList(0 until sourceCount)` are exactly the live nodes with no successors.
  def sources: Set[node] =
    val builder = sci.Set.newBuilder[node]
    var slot = 0

    while slot < sourceCount do
      builder += name(sourceList(slot))
      slot += 1

    Set.from(builder.result())

  def sinks: Set[node] =
    val builder = sci.Set.newBuilder[node]
    var slot = 0

    while slot < sinkCount do
      builder += name(sinkList(slot))
      slot += 1

    Set.from(builder.result())

  // The maintained order, dependencies first: a read, O(n).
  def linearized: List[node] =
    var result: sci.List[node] = sci.Nil
    var position = count - 1

    // `atRank` has an entry for every position below `count`; zero marks a removed node.
    while position >= 0 do
      if atRank(position) != 0 then result = name(atRank(position) - 1) :: result
      position -= 1

    List.from(result)

  def nodes: Set[node] = Set.from(List.iterator(linearized))

  private[acyclicity] def adjacency: sci.VectorMap[node, sci.Set[node]] =
    val builder = sci.VectorMap.newBuilder[node, sci.Set[node]]
    var position = 0

    while position < count do
      if atRank(position) != 0 then
        val id = atRank(position) - 1
        builder += ((name(id), outgoing(id)))

      position += 1

    builder.result()


  // Whether `from` reaches `to`, in O(1) once the matrix exists.
  def reaches(from: node, to: node): Boolean =
    val source = slotOf(from)
    val target = slotOf(to)
    source >= 0 && target >= 0 && bit(reach, ids(source)*words, ids(target))

  // Every implied edge made explicit.
  def closure: Dag[node] =
    val bits = reach
    val words = this.words
    val builder = sci.VectorMap.newBuilder[node, sci.Set[node]]
    var position = 0

    // `atRank` has an entry for every rank below `count`; zero marks a removed node.
    while position < count do
      if atRank(position) != 0 then
        val id = atRank(position) - 1
        val row = sci.Set.newBuilder[node]
        var other = 0

        while other < count do
          if alive(other) && bit(bits, id*words, other) then row += name(other)
          other += 1

        builder += ((name(id), row.result()))

      position += 1

    Dag.unchecked(builder.result())

  // Aho–Garey–Ullman: a successor is redundant when a later successor (higher rank) of the same
  // node already reaches it, so visiting the successors by descending rank with a running union
  // of their rows decides every edge in O(degree·n/64).
  def reduction: Dag[node] =
    val bits = reach
    val words = this.words
    val covered = new scala.Array[Long](words)
    var largest = 0
    var position = 0

    // The widest out-list, to size the sort buffer.
    while position < count do
      if atRank(position) != 0 then largest = largest.max(outDegree(atRank(position) - 1))
      position += 1

    val ordered = new scala.Array[Long](largest)
    val builder = sci.VectorMap.newBuilder[node, sci.Set[node]]
    position = 0

    // `degree <= largest`, so `ordered` holds every successor of `id`, each packed as its rank in
    // the high word and its id in the low.
    while position < count do
      if atRank(position) != 0 then
        val id = atRank(position) - 1
        java.util.Arrays.fill(covered, 0L)
        var degree = 0
        var slot = outHead(id)

        while slot != 0 do
          val child = edgeTo(slot - 1)
          ordered(degree) = (rank(child).toLong << 32) | child.toLong
          degree += 1
          slot = outNext(slot - 1)

        java.util.Arrays.sort(ordered, 0, degree)
        val kept = sci.Set.newBuilder[node]
        var index = degree - 1

        while index >= 0 do
          val child = (ordered(index) & 0xffffffffL).toInt

          if !bit(covered.asInstanceOf[scala.IArray[Long]], 0, child) then
            kept += name(child)
            var word = 0

            while word < words do
              covered(word) |= bits(child*words + word)
              word += 1

          index -= 1

        builder += ((name(id), kept.result()))

      position += 1

    Dag.unchecked(builder.result())

  // Every edge reversed, as a frozen topology: the reversed order is a valid order for the
  // reversed edges, so the copy needs no reordering.
  def invert: Topology[node]^{} =
    val reversed = List.from(List.iterator(linearized).toList.reverse)
    Topology.of(reversed, node => Set.iterator(predecessors(node)))

  // A persistent copy, without giving up the handle.
  def snapshot: Dag[node] = Dag.unchecked(adjacency)
