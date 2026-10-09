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

// A directed acyclic graph edited in place, under separation checking: the exclusive handle is
// the only route to its tables, so there is nothing for the checker to be unable to express, and
// a `Topology` given up to `Frozen` or `Dag` can never be edited behind the result's back. The
// name is for what it uniquely maintains — a live topological order of the nodes, kept valid
// through every `add` by Pearce and Kelly's algorithm, which touches only the nodes between the
// two ends of an offending edge and refuses one that would close a cycle. So the graph is
// acyclic by invariant and `Topological` by construction, and `linearized` is a read of the
// order.
//
// Every table is a dense array by node id: adjacency in both directions as singly-linked edge
// lists threaded through one slot pool, in- and out-degrees, the sources and sinks as swap-remove
// bags, and the node-to-id dictionary as an open-addressing hash table. Slot and bag indices are
// stored one-based, so a zero-filled array means "none". The loops are the second shape
// `doc/standards/loops.md` sanctions — index arithmetic derived from the data — with the
// invariant that bounds each stated above it.
object Topology:
  def apply[node](): Topology[node]^ = new Topology()

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
  private[acyclicity] var edges: Int = 0
  private[acyclicity] var slots: Int = 0       // edge slots allocated, unlinked ones included
  private[acyclicity] var epoch: Int = 0

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
  def edgeCount: Int = edges

  // ─── the dictionary ───────────────────────────────────────────────────────

  // The slot holding `node`, or -1. Terminated by state: the table is never full, so the probe
  // reaches an empty slot.
  private def slotOf(node: node): Int =
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
    edges += 1
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
        edges -= 1
        slot = next

      outHead(id) = 0
      slot = inHead(id)

      while slot != 0 do
        val next = inNext(slot - 1)
        unlinkOut(slot - 1)
        edges -= 1
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

  private def name(id: Int): node = names(id).asInstanceOf[node]

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

  // A persistent copy, without giving up the handle.
  def snapshot: Dag[node] = Dag.unchecked(adjacency)
