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

import scala.caps

// Candidate (d): a separation-checked mutable graph, edited in place and given up to `FrozenDag`
// or `AdjacencyDag` when the editing is done. It is a single-owner structure — the exclusive
// handle is the only route to its arrays, so there is nothing for separation checking to be
// unable to express — and every table is a dense array indexed by node id: adjacency in both
// directions as singly-linked edge lists threaded through one slot pool, in- and out-degrees,
// the sources and sinks as swap-remove bags, and a topological order kept valid through every
// `add` by Pearce and Kelly's algorithm, which touches only the nodes between the two ends of
// an offending edge and refuses an edge that would close a cycle. Slot and bag indices are
// stored one-based so that a zero-filled array means "none".
//
// Only the node-to-id dictionary is not an array: it is a persistent `Map`, since a stdlib
// mutable map passes as pure under separation checking and its mutation would go unchecked.
object Workspace:
  def apply[node](): Workspace[node]^ = new Workspace()

  def apply(count: Int, from: scala.IArray[Int], to: scala.IArray[Int]): Workspace[Int]^ =
    val workspace: Workspace[Int]^ = new Workspace()
    var index = 0

    while index < count do
      workspace.add(index)
      index += 1

    index = 0

    while index < from.length do
      workspace.add(from(index), to(index))
      index += 1

    workspace

  // The same edges in reverse order, so that most arrive before the nodes they depend on are
  // ranked below them, and the order has to be repaired as it goes.
  def reversed(count: Int, from: scala.IArray[Int], to: scala.IArray[Int]): Workspace[Int]^ =
    val workspace: Workspace[Int]^ = new Workspace()
    var index = count - 1

    while index >= 0 do
      workspace.add(index)
      index -= 1

    index = from.length - 1

    while index >= 0 do
      workspace.add(from(index), to(index))
      index -= 1

    workspace

  // Thawing a persistent graph: nodes in topological order, so no edge needs reordering.
  def apply[node](dag: AdjacencyDag[node]): Workspace[node]^ =
    val workspace: Workspace[node]^ = new Workspace()
    var rest = dag.sorted.get

    while rest.nonEmpty do
      workspace.add(rest.head)
      rest = rest.tail

    val entries = dag.adjacency.iterator

    while entries.hasNext do
      val (from, targets) = entries.next()
      val children = targets.iterator
      while children.hasNext do workspace.add(from, children.next())

    workspace

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

final class Workspace[node] private[acyclicity]()
extends caps.ExclusiveCapability, caps.Stateful:
  private[acyclicity] var index: Map[node, Int] = Map()
  private[acyclicity] var count: Int = 0       // ids allocated, removed ones included
  private[acyclicity] var living: Int = 0
  private[acyclicity] var edges: Int = 0
  private[acyclicity] var slots: Int = 0       // edge slots allocated, unlinked ones included
  private[acyclicity] var epoch: Int = 0

  // Per node, by id.
  private[acyclicity] var names: scala.Array[AnyRef]^ = new scala.Array[AnyRef](16)
  private[acyclicity] var alive: scala.Array[Boolean]^ = new scala.Array[Boolean](16)
  private[acyclicity] var outHead: scala.Array[Int]^ = new scala.Array[Int](16)
  private[acyclicity] var inHead: scala.Array[Int]^ = new scala.Array[Int](16)
  private[acyclicity] var outDegree: scala.Array[Int]^ = new scala.Array[Int](16)
  private[acyclicity] var inDegree: scala.Array[Int]^ = new scala.Array[Int](16)
  private[acyclicity] var rank: scala.Array[Int]^ = new scala.Array[Int](16)      // id -> position
  private[acyclicity] var atRank: scala.Array[Int]^ = new scala.Array[Int](16)    // position -> 1 + id
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
  def has(node: node): Boolean = index.contains(node)

  private[acyclicity] update def growNodes(): Unit =
    val capacity = names.length*2
    names = Workspace.grownRefs(names, capacity)
    alive = Workspace.grownFlags(alive, capacity)
    outHead = Workspace.grown(outHead, capacity)
    inHead = Workspace.grown(inHead, capacity)
    outDegree = Workspace.grown(outDegree, capacity)
    inDegree = Workspace.grown(inDegree, capacity)
    rank = Workspace.grown(rank, capacity)
    atRank = Workspace.grown(atRank, capacity)
    stamp = Workspace.grown(stamp, capacity)
    sourceList = Workspace.grown(sourceList, capacity)
    sourceSlot = Workspace.grown(sourceSlot, capacity)
    sinkList = Workspace.grown(sinkList, capacity)
    sinkSlot = Workspace.grown(sinkSlot, capacity)
    stack = Workspace.grown(stack, capacity)
    forward = Workspace.grownLongs(forward, capacity)
    backward = Workspace.grownLongs(backward, capacity)

  private[acyclicity] update def growEdges(): Unit =
    val capacity = edgeFrom.length*2
    edgeFrom = Workspace.grown(edgeFrom, capacity)
    edgeTo = Workspace.grown(edgeTo, capacity)
    outNext = Workspace.grown(outNext, capacity)
    inNext = Workspace.grown(inNext, capacity)

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

  // Adds a node, if absent, at the end of the order; answers its id.
  update def add(node: node): Int = index.get(node) match
    case Some(id) => id

    case None =>
      if count == names.length then growNodes()
      val id = count
      count += 1
      living += 1
      names(id) = node.asInstanceOf[AnyRef]
      alive(id) = true
      index = index.updated(node, id)
      rank(id) = id
      atRank(id) = id + 1
      enlistSource(id)
      enlistSink(id)
      id

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

  // Unthreads a slot from its origin's out-list.
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

  // Unthreads a slot from its target's in-list.
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
          backwardAt == backwardCount
          || (forwardAt < forwardCount && forward(forwardAt) < backward(backwardAt))

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

  // Adds the edge `from -> to` (`from` depends on `to`), answering whether the graph is still
  // acyclic; a refused edge leaves the graph unchanged.
  update def add(from: node, to: node): Boolean =
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
    else false

  // Drops a node and its incident edges.
  update def remove(node: node): Unit = index.get(node) match
    case None => ()

    case Some(id) =>
      var slot = outHead(id)

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
      index = index - node

  // Drops a node, rerouting each dependant to each dependency; no reordering can be needed,
  // since every dependant already ranks above every dependency.
  update def bypass(node: node): Unit = index.get(node) match
    case None => ()

    case Some(id) =>
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

  private def name(id: Int): node = names(id).asInstanceOf[node]

  // Each list reader fetches the fields itself: passing a field of `this` to a method on `this`
  // is a separation failure.
  private def outgoing(id: Int): Set[node] =
    val builder = Set.newBuilder[node]
    var slot = outHead(id)

    while slot != 0 do
      builder += name(edgeTo(slot - 1))
      slot = outNext(slot - 1)

    builder.result()

  private def incoming(id: Int): Set[node] =
    val builder = Set.newBuilder[node]
    var slot = inHead(id)

    while slot != 0 do
      builder += name(edgeFrom(slot - 1))
      slot = inNext(slot - 1)

    builder.result()

  def successors(node: node): Set[node] = index.get(node) match
    case Some(id) => outgoing(id)
    case None     => Set()

  def predecessors(node: node): Set[node] = index.get(node) match
    case Some(id) => incoming(id)
    case None     => Set()

  def sources: Set[node] =
    val builder = Set.newBuilder[node]
    var slot = 0

    while slot < sourceCount do
      builder += name(sourceList(slot))
      slot += 1

    builder.result()

  def sinks: Set[node] =
    val builder = Set.newBuilder[node]
    var slot = 0

    while slot < sinkCount do
      builder += name(sinkList(slot))
      slot += 1

    builder.result()

  // The maintained order, dependencies first.
  def linearized: List[node] =
    var result: List[node] = Nil
    var position = count - 1

    while position >= 0 do
      if atRank(position) != 0 then result = name(atRank(position) - 1) :: result
      position -= 1

    result

  def adjacency: Map[node, Set[node]] =
    val builder = Map.newBuilder[node, Set[node]]
    var id = 0

    while id < count do
      if alive(id) then builder += ((name(id), outgoing(id)))
      id += 1

    builder.result()

  def snapshot: AdjacencyDag[node] = AdjacencyDag(adjacency)
