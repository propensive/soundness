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
import scala.collection.mutable as scm

import anticipation.*
import contingency.*
import denominative.*
import fulminate.*
import nomenclature.*
import prepositional.*

// A directed acyclic graph as a persistent value: the representation of `Digraph`, with the
// fact that it holds no cycle established once, at construction — the factories check, and
// every edit either cannot create a cycle (`-`, `remove`, `bypass`, `subgraph`, `descendants`)
// or checks the one path that could (`add`) or answers a `Digraph` (`+`, `++`, `map`). What
// acyclicity buys is `linearized`, `reduction` and `traversal` without a failure mode, and the
// `Topological` instance that lets the generic operations have them too.
//
// Whole-graph queries that depend on reachability — `closure`, `reduction` — go through the
// frozen form, whose bit matrix answers them in O(e·n/64); a graph queried that way more than
// once should be frozen once and kept. As in `Digraph`, the operations needing a node's
// predecessors take a `Dysasymptotic.LinearScan`.
object Dag:
  @targetName("fromEdges")
  def apply[node](edges: (node, node)*): Dag[node] raises Dag.Error = Digraph(edges*).acyclic

  // An explicit `Tactic` rather than `raises`: under separation checking a context-function
  // result would hide the (possibly capturing) `successors` parameter.
  // Each node with its successors; a successor given no entry of its own becomes a node.
  @targetName("fromPairs")
  def apply[node, successors <: Set[node]](nodes: (node, successors)*): Dag[node] raises Dag.Error =
    Digraph(nodes*).acyclic

  @targetName("fromNodes")
  def apply[node](nodes: Set[node])(successors: node => Set[node])(using Tactic[Dag.Error])
  :   Dag[node] =

    Digraph(nodes)(successors).acyclic

  @targetName("fromAdjacency")
  def apply[node](adjacency: Map[node, Set[node]]): Dag[node] raises Dag.Error =
    Digraph(adjacency).acyclic

  // Acyclic by the topology's invariant, so no check; the caller gives up its handle.
  def apply[node](consume topology: Topology[node]^): Dag[node] = topology.snapshot

  private[acyclicity] def unchecked[node](adjacency: sci.VectorMap[node, sci.Set[node]])
  :   Dag[node] =

    new Dag(adjacency)

  extension (dag: Dag[Text])
    def dot: Dot = unsafely:
      val edges = Set.iterator(dag.edges).map: (a, b) => Name[Dot.Id](a) --> Name[Dot.Id](b)
      Dot.Digraph(None, false, edges.toSeq*)

  // DagError → Dag.Error
  object Error:
    enum Reason(val number: Int) extends Clarification:
      case NodeMissing(node: Text) extends Reason(1)
      case Cyclic                  extends Reason(2)

    given communicable: Reason is Communicable =
      case Reason.NodeMissing(node) => m"the node $node is not present in the graph"
      case Reason.Cyclic            => m"the graph contains a cycle"

  case class Error(reason: Dag.Error.Reason)(using Diagnostics)
  extends fulminate.Error(191, reason.number)(m"the DAG operation failed because $reason")

  given nodal: [node] => (Dag[node] is Nodal by node) = new Nodal:
    type Self = Dag[node]
    type Operand = node
    def nodes(self: Dag[node]): Iterator[node] = self.adjacency.keysIterator
    def has(self: Dag[node], node: node): Boolean = self.adjacency.contains(node)

    def successors(self: Dag[node], node: node): Iterator[node] =
      self.adjacency.get(node).fold(Iterator.empty)(_.iterator)

  given bidirectional: [node] => (complexity: Dysasymptotic.LinearScan)
  =>  ( Dag[node] is Bidirectional by node ) = new Bidirectional:
    type Self = Dag[node]
    type Operand = node

    def predecessors(self: Dag[node], node: node): Iterator[node] =
      self.transpose.getOrElse(node, sci.Set()).iterator

  given topological: [node] => Dag[node] is Topological = new Topological { type Self = Dag[node] }

final class Dag[node] private[acyclicity]
  ( private[acyclicity] val adjacency: sci.VectorMap[node, sci.Set[node]] ):

  private[acyclicity] lazy val transpose: sci.VectorMap[node, sci.Set[node]] =
    Search.transpose(adjacency.keysIterator, adjacency(_).iterator)

  private def missing(node: node): Text = node.toString.tt

  // The order every whole-graph operation shares, computed once per value: dependencies first.
  private lazy val order: sci.List[node] =
    Search.topological(adjacency.keysIterator, adjacency(_).iterator) match
      case Right(order) => order
      case Left(_)      => sci.Nil   // unreachable: the constructors admit no cycle

  def nodes: Set[node] = Set.from(adjacency.keySet)
  def size: Int = adjacency.size
  def has(node: node): Boolean = adjacency.contains(node)
  def successors(node: node): Set[node] = Set.from(adjacency.getOrElse(node, sci.Set()))

  def edges: Set[(node, node)] =
    Set.from(adjacency.iterator.flatMap { (from, targets) => targets.iterator.map(from -> _) })

  def sources: Set[node] =
    Set.from(adjacency.iterator.collect { case (node, targets) if targets.isEmpty => node })

  def sinks: Set[node] = Set.from(adjacency.keysIterator.filter(transpose(_).isEmpty))

  // Every node after everything it points at.
  def linearized: List[node] = List.from(order)

  def invert: Dag[node] = new Dag(transpose)
  def digraph: Digraph[node] = Digraph.of(adjacency)

  // The frozen form, for querying; `thaw` is the editable one.
  def freeze: Topology[node]^{} = Topology.of(linearized, adjacency(_).iterator)
  def thaw: Topology[node]^ = Topology(this)

  def including(node: node): Dag[node] =
    if adjacency.contains(node) then this else new Dag(adjacency.updated(node, sci.Set()))

  // Adds an edge, unless `to` already reaches `from`, which is the one way an edge can close a
  // cycle: O(reach of `to`).
  def add(from: node, to: node): Dag[node] raises Dag.Error =
    if from == to || Set.has(Search.reachable(to, adjacency.getOrElse(_, sci.Set()).iterator), from)
    then abort(Dag.Error(Dag.Error.Reason.Cyclic))
    else new Dag(including(to).adjacency.updated(from, adjacency.getOrElse(from, sci.Set()) + to))

  @targetName("addEdge")
  infix def + (edge: (node, node)): Digraph[node] = digraph + edge

  @targetName("addAll")
  infix def ++ (other: Dag[node]): Digraph[node] = digraph ++ other.digraph

  def remove(from: node, to: node): Dag[node] = adjacency.get(from) match
    case Some(targets) => new Dag(adjacency.updated(from, targets - to))
    case None          => this

  @targetName("removeNode")
  infix def - (node: node)(using Dysasymptotic.LinearScan): Dag[node] =
    val origins = transpose.getOrElse(node, sci.Set())

    val pruned = origins.foldLeft(adjacency - node): (acc, from) =>
      acc.updated(from, acc(from) - node)

    Dag.unchecked(pruned)

  // Rerouting predecessors to successors cannot create a cycle: every predecessor already
  // reaches every successor through the node being dropped.
  def bypass(node: node)(using Dysasymptotic.LinearScan): Dag[node] =
    val origins = transpose.getOrElse(node, sci.Set())
    val targets = adjacency.getOrElse(node, sci.Set())

    Dag.unchecked:
      origins.foldLeft(adjacency - node): (acc, from) =>
        acc.updated(from, acc(from) - node ++ targets)

  // Bypasses several nodes one at a time, so that connectivity through a run of dropped nodes
  // is preserved. (A `Set`, not a predicate: a lambda argument cannot be told from a node.)
  def bypassAll(nodes: Set[node])(using Dysasymptotic.LinearScan): Dag[node] =
    Set.iterator(nodes).foldLeft(this)(_.bypass(_))

  private def induced(keep: Set[node]): Dag[node] =
    Dag.unchecked:
      sci.VectorMap.from:
        adjacency.iterator.collect:
          case (node, targets) if Set.has(keep, node) => (node, targets.filter(Set.has(keep, _)))

  def subgraph(keep: Set[node])(using Dysasymptotic.LinearSize): Dag[node] = induced(keep)

  def map[node2](lambda: node => node2): Digraph[node2] = digraph.map(lambda)

  // Substitutes a graph for each node: the nodes of `lambda(a)` point at every node of
  // `lambda(b)` for each edge `a -> b`, and at their own successors within `lambda(a)`.
  def flatMap[node2](lambda: node => Dag[node2]): Digraph[node2] =
    val builder = scm.LinkedHashMap[node2, sci.Set[node2]]()

    adjacency.foreach: (node, targets) =>
      val replacement = lambda(node)
      val external = targets.flatMap(lambda(_).adjacency.keySet)

      replacement.adjacency.foreach: (inner, innerTargets) =>
        builder(inner) = builder.getOrElse(inner, sci.Set()) ++ innerTargets ++ external

      external.foreach: outer => if !builder.contains(outer) then builder(outer) = sci.Set()

    Digraph.of(sci.VectorMap.from(builder))

  def reachable(node: node): Set[node] raises Dag.Error =
    if !adjacency.contains(node) then abort(Dag.Error(Dag.Error.Reason.NodeMissing(missing(node))))
    else Search.reachable(node, adjacency(_).iterator)

  def descendants(node: node): Dag[node] raises Dag.Error = induced(reachable(node))

  def ancestors(node: node)(using Dysasymptotic.LinearScan): Dag[node] raises Dag.Error =
    if !adjacency.contains(node) then abort(Dag.Error(Dag.Error.Reason.NodeMissing(missing(node))))
    else induced(Search.reachable(node, transpose(_).iterator))

  def lineage(node: node)(using Dysasymptotic.LinearScan): Dag[node] raises Dag.Error =
    induced(Set.concat(reachable(node), Search.reachable(node, transpose(_).iterator)))

  // Both through the frozen form's bit matrix: O(e·n/64), against the O(Σ|reach|) of set unions.
  def closure: Dag[node] = freeze.closure
  def reduction: Dag[node] = freeze.reduction

  // A value for every node from the values of its successors, which are computed first.
  def traversal[result](lambda: (Set[result], node) => result): Map[node, result] =
    val values = scm.HashMap[node, result]()

    order.foreach: node => values(node) = lambda(Set.from(adjacency(node).map(values)), node)

    Map.from(values)

  override def equals(other: Any): Boolean = other.asInstanceOf[Matchable] match
    case that: Dag[?] => adjacency == that.adjacency
    case _            => false

  override def hashCode: Int = adjacency.hashCode
  override def toString: String = adjacency.mkString("Dag(", ", ", ")")
