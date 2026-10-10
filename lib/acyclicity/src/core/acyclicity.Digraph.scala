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
import murmuration.*
import prepositional.*
import vacuous.*

// A finite directed graph that may contain cycles, as a persistent value: an insertion-ordered
// map from each node to the set of nodes it points at — under the dependency reading, its
// dependencies. Every edge target is a node (the constructors close the node set), so a dangling
// edge is unrepresentable. Only the forward direction is stored, which is why the operations
// that need a node's predecessors — `-`, `bypass`, `ancestors`, `lineage` and the
// `Bidirectional` instance — take a `Dysasymptotic.LinearScan`: finding them is a scan of every
// edge, a cost outside the question's scope, and the gate makes a program say so where it
// accepts that cost. The transpose is computed once per value and kept.
//
// Nothing here presumes acyclicity, so `linearized`, `reduction` and `traversal` are not
// offered; `acyclic` checks the graph once and answers the `Dag` that has them.
object Digraph:
  // From edges alone: both ends of every edge become nodes, in order of first appearance.
  @targetName("fromEdges")
  def apply[node](edges: (node, node)*): Digraph[node] =
    val builder = scm.LinkedHashMap[node, sci.Set[node]]()

    edges.foreach: (from, to) =>
      builder(from) = builder.getOrElse(from, sci.Set()) + to
      if !builder.contains(to) then builder(to) = sci.Set()

    new Digraph(sci.VectorMap.from(builder))

  // Each node with its successors; a successor given no entry of its own becomes a node. The
  // successors' type is a parameter bounded by `Set[node]` rather than `Set[node]` itself: a
  // tuple built with `->` infers its right-hand type through the opaque alias, and only a free
  // variable then unifies with it.
  @targetName("fromPairs")
  def apply[node, successors <: Set[node]](nodes: (node, successors)*): Digraph[node] =
    val builder = scm.LinkedHashMap[node, sci.Set[node]]()

    nodes.foreach: (node, targets) =>
      val children = sci.Set.from(Set.iterator(targets))
      builder(node) = builder.getOrElse(node, sci.Set()) ++ children
      children.foreach: child => if !builder.contains(child) then builder(child) = sci.Set()

    new Digraph(sci.VectorMap.from(builder))

  // From a node set and each node's successors; a successor outside the set becomes a node.
  @targetName("fromNodes")
  def apply[node](nodes: Set[node])(successors: node => Set[node]): Digraph[node] =
    new Digraph(Search.adjacency(Set.iterator(nodes), node => Set.iterator(successors(node))))

  @targetName("fromAdjacency")
  def apply[node](adjacency: Map[node, Set[node]]): Digraph[node] =
    val successors = (node: node) => Set.iterator(Map.at(adjacency, node))
    Digraph.of(Search.adjacency(Set.iterator(Map.keys(adjacency)), successors))

  private[acyclicity] def of[node](adjacency: sci.VectorMap[node, sci.Set[node]]): Digraph[node] =
    new Digraph(adjacency)

  given nodal: [node] => (Digraph[node] is Nodal by node) = new Nodal:
    type Self = Digraph[node]
    type Operand = node
    def nodes(self: Digraph[node]): Iterator[node] = self.adjacency.keysIterator
    def has(self: Digraph[node], node: node): Boolean = self.adjacency.contains(node)

    def successors(self: Digraph[node], node: node): Iterator[node] =
      self.adjacency.get(node).fold(Iterator.empty)(_.iterator)

  given bidirectional: [node] => (complexity: Dysasymptotic.LinearScan)
  =>  ( Digraph[node] is Bidirectional by node ) = new Bidirectional:
    type Self = Digraph[node]
    type Operand = node

    def predecessors(self: Digraph[node], node: node): Iterator[node] =
      self.transpose.getOrElse(node, sci.Set()).iterator

  // As a collection, a `Digraph` is its nodes, in the order they were given.
  given traversable: [node] => Digraph[node] is Traversable by node = _.adjacency.keysIterator
  given inclusive: [node] => Digraph[node] is Inclusive by node = _.adjacency.contains(_)

  given countable: [node] => Digraph[node] is Countable:
    def size(self: Digraph[node]): Int = self.adjacency.size
    override def nil(self: Digraph[node]): Boolean = self.adjacency.isEmpty

  given mappable: [node]
  =>  ( Digraph[node] is Mappable { type Operand = node; type Result[node2] = Digraph[node2] } ) =
    new Mappable:
      type Self = Digraph[node]
      type Operand = node
      type Result[node2] = Digraph[node2]

      def map[node2](self: Digraph[node], lambda: node => node2): Digraph[node2] =
        self.renamed(lambda)

final class Digraph[node] private[acyclicity]
  ( private[acyclicity] val adjacency: sci.VectorMap[node, sci.Set[node]] ):

  private[acyclicity] lazy val transpose: sci.VectorMap[node, sci.Set[node]] =
    Search.transpose(adjacency.keysIterator, adjacency(_).iterator)

  def nodes: Set[node] = Set.from(adjacency.keySet)
  def successors(node: node): Set[node] = Set.from(adjacency.getOrElse(node, sci.Set()))

  def edges: Set[(node, node)] =
    Set.from(adjacency.iterator.flatMap { (from, targets) => targets.iterator.map(from -> _) })

  // The nodes with no successors: under the dependency reading, those depending on nothing.
  def sources: Set[node] =
    Set.from(adjacency.iterator.collect { case (node, targets) if targets.isEmpty => node })

  // The nodes with no predecessors: those nothing depends on. Whole-graph in scope, so ungated.
  def sinks: Set[node] = Set.from(adjacency.keysIterator.filter(transpose(_).isEmpty))

  def invert: Digraph[node] = new Digraph(transpose)
  def digraph: Digraph[node] = this

  // Adds a node, if absent, with no edges.
  def including(node: node): Digraph[node] =
    if adjacency.contains(node) then this else new Digraph(adjacency.updated(node, sci.Set()))

  def add(from: node, to: node): Digraph[node] =
    new Digraph(including(to).adjacency.updated(from, adjacency.getOrElse(from, sci.Set()) + to))

  @targetName("addEdge")
  infix def + (edge: (node, node)): Digraph[node] = add(edge(0), edge(1))

  @targetName("addAll")
  infix def ++ (other: Digraph[node]): Digraph[node] =
    other.adjacency.foldLeft(this): (acc, entry) =>
      entry(1).foldLeft(acc.including(entry(0))): (acc2, to) => acc2.add(entry(0), to)

  def remove(from: node, to: node): Digraph[node] = adjacency.get(from) match
    case Some(targets) => new Digraph(adjacency.updated(from, targets - to))
    case None          => this

  // Drops a node and every edge at either end of it.
  @targetName("removeNode")
  infix def - (node: node)(using Dysasymptotic.LinearScan): Digraph[node] =
    val origins = transpose.getOrElse(node, sci.Set())

    Digraph.of:
      origins.foldLeft(adjacency - node): (acc, from) => acc.updated(from, acc(from) - node)

  // Drops a node, rerouting each of its predecessors to each of its successors, so that every
  // path through it survives.
  def bypass(node: node)(using Dysasymptotic.LinearScan): Digraph[node] =
    val origins = transpose.getOrElse(node, sci.Set())
    val targets = adjacency.getOrElse(node, sci.Set())

    Digraph.of:
      origins.foldLeft(adjacency - node): (acc, from) =>
        acc.updated(from, acc(from) - node ++ targets)

  // Bypasses several nodes one at a time, so that connectivity through a run of dropped nodes
  // is preserved. (A `Set`, not a predicate: a lambda argument cannot be told from a node.)
  def bypassAll(nodes: Set[node])(using Dysasymptotic.LinearScan): Digraph[node] =
    Set.iterator(nodes).foldLeft(this)(_.bypass(_))

  // The subgraph induced by `keep`: those nodes and the edges among them. A bounded choice of
  // nodes rebuilds the whole map, which is the `LinearSize` cost.
  def subgraph(keep: Set[node])(using Dysasymptotic.LinearSize): Digraph[node] =
    Digraph.of:
      sci.VectorMap.from:
        adjacency.iterator.collect:
          case (node, targets) if Set.has(keep, node) => (node, targets.filter(Set.has(keep, _)))

  // Renames the nodes (the `Mappable` instance's `map`); where two nodes map to one, their edges
  // are united rather than one set being lost, and the result may therefore contain a cycle.
  private[acyclicity] def renamed[node2](lambda: node => node2): Digraph[node2] =
    val builder = scm.LinkedHashMap[node2, sci.Set[node2]]()

    adjacency.foreach: (node, targets) =>
      val renamed = lambda(node)
      builder(renamed) = builder.getOrElse(renamed, sci.Set()) ++ targets.map(lambda)

    new Digraph(sci.VectorMap.from(builder))

  private def missing(node: node): Text = node.toString.tt

  def reachable(node: node): Set[node] raises Dag.Error =
    if !adjacency.contains(node) then abort(Dag.Error(Dag.Error.Reason.NodeMissing(missing(node))))
    else Search.reachable(node, adjacency(_).iterator)

  private def induced(keep: Set[node]): Digraph[node] =
    Digraph.of:
      sci.VectorMap.from:
        adjacency.iterator.collect:
          case (node, targets) if Set.has(keep, node) => (node, targets.filter(Set.has(keep, _)))

  // The subgraph a node reaches, itself included.
  def descendants(node: node): Digraph[node] raises Dag.Error = induced(reachable(node))

  // The subgraph that reaches a node, itself included.
  def ancestors(node: node)(using Dysasymptotic.LinearScan): Digraph[node] raises Dag.Error =
    if !adjacency.contains(node) then abort(Dag.Error(Dag.Error.Reason.NodeMissing(missing(node))))
    else induced(Search.reachable(node, transpose(_).iterator))

  def lineage(node: node)(using Dysasymptotic.LinearScan): Digraph[node] raises Dag.Error =
    induced(Set.concat(reachable(node), Search.reachable(node, transpose(_).iterator)))

  // A cycle, if the graph has one: a list of nodes each pointing at the next, ending where it
  // began.
  def cycle: Optional[List[node]] =
    Search.topological(adjacency.keysIterator, adjacency(_).iterator) match
      case Left(witness) => List.from(witness)
      case Right(_)      => Unset

  // The one checked conversion: O(n + e), once, after which the `Dag` needs no checks.
  def acyclic: Dag[node] raises Dag.Error =
    Search.topological(adjacency.keysIterator, adjacency(_).iterator) match
      case Left(_)  => abort(Dag.Error(Dag.Error.Reason.Cyclic))
      case Right(_) => Dag.unchecked(adjacency)

  override def equals(other: Any): Boolean = other.asInstanceOf[Matchable] match
    case that: Digraph[?] => adjacency == that.adjacency
    case _                => false

  override def hashCode: Int = adjacency.hashCode
  override def toString: String = adjacency.mkString("Digraph(", ", ", ")")
