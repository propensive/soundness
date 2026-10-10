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
import prepositional.*
import vacuous.*

// The membership test for the `Dot.Identifier` rule: a DOT identifier is any
// non-empty text containing neither a double-quote nor a newline.
private[acyclicity] def dotIdentifierValid(name: Text): Boolean =
  val text = name.s

  text.length > 0 && (0 until text.length).forall: index =>
    val char = text.charAt(index)
    char != '"' && char != '\n'

// Materialises a successor function, which has no finite node set of its own, into a graph by
// exploring outward from a node. The result may contain a cycle, so it is a `Digraph`.
extension [node](start: node)
  def explore(dependencies: node => Iterable[node]): Digraph[node] =
    val builder = scm.LinkedHashMap[node, sci.Set[node]]()
    val todo = scm.ArrayBuffer[node](start)

    // Terminated by state: each node is entered into the builder once, when first expanded.
    while todo.nonEmpty do
      val node = todo.remove(todo.length - 1)

      if !builder.contains(node) then
        val children = sci.Set.from(dependencies(node))
        builder(node) = children
        children.foreach: child => if !builder.contains(child) then todo += child

    Digraph.of(sci.VectorMap.from(builder))

// The operations every `Nodal` graph has, generically — one implementation for every instance,
// so a `Map[node, Set[node]]`, a `Ledger`, a `Hasse` and a `Topology` answer them alongside the
// graph types. The concrete types offer the same names as methods, which take precedence and
// read their own representation directly. Result types are plain type parameters, never
// path-dependent members, so the export forwarders keep them (#1411).
extension [self, node](graph: self)(using nodal: self is Nodal by node)
  def nodes: Set[node] = Set.from(nodal.nodes(graph))
  def successors(node: node): Set[node] = Set.from(nodal.successors(graph, node))

  def edges: Set[(node, node)] =
    Set.from(nodal.nodes(graph).flatMap { from => nodal.successors(graph, from).map(from -> _) })

  // The nodes with no successors — those depending on nothing. Whole-graph in scope.
  def sources: Set[node] = Set.from(nodal.nodes(graph).filter(!nodal.successors(graph, _).hasNext))

  // The nodes with no predecessors — those nothing depends on. Whole-graph in scope, so no
  // `Bidirectional` is needed: one pass over the edges.
  def sinks: Set[node] =
    val targets = scm.HashSet[node]()
    nodal.nodes(graph).foreach: from => nodal.successors(graph, from).foreach(targets += _)
    Set.from(nodal.nodes(graph).filterNot(targets.contains))

  def predecessors(node: node)(using bidirectional: self is Bidirectional by node): Set[node] =
    Set.from(bidirectional.predecessors(graph, node))

  // A cycle, if the graph has one: a list of nodes each pointing at the next, ending where it
  // began. Total on any graph.
  def cycle: Optional[List[node]] =
    Search.topological(nodal.nodes(graph), nodal.successors(graph, _)) match
      case Left(witness) => List.from(witness)
      case Right(_)      => Unset

  // The one checked conversion to a `Dag`: O(n + e), once.
  def acyclic: Dag[node] raises Dag.Error =
    Search.topological(nodal.nodes(graph), nodal.successors(graph, _)) match
      case Left(_)  => abort(Dag.Error(Dag.Error.Reason.Cyclic))

      case Right(_) =>
        Dag.unchecked(Search.adjacency(nodal.nodes(graph), nodal.successors(graph, _)))

  def digraph: Digraph[node] =
    Digraph.of(Search.adjacency(nodal.nodes(graph), nodal.successors(graph, _)))

  // Everything a node reaches, itself included; total on cyclic input, raising only for a node
  // the graph does not have.
  def reachable(node: node)
    ( using reach: (self is Reachable by node) = Reachable.generic(nodal) )
  :   Set[node] raises Dag.Error =

    if !nodal.has(graph, node)
    then abort(Dag.Error(Dag.Error.Reason.NodeMissing(node.toString.tt)))
    else reach.reachable(graph, node)

  // Every implied edge made explicit, for any graph: one search per node.
  def closure(using reach: (self is Reachable by node) = Reachable.generic(nodal)): Digraph[node] =
    Digraph.of:
      sci.VectorMap.from:
        nodal.nodes(graph).map: node =>
          (node, sci.Set.from(Set.iterator(reach.reachable(graph, node))) - node)

  def invert[result]
    ( using invertible: (self is Invertible by node to result) = Invertible.generic(nodal) )
  :   result =

    invertible.invert(graph)

// The operations that need the graph to be acyclic, which `Topological` certifies: none of them
// has a failure mode.
extension [self, node](graph: self)(using nodal: self is Nodal by node, dag: self is Topological)
  // Every node after everything it points at.
  def linearized: List[node] =
    Search.topological(nodal.nodes(graph), nodal.successors(graph, _)) match
      case Right(order) => List.from(order)
      case Left(_)      => List()   // unreachable: the instance certifies there is no cycle

  // The minimal graph with the same reachability, through the frozen form's bit matrix.
  def reduction: Dag[node] = Topology.of(graph.linearized, nodal.successors(graph, _)).reduction

  // A value for every node from the values of its successors, which are computed first.
  def traversal[result](lambda: (Set[result], node) => result): Map[node, result] =
    val values = scm.HashMap[node, result]()

    List.iterator(graph.linearized).foreach: node =>
      values(node) = lambda(Set.from(nodal.successors(graph, node).map(values)), node)

    Map.from(values)

  // Arranged for drawing, as `Dag#layered`: the layering is computed on a `Dag`, so any other
  // acyclic graph is converted once, unchecked, since `Topological` certifies it.
  def layered(using ranking: Ranking): Layering[node] =
    val adjacency = Search.adjacency(nodal.nodes(graph), nodal.successors(graph, _))
    Layering(Dag.unchecked(adjacency), ranking)

// The choice package: `import rankings.balancedRanking` pulls nodes down toward their dependents.
package rankings:
  given longestPathRanking: Ranking = Ranking.LongestPath
  given balancedRanking: Ranking = Ranking.Balanced
