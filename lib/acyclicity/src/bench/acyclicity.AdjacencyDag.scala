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

// Candidate (a): the persistent adjacency map `Dag` has today, with every algorithm repaired —
// iterative depth-first search in place of the quadratic `sorted` and the recursive `reach`,
// `add` by two `updated`s rather than a rebuild of the whole map, and no mutable memo. Only the
// forward direction is stored, so the transpose is a lazy value and `predecessors`, `sinks`,
// `remove` and `bypass` cost O(n + e) the first time each instance needs it.
final class AdjacencyDag[node](val adjacency: Map[node, Set[node]]):
  def nodes: Set[node] = adjacency.keySet
  def size: Int = adjacency.size
  def has(node: node): Boolean = adjacency.contains(node)
  def successors(node: node): Set[node] = adjacency.getOrElse(node, Set())

  def edges: Set[(node, node)] =
    adjacency.iterator.flatMap { (from, targets) => targets.iterator.map(from -> _) }.toSet

  def edgeCount: Int = adjacency.valuesIterator.map(_.size).sum

  // The nodes with no dependencies: a whole-graph scan.
  def sources: Set[node] =
    adjacency.iterator.collect { case (from, targets) if targets.isEmpty => from }.toSet

  lazy val inverse: Map[node, Set[node]] =
    val empty: Map[node, Set[node]] = adjacency.map { (from, _) => (from, Set[node]()) }

    adjacency.foldLeft(empty): (acc, entry) =>
      entry(1).foldLeft(acc): (acc2, target) =>
        acc2.updated(target, acc2.getOrElse(target, Set()) + entry(0))

  def predecessors(node: node): Set[node] = inverse.getOrElse(node, Set())
  def sinks: Set[node] = adjacency.keysIterator.filter(predecessors(_).isEmpty).toSet
  def invert: AdjacencyDag[node] = AdjacencyDag(inverse)

  def add(from: node, to: node): AdjacencyDag[node] =
    val added = adjacency.updated(from, successors(from) + to)
    AdjacencyDag(if added.contains(to) then added else added.updated(to, Set()))

  // Drops the node and its incident edges.
  def remove(node: node): AdjacencyDag[node] =
    AdjacencyDag:
      predecessors(node).foldLeft(adjacency - node): (acc, from) =>
        acc.updated(from, acc(from) - node)

  // Drops the node, rerouting each of its dependants to each of its dependencies.
  def bypass(node: node): AdjacencyDag[node] =
    val targets = successors(node)

    AdjacencyDag:
      predecessors(node).foldLeft(adjacency - node): (acc, from) =>
        acc.updated(from, acc(from) - node ++ targets)

  private def search: Either[List[node], List[node]] = Search.topological(adjacency.keys, successors)

  def sorted: Option[List[node]] = search.toOption
  def cycle: Option[List[node]] = search.left.toOption
  def reachable(node: node): Set[node] = Search.reachable(node, successors)

  private def reach: Map[node, Set[node]] = Search.closure(sorted.get, successors)

  def closure: AdjacencyDag[node] = AdjacencyDag(reach)
  def reduction: AdjacencyDag[node] = AdjacencyDag(Search.reduction(adjacency.keys, successors, reach))
  def freeze: FrozenDag[node] = FrozenDag(sorted.get, successors)

object AdjacencyDag:
  def apply[node](adjacency: Map[node, Set[node]]): AdjacencyDag[node] = new AdjacencyDag(adjacency)

  // From parallel edge arrays over the nodes `0 until count`, by persistent updates: the cost
  // that a fold of `add` pays.
  def apply(count: Int, from: scala.IArray[Int], to: scala.IArray[Int]): AdjacencyDag[Int] =
    var adjacency: Map[Int, Set[Int]] = Map()
    var index = 0

    while index < count do
      adjacency = adjacency.updated(index, Set())
      index += 1

    index = 0

    while index < from.length do
      val source = from(index)
      adjacency = adjacency.updated(source, adjacency(source) + to(index))
      index += 1

    new AdjacencyDag(adjacency)
