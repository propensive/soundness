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
import scala.collection.immutable.{List, Map, Set}

// Candidate (b), not adopted: the persistent adjacency map in both directions, with the sources
// (nodes that depend on nothing) and sinks (nodes nothing depends on) maintained as sets through
// every edit — issue #488's amortisation done as bookkeeping rather than as a sentinel node.
// `sources`, `sinks` and `invert` are O(1), `predecessors` O(log n), and `remove`/`bypass` need
// no transpose, at the price of a second map update per edge, which made construction 3.7×
// slower than `Dag`'s; kept so that the comparison stays reproducible.
final class MirroredDag[node] private
  ( val forward:  Map[node, Set[node]],
    val backward: Map[node, Set[node]],
    val sources:  Set[node],
    val sinks:    Set[node] ):

  def nodes: Set[node] = forward.keySet
  def size: Int = forward.size
  def has(node: node): Boolean = forward.contains(node)
  def successors(node: node): Set[node] = forward.getOrElse(node, Set())
  def predecessors(node: node): Set[node] = backward.getOrElse(node, Set())

  def edges: Set[(node, node)] =
    forward.iterator.flatMap { (from, targets) => targets.iterator.map(from -> _) }.toSet

  def edgeCount: Int = forward.valuesIterator.map(_.size).sum

  def invert: MirroredDag[node] = new MirroredDag(backward, forward, sinks, sources)

  def add(from: node, to: node): MirroredDag[node] =
    val forward1 = forward.updated(from, successors(from) + to)
    val forward2 = if forward1.contains(to) then forward1 else forward1.updated(to, Set())
    val backward1 = backward.updated(to, predecessors(to) + from)
    val backward2 = if backward1.contains(from) then backward1 else backward1.updated(from, Set())
    val sources2 = (if forward.contains(to) then sources else sources + to) - from
    val sinks2 = (if backward.contains(from) then sinks else sinks + from) - to

    new MirroredDag(forward2, backward2, sources2, sinks2)

  def remove(node: node): MirroredDag[node] =
    val above = predecessors(node)
    val below = successors(node)
    val forward2 = above.foldLeft(forward - node) { (acc, from) => acc.updated(from, acc(from) - node) }
    val backward2 = below.foldLeft(backward - node) { (acc, to) => acc.updated(to, acc(to) - node) }
    val sources2 = (sources - node) ++ above.filter(forward2(_).isEmpty)
    val sinks2 = (sinks - node) ++ below.filter(backward2(_).isEmpty)

    new MirroredDag(forward2, backward2, sources2, sinks2)

  def bypass(node: node): MirroredDag[node] =
    val above = predecessors(node)
    val below = successors(node)

    val forward2 =
      above.foldLeft(forward - node) { (acc, from) => acc.updated(from, acc(from) - node ++ below) }

    val backward2 =
      below.foldLeft(backward - node) { (acc, to) => acc.updated(to, acc(to) - node ++ above) }

    val sources2 = (sources - node) ++ above.filter(forward2(_).isEmpty)
    val sinks2 = (sinks - node) ++ below.filter(backward2(_).isEmpty)

    new MirroredDag(forward2, backward2, sources2, sinks2)

  private def search: Either[List[node], List[node]] =
    Search.topological(forward.keysIterator, successors(_).iterator)

  def sorted: Option[List[node]] = search.toOption
  def cycle: Option[List[node]] = search.left.toOption

  def reachable(node: node): Set[node] =
    proscenium.Set.iterator(Search.reachable(node, successors(_).iterator)).toSet

  // Through the frozen form, as `Dag` does.
  private def frozen: Topology[node]^{} =
    Topology.of(proscenium.List.from(sorted.get), successors(_).iterator)

  private def entries(dag: Dag[node]): Map[node, Set[node]] =
    proscenium.Set.iterator(dag.nodes).map { node => node -> proscenium.Set.iterator(dag.successors(node)).toSet }.toMap

  def closure: MirroredDag[node] = MirroredDag(entries(frozen.closure))
  def reduction: MirroredDag[node] = MirroredDag(entries(frozen.reduction))

object MirroredDag:
  def empty[node]: MirroredDag[node] = new MirroredDag(Map(), Map(), Set(), Set())

  // From a forward map alone, deriving the mirror: O(n + e), the price of a whole-graph result.
  def apply[node](forward: Map[node, Set[node]]): MirroredDag[node] =
    val empty: Map[node, Set[node]] = forward.map { (from, _) => (from, Set[node]()) }

    val backward =
      forward.foldLeft(empty): (acc, entry) =>
        entry(1).foldLeft(acc): (acc2, target) =>
          acc2.updated(target, acc2.getOrElse(target, Set()) + entry(0))

    val sources = forward.iterator.collect { case (from, targets) if targets.isEmpty => from }.toSet
    val sinks = backward.iterator.collect { case (to, origins) if origins.isEmpty => to }.toSet

    new MirroredDag(forward, backward, sources, sinks)

  def apply(count: Int, from: scala.IArray[Int], to: scala.IArray[Int]): MirroredDag[Int] =
    var forward: Map[Int, Set[Int]] = Map()
    var sources: Set[Int] = Set()
    var index = 0

    while index < count do
      forward = forward.updated(index, Set())
      sources = sources + index
      index += 1

    var dag = new MirroredDag(forward, forward, sources, sources)
    index = 0

    while index < from.length do
      dag = dag.add(from(index), to(index))
      index += 1

    dag
