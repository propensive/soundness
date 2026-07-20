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
┃    Soundness, version 0.63.0.                                                                    ┃
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

// Deliberate stdlib opt-out: `Dag`'s domain model is set-algebraic throughout (`edgeMap`,
// `keys`, intersections, differences), and migrating it to the total surface is deferred —
// this import shadows the opaque `Set` for the whole file, greppably.
import scala.collection.immutable.{List, Map, Nil, Set, ::}
import scala.collection.mutable as scm

import anticipation.*
import contingency.*
import denominative.*
import nomenclature.*
import rudiments.*

import Dot.*

object Dag:
  @targetName("apply2")
  def apply[node](keys: Set[node])(dependencies: node => Set[node]): Dag[node] =
    Dag(keys.map { key => (key, dependencies(key)) }.to(Map))

  @targetName("fromEdges")
  def apply[node](edges: (node, node)*): Dag[node] = Dag:
    edges.foldLeft(Map[node, Set[node]]()): case (acc, (key, value)) =>
      acc.updated(key, acc.get(key).fold(Set(value))(_ + value))

  @targetName("fromNodes")
  def apply[node](nodes: (node, Set[node])*): Dag[node] = Dag(Map(nodes*))

  extension (dag: Dag[Text])
    def dot: Dot = unsafely:
      val edges = dag.edges.to(List).map: (a, b) =>
        Name[DotId](a) --> Name[DotId](b)

      Dot.Digraph(None, false, edges*)

case class Dag[node] private[acyclicity](edgeMap: Map[node, Set[node]] = Map()):
  private val reachableCache: scm.HashMap[node, Set[node]] = scm.HashMap()

  def keys: Set[node] = edgeMap.keySet

  def map[node2](lambda: node => node2): Dag[node2] =
    Dag(edgeMap.map { (k, v) => (lambda(k), v.map(lambda)) })

  def subgraph(keep: Set[node]): Dag[node] = (keys &~ keep).fuse(this)(state.remove(next))
  def apply(key: node): Set[node] = edgeMap.getOrElse(key, Set())

  def descendants(key: node): Dag[node] raises DagError = subgraph(reachable(key))
  def ancestors(key: node): Dag[node] raises DagError = subgraph(invert.reachable(key))

  def lineage(key: node): Dag[node] raises DagError =
    subgraph(reachable(key) ++ invert.reachable(key))

  @targetName("removeKey")
  infix def - (key: node): Dag[node] = Dag(edgeMap - key)

  def sources: Set[node] = edgeMap.collect { case (k, v) if v.isEmpty => k }.to(Set)
  def edges: Set[(node, node)] = edgeMap.to(Set).flatMap: (key, values) => values.map(key -> _)
  def closure: Dag[node] = Dag(keys.map { k => k -> (reach(k) - k) }.to(Map))
  def sorted: List[node] raises DagError = sort(edgeMap, Nil).reverse
  def hasCycle(start: node): Boolean raises DagError = findCycle(start).isDefined

  def remove(key: node, value: node): Dag[node] =
    Dag(edgeMap.updated(key, edgeMap.get(key).fold(Set())(_ - value)))

  def has(key: node): Boolean = edgeMap.contains(key)

  def traversal[node2](lambda: (Set[node2], node) => node2): Map[node, node2] raises DagError =

    sorted.fuse(Map[node, node2]()):
      state.updated(next, lambda(apply(next).map(state), next))

  @targetName("addAll")
  infix def ++ (dag: Dag[node]): Dag[node] =
    val joined = edgeMap.to(List) ++ dag.edgeMap.to(List)
    Dag(joined.groupBy(_._1).view.mapValues(_.flatMap(_._2).to(Set)).to(Map))

  def add(key: node, value: node): Dag[node] = this ++ Dag(key -> value)

  def flatMap[node2](lambda: node => Dag[node2]): Dag[node2] = Dag:
    edgeMap.flatMap:
      case (k, v) => lambda(k).edgeMap.map:
        case (h, w) => (h, (w ++ v.flatMap(lambda(_).keys)))

  . reduction

  def reduction: Dag[node] =
    val allEdges = closure.edgeMap

    val removals =
      for
        key     <- keys
        edge    <- edgeMap(key)
        target  <- edgeMap.getOrElse(edge, Set())
                if allEdges(key)(target)
      yield (key, target)

    Dag:
      removals.foldLeft(edgeMap):
        case (m, (k, v)) => m.updated(k, m(k) - v)

  def reachable(node: node): Set[node] raises DagError =
    if !edgeMap.defines(node)
    then abort(DagError(DagError.Reason.NodeMissing(node.toString.tt)))
    else reach(node)

  private def reach(node: node): Set[node] =
    reachableCache.getOrElseUpdate(node, edgeMap.getOrElse(node, Set()).flatMap(reach) + node)

  def invert: Dag[node] = Dag:
    edgeMap.foldLeft(Map[node, Set[node]]()):
      case (acc, (k, vs)) =>
        vs.fuse(acc)(state.updated(next, state.get(next).fold(Set(k))(_ + k)))

  def remove(element: node): Dag[node] =
    val outgoing = edgeMap.getOrElse(element, Set())

    Dag:
      (edgeMap - element).view.mapValues:
        map => if map(element) then map ++ outgoing - element else map

      . to(Map)

  private def sort(todo: Map[node, Set[node]], done: List[node])
  :   List[node] raises DagError =

    if todo.isEmpty then done
    else todo.find { (k, vs) => (vs -- done).isEmpty } match
      case None => abort(DagError(DagError.Reason.Cyclic))

      case Some((node, _)) =>
        sort((todo - node).view.mapValues(_.filter(_ != node)).to(Map), node :: done)

  def filter(predicate: node => Boolean): Dag[node] =
    val deletions = keys.filter(!predicate(_))
    val inverted = invert

    Dag:
      val pruned =
        deletions.foldLeft(edgeMap): (acc, next) =>
          inverted(next).foldLeft(acc):
            (acc2, ref) => acc2.updated(ref, acc2(ref) - next ++ acc(next))

      pruned -- deletions

  private def findCycle(start: node): Option[List[node]] raises DagError =
    if !edgeMap.defines(start)
    then abort(DagError(DagError.Reason.NodeMissing(start.toString.tt)))

    @tailrec
    def recur(queue: List[(node, List[node])], finished: Set[node])
    :   Option[List[node]] =

      queue match
        case Nil => None

        case (vertex, trace) :: tail =>
          trace.to(Set).intersect(apply(vertex)).headOption match
            case Some(element) => Some(trace ++ List(vertex, element))

            case None =>
              val queue = tail ++ apply(vertex).diff(finished).to(List).map((_, trace :+ vertex))
              recur(queue, finished + vertex)

    recur(List((start, List())), Set())
