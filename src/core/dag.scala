/*
    Acyclicity, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package acyclicity

import rudiments.*
import anticipation.*

import scala.collection.mutable.HashMap

import language.experimental.captureChecking

object Dag:
  @targetName("build")
  def apply[NodeType](keys: Set[NodeType])(dependencies: NodeType => Set[NodeType]): Dag[NodeType] =
    Dag(keys.map { k => (k, dependencies(k)) }.to(Map))

  @targetName("fromEdges")
  def apply[NodeType](edges: (NodeType, NodeType)*): Dag[NodeType] = Dag:
    edges.foldLeft(Map[NodeType, Set[NodeType]]()):
      case (acc, (k, v)) => acc.updated(k, acc.get(k).fold(Set(v))(_ + v))
  
  @targetName("fromNodes")
  def apply[NodeType](nodes: (NodeType, Set[NodeType])*): Dag[NodeType] = Dag(Map(nodes*))

case class Dag[NodeType] private(edgeMap: Map[NodeType, Set[NodeType]] = Map()):
  private val reachableCache: HashMap[NodeType, Set[NodeType]] = HashMap()
  
  def keys: Set[NodeType] = edgeMap.keySet
  def map[NodeType2](lambda: NodeType => NodeType2): Dag[NodeType2] = Dag(edgeMap.map { case (k, v) => (lambda(k), v.map(lambda)) })
  def subgraph(keep: Set[NodeType]): Dag[NodeType] = (keys &~ keep).foldLeft(this)(_.remove(_))
  def apply(key: NodeType): Set[NodeType] = edgeMap.getOrElse(key, Set())
  def descendants(key: NodeType): Dag[NodeType] = subgraph(reachable(key))

  @targetName("removeKey")
  infix def -(key: NodeType): Dag[NodeType] = Dag(edgeMap - key)
  
  def sources: Set[NodeType] = edgeMap.collect { case (k, v) if v.isEmpty => k }.to(Set)
  def edges: Set[(NodeType, NodeType)] = edgeMap.to(Set).flatMap { (k, vs) => vs.map(k -> _) }
  def closure: Dag[NodeType] = Dag(keys.map { k => k -> (reachable(k) - k) }.to(Map))
  def sorted: List[NodeType] = sort(edgeMap, Nil).reverse
  def hasCycle(start: NodeType): Boolean = findCycle(start).isDefined
  
  def remove(key: NodeType, value: NodeType): Dag[NodeType] =
    Dag(edgeMap.updated(key, edgeMap.get(key).fold(Set())(_ - value)))

  def traversal[NodeType2](lambda: (Set[NodeType2], NodeType) -> NodeType2): Map[NodeType, NodeType2] =
    sorted.foldLeft(Map[NodeType, NodeType2]()):
      (map, next) => map.updated(next, lambda(apply(next).map(map), next))

  @targetName("addAll")
  infix def ++(dag: Dag[NodeType]): Dag[NodeType] =
    val joined = edgeMap.to(List) ++ dag.edgeMap.to(List)
    Dag(joined.groupBy(_._1).view.mapValues(_.flatMap(_._2).to(Set)).to(Map))
  
  def add(key: NodeType, value: NodeType): Dag[NodeType] = this ++ Dag(key -> value)

  def flatMap[NodeType2](lambda: NodeType => Dag[NodeType2]): Dag[NodeType2] = Dag:
    edgeMap.flatMap:
      case (k, v) => lambda(k).edgeMap.map:
        case (h, w) => (h, (w ++ v.flatMap(lambda(_).keys)))
  .reduction

  def reduction: Dag[NodeType] =
    val allEdges = closure.edgeMap
    val removals = for i <- keys; j <- edgeMap(i); k <- edgeMap(j) if allEdges(i)(k) yield (i, k)
    
    Dag:
      removals.foldLeft(edgeMap):
        case (m, (k, v)) => m.updated(k, m(k) - v)


  def reachable(node: NodeType): Set[NodeType] =
    reachableCache.getOrElseUpdate(node, edgeMap(node).flatMap(reachable) + node)

  def invert: Dag[NodeType] = Dag:
    edgeMap.foldLeft(Map[NodeType, Set[NodeType]]()):
      case (acc, (k, vs)) =>
        vs.foldLeft(acc):
          (acc2, v) => acc2.updated(v, acc2.get(v).fold(Set(k))(_ + k))

  def remove(elem: NodeType): Dag[NodeType] = Dag:
    (edgeMap - elem).view.mapValues:
      map => if map(elem) then map ++ edgeMap(elem) - elem else map
    .to(Map)
  
  private def sort(todo: Map[NodeType, Set[NodeType]], done: List[NodeType]): List[NodeType] =
    if todo.isEmpty then done
    else
      val (node, _) = todo.find { (k, vs) => (vs -- done).isEmpty }.get
      sort((todo - node).view.mapValues(_.filter(_ != node)).to(Map), node :: done)

  def filter(pred: NodeType => Boolean): Dag[NodeType] =
    val deletions = keys.filter(!pred(_))
    val inverted = invert
    
    Dag:
      deletions.foldLeft(edgeMap) { (acc, next) =>
        inverted(next).foldLeft(acc):
          (acc2, ref) => acc2.updated(ref, acc2(ref) - next ++ acc(next))
      } -- deletions

  private def findCycle(start: NodeType): Option[List[NodeType]] =
    @tailrec
    def recur(queue: List[(NodeType, List[NodeType])], finished: Set[NodeType]): Option[List[NodeType]] = queue match
      case Nil =>
        None
      case (vertex, trace) :: tail =>
        trace.to(Set).intersect(apply(vertex)).headOption match
          case Some(element) =>
            Some(trace ++ List(vertex, element))
            
          case None =>
            val queue = tail ++ apply(vertex).diff(finished).toList.map((_, trace :+ vertex))
            recur(queue, finished + vertex)

    recur(List((start, List())), Set())

extension (dag: Dag[Text])
  def dot: Dot = Digraph(dag.edges.to(List).map(Dot.Ref(_) --> Dot.Ref(_))*)
