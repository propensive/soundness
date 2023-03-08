/*
    Acyclicity, version 0.4.0. Copyright 2021-23 Jon Pretty, Propensive OÜ.

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

object Dag:
  @targetName("build")
  def apply[T](keys: Set[T])(dependencies: T => Set[T]): Dag[T] =
    Dag(keys.map { k => (k, dependencies(k)) }.to(Map))

  @targetName("fromEdges")
  def apply[T](edges: (T, T)*): Dag[T] = Dag:
    edges.foldLeft(Map[T, Set[T]]()):
      case (acc, (k, v)) => acc.updated(k, acc.get(k).fold(Set(v))(_ + v))
  
  @targetName("fromNodes")
  def apply[T](nodes: (T, Set[T])*): Dag[T] = Dag(Map(nodes*))

case class Dag[T] private(edgeMap: Map[T, Set[T]] = Map()):
  def keys: Set[T] = edgeMap.keySet
  def map[S](fn: T => S): Dag[S] = Dag(edgeMap.map { case (k, v) => (fn(k), v.map(fn)) })
  def subgraph(keep: Set[T]): Dag[T] = (keys &~ keep).foldLeft(this)(_.remove(_))
  def apply(key: T): Set[T] = edgeMap.getOrElse(key, Set())
  def descendants(key: T): Dag[T] = subgraph(reachable(key))

  @targetName("removeKey")
  infix def -(key: T): Dag[T] = Dag(edgeMap - key)
  
  def sources: Set[T] = edgeMap.collect { case (k, v) if v.isEmpty => k }.to(Set)
  def edges: Set[(T, T)] = edgeMap.to(Set).flatMap { (k, vs) => vs.map(k -> _) }
  def closure: Dag[T] = Dag(keys.map { k => k -> (reachable(k) - k) }.to(Map))
  def sorted: List[T] = sort(edgeMap, Nil).reverse
  def hasCycle(start: T): Boolean = findCycle(start).isDefined
  
  def remove(key: T, value: T): Dag[T] =
    Dag(edgeMap.updated(key, edgeMap.get(key).fold(Set())(_ - value)))

  def traversal[S](fn: (Set[S], T) => S): Map[T, S] =
    sorted.foldLeft(Map[T, S]()):
      (map, next) => map.updated(next, fn(apply(next).map(map), next))

  @targetName("addAll")
  infix def ++(dag: Dag[T]): Dag[T] =
    val joined = edgeMap.to(List) ++ dag.edgeMap.to(List)
    Dag(joined.groupBy(_._1).view.mapValues(_.flatMap(_._2).to(Set)).to(Map))
  
  def add(key: T, value: T): Dag[T] = this ++ Dag(key -> value)

  def flatMap[S](fn: T => Dag[S]): Dag[S] = Dag:
    edgeMap.flatMap:
      case (k, v) => fn(k).edgeMap.map:
        case (h, w) => (h, (w ++ v.flatMap(fn(_).keys)))
  .reduction

  def reduction: Dag[T] =
    val allEdges = closure.edgeMap
    val removals = for i <- keys; j <- edgeMap(i); k <- edgeMap(j) if allEdges(i)(k) yield (i, k)
    
    Dag:
      removals.foldLeft(edgeMap):
        case (m, (k, v)) => m.updated(k, m(k) - v)

  // FIXME: This may be a slow implementation if called repeatedly
  def reachable(node: T): Set[T] = edgeMap(node).flatMap(reachable) + node

  def invert: Dag[T] = Dag:
    edgeMap.foldLeft(Map[T, Set[T]]()):
      case (acc, (k, vs)) =>
        vs.foldLeft(acc):
          (acc2, v) => acc2.updated(v, acc2.get(v).fold(Set(k))(_ + k))

  def remove(elem: T): Dag[T] = Dag:
    (edgeMap - elem).view.mapValues:
      map => if map(elem) then map ++ edgeMap(elem) - elem else map
    .to(Map)
  
  private def sort(todo: Map[T, Set[T]], done: List[T]): List[T] =
    if todo.isEmpty then done
    else
      val (node, _) = todo.find { (k, vs) => (vs -- done).isEmpty }.get
      sort((todo - node).view.mapValues(_.filter(_ != node)).to(Map), node :: done)

  def filter(pred: T => Boolean): Dag[T] =
    val deletions = keys.filter(!pred(_))
    val inverted = invert
    
    Dag:
      deletions.foldLeft(edgeMap) { (acc, next) =>
        inverted(next).foldLeft(acc):
          (acc2, ref) => acc2.updated(ref, acc2(ref) - next ++ acc(next))
      } -- deletions

  private def findCycle(start: T): Option[List[T]] =
    @tailrec
    def recur(queue: List[(T, List[T])], finished: Set[T]): Option[List[T]] = queue match
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
