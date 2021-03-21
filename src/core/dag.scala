/*

    Acyclicity, version 0.1.0. Copyright 2021-21 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
    compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the License is
    distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and limitations under the License.

*/
package acyclicity

import scala.annotation.tailrec

object Dag {
  def from[T](keys: Set[T])(dependencies: T => Set[T]): Dag[T] =
    Dag(keys.map { k => (k, dependencies(k)) }.toMap)
}
case class Dag[T](edges: Map[T, Set[T]] = Map[T, Set[T]]()) {
  def keys: Set[T] = edges.keySet
  def map[S](fn: T => S): Dag[S] = Dag[S](edges.map { case (k, v) => (fn(k), v.map(fn)) })
  def subgraph(keep: Set[T]): Dag[T] = (edges.keySet &~ keep).foldLeft(this)(_.remove(_))
  def apply(key: T): Set[T] = edges.getOrElse(key, Set())
  def remove(key: T, value: T): Dag[T] = Dag(edges.updated(key, edges.get(key).map(_ - value).getOrElse(Set())))
  def -(key: T): Dag[T] = Dag(edges - key)
  def sources: Set[T] = edges.collect { case (k, v) if v.isEmpty => k }.to[Set]

  def ++(dag: Dag[T]): Dag[T] =
    Dag((edges.to[List] ++ dag.edges.to[List]).groupBy(_._1).mapValues(_.flatMap(_._2).to[Set]).toMap)

  def flatMap[S](fn: T => Dag[S]): Dag[S] = Dag[S](edges.flatMap { case (k, v) =>
    fn(k).edges.map { case (k2, v2) => k2 -> (v2 ++ v.flatMap(fn(_).edges.keySet)) }
  }).simplify

  def simplify: Dag[T] = {
    val allEdges = closure.edges
    val removals = for(i <- edges.keySet; j <- edges(i); k <- edges(j) if allEdges(i)(k)) yield i -> k
    Dag(removals.foldLeft(edges) { case (m, (k, v)) => m.updated(k, m(k) - v) })
  }

  def closure: Dag[T] = Dag(edges.keySet.map { k => k -> (reachable(k) - k) }.toMap)

  // FIXME: This may be a slow implementation if called repeatedly
  def reachable(node: T): Set[T] = edges(node).flatMap(reachable) + node

  def invert: Dag[T] = Dag(edges.foldLeft(Map[T, Set[T]]()) { case (acc, (k, vs)) => vs.foldLeft(acc) {
    case (acc2, v) => acc2.updated(v, acc2.get(v).fold(Set(k))(_ + k))
  } })

  def remove(elem: T): Dag[T] =
    Dag((edges - elem).mapValues { map => if(map(elem)) map ++ edges(elem) - elem else map })

  def sorted: List[T] = sort(edges, Nil).reverse
  
  private def sort(todo: Map[T, Set[T]], done: List[T]): List[T] =
    if(todo.isEmpty) done else {
      val node = todo.find { case (k, vs) => (vs -- done).isEmpty }.get._1
      sort((todo - node).mapValues(_.filter(_ != node)), node :: done)
    }

  def filter(pred: T => Boolean): Dag[T] = {
    val deletions = edges.keySet.filter(!pred(_))
    val inverted = invert
    Dag(deletions.foldLeft(edges) { case (acc, next) =>
      val indirect: Set[T] = acc(next)
      inverted(next).foldLeft(acc) { case (acc2, ref) => acc2.updated(ref, acc2(ref) - next ++ indirect) }
    } -- deletions)
    
  }

  def neighbours(start: T): Set[T] = edges.getOrElse(start, Set())
  def hasCycle(start: T): Boolean = findCycle(start).isDefined

  def findCycle(start: T): Option[List[T]] = {
    @tailrec
    def findCycleHelper(queue: List[(T, List[T])], finished: Set[T]): Option[List[T]] = queue match {
      case List() =>
        None
      case (vertex, trace) :: tail =>
        trace.toSet.intersect(neighbours(vertex)).headOption match {
          case Some(element) =>
            Some(trace ++ List(vertex, element))
          case None =>
            val queue = tail ++ neighbours(vertex).diff(finished).toList.map((_, trace :+ vertex))
            findCycleHelper(queue, finished + vertex)
        }
    }

    findCycleHelper(List((start, List())), Set())
  }

  def allDescendants(start: T): Either[List[T], Set[T]] = {

    @tailrec
    def allDescendantsHelper(stack: List[T], ans: Set[T]): Set[T] = stack match {
      case List()       => ans
      case head :: tail => allDescendantsHelper(neighbours(head).toList ++ tail, ans + head)
    }

    findCycle(start) match {
      case Some(cycle) => Left(cycle)
      case None        => Right(neighbours(start).flatMap(c => allDescendantsHelper(List(c), Set())))
    }
  }
}
