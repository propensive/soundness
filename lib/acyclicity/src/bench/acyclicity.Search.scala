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

// Deliberate stdlib opt-out, as in `Dag`: the persistent candidates measure the representation
// `Dag` has today, so they stay on the same collections.
import scala.collection.immutable.{List, Map, Nil, Set, ::}
import scala.collection.mutable as scm
import scala.util.boundary, boundary.break

// The algorithms shared by the persistent candidates (`AdjacencyDag`, `MirroredDag`). Each is a
// function of the node set and the successor relation alone, so the two representations differ
// only in what they store, never in how they search. Successors are dependencies throughout:
// an edge `a -> b` means `a` requires `b`, and a topological order lists `b` before `a`.
object Search:
  // Iterative depth-first search with three colours: `Right` is the postorder, a topological
  // order with every node after its dependencies; `Left` is a cycle witness, the grey chain
  // from the revisited node to the top of the stack, closed by that node again.
  def topological[node](nodes: Iterable[node], successors: node => Set[node])
  :   Either[List[node], List[node]] =

    val colour = scm.HashMap[node, Int]()            // absent white, 1 grey, 2 black
    val order = scm.ListBuffer[node]()
    val path = scm.ArrayBuffer[node]()               // the grey chain
    val pending = scm.ArrayBuffer[Iterator[node]]()  // each grey node's unvisited successors

    boundary:
      nodes.foreach: start =>
        if colour.getOrElse(start, 0) == 0 then
          colour(start) = 1
          path += start
          pending += successors(start).iterator

          while path.nonEmpty do
            val iterator = pending(pending.length - 1)

            if iterator.hasNext then
              val next = iterator.next()

              colour.getOrElse(next, 0) match
                case 0 =>
                  colour(next) = 1
                  path += next
                  pending += successors(next).iterator

                case 1 =>
                  val from = path.indexOf(next)
                  break(Left(path.drop(from).toList :+ next))

                case _ =>
                  ()
            else
              val finished = path.remove(path.length - 1)
              pending.remove(pending.length - 1)
              colour(finished) = 2
              order += finished

      Right(order.toList)

  // Everything reachable from `start`, inclusive, by an iterative search with a visited set:
  // total on cyclic input, and O(reach) rather than O(depth) stack.
  def reachable[node](start: node, successors: node => Set[node]): Set[node] =
    val seen = scm.HashSet[node](start)
    val stack = scm.ArrayBuffer[node](start)

    while stack.nonEmpty do
      val next = stack.remove(stack.length - 1)
      successors(next).foreach { child => if seen.add(child) then stack += child }

    seen.toSet

  // The reach set of every node, computed dependencies-first so that each node's set is the
  // union of its successors' sets plus the successors themselves: O(Σ|reach|) set unions.
  def closure[node](order: List[node], successors: node => Set[node]): Map[node, Set[node]] =
    val reach = scm.HashMap[node, Set[node]]()

    order.foreach: node =>
      reach(node) = successors(node).foldLeft(Set[node]()) { (acc, child) => acc ++ reach(child) + child }

    reach.toMap

  // Transitive reduction from the closure: an edge `v -> w` is redundant when another successor
  // `u` of `v` reaches `w`. O(Σ degree²) hash lookups.
  def reduction[node]
    (nodes: Iterable[node], successors: node => Set[node], reach: Map[node, Set[node]])
  :   Map[node, Set[node]] =

    nodes.iterator.map: node =>
      val children = successors(node)
      node -> children.filter { child => !children.exists { other => other != child && reach(other)(child) } }

    . toMap
