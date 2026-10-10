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
import scala.util.boundary, boundary.break

// The algorithms every representation shares, each a function of a node iterator and a
// successor relation alone — so the persistent and the frozen forms differ in what they store,
// never in how they search. Successors are dependencies throughout: a topological order lists a
// node after its successors.
private[acyclicity] object Search:
  // Iterative depth-first search with three colours, O(n + e). `Right` is the postorder, a
  // topological order with every node after its successors; `Left` is a cycle witness — the grey
  // chain from the revisited node to the top of the stack, closed by that node again. The
  // iteration order of `nodes` decides between equally valid orders, so an insertion-ordered
  // representation gives a stable one.
  def topological[node](nodes: Iterator[node], successors: node => Iterator[node])
  :   Either[sci.List[node], sci.List[node]] =

    val colour = scm.HashMap[node, Int]()            // absent white, 1 grey, 2 black
    val order = scm.ListBuffer[node]()
    val path = scm.ArrayBuffer[node]()               // the grey chain
    val pending = scm.ArrayBuffer[Iterator[node]]()  // each grey node's unvisited successors

    boundary:
      nodes.foreach: start =>
        if colour.getOrElse(start, 0) == 0 then
          colour(start) = 1
          path += start
          pending += successors(start)

          // Terminated by state: the chain empties when every node reached is black.
          while path.nonEmpty do
            val iterator = pending(pending.length - 1)

            if iterator.hasNext then
              val next = iterator.next()

              colour.getOrElse(next, 0) match
                case 0 =>
                  colour(next) = 1
                  path += next
                  pending += successors(next)

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
  // total on cyclic input, and O(reach) in both time and stack.
  def reachable[node](start: node, successors: node => Iterator[node]): Set[node] =
    val seen = scm.HashSet[node](start)
    val stack = scm.ArrayBuffer[node](start)

    // Terminated by state: every node is pushed at most once, when first seen.
    while stack.nonEmpty do
      val next = stack.remove(stack.length - 1)
      successors(next).foreach: child => if seen.add(child) then stack += child

    Set.from(seen)

  // Every edge reversed, every node kept, in the nodes' own order.
  def transpose[node](nodes: Iterator[node], successors: node => Iterator[node])
  :   sci.VectorMap[node, sci.Set[node]] =

    val builder = scm.LinkedHashMap[node, scm.HashSet[node]]()
    nodes.foreach: node => builder(node) = scm.HashSet()

    builder.keysIterator.toList.foreach: node =>
      successors(node).foreach: target => builder.getOrElseUpdate(target, scm.HashSet()) += node

    sci.VectorMap.from(builder.iterator.map { (node, origins) => (node, sci.Set.from(origins)) })

  // The adjacency of every node, closed over edge targets, in the nodes' own order.
  def adjacency[node](nodes: Iterator[node], successors: node => Iterator[node])
  :   sci.VectorMap[node, sci.Set[node]] =

    val builder = scm.LinkedHashMap[node, sci.Set[node]]()

    nodes.foreach: node =>
      val targets = sci.Set.from(successors(node))
      builder(node) = builder.getOrElse(node, sci.Set()) ++ targets
      targets.foreach: target => if !builder.contains(target) then builder(target) = sci.Set()

    sci.VectorMap.from(builder)
