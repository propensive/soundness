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

import scala.collection.immutable.{List, Map, Nil, Set, ::}
import scala.collection.mutable as scm

// The baseline: the dependency graph a project writes for itself when it does not reach for a
// library — a mutable hash map of immutable sets, Kahn's algorithm with a queue, a recursive
// memoised closure. Nodes are `Int`s, as a project's would be once it had numbered them.
final class StdlibDag(val adjacency: scm.HashMap[Int, Set[Int]]):
  def size: Int = adjacency.size
  def edgeCount: Int = adjacency.valuesIterator.map(_.size).sum
  def copy: StdlibDag = new StdlibDag(adjacency.clone())

  def add(from: Int, to: Int): Unit =
    adjacency(from) = adjacency.getOrElse(from, Set()) + to
    if !adjacency.contains(to) then adjacency(to) = Set()

  def successors(node: Int): Set[Int] = adjacency.getOrElse(node, Set())

  def edges: Set[(Int, Int)] =
    adjacency.iterator.flatMap { (from, targets) => targets.iterator.map(from -> _) }.toSet

  private def dependants: scm.HashMap[Int, Set[Int]] =
    val result = scm.HashMap[Int, Set[Int]]()

    adjacency.foreach: (node, targets) =>
      if !result.contains(node) then result(node) = Set()
      targets.foreach { target => result(target) = result.getOrElse(target, Set()) + node }

    result

  def sorted: Option[List[Int]] =
    val remaining = scm.HashMap[Int, Int]()
    val above = dependants
    val queue = scm.Queue[Int]()
    val order = scm.ListBuffer[Int]()

    adjacency.foreach: (node, targets) =>
      remaining(node) = targets.size
      if targets.isEmpty then queue += node

    while queue.nonEmpty do
      val node = queue.dequeue()
      order += node

      above(node).foreach: dependant =>
        remaining(dependant) -= 1
        if remaining(dependant) == 0 then queue += dependant

    if order.length == adjacency.size then Some(order.toList) else None

  def reachable(start: Int): Set[Int] =
    val seen = scm.HashSet[Int](start)
    val stack = scm.ListBuffer[Int](start)

    while stack.nonEmpty do
      val node = stack.remove(stack.length - 1)
      adjacency(node).foreach { child => if seen.add(child) then stack += child }

    seen.toSet

  def sources: Set[Int] =
    adjacency.iterator.collect { case (node, targets) if targets.isEmpty => node }.toSet

  def sinks: Set[Int] =
    dependants.iterator.collect { case (node, origins) if origins.isEmpty => node }.toSet

  // The memoised recursion a project writes first.
  def closure: Map[Int, Set[Int]] =
    val memo = scm.HashMap[Int, Set[Int]]()

    def reach(node: Int): Set[Int] =
      memo.getOrElseUpdate
        (node, adjacency(node).foldLeft(Set[Int]()) { (acc, child) => acc ++ reach(child) + child })

    adjacency.keysIterator.foreach(reach)
    memo.toMap

  def reduction: Map[Int, Set[Int]] =
    val reach = closure

    adjacency.iterator.map: (node, children) =>
      node -> children.filter { child => !children.exists { other => other != child && reach(other)(child) } }

    . toMap

  def invert: StdlibDag = new StdlibDag(dependants)

  def bypass(node: Int): Unit =
    val targets = adjacency(node)

    adjacency.foreach: (from, children) =>
      if children.contains(node) then adjacency(from) = children - node ++ targets

    adjacency -= node

object StdlibDag:
  def apply(count: Int, from: scala.IArray[Int], to: scala.IArray[Int]): StdlibDag =
    val adjacency = scm.HashMap[Int, Set[Int]]()
    var index = 0

    while index < count do
      adjacency(index) = Set()
      index += 1

    index = 0

    while index < from.length do
      adjacency(from(index)) = adjacency(from(index)) + to(index)
      index += 1

    new StdlibDag(adjacency)
