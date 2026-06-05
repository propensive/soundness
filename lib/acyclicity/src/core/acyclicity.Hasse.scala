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
┃    Soundness, version 0.54.0.                                                                    ┃
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

import scala.collection.mutable as scm

object Hasse:
  // Build the Hasse diagram (covering relation) of the poset on `elements` ordered
  // by `lessOrEqual` (where `lessOrEqual(a, b)` means `a ≤ b`). Elements are
  // inserted one at a time; each insertion locates its immediate supertypes by
  // descending from the current maxima and its immediate subtypes by ascending
  // from the current minima, pruning whole subtrees as soon as a comparison rules
  // them out (transitivity). Comparisons are memoised, so no pair is tested twice.
  def apply[element](elements: Set[element])
    ( lessOrEqual: (element, element) => Boolean )
  :   Hasse[element] =

    val greater = scm.HashMap[element, scm.HashSet[element]]()  // node -> immediate supertypes
    val lesser  = scm.HashMap[element, scm.HashSet[element]]()  // node -> immediate subtypes
    val maxima  = scm.HashSet[element]()
    val minima  = scm.HashSet[element]()
    val aliases = scm.HashMap[element, element]()
    val memo    = scm.HashMap[(element, element), Boolean]()

    def le(left: element, right: element): Boolean =
      left == right || memo.getOrElseUpdate((left, right), lessOrEqual(left, right))

    def insert(x: element): Unit =
      val parents = scm.HashSet[element]()
      val seenDown = scm.HashSet[element]()

      // `node` is already known to be a supertype of `x`; find the immediate ones.
      def descend(node: element): Unit =
        val lower = lesser(node).filter(le(x, _))

        if lower.isEmpty then parents += node
        else lower.foreach: child =>
          if !seenDown.contains(child) then
            seenDown += child
            descend(child)

      maxima.foreach: max =>
        if le(x, max) && !seenDown.contains(max) then
          seenDown += max
          descend(max)

      parents.find(le(_, x)) match
        case Some(equivalent) =>
          aliases(x) = equivalent   // `x` is equivalent to an existing node; merge

        case None =>
          val children = scm.HashSet[element]()
          val seenUp = scm.HashSet[element]()

          // `node` is already known to be a subtype of `x`; find the immediate ones.
          def ascend(node: element): Unit =
            val higher = greater(node).filter(le(_, x))

            if higher.isEmpty then children += node
            else higher.foreach: parent =>
              if !seenUp.contains(parent) then
                seenUp += parent
                ascend(parent)

          minima.foreach: min =>
            if le(min, x) && !seenUp.contains(min) then
              seenUp += min
              ascend(min)

          greater(x) = scm.HashSet[element]() ++= parents
          lesser(x) = scm.HashSet[element]() ++= children

          // Splice: any direct cover `p → c` between an immediate parent and an
          // immediate child is now mediated by `x`, so redirect it through `x`.
          parents.foreach: parent =>
            lesser(parent) --= children
            lesser(parent) += x
            minima -= parent

          children.foreach: child =>
            greater(child) --= parents
            greater(child) += x
            maxima -= child

          if parents.isEmpty then maxima += x
          if children.isEmpty then minima += x

    elements.foreach(insert)

    new Hasse
      ( greater.view.mapValues(_.to(Set)).to(Map),
        lesser.view.mapValues(_.to(Set)).to(Map),
        aliases.to(Map) )

case class Hasse[element] private[acyclicity]
  ( greater: Map[element, Set[element]],     // node -> immediate supertypes (parents, ↑)
    lesser:  Map[element, Set[element]],     // node -> immediate subtypes  (children, ↓)
    aliases: Map[element, element] = Map() ):

  private def canonical(node: element): element = aliases.getOrElse(node, node)

  def parents(node: element): Set[element] = greater.getOrElse(canonical(node), Set())
  def children(node: element): Set[element] = lesser.getOrElse(canonical(node), Set())
  def elements: Set[element] = greater.keySet
  def maxima: Set[element] = elements.filter(greater(_).isEmpty)
  def minima: Set[element] = elements.filter(lesser(_).isEmpty)
  def dag: Dag[element] = Dag(lesser)

  // Attach `value` below every current minimum in one comparison-free sweep — it
  // is, by construction, less than everything (e.g. `Nothing` among types).
  def bottom(value: element): Hasse[element] =
    val mins = minima

    val lesser2 =
      mins.foldLeft(lesser.updated(value, Set())): (map, min) =>
        map.updated(min, map.getOrElse(min, Set()) + value)

    new Hasse(greater.updated(value, mins), lesser2, aliases)

  // Attach `value` above every current maximum in one comparison-free sweep.
  def top(value: element): Hasse[element] =
    val maxs = maxima

    val greater2 =
      maxs.foldLeft(greater.updated(value, Set())): (map, max) =>
        map.updated(max, map.getOrElse(max, Set()) + value)

    new Hasse(greater2, lesser.updated(value, maxs), aliases)
