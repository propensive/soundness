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

import prepositional.*

// A finite directed graph seen through its nodes: an enumerable node set and, for each node, the
// nodes it has edges to — its `Operand`, bound with `by`, as `Dag[node] is Nodal by node`. Edge
// direction is fixed by contract, not by instance: `successors(n)` are the nodes `n` points at,
// which under the dependency reading used throughout Soundness (`a -> b` means `a` requires `b`)
// are `a`'s dependencies, so a topological order lists a node after its successors. A
// representation that also stores the other direction says so through `Bidirectional`, and one
// whose values can hold no cycle through `Topological`.
//
// The iterators are internal currency, as in `Traversable`: the cheapest thing every
// representation can produce, never user-facing; the extensions in `acyclicity_core` return
// `Set`s and `List`s. `has` is a primitive rather than a scan of `nodes`, so a missing-node
// check is never O(n). A successor *function* (`node => Iterable[node]`) is deliberately not
// `Nodal`: it has no finite node set and no decidable `has`; `explore` materialises one.
object Nodal:
  // `Map` and `Ledger` belong to proscenium, so their instances anchor here, in the typeclass's
  // companion. A target absent from the key set is not a node: the instance reports the map as
  // it is, and `Digraph(adjacency)` is the constructor that closes the node set over targets.
  // Subtype-parametric, as murmuration's instances are: an exact `Map[node, Set[node]]` fails to
  // match when a nested summon sees the alias dealiased.
  given map: [node, map <: Map[node, Set[node]]] => (map is Nodal by node) = new Nodal:
    type Self = map
    type Operand = node
    def nodes(self: map): Iterator[node] = Set.iterator(Map.keys(self))
    def has(self: map, node: node): Boolean = Map.defines(self, node)

    def successors(self: map, node: node): Iterator[node] =
      Map.read(self, node) match
        case Some(targets) => Set.iterator(targets)
        case None          => Iterator.empty

  given ledger: [node, ledger <: Ledger[node, Set[node]]] => (ledger is Nodal by node) = new Nodal:
    type Self = ledger
    type Operand = node
    def nodes(self: ledger): Iterator[node] = List.iterator(Ledger.keys(self))
    def has(self: ledger, node: node): Boolean = Ledger.defines(self, node)

    def successors(self: ledger, node: node): Iterator[node] =
      Ledger.read(self, node) match
        case Some(targets) => Set.iterator(targets)
        case None          => Iterator.empty

trait Nodal extends Typeclass.Pure, Operable:
  def nodes(self: Self): Iterator[Operand]
  def successors(self: Self, node: Operand): Iterator[Operand]
  def has(self: Self, node: Operand): Boolean
