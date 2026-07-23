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
package murmuration

import scala.collection.immutable as sci

import prepositional.*

// The shape-preserving `map`, pulled out of `Traversable` so a `Map`'s `map` maps its *values*
// (keys structural) rather than iterating `(key, value)` pairs. `Operand` is the element the lambda
// receives (the value for a `Map`); `Result[_]` is the mapped-container constructor. Each instance
// is keyed on the container alone — `Operand` and `Result[_]` are fixed by the shape, independent of
// the new element type — so a `Self`-keyed summon determines both before the lambda is elaborated
// (`xs.map(_.field)` works). `map` (below, in `rudiments_core`) binds `Result[_]` as an HK type
// *parameter* matched from this refinement, so its return type is a plain application, never a path-
// dependent projection: that is what survives the cross-package export forwarder (#1411). Not
// `Resultant`: its `Result` has kind `*`, but here `Result` must be `* -> *`.
// The instances are subtype-parametric (`container <: List[element]`, not an exact `List[element]`),
// mirroring the `Traversable` givens: this is what matches both `List[e] & Populated` receivers and
// the `soundness.*` re-export aliases (which are *distinct* opaque types from the `proscenium`
// originals) — an exact match would let the `Iterable` fallback win for them (wrong shape for `Set`,
// pair operand for `Map`).
object Mappable extends Mappable.Fallback:
  given list: [element, container <: List[element]]
  =>  (container is Mappable { type Operand = element; type Result[element2] = List[element2] }) =
    new Mappable:
      type Self = container
      type Operand = element
      type Result[element2] = List[element2]
      def map[element2](self: container, lambda: element => element2): List[element2] =
        List.of(self.stdlib.map(lambda))

  given set: [element, container <: Set[element]]
  =>  (container is Mappable { type Operand = element; type Result[element2] = Set[element2] }) =
    new Mappable:
      type Self = container
      type Operand = element
      type Result[element2] = Set[element2]
      def map[element2](self: container, lambda: element => element2): Set[element2] =
        Set.of(self.stdlib.map(lambda))

  given series: [element, container <: Series[element]]
  =>  (container is Mappable { type Operand = element; type Result[element2] = Series[element2] }) =
    new Mappable:
      type Self = container
      type Operand = element
      type Result[element2] = Series[element2]
      def map[element2](self: container, lambda: element => element2): Series[element2] =
        Series.of(self.stdlib.map(lambda))

  given progression: [element, container <: Progression[element]]
  =>  (container is Mappable
         { type Operand = element; type Result[element2] = Progression[element2] }) =
    new Mappable:
      type Self = container
      type Operand = element
      type Result[element2] = Progression[element2]
      def map[element2](self: container, lambda: element => element2): Progression[element2] =
        Progression.of(self.stdlib.map(lambda))

  // A `Map` maps its *values*, preserving keys: `Operand` is the value type; `Result` re-
  // parameterizes the value, with `key` fixed by the receiver.
  given map: [key, value, container <: Map[key, value]]
  =>  (container is Mappable { type Operand = value; type Result[value2] = Map[key, value2] }) =
    new Mappable:
      type Self = container
      type Operand = value
      type Result[value2] = Map[key, value2]
      def map[value2](self: container, lambda: value => value2): Map[key, value2] =
        Map.of(self.stdlib.view.mapValues(lambda).toMap)

  trait Fallback:
    // Any raw `Iterable` (stdlib collections, ranges, …) maps to a `List`, as the old umbrella `map`
    // did. Lower priority than the alias instances above (companion-parent placement).
    given iterable: [element, collection <: Iterable[element]]
    =>  (collection is Mappable { type Operand = element; type Result[element2] = List[element2] }) =
      new Mappable:
        type Self = collection
        type Operand = element
        type Result[element2] = List[element2]
        def map[element2](self: collection, lambda: element => element2): List[element2] =
          List.of(self.iterator.map(lambda).to(sci.List))

trait Mappable extends Typeclass.Pure, Operable:
  type Result[_]
  def map[element2](self: Self, lambda: Operand => element2): Result[element2]
