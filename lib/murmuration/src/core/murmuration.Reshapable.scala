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
import scala.collection.immutable.IndexedSeq
import scala.collection.mutable as scm
import scala.reflect.ClassTag

import prepositional.*

// The rebuild relation behind the transforming operations (`map`, `filter`, `flatMap`, …): from a
// source collection `Self`, with a *new* element type `Operand`, implicit search selects the
// natural `Result` shape and builds it from an iterator of elements. Result shapes are chosen per
// (source shape, new element type), which lets them be more precise than an inheritance hierarchy
// allows: a `Map` mapped to pairs stays a `Map`, while a `Map` mapped to anything else naturally
// yields a `List`.
//
// `reshape`'s parameter is a *capturing* iterator (`Iterator[Operand]^`): the iterators produced
// by `map` et al capture the caller's lambda, and a pure parameter type would reject them. Strict
// instances consume the iterator eagerly, so nothing escapes; `lazyList` is the deliberate
// exception, preserving the source's laziness.
object Reshapable extends Reshapable.Fallback:
  // Order-preserving instances additionally extend `Stable`; order-sensitive operations
  // (`sortBy`, `distinct`, `zip`) demand it, making them unavailable on unordered shapes
  // rather than silently reordering.
  trait Stable extends Reshapable

  // `Self` is subtype-parametric (as `Traversable`'s instances are) so intersections like
  // `List[T] & Populated` — from `occupied` or a branded literal — also match.
  given list: [element, element2, list <: List[element]]
  =>  list is Reshapable.Stable by element2 to List[element2] =
    elements => List.from(build(elements, sci.List.newBuilder[element2]))

  given set: [element, element2, set <: Set[element]]
  =>  set is Reshapable by element2 to Set[element2] =
    elements => Set.from(build(elements, sci.Set.newBuilder[element2]))

  // `Text` rebuilds from its own characters, so the generic operations (`keep`, `skip`, `filter`,
  // …) serve text as well as collections, with no competing text-only extension at the umbrella.
  // Rebuilding from anything else is deliberately absent: it would have no natural result shape.
  given sequence: [element, element2, sequence <: Sequence[element]]
  =>  sequence is Reshapable.Stable by element2 to Sequence[element2] =
    elements => Sequence.from(build(elements, sci.Vector.newBuilder[element2]))

  given indexedSeq: [element, element2]
  =>  IndexedSeq[element] is Reshapable.Stable by element2 to IndexedSeq[element2] =
    elements => build(elements, IndexedSeq.newBuilder[element2])

  // The lazy exception: the chain pulls from the iterator as it is forced, so the iterator
  // escapes into it — which `Chain.from`'s pure result does not record, as it never did.
  given chain: [element, element2]
  =>  Chain[element] is Reshapable.Stable by element2 to Chain[element2] =
    elements => elements.chain

  // The frozen array reshapes to a frozen array: the rebuilt array is fresh, so freezing
  // it is discharged by construction.
  given frozenArray: [element, element2: ClassTag]
  =>  (Array[element]^{}) is Reshapable.Stable by element2 to (Array[element2]^{}) =
    elements => Array.unsafeFrozen(build(elements, sci.List.newBuilder[element2]).toArray)

  // A `Map` rebuilt from pairs remains a `Map`…
  given map: [key, value, key2, value2]
  =>  Map[key, value] is Reshapable by (key2, value2) to Map[key2, value2] =
    elements => Map.from(build(elements, sci.Map.newBuilder[key2, value2]))

  // A `Ledger` rebuilt from pairs remains a `Ledger`, and legitimately `Stable`: insertion
  // order is its identity, so it carries the whole order-sensitive surface (`sort`, `distinct`,
  // `trace`, `zip`).
  given ledger: [key, value, key2, value2]
  =>  Ledger[key, value] is Reshapable.Stable by (key2, value2) to Ledger[key2, value2] =
    elements => Ledger.from(build(elements, sci.VectorMap.newBuilder[key2, value2]))

  trait Fallback extends Fallback2:
    // A `Stable` pair-rebuild of a `Map` yields a `Ledger`: the unordered `Map` shape cannot
    // honestly receive an order-sensitive result, but the insertion-ordered map can — so
    // `map.order(…)` is an ordered map iterating in sorted order. Lower priority than `map`
    // above, so plain reshapes (`filter`, pairwise `bind`) still rebuild the cheaper `Map`.
    given mapToLedger: [key, value, key2, value2]
    =>  Map[key, value] is Reshapable.Stable by (key2, value2) to Ledger[key2, value2] =
      elements => Ledger.from(build(elements, sci.VectorMap.newBuilder[key2, value2]))

    // A `Ledger` rebuilt from non-pair elements naturally yields a `List`, order preserved.
    given ledgerToList: [key, value, element2]
    =>  Ledger[key, value] is Reshapable.Stable by element2 to List[element2] =
      elements => List.from(build(elements, sci.List.newBuilder[element2]))

  trait Fallback2:
    // …and a `Map` rebuilt from non-pair elements likewise yields a `List` — a more precise
    // result than the stdlib's `Iterable`. Lowest priority, so pair results prefer the `Map`
    // (plain) or `Ledger` (`Stable`) shapes above.
    given mapToList: [key, value, element2]
    =>  Map[key, value] is Reshapable.Stable by element2 to List[element2] =
      elements => List.from(build(elements, sci.List.newBuilder[element2]))

  // The strict rebuild: drain a borrowed iterator into a stdlib builder. Public only because the
  // instances above are lambdas in a companion; it is the one place the layer meets a builder.
  def build[element, result](elements: Iterator[element]^, builder: scm.Builder[element, result])
  :   result =

    elements.each(builder += _)
    builder.result()

trait Reshapable extends Typeclass.Pure, Operable, Resultant:
  def reshape(elements: Iterator[Operand]^): Result
