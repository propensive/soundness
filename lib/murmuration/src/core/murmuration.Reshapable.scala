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

import scala.collection.immutable.IndexedSeq

import scala.reflect.ClassTag

import anticipation.*
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

  given list: [element, element2]
  =>  List[element] is Reshapable.Stable by element2 to List[element2] =
    List.from(_)

  given set: [element, element2] => Set[element] is Reshapable by element2 to Set[element2] =
    Set.from(_)

  // `Text` rebuilds from its own characters, so the generic operations (`keep`, `skip`, `filter`,
  // …) serve text as well as collections, with no competing text-only extension at the umbrella.
  // Rebuilding from anything else is deliberately absent: it would have no natural result shape.
  given text: [text <: Text] => text is Reshapable.Stable by Char to Text =
    chars => Text(String(chars.toArray))

  given sequence: [element, element2]
  =>  Sequence[element] is Reshapable.Stable by element2 to Sequence[element2] =
    Sequence.from(_)

  given indexedSeq: [element, element2]
  =>  IndexedSeq[element] is Reshapable.Stable by element2 to IndexedSeq[element2] =
    IndexedSeq.from(_)

  given lazyList: [element, element2]
  =>  Chain[element] is Reshapable.Stable by element2 to Chain[element2] =
    Chain.from(_)

  // The frozen array reshapes to a frozen array: the rebuilt array is fresh, so freezing
  // it is discharged by construction.
  given frozenArray: [element, element2: ClassTag]
  =>  (Array[element]^{}) is Reshapable.Stable by element2 to (Array[element2]^{}) =
    elements => Array.unsafeFrozen(elements.toArray)

  // A `Map` rebuilt from pairs remains a `Map`…
  given map: [key, value, key2, value2]
  =>  Map[key, value] is Reshapable by (key2, value2) to Map[key2, value2] =
    Map.from(_)

  trait Fallback:
    // …but a `Map` rebuilt from non-pair elements naturally yields a `List` — a more precise
    // result than the stdlib's `Iterable`. Lower priority than `map` above (companion-parent
    // placement), so pair results still prefer the `Map` shape.
    given mapToList: [key, value, element2]
    =>  Map[key, value] is Reshapable.Stable by element2 to List[element2] =
      List.from(_)

trait Reshapable extends Typeclass.Pure, Operable, Resultant:
  def reshape(elements: Iterator[Operand]^): Result
