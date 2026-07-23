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

import anticipation.*
import prepositional.*

// A type whose values can be tested for membership of a value (the queried type
// is its `Operand`, bound with `by` — e.g. `List[Int] is Inclusive by Int`).
// Backs the `collection.has(value)` extension. Distinct from `Indexable`, whose
// `defines` answers whether a *key/index* is present rather than a *value*.
// The single `Iterable` instance fixes the queried type to the collection's
// *exact* element type via the `Element` match type, rather than a bounded
// `collection <: Iterable[element]` whose covariance would widen the element to
// `Matchable` — silently accepting `map.has(key)` (a key, not a value) or
// `list.has(wrongType)`. With the exact element, `Map`'s element is a key/value
// pair, so `map.has(key)` is a compile error; key membership is `Indexable`'s
// `defines`.
object Inclusive extends Inclusive.Fallback:
  type Element[collection] = collection match
    case Iterable[element] => element

  // The blanket `Iterable` instance lives in a lower-priority parent: for an *unrefined*
  // `X is Inclusive` query the compiler otherwise reports it ambiguous with the per-alias
  // instances below, without ever reducing `Element` to discover the mismatch.
  trait Fallback:
    given iterable: [collection <: Iterable[?]] => collection is Inclusive by Element[collection] =
      (collection, value) => collection.exists(_ == value)

  given iarray: [element <: Matchable] => IArray[element] is Inclusive by element =
    (iarray, value) => iarray.exists(_ == value)

  // Opaque `Series` is no longer an `Iterable` subtype, so it needs its own instance.
  given series: [element] => Series[element] is Inclusive by element =
    (series, value) => series.stdlib.exists(_ == value)

  // Opaque `Set` likewise.
  given set: [element] => Set[element] is Inclusive by element =
    (set, value) => set.stdlib.contains(value)

  // Opaque `List` likewise (membership is a single linear pass, so ungated).
  given list: [element] => List[element] is Inclusive by element =
    (list, value) => list.stdlib.contains(value)

  given array: [element <: Matchable] => Array[element] is Inclusive by element =
    (array, value) => array.exists(_ == value)

  // `Text` (opaque over `String`) is not an `Iterable`, so it needs its own
  // instance for `text.has(char)`; substring containment is `subsumes` instead.
  given text: Text is Inclusive by Char = (text, char) => text.s.indexOf(char.toInt) >= 0

trait Inclusive extends Typeclass.Pure, Operable:
  def has(self: Self, value: Operand): Boolean
