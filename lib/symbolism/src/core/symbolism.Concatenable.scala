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
package symbolism

import scala.reflect.ClassTag

import prepositional.*

// The opaque collections concatenate with `+`, through `Addable`'s `concatenable` bridge:
// these instances are what retire the stdlib spellings (`:::`, `++`). They live here in the
// typeclass companion because proscenium sits below symbolism, so the collections' own
// companions cannot host them.
object Concatenable:
  // `Self` AND `Operand` are subtype-parametric so branded values (`List[T] & Populated`, from
  // `occupied` or a non-empty literal) match on either side; the declared `Result` is the
  // PLAIN type, so no union or brand leaks downstream. (Only a branded left operand would
  // prove the result non-empty; asserting that stays out of scope.)
  given list: [element, list <: List[element], operand <: List[element]]
  =>  list is Concatenable by operand to List[element] =
    (left, right) => List.concat(left, right)

  given sequence: [element, sequence <: Sequence[element], operand <: Sequence[element]]
  =>  sequence is Concatenable by operand to Sequence[element] =
    (left, right) => Sequence.concat(left, right)

  // Lazily: neither side is forced by the concatenation itself.
  given chain: [element] => Chain[element] is Concatenable by Chain[element] to Chain[element] =
    (left, right) => Chain.concat(left, right)

  given set: [element] => Set[element] is Concatenable by Set[element] to Set[element] =
    (left, right) => Set.concat(left, right)

  // Right-biased, matching the stdlib's `concat`: keys in the right operand win. This is the
  // same ruling as `Set`, where union is concatenation.
  given map: [key, value] => Map[key, value] is Concatenable by Map[key, value] to Map[key, value] =
    (left, right) => Map.concat(left, right)

  given ledger: [key, value]
  =>  Ledger[key, value] is Concatenable by Ledger[key, value] to Ledger[key, value] =
    (left, right) => Ledger.concat(left, right)

  given frozenArray: [element: ClassTag]
  =>  (Array[element]^{}) is Concatenable by (Array[element]^{}) to (Array[element]^{}) =
    (left, right) => Array.frozen(left.readable ++ right.readable)

trait Concatenable extends Typeclass.Pure, Operable, Resultant:
  def concat(left: Self, right: Operand): Result
