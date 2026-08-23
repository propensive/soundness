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
package denominative

import scala.collection.immutable.IndexedSeq

import anticipation.*
import prepositional.*

// The final element (`last`) and everything before it (`lead`) — the back-end counterparts of
// `head` and `tail`. They are two typeclasses rather than one because their costs differ per
// container: reading the last element of an array is O(1) while dropping it rebuilds the whole
// array. Both are gated exactly like `Countable`, and for the same reason: an operation whose
// name sounds like a peek should announce it when it is not.
object Terminable:
  // O(1): the underlying `Vector` is indexed.
  given sequence: [element, seq <: Sequence[element]] => seq is Terminable by element =
    _.stdlib.last

  // O(1): the last slot is read directly.
  given frozenArray: [element] => (Array[element]^{}) is Terminable by element =
    array => array.readUnchecked(array.length - 1)

  given text: Text is Terminable by Char = text => text.s.charAt(text.s.length - 1)

  given indexedSeq: [element] => IndexedSeq[element] is Terminable by element = _.last

  // Walking to the end of a strict linked structure is O(n).
  given list: [element, list <: List[element]] => (complexity: LinearSizeComplexity)
  =>  list is Terminable by element =
    _.stdlib.last

  // Forcing a lazy structure to its end diverges on an infinite one: unbounded, not merely
  // linear — the same gate `Chain.size` demands.
  given lazyList: [element, chain <: Chain[element]] => (complexity: UnboundedSizeComplexity)
  =>  chain is Terminable by element =
    _.stdlib.last

trait Terminable extends Typeclass.Pure, Operable:
  def last(value: Self): Operand

// `lead` is `last`'s complement: `lead` followed by `last` is the whole container. The result
// is `Resultant` rather than `Self` because dropping the last element can empty the container,
// so a `Populated` receiver must not carry its proof through.
object Truncable:
  // O(1) amortised: `Vector` drops from either end cheaply.
  given sequence: [element, seq <: Sequence[element]] => seq is Truncable to Sequence[element] =
    value => Sequence.of(value.stdlib.init)

  given text: [text <: Text] => text is Truncable to Text =
    text => text.s.substring(0, text.s.length - 1).nn.tt

  given indexedSeq: [element, seq <: IndexedSeq[element]] => seq is Truncable to IndexedSeq[element] =
    _.init

  // Dropping the last element copies the whole spine.
  given list: [element, list <: List[element]] => (complexity: LinearSizeComplexity)
  =>  list is Truncable to List[element] =
    value => List.of(value.stdlib.init)

  // The rebuilt array is fresh, so freezing it is discharged by construction.
  given frozenArray: [element: scala.reflect.ClassTag, array <: (Array[element]^{})]
  =>  (complexity: LinearSizeComplexity)
  =>  array is Truncable to (Array[element]^{}) =
    value => Array.frozen(value.readable.init)

  given lazyList: [element, chain <: Chain[element]] => (complexity: UnboundedSizeComplexity)
  =>  chain is Truncable to Chain[element] =
    value => Chain.of(value.stdlib.init)

trait Truncable extends Typeclass.Pure, Resultant:
  def lead(value: Self): Result
