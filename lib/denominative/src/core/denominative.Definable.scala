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

import scala.language.experimental.pureFunctions

import anticipation.*
import prepositional.*

// The write-twin of `Applicable`: `define(index, value)` produces a copy of the container in
// which `at(index)` yields `value` — positionally for `Ordinal`-indexed containers, by key for
// maps. Total by construction: an out-of-range ordinal returns the container unchanged, and an
// absent key defines a new entry.
object Definable:
  given sequence: [element] => Sequence[element] is Definable:
    type Self = Sequence[element]
    type Operand = Ordinal
    type Result = element

    def define(sequence: Self, index: Ordinal, value: element): Self =
      if index.n0 >= 0 && index.n0 < sequence.stdlib.length
      then Sequence.of(sequence.stdlib.updated(index.n0, value))
      else sequence

  // The rebuilt array is fresh, so freezing it is discharged by construction; the `ClassTag`
  // is captured at the instance (as `Segmentable.iarray` does), not per call.
  given frozenArray: [element: scala.reflect.ClassTag] => (Array[element]^{}) is Definable:
    type Self = Array[element]^{}
    type Operand = Ordinal
    type Result = element

    def define(array: Self, index: Ordinal, value: element): Self =
      if index.n0 >= 0 && index.n0 < array.length then
        val copy = new scala.Array[element](array.length)
        var source = 0

        while source < array.length do
          copy(source) = array.readUnchecked(source)
          source += 1

        copy(index.n0) = value
        Array.unsafeFrozen(copy)
      else array

  given indexedSeq: [element] => IndexedSeq[element] is Definable:
    type Self = IndexedSeq[element]
    type Operand = Ordinal
    type Result = element

    def define(sequence: Self, index: Ordinal, value: element): Self =
      if index.n0 >= 0 && index.n0 < sequence.length then sequence.updated(index.n0, value)
      else sequence

  given map: [key, value] => Map[key, value] is Definable:
    type Self = Map[key, value]
    type Operand = key
    type Result = value

    def define(map: Self, index: key, value: value): Self =
      Map.of(map.stdlib.updated(index, value))

  given ledger: [key, value] => Ledger[key, value] is Definable:
    type Self = Ledger[key, value]
    type Operand = key
    type Result = value

    def define(ledger: Self, index: key, value: value): Self =
      Ledger.of(ledger.stdlib.updated(index, value))

trait Definable extends Typeclass.Pure, Operable, Resultant:
  def define(value: Self, index: Operand, result: Result): Self

// The inverse of `define`, and a separate typeclass rather than a method on `Definable`
// because only key-addressed containers can implement it: deleting at an `Ordinal` would shift
// every later index, so the positional containers have no instance at all, and `omit` is simply
// unavailable on them.
object Omissible:
  given map: [key, value] => (Map[key, value] is Omissible { type Operand = key }) =
    new Omissible:
      type Self = Map[key, value]
      type Operand = key

      def omit(map: Self, index: key): Self = Map.of(map.stdlib.removed(index))

  given ledger: [key, value] => (Ledger[key, value] is Omissible { type Operand = key }) =
    new Omissible:
      type Self = Ledger[key, value]
      type Operand = key

      def omit(ledger: Self, index: key): Self = Ledger.of(ledger.stdlib.removed(index))

trait Omissible extends Typeclass.Pure, Operable:
  def omit(value: Self, index: Operand): Self
