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
package denominative

import scala.collection.immutable.IndexedSeq

import scala.language.experimental.pureFunctions

import scala.collection.mutable as scm

import anticipation.*
import prepositional.*

object Indexable:
  given iarray: [element] => IArray[element] is Indexable:
    type Self = IArray[element]
    type Operand = Ordinal
    type Result = element

    def contains(array: IArray[element], index: Ordinal): Boolean =
      index.n0 >= 0 && index.n0 < array.length

    def access(array: IArray[element], index: Ordinal): Result = array(index.n0)

  given sequence: [element] => IndexedSeq[element] is Indexable:
    type Self = IndexedSeq[element]
    type Operand = Ordinal
    type Result = element

    def contains(sequence: IndexedSeq[element], index: Ordinal): Boolean =
      index.n0 >= 0 && index.n0 < sequence.length

    def access(sequence: IndexedSeq[element], index: Ordinal): Result = sequence(index.n0)

  // Opaque `Series` is no longer an `IndexedSeq` subtype, so it needs its own instance.
  given series: [element] => Series[element] is Indexable:
    type Self = Series[element]
    type Operand = Ordinal
    type Result = element

    def contains(series: Series[element], index: Ordinal): Boolean =
      index.n0 >= 0 && index.n0 < series.stdlib.length

    def access(series: Series[element], index: Ordinal): Result = series.stdlib(index.n0)

  // Opaque `List`: positional access is O(n), so the instance is gated behind `LinearAccessComplexity`.
  given list: [element] => (complexity: LinearAccessComplexity) => List[element] is Indexable:
    type Self = List[element]
    type Operand = Ordinal
    type Result = element

    def contains(list: List[element], index: Ordinal): Boolean =
      index.n0 >= 0 && index.n0 < list.stdlib.length

    def access(list: List[element], index: Ordinal): Result = list.stdlib(index.n0)

  given text: [element] => Text is Indexable:
    type Self = Text
    type Operand = Ordinal
    type Result = Char

    def contains(text: Text, index: Ordinal): Boolean = index.n0 >= 0 && index.n0 < text.s.length
    def access(text: Text, index: Ordinal): Result = text.s.charAt(index.n0)

  given map: [key, value] => Map[key, value] is Indexable:
    type Self = Map[key, value]
    type Operand = key
    type Result = value

    def contains(value: Self, index: key): Boolean = value.stdlib.contains(index)
    def access(value: Self, index: key): value = value.stdlib(index)

  given hashMap: [key, value] => scm.HashMap[key, value] is Indexable:
    type Self = scm.HashMap[key, value]
    type Operand = key
    type Result = value

    def contains(value: Self, index: key): Boolean = value.contains(index)
    def access(value: Self, index: key): value = value(index)


trait Indexable extends Typeclass.Pure, Operable, Resultant:
  def contains(value: Self, index: Operand): Boolean
  def access(value: Self, index: Operand): Result
