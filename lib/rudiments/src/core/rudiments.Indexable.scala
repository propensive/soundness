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
┃    Soundness, version 0.38.0.                                                                    ┃
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
package rudiments

import language.experimental.pureFunctions

import scala.collection.mutable as scm

import anticipation.*
import denominative.*
import prepositional.*

object Indexable:
  given iarray: [element] => IArray[element] is Indexable by Ordinal to element =
    new Indexable:
      type Self = IArray[element]
      type Operand = Ordinal
      type Result = element

      def contains(array: IArray[element], index: Ordinal): Boolean =
        index.n0 >= 0 && index.n0 <= Ult.of(array).n0

      def access(array: IArray[element], index: Ordinal): Result = array(index.n0)

  given seq: [element] => IndexedSeq[element] is Indexable by Ordinal to element =
    new Indexable:
      type Self = IndexedSeq[element]
      type Operand = Ordinal
      type Result = element

      def contains(seq: IndexedSeq[element], index: Ordinal): Boolean =
        index.n0 >= 0 && index.n0 <= Ult.of(seq).n0

      def access(seq: IndexedSeq[element], index: Ordinal): Result = seq(index.n0)

  given text: [element] => Text is Indexable by Ordinal to Char = new Indexable:
    type Self = Text
    type Operand = Ordinal
    type Result = Char

    def contains(text: Text, index: Ordinal): Boolean =
      index.n0 >= 0 && index.n0 < text.s.length

    def access(text: Text, index: Ordinal): Result = text.s.charAt(index.n0)

  given map: [key, value] => Map[key, value] is Indexable by key to value =
    new Indexable:
      type Self = Map[key, value]
      type Operand = key
      type Result = value

      def contains(value: Self, index: key): Boolean = value.contains(index)
      def access(value: Self, index: key): value = value(index)

  given bijection: [key, value] => Bijection[key, value] is Indexable by key to value =
    new Indexable:
      type Self = Bijection[key, value]
      type Operand = key
      type Result = value

      def contains(value: Self, index: key): Boolean = value.map.contains(index)
      def access(value: Self, index: key): value = value.map(index)

  given hashMap: [key, value] => scm.HashMap[key, value] is Indexable by key to value =
    new Indexable:
      type Self = scm.HashMap[key, value]
      type Operand = key
      type Result = value

      def contains(value: Self, index: key): Boolean = value.contains(index)
      def access(value: Self, index: key): value = value(index)


trait Indexable extends Typeclass, Operable, Resultant:
  def contains(value: Self, index: Operand): Boolean
  def access(value: Self, index: Operand): Result
