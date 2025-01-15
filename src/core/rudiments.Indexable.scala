/*
    Rudiments, version 0.26.0. Copyright 2025 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package rudiments

import language.experimental.pureFunctions

import scala.collection.mutable as scm

import anticipation.*
import denominative.*
import prepositional.*

object Indexable:
  given [ElementType] => IArray[ElementType] is Indexable by Ordinal into ElementType as iarray =
    new Indexable:
      type Self = IArray[ElementType]
      type Operand = Ordinal
      type Result = ElementType

      def contains(array: IArray[ElementType], index: Ordinal): Boolean =
        index.n0 >= 0 && index.n0 <= Ult.of(array.length).n0

      def access(array: IArray[ElementType], index: Ordinal): Result = array(index.n0)

  given [ElementType] => IndexedSeq[ElementType] is Indexable by Ordinal into ElementType as seq =
    new Indexable:
      type Self = IndexedSeq[ElementType]
      type Operand = Ordinal
      type Result = ElementType

      def contains(seq: IndexedSeq[ElementType], index: Ordinal): Boolean =
        index.n0 >= 0 && index.n0 <= Ult.of(seq.length).n0

      def access(seq: IndexedSeq[ElementType], index: Ordinal): Result = seq(index.n0)

  given [ElementType] => Text is Indexable by Ordinal into Char =
    new Indexable:
      type Self = Text
      type Operand = Ordinal
      type Result = Char

      def contains(text: Text, index: Ordinal): Boolean =
        index.n0 >= 0 && index.n0 <= Ult.of(text).n0

      def access(text: Text, index: Ordinal): Result = text.s.charAt(index.n0)

  given [KeyType, ValueType]
      => Map[KeyType, ValueType] is Indexable by KeyType into ValueType as map =
    new Indexable:
      type Self = Map[KeyType, ValueType]
      type Operand = KeyType
      type Result = ValueType

      def contains(value: Self, index: KeyType): Boolean = value.contains(index)
      def access(value: Self, index: KeyType): ValueType = value(index)

  given [KeyType, ValueType]
      => Bijection[KeyType, ValueType] is Indexable by KeyType into ValueType as bijection =
    new Indexable:
      type Self = Bijection[KeyType, ValueType]
      type Operand = KeyType
      type Result = ValueType

      def contains(value: Self, index: KeyType): Boolean = value.map.contains(index)
      def access(value: Self, index: KeyType): ValueType = value.map(index)

  given [KeyType, ValueType]
      => scm.HashMap[KeyType, ValueType] is Indexable by KeyType into ValueType as hashMap =

    new Indexable:
      type Self = scm.HashMap[KeyType, ValueType]
      type Operand = KeyType
      type Result = ValueType

      def contains(value: Self, index: KeyType): Boolean = value.contains(index)
      def access(value: Self, index: KeyType): ValueType = value(index)


trait Indexable:
  type Self
  type Operand
  type Result

  def contains(value: Self, index: Operand): Boolean
  def access(value: Self, index: Operand): Result
