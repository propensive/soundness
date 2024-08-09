/*
    Rudiments, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

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

import anticipation.*
import symbolism.*

object Indexable:
  given [ElementType] => IArray[ElementType] is Indexable by Int into ElementType as iarray =
    new Indexable:
      type Self = IArray[ElementType]
      type Operand = Int
      type Result = ElementType

      def contains(array: IArray[ElementType], index: Int): Boolean =
        index >= 0 && index < array.length

      def access(array: IArray[ElementType], index: Int): Result = array(index)

  given [ElementType] => IndexedSeq[ElementType] is Indexable by Int into ElementType as indexedSeq =
    new Indexable:
      type Self = IndexedSeq[ElementType]
      type Operand = Int
      type Result = ElementType

      def contains(seq: IndexedSeq[ElementType], index: Int): Boolean =
        index >= 0 && index < seq.length

      def access(seq: IndexedSeq[ElementType], index: Int): Result = seq(index)

  given [ElementType] => Text is Indexable by Int into Char =
    new Indexable:
      type Self = Text
      type Operand = Int
      type Result = Char

      def contains(text: Text, index: Int): Boolean = index >= 0 && index < text.s.length
      def access(text: Text, index: Int): Result = text.s.charAt(index)

  given [KeyType, ValueType] => Map[KeyType, ValueType] is Indexable by KeyType into ValueType =
    new Indexable:
      type Self = Map[KeyType, ValueType]
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
