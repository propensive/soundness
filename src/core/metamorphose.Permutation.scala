/*
    Metamorphose, version 0.24.0. Copyright 2025 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package metamorphose

import scala.collection.mutable.BitSet
import scala.annotation.*

import anticipation.*
import contingency.*
import rudiments.*

object Permutation:

  def bySize(n: Int): LazyList[Permutation] = LazyList.range[BigInt](0, Factorial(n)).map: i =>
    Permutation(Factoradic(i))

  def apply(sequence: IndexedSeq[Int]): Permutation raises PermutationError =
    val array: Array[Int] = new Array(sequence.length)
    val seen: BitSet = BitSet()

    for index <- sequence.indices do
      val element = sequence(index)
      array(index) = element - seen.count(_ < element)

      if element >= sequence.length || element < 0
      then
        raise
         (PermutationError(PermutationError.Reason.InvalidIndex(element, sequence.length - 1)))

      if seen.contains(element)
      then raise(PermutationError(PermutationError.Reason.DuplicateIndex(element, index)))

      seen(element) = true

    Permutation(Factoradic(array.to(List)))

case class Permutation(factoradic: Factoradic):
  lazy val lehmer: List[Int] = factoradic.expand
  lazy val expansion: List[Int] = unsafely(apply[Int](List.range(0, lehmer.length)))

  def bytes: Bytes = unsafely(factoradic.number.toByteArray.immutable)

  def apply(n: Int): Int = expansion(n)

  def apply[ElementType](sequence: List[ElementType]): List[ElementType] raises PermutationError =
    if sequence.length < lehmer.length then
      raise(PermutationError(PermutationError.Reason.TooShort(sequence.length, lehmer.length)))

    def recur
       (lehmer:  List[Int],
        prefix:  List[ElementType],
        list:    List[ElementType],
        current: Int,
        result:  List[ElementType])
            : List[ElementType] =

      lehmer match
        case head :: tail =>
          if current == head
          then recur(tail, prefix, list.tail, current, list.head :: result)
          else
            if current < head
            then recur(lehmer, list.head :: prefix, list.tail, current + 1, result)
            else recur(lehmer, prefix.tail, prefix.head :: list, current - 1, result)

        case Nil =>
          result.reverse

    val prefix = sequence.length - lehmer.length
    sequence.take(prefix) ::: recur(lehmer, Nil, sequence.drop(prefix), 0, Nil)

  def inverse: Permutation = if lehmer.isEmpty then this else
    val length = lehmer.length
    val array: Array[Int] = new Array(lehmer.length)

    def recur(index: Int, sequence: List[Int]): Permutation = sequence match
      case head :: tail =>
        array(head) = index
        recur(index + 1, tail)

      case Nil =>
        unsafely(Permutation(IArray.from(array)))

    recur(0, expansion)
