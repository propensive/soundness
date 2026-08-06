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
package metamorphose

import proscenium.compat.*

import scala.annotation.*
import scala.collection.mutable.BitSet

import anticipation.*
import contingency.*
import denominative.*
import rudiments.*
import vacuous.*

object Permutation:
  def bySize(n: Int): Chain[Permutation] = Chain.range[BigInt](0, Factorial(n)).map: i =>
    Permutation(Factoradic(i))

  def apply(sequence: Sequence[Int]): Permutation raises PermutationError =
    val elements = sequence.stdlib
    val array: scala.Array[Int]^ = new scala.Array(elements.length)
    val seen: BitSet = BitSet()
    var index = 0

    while index < elements.length do
      val element = elements(index)
      array(index) = element - seen.count(_ < element)

      if element >= elements.length || element < 0
      then
        raise
          ( PermutationError(PermutationError.Reason.InvalidIndex(element, elements.length - 1)) )

      if seen.has(element)
      then raise(PermutationError(PermutationError.Reason.DuplicateIndex(element, index)))

      seen(element) = true
      index += 1

    Permutation(Factoradic(array.iterator.to(List)))

case class Permutation(factoradic: Factoradic):
  lazy val lehmer: List[Int] = factoradic.expand
  lazy val expansion: List[Int] = unsafely(apply[Int](List.range(0, lehmer.length)))

  def bytes: Data = unsafely(factoradic.number.toByteArray.immutable)
  def apply(n: Int): Int =
    // A permutation fixes every point outside its domain; `List` positional access is
    // O(n), accepted here explicitly through the asymptotic gate.
    import denominative.asymptotics.linearAccessComplexity
    expansion(Ordinal.zerary(n)).or(n)

  def apply[element](sequence: List[element]): List[element] raises PermutationError =
    if sequence.length < lehmer.length then
      raise(PermutationError(PermutationError.Reason.TooShort(sequence.length, lehmer.length)))


    def recur
      ( lehmer:  List[Int],
        prefix:  List[element],
        list:    List[element],
        current: Int,
        result:  List[element] )
    :   List[element] =

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

  def inverse: Permutation = if lehmer.nil then this else
    val length = lehmer.length
    val array: scala.Array[Int]^ = new scala.Array(lehmer.length)
    var index = 0
    var sequence: List[Int] = expansion

    while sequence match
        case head :: tail => array(head) = index
                             index += 1
                             sequence = tail
                             true
        case Nil          => false
    do ()

    unsafely(Permutation(Sequence.from(array.iterator)))
