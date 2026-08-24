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
package ulysses

import scala.collection.immutable as sci
import scala.collection.mutable as scm

import cardinality.*
import gastronomy.*
import hypotenuse.*
import prepositional.*
import rudiments.*
import vacuous.*

object BloomFilter:
  def apply[element: Digestible](approximateSize: Int, targetErrorRate: 0.0 ~ 1.0)
    [ algorithm <: Algorithm ]
    ( using hash0: Hash in algorithm, erased weakness: Permit[HashWeakness[algorithm]] )
  :   BloomFilter[element, algorithm] =

    val bitSize: Int = (-1.44*approximateSize*ln(targetErrorRate.double).double).toInt
    val hashCount: Int = ((bitSize.toDouble/approximateSize.toDouble)*ln(2.0).double + 0.5).toInt
    new BloomFilter(bitSize, hashCount, sci.BitSet())


case class BloomFilter[element: Digestible, algorithm <: Algorithm]
  ( bitSize: Int, hashCount: Int, bits: sci.BitSet )
  ( using hash0: Hash in algorithm, erased weakness: Permit[HashWeakness[algorithm]] ):

  private val requiredEntropyBits = ln(bitSize ** hashCount).double.toInt + 1

  private def hash(value: element): BigInt =
    def recur(count: Int = 0, data: List[Array[Byte]^{}] = Nil): BigInt =
      if data.stdlib.map(_.length).sum*8 < requiredEntropyBits
      then recur(count + 1, (count, value).digest[algorithm].data :: data)
      else
        // A manual concatenation into a fresh exclusive array: `toArray` yields a read-only
        // array, which `BigInt`'s pure formal rejects.
        val whole = Array.allocate[Byte](data.stdlib.map(_.length).sum)
        var offset = 0
        var rest = data.stdlib

        while rest.nonEmpty do
          val chunk = rest.head
          whole.copyFrom(chunk, 0, offset, chunk.length)
          offset += chunk.length
          rest = rest.tail

        // Via `java.math.BigInteger`: the Java constructor accepts the array where
        // `BigInt.apply`'s pure Scala formal does not.
        BigInt(java.math.BigInteger(whole.raw)).abs

    recur()

  private def additions(value: element, bitSet: scm.BitSet): Unit =
    @tailrec
    def recur(hash: BigInt, count: Int): Unit =
      if count < hashCount then
        bitSet((hash%bitSize).toInt) = true
        recur(hash/bitSize, count + 1)

    recur(hash(value), 0)

  @targetName("add")
  infix def + (value: element): BloomFilter[element, algorithm] =
    val bitSet = scm.BitSet()
    additions(value, bitSet)
    BloomFilter(bitSize, hashCount, bits | bitSet)

  @targetName("addAll")
  infix def ++ (elements: Iterable[element]): BloomFilter[element, algorithm] =
    val bitSet = scm.BitSet()
    elements.each(additions(_, bitSet))
    BloomFilter(bitSize, hashCount, bits | bitSet)

  def hits(value: element): Boolean =
    val bitSet = scm.BitSet()
    additions(value, bitSet)
    bitSet.subsetOf(bits)
