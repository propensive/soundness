/*
    Ulysses, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package ulysses

import gastronomy.*
import cardinality.*
import rudiments.*
import vacuous.*

import scala.collection.immutable as sci
import scala.collection.mutable as scm

object BloomFilter:
  def apply[ElementType: Digestible]
      (approximateSize: Int, targetErrorRate: 0.0 ~ 1.0)
      [HashType <: HashScheme[?]: HashFunction]
      : BloomFilter[ElementType, HashType] =
    val bitSize: Int = (-1.44*approximateSize*math.log(targetErrorRate.double)).toInt
    val hashCount: Int = ((bitSize.toDouble/approximateSize.toDouble)*math.log(2.0) + 0.5).toInt
    new BloomFilter(bitSize, hashCount, sci.BitSet())

case class BloomFilter
    [ElementType: Digestible, HashType <: HashScheme[?]: HashFunction]
    (bitSize: Int, hashCount: Int, bits: sci.BitSet):
  private val requiredEntropyBits = math.log(math.pow(bitSize, hashCount)).toInt + 1
  
  private def hash(value: ElementType): BigInt =
    def recur(count: Int = 0, bytes: List[Array[Byte]] = Nil): BigInt =
      if bytes.map(_.length).sum*8 < requiredEntropyBits
      then recur(count + 1, (count, value).digest[HashType].bytes.mutable(using Unsafe) :: bytes)
      else BigInt(bytes.to(Array).flatten).abs
    
    recur()
  
  private def additions(value: ElementType, bitSet: scm.BitSet): Unit =
    @tailrec
    def recur(hash: BigInt, count: Int): Unit =
      if count < hashCount then
        bitSet((hash%bitSize).toInt) = true
        recur(hash/bitSize, count + 1)

    recur(hash(value), 0)
  
  @targetName("add")
  def +(value: ElementType): BloomFilter[ElementType, HashType] =
    val bitSet = scm.BitSet()
    additions(value, bitSet)
    BloomFilter(bitSize, hashCount, bits | bitSet)

  @targetName("addAll")
  def ++(elements: Iterable[ElementType]): BloomFilter[ElementType, HashType] =
    val bitSet = scm.BitSet()
    elements.each(additions(_, bitSet))
    BloomFilter(bitSize, hashCount, bits | bitSet)

  def mayContain(value: ElementType): Boolean =
    val bitSet = scm.BitSet()
    additions(value, bitSet)
    bitSet.subsetOf(bits)

