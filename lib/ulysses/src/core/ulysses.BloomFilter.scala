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

import scala.compiletime.asMatchable

import anticipation.*
import cardinality.*
import corpuscular.*
import gastronomy.*
import hypotenuse.*
import prepositional.*
import rudiments.*

object BloomFilter:
  private val ln2: Double = ln(2.0).double

  // No finite filter delivers a rate of zero; a request for one is sized for the smallest rate
  // a `Double` distinguishes from it below one, 2⁻⁵³, which costs 76 bits an element.
  private val minimumErrorRate: Double = 1.1102230246251565e-16

  def apply[element: Digestible](approximateSize: Int, targetErrorRate: 0.0 ~ 1.0)
    [ algorithm <: Algorithm ]
    ( using hash0: Hash in algorithm, erased weakness: Permit[HashWeakness[algorithm]] )
  :   BloomFilter[element, algorithm] =

    // The standard sizing: m = -n·ln p/(ln 2)² bits and k = (m/n)·ln 2 hashes, each at least
    // one so that a filter for no elements, or one tolerating every false positive, still works.
    val size: Int = approximateSize.max(1)
    val rate: Double = targetErrorRate.double.max(minimumErrorRate)
    val bitSize: Int = Math.ceil(-size*ln(rate).double/(ln2*ln2)).toInt.max(1)
    val hashCount: Int = Math.round(bitSize.toDouble/size*ln2).toInt.max(1)

    new BloomFilter(bitSize, hashCount, Array.fill[Long]((bitSize + 63) >>> 6)(0L))


// The bits are the words of one array, bit i being bit (i mod 64) of word (i div 64), the
// layout of `sci.BitSet#toBitMask`; its length is fixed at ⌈bitSize/64⌉, so adding elements
// copies the words exactly once however many bits it sets.
case class BloomFilter[element: Digestible, algorithm <: Algorithm]
  ( bitSize: Int, hashCount: Int, bits: Array[Long]^{} )
  ( using hash0: Hash in algorithm, erased weakness: Permit[HashWeakness[algorithm]] ):

  // The element's digest, prefixed by `count` when it is nonzero: the second and later digests
  // of an element whose algorithm yields fewer than the sixteen bytes the positions need.
  private def digest(value: element, count: Int): Data =
    val digestion = hash0.initialize()
    if count > 0 then Digestible.int.digest(digestion, count)
    element.digest(digestion, value)
    digestion.digest()

  private def word(bytes: scala.IArray[Byte], offset: Int): Long =
    @tailrec
    def recur(index: Int, result: Long): Long =
      if index == 8 then result
      else recur(index + 1, (result << 8) | (bytes(offset + index) & 0xff))

    recur(0, 0L)

  // The k positions come from two 64-bit words of the digest by double hashing (Kirsch and
  // Mitzenmacher): position i is (h₁ + i·h₂) mod m. A digest of sixteen bytes or more supplies
  // both words directly; a shorter one (the checksums) is extended by digesting the element
  // again under successive counters until sixteen bytes have been gathered. Inline, so the
  // continuation is expanded in place: no closure, and the words stay unboxed.
  private inline def positions[result](value: element)(inline fold: (Long, Long) => result)
  :   result =

    val first = digest(value, 0)

    if first.length >= 16 then fold(word(first.readable, 0), word(first.readable, 8))
    else gather(value, first.readable, 0, 1, 0, 0L, 0L)(fold)

  // Fewer than sixteen bytes: the bytes of successive digests are folded into the two words, the
  // first eight into h₁ and the next eight into h₂.
  @tailrec
  private def gather[result]
    ( value:  element,
      bytes:  scala.IArray[Byte],
      index:  Int,
      count:  Int,
      filled: Int,
      h1:     Long,
      h2:     Long )
    ( fold: (Long, Long) => result )
  :   result =

    if filled == 16 then fold(h1, h2)
    else if index == bytes.length then
      gather(value, digest(value, count).readable, 0, count + 1, filled, h1, h2)(fold)
    else
      val byte = bytes(index) & 0xff

      if filled < 8
      then gather(value, bytes, index + 1, count, filled + 1, (h1 << 8) | byte, h2)(fold)
      else gather(value, bytes, index + 1, count, filled + 1, h1, (h2 << 8) | byte)(fold)

  private inline def position(combined: Long): Int = ((combined & Long.MaxValue)%bitSize).toInt
  private inline def set(index: Int): Boolean = ((bits.readable(index >>> 6) >>> index) & 1L) != 0L
  private inline def words: scala.Array[Long] = bits.readable.asInstanceOf[scala.Array[Long]]

  private def copy(): Array[Long]^ =
    val words = Array.allocate[Long](bits.length)
    words.place(bits)
    words

  // Sets the element's positions in `words`, an exclusive copy of the bits.
  private def setting(words: Array[Long]^, value: element): Unit = positions(value): (h1, h2) =>
    @tailrec
    def recur(combined: Long, count: Int): Unit =
      if count < hashCount then
        val index = position(combined)
        words(index >>> 6) = words.readable(index >>> 6) | (1L << index)
        recur(combined + h2, count + 1)

    recur(h1, 0)

  @targetName("add")
  infix def + (value: element): BloomFilter[element, algorithm] =
    val words = copy()
    setting(words, value)
    BloomFilter(bitSize, hashCount, Array.freeze(words))

  // One pass and one copy of the bits for the whole collection, where repeated `+` would copy
  // them once per element.
  @targetName("addAll")
  infix def ++ [collection: Traversable by element](elements: collection)
  :   BloomFilter[element, algorithm] =

    val words = copy()
    elements.each(setting(words, _))
    BloomFilter(bitSize, hashCount, Array.freeze(words))

  // Allocates nothing beyond the digest, and answers `false` at the first clear position.
  def hits(value: element): Boolean = positions(value): (h1, h2) =>
    @tailrec
    def recur(combined: Long, count: Int): Boolean =
      count == hashCount || (set(position(combined)) && recur(combined + h2, count + 1))

    recur(h1, 0)

  // Two filters are equal when their bits are, which the array's own reference equality is not.
  override def equals(that: Any): Boolean = that.asMatchable match
    case that: BloomFilter[?, ?] =>
      bitSize == that.bitSize && hashCount == that.hashCount &&
        java.util.Arrays.equals(words, that.words)

    case _ =>
      false

  override def hashCode: Int =
    31*(31*bitSize + hashCount) + java.util.Arrays.hashCode(words)
