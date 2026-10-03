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

import scala.caps
import scala.compiletime.asMatchable

import anticipation.*
import cardinality.*
import corpuscular.*
import gastronomy.*
import hypotenuse.*
import prepositional.*
import rudiments.*

// A Bloom filter whose access rights are tracked by separation checking, as an `Array`'s are:
// `apply` yields an exclusive (`^`) filter, into which `add` and `addAll` set bits in place;
// `freeze` consumes it to yield the frozen form, `BloomFilter^{}`, without copying, which any
// reference can query but none can write to. The copying `+` and `++` remain for growing a
// frozen filter, at one copy of the bits each.
object BloomFilter:
  private val ln2: Double = ln(2.0).double

  // No finite filter delivers a rate of zero; a request for one is sized for the smallest rate
  // a `Double` distinguishes from it below one, 2⁻⁵³, which costs 76 bits an element.
  private val minimumErrorRate: Double = 1.1102230246251565e-16

  def apply[element: Digestible](approximateSize: Int, targetErrorRate: 0.0 ~ 1.0)
    [ algorithm <: Algorithm ]
    ( using hash0: Hash in algorithm, erased weakness: Permit[HashWeakness[algorithm]] )
  :   BloomFilter[element, algorithm]^ =

    // The standard sizing: m = -n·ln p/(ln 2)² bits and k = (m/n)·ln 2 hashes, each at least
    // one so that a filter for no elements, or one tolerating every false positive, still works.
    val size: Int = approximateSize.max(1)
    val rate: Double = targetErrorRate.double.max(minimumErrorRate)
    val bitSize: Int = Math.ceil(-size*ln(rate).double/(ln2*ln2)).toInt.max(1)
    val hashCount: Int = Math.round(bitSize.toDouble/size*ln2).toInt.max(1)

    new BloomFilter(bitSize, hashCount, Array.scratch[Long]((bitSize + 63) >>> 6))

  // Freezing consumes the filter: `consume` statically retires every writer, so the surviving
  // reference can drop its write capability without copying, exactly as `Array.freeze` does.
  def freeze[element, algorithm <: Algorithm](consume filter: BloomFilter[element, algorithm]^)
  :   BloomFilter[element, algorithm]^{} =

    caps.unsafe.unsafeAssumePure(filter)

  // Sets the k positions, (h₁ + i·h₂) mod bitSize, in a word array: the one loop behind `add`,
  // `+` and `++`, over a parameter rather than a field so that it may write into a fresh copy as
  // readily as into an exclusive filter's own words.
  @tailrec
  private def setting(words: scala.Array[Long]^, bitSize: Int, count: Int, combined: Long, h2: Long)
  :   Unit =

    if count > 0 then
      val index = ((combined & Long.MaxValue)%bitSize).toInt
      words(index >>> 6) = words(index >>> 6) | (1L << index)
      setting(words, bitSize, count - 1, combined + h2, h2)


// The bits are the words of one array, bit i being bit (i mod 64) of word (i div 64); its length
// is fixed at ⌈bitSize/64⌉, so growing a frozen filter copies the words exactly once.
class BloomFilter[element: Digestible, algorithm <: Algorithm] private[ulysses]
  ( val bitSize: Int, val hashCount: Int, private val words: scala.Array[Long]^ )
  ( using hash0: Hash in algorithm, erased weakness: Permit[HashWeakness[algorithm]] )
extends caps.Mutable:

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
    else
      // Fewer than sixteen bytes: the bytes of successive digests are folded into the two words,
      // the first eight into h₁ and the next eight into h₂.
      @tailrec
      def gather(bytes: scala.IArray[Byte], index: Int, count: Int, filled: Int, h1: Long, h2: Long)
      :   result =

        if filled == 16 then fold(h1, h2)
        else if index == bytes.length then
          gather(digest(value, count).readable, 0, count + 1, filled, h1, h2)
        else
          val byte = bytes(index) & 0xff

          if filled < 8 then gather(bytes, index + 1, count, filled + 1, (h1 << 8) | byte, h2)
          else gather(bytes, index + 1, count, filled + 1, h1, (h2 << 8) | byte)

      gather(first.readable, 0, 1, 0, 0L, 0L)

  private inline def set(index: Int): Boolean = ((words(index >>> 6) >>> index) & 1L) != 0L

  // Sets the element's positions in place; only an exclusive filter can.
  update def add(value: element): Unit =
    positions(value)(BloomFilter.setting(words, bitSize, hashCount, _, _))

  update def addAll[collection: Traversable by element](elements: collection): Unit =
    val iterator = summon[collection is Traversable by element].traverse(elements)

    @tailrec
    def recur(): Unit = if iterator.hasNext then
      add(iterator.next())
      recur()

    recur()

  // Growing by copying, for a frozen filter: a fresh filter with the bits and the new elements,
  // at one copy of the bits. The copy is made here rather than by a method, so that the fresh
  // filter is separate from `this` and may be written to from this read-only method.
  @targetName("add")
  infix def + (value: element): BloomFilter[element, algorithm]^{} =
    val copy: scala.Array[Long]^ = new scala.Array[Long](words.length)
    java.lang.System.arraycopy(words, 0, copy, 0, words.length)
    val filter = new BloomFilter(bitSize, hashCount, copy)
    filter.add(value)
    BloomFilter.freeze(filter)

  @targetName("addAll")
  infix def ++ [collection: Traversable by element](elements: collection)
  :   BloomFilter[element, algorithm]^{} =

    val iterator = summon[collection is Traversable by element].traverse(elements)

    // The fresh filter is threaded through `consume`, as a `var` cannot hold an exclusive value.
    @tailrec
    def recur(consume next: BloomFilter[element, algorithm]^): BloomFilter[element, algorithm]^{} =
      if iterator.hasNext then
        next.add(iterator.next())
        recur(next)
      else
        BloomFilter.freeze(next)

    val copy: scala.Array[Long]^ = new scala.Array[Long](words.length)
    java.lang.System.arraycopy(words, 0, copy, 0, words.length)
    val filter = new BloomFilter(bitSize, hashCount, copy)
    recur(filter)

  // Allocates nothing beyond the digest, and answers `false` at the first clear position.
  def hits(value: element): Boolean = positions(value): (h1, h2) =>
    @tailrec
    def recur(combined: Long, count: Int): Boolean =
      count == hashCount ||
        (set(((combined & Long.MaxValue)%bitSize).toInt) && recur(combined + h2, count + 1))

    recur(h1, 0)

  // The number of set bits, from which the number of elements added can be estimated.
  def population: Int =
    @tailrec
    def recur(index: Int, total: Int): Int =
      if index == words.length then total
      else recur(index + 1, total + java.lang.Long.bitCount(words(index)))

    recur(0, 0)

  private def sameBits(that: BloomFilter[?, ?]^{caps.any.rd}): Boolean =
    bitSize == that.bitSize && hashCount == that.hashCount &&
      java.util.Arrays.equals(words, that.words)

  // Two filters are equal when their bits are.
  override def equals(that: Any): Boolean = that.asMatchable match
    case that: BloomFilter[?, ?] => sameBits(that)
    case _                       => false

  override def hashCode: Int = 31*(31*bitSize + hashCount) + java.util.Arrays.hashCode(words)
