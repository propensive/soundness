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
package pneumatic

import proscenium.compat.*

// The LZMA binary range coder, the entropy stage beneath LZMA/LZMA2. It codes single bits against
// adaptive 11-bit probabilities (`Array[Short]` in [0, 2048]), plus "direct" equiprobable bits and
// MSB-/LSB-first probability trees. This is a clean-room implementation of the well-documented LZMA
// range coder (the same arithmetic used by 7-Zip and XZ Utils), following the structure of the
// public-domain XZ for Java reference so that streams interoperate byte-for-byte.
//
// All 32-bit quantities (`range`, `code`, `bound`) are handled as unsigned via explicit masking and
// the sign-flip comparison trick; the encoder's `low` is a 33-bit accumulator held in a `Long`.

private[pneumatic] object RangeCoder:
  inline val TopMask = 0xff000000
  inline val BitModelTotalBits = 11
  inline val BitModelTotal = 1 << BitModelTotalBits // 2048
  inline val MoveBits = 5
  val ProbInit: Short = 1024 // BitModelTotal / 2

  inline val MoveReducingBits = 4
  inline val BitPriceShiftBits = 4

  // A fresh probability array, every entry initialised to the neutral 0.5 estimate.
  def probabilities(size: Int): Array[Short] =
    val array: Array[Short]^ = new Array[Short](size)
    var i = 0
    while i < size do { array(i) = ProbInit; i += 1 }
    // The array is fresh; purity is assumed so it can seed untracked probability fields.
    scala.caps.unsafe.unsafeAssumePure(array)

  // A fresh matrix of probability arrays. Built with a loop rather than `Array.tabulate`
  // because the synthesized `ClassTag` for a capture-typed element does not conform.
  def probabilityMatrix(rows: Int, symbols: Int): Array[Array[Short]] =
    val matrix: Array[Array[Short]]^ = new Array[Array[Short]](rows)
    var i = 0
    while i < rows do { matrix(i) = probabilities(symbols); i += 1 }
    scala.caps.unsafe.unsafeAssumePure(matrix)

  def resetProbabilities(array: Array[Short]^): Unit =
    var i = 0
    while i < array.length do { array(i) = ProbInit; i += 1 }

  // Fixed-point bit prices (in 1/16-bit units): the cost of coding a bit whose probability index is
  // `prob >>> MoveReducingBits`. Built once, then read by the encoder's cost estimator.
  val prices: IArray[Short] =
    val table: Array[Short]^ = new Array[Short](BitModelTotal >>> MoveReducingBits)
    var i = 1 << (MoveReducingBits - 1)

    while i < BitModelTotal do
      var w = i
      var bitCount = 0
      var j = 0

      while j < BitPriceShiftBits do
        w *= w
        bitCount <<= 1
        while (w & 0xffff0000) != 0 do { w >>>= 1; bitCount += 1 }
        j += 1

      table(i >> MoveReducingBits) =
        ((BitModelTotalBits << BitPriceShiftBits) - 15 - bitCount).toShort

      i += 1 << MoveReducingBits

    // The table is fresh and never written after construction.
    IArray.unsafeFromArray(table)

  def bitPrice(prob: Int, bit: Int): Int =
    prices((prob ^ ((-bit) & (BitModelTotal - 1))) >>> MoveReducingBits).toInt

  def directBitsPrice(count: Int): Int = count << BitPriceShiftBits

  def bitTreePrice(probs: Array[Short], symbol0: Int): Int =
    var price = 0
    var symbol = symbol0 | probs.length

    while symbol != 1 do
      val bit = symbol & 1
      symbol >>>= 1
      price += bitPrice(probs(symbol).toInt, bit)

    price

  def bitTreeReversePrice(probs: Array[Short], symbol0: Int): Int =
    var price = 0
    var index = 1
    var symbol = symbol0
    var continue = true

    while continue do
      val bit = symbol & 1
      symbol >>>= 1
      price += bitPrice(probs(index).toInt, bit)
      index = (index << 1) | bit
      continue = index < probs.length

    price

import RangeCoder.*

// Encodes single bits, direct bits and probability trees into a growable buffer. `low` is a 33-bit
// carry accumulator; `shiftLow` propagates carries into already-buffered bytes through the one-byte
// `cache` plus a run of `cacheSize` deferred `0xff`s.
private[pneumatic] final class RangeEncoder:
  @scala.caps.unsafe.untrackedCaptures
  private var buffer: Array[Byte] = new Array[Byte](1 << 16)
  @scala.caps.unsafe.untrackedCaptures
  private var count: Int = 0
  @scala.caps.unsafe.untrackedCaptures
  private var low: Long = 0
  @scala.caps.unsafe.untrackedCaptures
  private var range: Int = 0
  @scala.caps.unsafe.untrackedCaptures
  private var cache: Int = 0
  @scala.caps.unsafe.untrackedCaptures
  private var cacheSize: Long = 0

  reset()

  def reset(): Unit =
    low = 0
    range = 0xffffffff
    cache = 0
    cacheSize = 1
    count = 0

  // A conservative upper bound on how many bytes finishing now would append: the already-buffered
  // bytes, the deferred cache run, and the five flush bytes. Used to keep LZMA2 chunks within their
  // 64 KiB compressed limit.
  def pendingSize: Int = count + cacheSize.toInt + 5 - 1

  private def writeByte(byte: Int): Unit =
    if count == buffer.length then
      val grown: Array[Byte]^ = new Array[Byte](buffer.length*2)
      System.arraycopy(buffer, 0, grown, 0, count)
      buffer = grown

    writable(buffer)(count) = byte.toByte
    count += 1

  private def shiftLow(): Unit =
    val lowHigh = (low >>> 32).toInt

    if lowHigh != 0 || low < 0xff000000L then
      var temp = cache
      var continue = true

      while continue do
        writeByte((temp + lowHigh) & 0xff)
        temp = 0xff
        cacheSize -= 1
        continue = cacheSize != 0

      cache = (low >>> 24).toInt & 0xff

    cacheSize += 1
    low = (low & 0x00ffffff) << 8

  def encodeBit(probs: Array[Short]^, index: Int, bit: Int): Unit =
    val prob = probs(index).toInt
    val bound = (range >>> BitModelTotalBits) * prob

    if bit == 0 then
      range = bound
      probs(index) = (prob + ((BitModelTotal - prob) >>> MoveBits)).toShort
    else
      low += bound & 0xffffffffL
      range -= bound
      probs(index) = (prob - (prob >>> MoveBits)).toShort

    if (range & TopMask) == 0 then
      range <<= 8
      shiftLow()

  def encodeBitTree(probs: Array[Short]^, symbol: Int): Unit =
    var index = 1
    var mask = probs.length
    var continue = true

    while continue do
      mask >>>= 1
      val bit = symbol & mask
      encodeBit(probs, index, if bit != 0 then 1 else 0)
      index <<= 1
      if bit != 0 then index |= 1
      continue = mask != 1

  def encodeBitTreeReverse(probs: Array[Short]^, symbol0: Int): Unit =
    var index = 1
    var symbol = symbol0
    var continue = true

    while continue do
      val bit = symbol & 1
      symbol >>>= 1
      encodeBit(probs, index, bit)
      index = (index << 1) | bit
      continue = index < probs.length

  def encodeDirectBits(value: Int, count0: Int): Unit =
    var i = count0

    while i != 0 do
      i -= 1
      range >>>= 1
      if ((value >>> i) & 1) != 0 then low += range.toLong & 0xffffffffL

      if (range & TopMask) == 0 then
        range <<= 8
        shiftLow()

  // Flush the last five bytes of `low`, returning the total encoded length. After this the buffer
  // holds a complete range-coded payload.
  def finish(): Int =
    var i = 0
    while i < 5 do { shiftLow(); i += 1 }
    count

  // Copy the encoded bytes into `target` at `offset`; the caller has ensured the space exists.
  def copyTo(target: Array[Byte], offset: Int): Unit =
    System.arraycopy(buffer, 0, target, offset, count)

  def bytes: Array[Byte] =
    val result: Array[Byte]^ = new Array[Byte](count)
    System.arraycopy(buffer, 0, result, 0, count)
    result
