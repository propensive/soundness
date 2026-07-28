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

  // The price of coding `symbol0` against the MSB-first tree of `size` entries at `offset`
  // within `probs` (read-only: prices never adapt the model).
  def bitTreePrice
    ( probs: Array[Short]^{scala.caps.any.rd}, offset: Int, size: Int, symbol0: Int )
  :   Int =

    var price = 0
    var symbol = symbol0 | size

    while symbol != 1 do
      val bit = symbol & 1
      symbol >>>= 1
      price += bitPrice(probs(offset + symbol).toInt, bit)

    price

  def bitTreeReversePrice
    ( probs: Array[Short]^{scala.caps.any.rd}, offset: Int, size: Int, symbol0: Int )
  :   Int =

    var price = 0
    var index = 1
    var symbol = symbol0
    var continue = true

    while continue do
      val bit = symbol & 1
      symbol >>>= 1
      price += bitPrice(probs(offset + index).toInt, bit)
      index = (index << 1) | bit
      continue = index < size

    price

