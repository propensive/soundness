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
package hallucination

import contingency.*

import scala.caps

import Raster.Error.Reason

// A canonical-Huffman decoder for JPEG entropy-coded data, ported from image-rs/jpeg-decoder
// (`src/huffman.rs`, MIT/Apache-2.0). Bits are buffered MSB-first in a 64-bit accumulator; an
// 8-bit primary lookup table resolves short codes in one step, with a linear fallback for longer
// codes. AC tables additionally carry a combined run/value table for small coefficients.
private[hallucination] object JpegHuffman:
  inline val LutBits = 8

  // Section F.2.2.1, Figure F.12: sign-extend a `count`-bit magnitude to a signed value.
  def extend(value: Int, count: Int): Int =
    val threshold = 1 << (count - 1)
    if value < threshold then value + (-1 << count) + 1 else value

private[hallucination] object JpegHuffmanTable:
  // A real `using` clause rather than the `raises` sugar: a context-function result would
  // hide the array parameters, which the separation checker rejects.
  def apply(counts: scala.Array[Int], values: scala.Array[Int], ac: Boolean)(using Tactic[Raster.Error])
  :   JpegHuffmanTable =

    val lutBits = JpegHuffman.LutBits

    // Section C.2, Figures C.1 and C.2: derive the canonical code sizes and codes.
    var totalSize = 0
    var index = 0
    while index < 16 do { totalSize += counts(index); index += 1 }

    val huffsize = new scala.Array[Int](totalSize)
    var position = 0
    index = 0

    while index < 16 do
      var repeat = 0

      while repeat < counts(index) do
        huffsize(position) = index + 1
        position += 1
        repeat += 1

      index += 1

    val huffcode = new scala.Array[Int](totalSize)
    var code = 0
    var codeSize = if totalSize > 0 then huffsize(0) else 0
    index = 0

    while index < totalSize do
      while codeSize < huffsize(index) do { code <<= 1; codeSize += 1 }
      if code >= (1 << huffsize(index)) then abort(Raster.Error(Jpeg(), Reason.Huffman))
      huffcode(index) = code
      code += 1
      index += 1

    // Section F.2.2.3, Figure F.15: delta[i] = VALPTR(i) - MINCODE(i); maxcode[i] = MAXCODE(i).
    val delta = new scala.Array[Int](16)
    val maxcode = scala.Array.fill(16)(-1)
    var j = 0
    index = 0

    while index < 16 do
      if counts(index) != 0 then
        delta(index) = j - huffcode(j)
        j += counts(index)
        maxcode(index) = huffcode(j - 1)

      index += 1

    // The primary lookup table: every code no longer than `lutBits` maps its prefix to a value.
    val lutValue = new scala.Array[Int](1 << lutBits)
    val lutSize = new scala.Array[Int](1 << lutBits)
    index = 0

    while index < totalSize do
      val size = huffsize(index)

      if size <= lutBits then
        val remaining = lutBits - size
        val start = huffcode(index) << remaining
        var fill = 0

        while fill < (1 << remaining) do
          lutValue(start + fill) = values(index)
          lutSize(start + fill) = size
          fill += 1

      index += 1

    // For AC tables, a secondary table decoding both the value and its magnitude extension for
    // small coefficients (Section F.2.2.2).
    val acValue = if ac then new scala.Array[Int](1 << lutBits) else new scala.Array[Int](0)
    val acRunSize = if ac then new scala.Array[Int](1 << lutBits) else new scala.Array[Int](0)

    if ac then
      index = 0

      while index < (1 << lutBits) do
        val value = lutValue(index)
        val size = lutSize(index)
        val runLength = value >> 4
        val magnitude = value & 0x0f

        if magnitude > 0 && size + magnitude <= lutBits then
          val unextended = ((index << size) & ((1 << lutBits) - 1)) >> (lutBits - magnitude)
          acValue(index) = JpegHuffman.extend(unextended, magnitude)
          acRunSize(index) = (runLength << 4) | (size + magnitude)

        index += 1

    // The freshly-built arrays are frozen zero-copy; `values` is likewise freshly built by
    // `parseDht` and never written after this call.
    new JpegHuffmanTable
      ( values.asInstanceOf[Array[Int]^{}], delta.asInstanceOf[Array[Int]^{}],
        maxcode.asInstanceOf[Array[Int]^{}], lutValue.asInstanceOf[Array[Int]^{}],
        lutSize.asInstanceOf[Array[Int]^{}], acValue.asInstanceOf[Array[Int]^{}],
        acRunSize.asInstanceOf[Array[Int]^{}] )

// Immutable after construction: the factory above freezes the freshly-built tables.
private[hallucination] final class JpegHuffmanTable
  ( val values:    Array[Int]^{},
    val delta:     Array[Int]^{},
    val maxcode:   Array[Int]^{},
    val lutValue:  Array[Int]^{},
    val lutSize:   Array[Int]^{},
    val acValue:   Array[Int]^{},
    val acRunSize: Array[Int]^{} ):

  // `acValue` and `acRunSize` are empty for DC tables.
  def hasAcLut: Boolean = acRunSize.length > 0

private[hallucination] final class JpegHuffmanDecoder extends caps.Mutable:
  private var bits: Long = 0L
  private var numBits: Int = 0
  private var marker: Int = -1

  // Results of the most recent `decodeFastAc`, valid when it returns true.
  var fastAcValue: Int = 0
  var fastAcRun: Int = 0

  // Section F.2.2.3, Figure F.16.
  update def decode(reader: JpegReader^, table: JpegHuffmanTable)(using Tactic[Raster.Error])
  :   Int =
    if numBits < 16 then readBits(reader)

    val lookup = peekBits(JpegHuffman.LutBits)
    val size = table.lutSize.readable(lookup)

    if size > 0 then
      consumeBits(size)
      table.lutValue.readable(lookup)
    else
      val code0 = peekBits(16)
      var index = JpegHuffman.LutBits
      var result = -1

      while result == -1 && index < 16 do
        val code = code0 >> (15 - index)

        if code <= table.maxcode.readable(index) then
          consumeBits(index + 1)
          result = table.values.readable(code + table.delta.readable(index))

        index += 1

      if result == -1 then abort(Raster.Error(Jpeg(), Reason.Huffman)) else result

  // Decodes a small AC coefficient in one step, if the combined table has an entry; returns true
  // and sets `fastAcValue`/`fastAcRun` when it does.
  update def decodeFastAc(reader: JpegReader^, table: JpegHuffmanTable)
    ( using Tactic[Raster.Error] )
  :   Boolean =
    if !table.hasAcLut then false else
      if numBits < JpegHuffman.LutBits then readBits(reader)
      val lookup = peekBits(JpegHuffman.LutBits)
      val runSize = table.acRunSize.readable(lookup)

      if runSize == 0 then false else
        fastAcValue = table.acValue.readable(lookup)
        fastAcRun = runSize >> 4
        consumeBits(runSize & 0x0f)
        true

  update def getBits(reader: JpegReader^, count: Int)(using Tactic[Raster.Error]): Int =
    if count == 0 then 0 else
      if numBits < count then readBits(reader)
      val value = peekBits(count)
      consumeBits(count)
      value

  update def receiveExtend(reader: JpegReader^, count: Int)(using Tactic[Raster.Error]): Int =
    JpegHuffman.extend(getBits(reader, count), count)

  update def reset(): Unit =
    bits = 0L
    numBits = 0

  update def takeMarker(reader: JpegReader^)(using Tactic[Raster.Error]): Int =
    readBits(reader)
    val result = marker
    marker = -1
    result

  private def peekBits(count: Int): Int =
    if count == 0 then 0 else ((bits >>> (64 - count)) & ((1L << count) - 1)).toInt

  private update def consumeBits(count: Int): Unit =
    bits <<= count
    numBits -= count

  private update def readBits(reader: JpegReader^)(using Tactic[Raster.Error]): Unit =
    while numBits <= 56 do
      val byte = if marker != -1 then 0 else reader.u8()

      if byte == 0xff && marker == -1 then
        var next = reader.u8()
        if next != 0x00 then
          // Section B.1.1.2: a marker, optionally preceded by fill bytes, ends the entropy data.
          while next == 0xff do next = reader.u8()
          if next == 0x00 then abort(Raster.Error(Jpeg(), Reason.Bitstream)) else marker = next
          numBits += 8
        else
          bits |= 0xffL << (56 - numBits)
          numBits += 8
      else
        bits |= (byte.toLong & 0xff) << (56 - numBits)
        numBits += 8
