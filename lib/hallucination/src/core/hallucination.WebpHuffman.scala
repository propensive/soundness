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
┃    Soundness, version 0.63.0.                                                                    ┃
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
import vacuous.*

import RasterError.Reason

// A canonical-Huffman decoder using a two-level lookup table, ported from image-rs/image-webp
// (`src/lossless/decoder/huffman.rs`, MIT/Apache-2.0). A code is decoded by one primary-table
// lookup on the low `MaxTableBits` bits; codes longer than that index a secondary table. A tree
// with a single symbol needs no bits at all.
private[hallucination] object WebpHuffman:
  private val MaxAllowedCodeLength: Int = 15
  val MaxTableBits: Int = 10

  // The next canonical codeword after `codeword` for a table of `tableSize` entries. Codewords are
  // enumerated in bit-reversed order, so this flips the highest differing bit.
  private def nextCodeword(codeword0: Int, tableSize: Int): Int =
    if codeword0 == tableSize - 1 then codeword0 else
      val advance = 31 - java.lang.Integer.numberOfLeadingZeros(codeword0 ^ (tableSize - 1))
      val bit = 1 << advance
      (codeword0 & (bit - 1)) | bit

  // Builds a tree from per-symbol code lengths (0 meaning "absent").
  def buildImplicit(codeLengths: Array[Int]): WebpHuffman raises RasterError =
    var numSymbols = 0
    val histogram = new Array[Int](MaxAllowedCodeLength + 1)
    var index = 0

    while index < codeLengths.length do
      histogram(codeLengths(index)) += 1

      if codeLengths(index) != 0 then numSymbols += 1
      index += 1

    if numSymbols == 0 then abort(RasterError(Webp(), Reason.Huffman))
    else if numSymbols == 1 then
      var symbol = 0

      while codeLengths(symbol) == 0 do symbol += 1
      single(symbol)
    else
      var maxLength = MaxAllowedCodeLength

      while maxLength > 1 && histogram(maxLength) == 0 do maxLength -= 1

      // The starting sorted-order offset for each code length, and a validity check that the codes
      // fill the code space exactly.
      val offsets = new Array[Int](16)
      var codespaceUsed = 0
      offsets(1) = histogram(0)
      var length = 1

      while length < maxLength do
        offsets(length + 1) = offsets(length) + histogram(length)
        codespaceUsed = (codespaceUsed << 1) + histogram(length)
        length += 1

      codespaceUsed = (codespaceUsed << 1) + histogram(maxLength)

      if codespaceUsed != (1 << maxLength) then abort(RasterError(Webp(), Reason.Huffman))

      val tableBits = maxLength.min(MaxTableBits)
      val tableSize = 1 << tableBits
      val primaryTable = new Array[Int](tableSize)

      // Sort the symbols by code length.
      val nextByLength = offsets.clone()
      val sortedSymbols = new Array[Int](codeLengths.length)
      var symbol = 0

      while symbol < codeLengths.length do
        val len = codeLengths(symbol)
        sortedSymbols(nextByLength(len)) = symbol
        nextByLength(len) += 1
        symbol += 1

      var codeword = 0
      var sorted = histogram(0)
      val primaryTableBits = numberOfTrailingZeros(primaryTable.length)
      val primaryTableMask = (1 << primaryTableBits) - 1

      // Populate the primary table, doubling it as each code length is filled in.
      var plen = 1

      while plen <= primaryTableBits do
        val currentTableEnd = 1 << plen
        var count = histogram(plen)

        while count > 0 do
          val entry = (plen << 12) | sortedSymbols(sorted)
          sorted += 1
          primaryTable(codeword) = entry
          codeword = nextCodeword(codeword, currentTableEnd)
          count -= 1

        if plen < primaryTableBits then
          System.arraycopy(primaryTable, 0, primaryTable, currentTableEnd, currentTableEnd)

        plen += 1

      // Populate the secondary table for codes longer than the primary table.
      var secondaryTable = new Array[Int](0)

      if maxLength > primaryTableBits then
        var subtableStart = 0
        var subtablePrefix = -1
        var slen = primaryTableBits + 1

        while slen <= maxLength do
          val subtableSize = 1 << (slen - primaryTableBits)
          var count = histogram(slen)

          while count > 0 do
            if (codeword & primaryTableMask) != subtablePrefix then
              subtablePrefix = codeword & primaryTableMask
              subtableStart = secondaryTable.length
              primaryTable(subtablePrefix) = (slen << 12) | subtableStart
              secondaryTable = grow(secondaryTable, subtableStart + subtableSize)

            secondaryTable(subtableStart + (codeword >>> primaryTableBits)) =
              (sortedSymbols(sorted) << 4) | slen

            sorted += 1
            codeword = nextCodeword(codeword, 1 << slen)
            count -= 1

          if slen < maxLength && (codeword & primaryTableMask) == subtablePrefix then
            secondaryTable = extendFromWithin(secondaryTable, subtableStart)
            primaryTable(subtablePrefix) = ((slen + 1) << 12) | subtableStart

          slen += 1

      WebpHuffman(single = -1, (tableSize - 1), primaryTable, secondaryTable)

  private val empty: Array[Int] = new Array[Int](0)

  def single(symbol: Int): WebpHuffman = WebpHuffman(symbol, 0, empty, empty)

  def twoNode(zero: Int, one: Int): WebpHuffman =
    WebpHuffman(-1, 0x1, Array((1 << 12) | zero, (1 << 12) | one), new Array[Int](0))

  private def numberOfTrailingZeros(value: Int): Int =
    java.lang.Integer.numberOfTrailingZeros(value)

  private def grow(array: Array[Int], size: Int): Array[Int] =
    if array.length >= size then array else
      val grown = new Array[Int](size)
      System.arraycopy(array, 0, grown, 0, array.length)
      grown

  // Appends a copy of `array[from..]` to `array` (Rust's `extend_from_within`).
  private def extendFromWithin(array: Array[Int], from: Int): Array[Int] =
    val tail = array.length - from
    val grown = new Array[Int](array.length + tail)
    System.arraycopy(array, 0, grown, 0, array.length)
    System.arraycopy(array, from, grown, array.length, tail)
    grown

// A `single` symbol >= 0 marks a one-symbol tree; otherwise the primary/secondary tables decode.
private[hallucination] final class WebpHuffman
  ( val single: Int, tableMask: Int, primaryTable: Array[Int], secondaryTable: Array[Int] ):

  def isSingleNode: Boolean = single >= 0

  def readSymbol(reader: WebpBitReader): Int raises RasterError =
    if single >= 0 then single else
      val value = reader.peekFull.toInt & 0xffff
      val entry = primaryTable(value & tableMask)

      if (entry >>> 12) <= WebpHuffman.MaxTableBits then
        reader.consume(entry >>> 12)
        entry & 0xfff
      else
        val length = entry >>> 12
        val mask = (1 << (length - WebpHuffman.MaxTableBits)) - 1
        val secondaryIndex = (entry & 0xfff) + ((value >>> WebpHuffman.MaxTableBits) & mask)
        val secondaryEntry = secondaryTable(secondaryIndex)
        reader.consume(secondaryEntry & 0xf)
        secondaryEntry >>> 4

  // Peeks the next symbol if it needs only a primary-table lookup, returning (codeLength, symbol).
  def peekSymbol(reader: WebpBitReader): Optional[(Int, Int)] =
    if single >= 0 then (0, single) else
      val value = reader.peekFull.toInt & 0xffff
      val entry = primaryTable(value & tableMask)

      if (entry >>> 12) <= WebpHuffman.MaxTableBits then (entry >>> 12, entry & 0xfff) else Unset
