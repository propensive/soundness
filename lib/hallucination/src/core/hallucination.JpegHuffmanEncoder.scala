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

// Huffman encoding tables, ported from jpeg-encoder's `huffman.rs` (Apache-2.0/MIT). A table maps
// each byte symbol to a (bit-length, code) pair; the code-length list and value list are also
// retained for writing the DHT segment. Both the standard Annex K tables and per-image optimized
// tables (Annex K.2) are supported.
private[hallucination] final class JpegEncodeTable
  ( val lengths: Array[Int],
    val values:  Array[Int] ):

  // The per-symbol (size, code) lookup, from the canonical code assignment (Figures C.1–C.3).
  val sizeOf: Array[Int] = new Array[Int](256)
  val codeOf: Array[Int] = new Array[Int](256)

  locally:
    val sizes = new Array[Int](256)
    var k = 0
    var i = 0

    while i < 16 do
      var count = 0

      while count < lengths(i) do
        sizes(k) = i + 1
        k += 1
        count += 1

      i += 1

    val codes = new Array[Int](256)
    var code = 0
    var currentSize = if k > 0 then sizes(0) else 0
    var index = 0

    while index < k do
      if sizes(index) != currentSize then
        code <<= (sizes(index) - currentSize)
        currentSize = sizes(index)

      codes(index) = code
      code += 1
      index += 1

    i = 0

    while i < values.length do
      sizeOf(values(i)) = sizes(i)
      codeOf(values(i)) = codes(i)
      i += 1

private[hallucination] object JpegHuffmanEncoder:
  private val LumaDcLengths = Array(0, 1, 5, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0)
  private val LumaDcValues = Array(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)
  private val ChromaDcLengths = Array(0, 3, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0)
  private val ChromaDcValues = Array(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)

  private val LumaAcLengths = Array(0, 2, 1, 3, 3, 2, 4, 3, 5, 5, 4, 4, 0, 0, 1, 0x7d)

  private val LumaAcValues = Array(
    0x01, 0x02, 0x03, 0x00, 0x04, 0x11, 0x05, 0x12, 0x21, 0x31, 0x41, 0x06, 0x13, 0x51, 0x61, 0x07,
    0x22, 0x71, 0x14, 0x32, 0x81, 0x91, 0xa1, 0x08, 0x23, 0x42, 0xb1, 0xc1, 0x15, 0x52, 0xd1, 0xf0,
    0x24, 0x33, 0x62, 0x72, 0x82, 0x09, 0x0a, 0x16, 0x17, 0x18, 0x19, 0x1a, 0x25, 0x26, 0x27, 0x28,
    0x29, 0x2a, 0x34, 0x35, 0x36, 0x37, 0x38, 0x39, 0x3a, 0x43, 0x44, 0x45, 0x46, 0x47, 0x48, 0x49,
    0x4a, 0x53, 0x54, 0x55, 0x56, 0x57, 0x58, 0x59, 0x5a, 0x63, 0x64, 0x65, 0x66, 0x67, 0x68, 0x69,
    0x6a, 0x73, 0x74, 0x75, 0x76, 0x77, 0x78, 0x79, 0x7a, 0x83, 0x84, 0x85, 0x86, 0x87, 0x88, 0x89,
    0x8a, 0x92, 0x93, 0x94, 0x95, 0x96, 0x97, 0x98, 0x99, 0x9a, 0xa2, 0xa3, 0xa4, 0xa5, 0xa6, 0xa7,
    0xa8, 0xa9, 0xaa, 0xb2, 0xb3, 0xb4, 0xb5, 0xb6, 0xb7, 0xb8, 0xb9, 0xba, 0xc2, 0xc3, 0xc4, 0xc5,
    0xc6, 0xc7, 0xc8, 0xc9, 0xca, 0xd2, 0xd3, 0xd4, 0xd5, 0xd6, 0xd7, 0xd8, 0xd9, 0xda, 0xe1, 0xe2,
    0xe3, 0xe4, 0xe5, 0xe6, 0xe7, 0xe8, 0xe9, 0xea, 0xf1, 0xf2, 0xf3, 0xf4, 0xf5, 0xf6, 0xf7, 0xf8,
    0xf9, 0xfa)

  private val ChromaAcLengths = Array(0, 2, 1, 2, 4, 4, 3, 4, 7, 5, 4, 4, 0, 1, 2, 0x77)

  private val ChromaAcValues = Array(
    0x00, 0x01, 0x02, 0x03, 0x11, 0x04, 0x05, 0x21, 0x31, 0x06, 0x12, 0x41, 0x51, 0x07, 0x61, 0x71,
    0x13, 0x22, 0x32, 0x81, 0x08, 0x14, 0x42, 0x91, 0xa1, 0xb1, 0xc1, 0x09, 0x23, 0x33, 0x52, 0xf0,
    0x15, 0x62, 0x72, 0xd1, 0x0a, 0x16, 0x24, 0x34, 0xe1, 0x25, 0xf1, 0x17, 0x18, 0x19, 0x1a, 0x26,
    0x27, 0x28, 0x29, 0x2a, 0x35, 0x36, 0x37, 0x38, 0x39, 0x3a, 0x43, 0x44, 0x45, 0x46, 0x47, 0x48,
    0x49, 0x4a, 0x53, 0x54, 0x55, 0x56, 0x57, 0x58, 0x59, 0x5a, 0x63, 0x64, 0x65, 0x66, 0x67, 0x68,
    0x69, 0x6a, 0x73, 0x74, 0x75, 0x76, 0x77, 0x78, 0x79, 0x7a, 0x82, 0x83, 0x84, 0x85, 0x86, 0x87,
    0x88, 0x89, 0x8a, 0x92, 0x93, 0x94, 0x95, 0x96, 0x97, 0x98, 0x99, 0x9a, 0xa2, 0xa3, 0xa4, 0xa5,
    0xa6, 0xa7, 0xa8, 0xa9, 0xaa, 0xb2, 0xb3, 0xb4, 0xb5, 0xb6, 0xb7, 0xb8, 0xb9, 0xba, 0xc2, 0xc3,
    0xc4, 0xc5, 0xc6, 0xc7, 0xc8, 0xc9, 0xca, 0xd2, 0xd3, 0xd4, 0xd5, 0xd6, 0xd7, 0xd8, 0xd9, 0xda,
    0xe2, 0xe3, 0xe4, 0xe5, 0xe6, 0xe7, 0xe8, 0xe9, 0xea, 0xf2, 0xf3, 0xf4, 0xf5, 0xf6, 0xf7, 0xf8,
    0xf9, 0xfa)

  def defaultLumaDc: JpegEncodeTable = JpegEncodeTable(LumaDcLengths, LumaDcValues)
  def defaultLumaAc: JpegEncodeTable = JpegEncodeTable(LumaAcLengths, LumaAcValues)
  def defaultChromaDc: JpegEncodeTable = JpegEncodeTable(ChromaDcLengths, ChromaDcValues)
  def defaultChromaAc: JpegEncodeTable = JpegEncodeTable(ChromaAcLengths, ChromaAcValues)

  // The magnitude category and coefficient bits of a value, as used for DC differences and AC
  // coefficients (Section F.1.2).
  def getCode(value: Int): (Int, Int) =
    val magnitude = if value < 0 then -value else value
    val numBits = if magnitude == 0 then 0 else 32 - Integer.numberOfLeadingZeros(magnitude)
    val adjusted = value - (if value < 0 then 1 else 0)
    (numBits, adjusted & ((1 << numBits) - 1))

  // The number of magnitude bits of a value.
  def numBits(value: Int): Int =
    val magnitude = if value < 0 then -value else value
    if magnitude == 0 then 0 else 32 - Integer.numberOfLeadingZeros(magnitude)

  // Builds an image-optimized table from symbol frequencies (Annex K.2, Figures K.1–K.4). `freq`
  // has 257 entries; index 256 is a reserved sentinel guaranteeing a spare longest code.
  def newOptimized(freq0: Array[Int]): JpegEncodeTable =
    val freq = freq0.clone()
    val others = Array.fill(257)(-1)
    val codesize = new Array[Int](257)
    var running = true

    // Figure K.1: combine the two least-frequent symbols repeatedly, tracking merged chains.
    while running do
      var v1 = -1
      var v1min = Int.MaxValue
      var i = 0

      while i < 257 do
        if freq(i) > 0 && freq(i) <= v1min then { v1min = freq(i); v1 = i }
        i += 1

      if v1 == -1 then running = false else
        var v2 = -1
        var v2min = Int.MaxValue
        i = 0

        while i < 257 do
          if freq(i) > 0 && freq(i) <= v2min && i != v1 then { v2min = freq(i); v2 = i }
          i += 1

        if v2 == -1 then running = false else
          freq(v1) += freq(v2)
          freq(v2) = 0
          codesize(v1) += 1
          var w1 = v1

          while others(w1) >= 0 do
            w1 = others(w1)
            codesize(w1) += 1

          others(w1) = v2
          codesize(v2) += 1
          var w2 = v2

          while others(w2) >= 0 do
            w2 = others(w2)
            codesize(w2) += 1

    // Figure K.2: count codes of each length.
    val bits = new Array[Int](33)
    var i = 0

    while i < 257 do
      if codesize(i) > 0 then bits(codesize(i)) += 1
      i += 1

    // Figure K.3: force all code lengths down to at most 16 bits.
    var length = 32

    while length > 16 do
      while bits(length) > 0 do
        var j = length - 2
        while bits(j) == 0 do j -= 1
        bits(length) -= 2
        bits(length - 1) += 1
        bits(j + 1) += 2
        bits(j) -= 1

      length -= 1

    while bits(length) == 0 do length -= 1
    bits(length) -= 1

    // Figure K.4: sort the symbols by increasing code length.
    val huffval = new Array[Int](256)
    var k = 0
    var size = 1

    while size <= 32 do
      var symbol = 0

      while symbol <= 255 do
        if codesize(symbol) == size then { huffval(k) = symbol; k += 1 }
        symbol += 1

      size += 1

    val lengths = new Array[Int](16)
    i = 0
    while i < 16 do { lengths(i) = bits(i + 1); i += 1 }

    JpegEncodeTable(lengths, huffval.slice(0, k))
