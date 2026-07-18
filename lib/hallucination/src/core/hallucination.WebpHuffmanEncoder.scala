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

import scala.collection.mutable as scm

// Builds and writes length-limited canonical Huffman codes in the VP8L format, ported from
// image-rs/image-webp (`src/lossless/encoder/huffman.rs`, MIT/Apache-2.0). `lengths` and `codes`
// are filled in for each symbol; the tree is written in the format the decoder expects.
private[hallucination] object WebpHuffmanEncoder:
  // The order in which the 19 code-length codes are written.
  private val CodeLengthCodeOrder: Array[Int] =
    Array(17, 18, 0, 1, 2, 3, 4, 5, 16, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15)

  def writeSingleEntry(writer: WebpBitWriter, symbol: Int): Unit =
    writer.writeBits(1, 2)

    if symbol <= 1 then
      writer.writeBits(0, 1)
      writer.writeBits(symbol.toLong, 1)
    else
      writer.writeBits(1, 1)
      writer.writeBits(symbol.toLong, 8)

  // Builds codes for `frequencies` and writes the tree; falls back to a single-entry tree when
  // fewer than two symbols occur.
  def writeTree
    ( writer: WebpBitWriter, frequencies: Array[Int], lengths: Array[Int], codes: Array[Int] )
  :   Unit =

    if !build(frequencies, lengths, codes, 15) then
      var symbol = 0

      while symbol < frequencies.length && frequencies(symbol) == 0 do symbol += 1
      writeSingleEntry(writer, if symbol < frequencies.length then symbol else 0)
    else
      val codeLengthLengths = new Array[Int](16)
      val codeLengthCodes = new Array[Int](16)
      val codeLengthFrequencies = new Array[Int](16)
      var i = 0

      while i < lengths.length do
        codeLengthFrequencies(lengths(i)) += 1
        i += 1

      val singleCodeLength = !build(codeLengthFrequencies, codeLengthLengths, codeLengthCodes, 7)

      writer.writeBits(0, 1)
      writer.writeBits(19 - 4, 4)

      for j <- CodeLengthCodeOrder do
        if j > 15 || codeLengthFrequencies(j) == 0 then writer.writeBits(0, 3)
        else if singleCodeLength then writer.writeBits(1, 3)
        else writer.writeBits(codeLengthLengths(j).toLong, 3)

      lengths.length match
        case 256 =>
          writer.writeBits(1, 1)
          writer.writeBits(3, 3)
          writer.writeBits(254, 8)

        case _ =>
          writer.writeBits(0, 1)

      if !singleCodeLength then
        var k = 0

        while k < lengths.length do
          writer.writeBits(codeLengthCodes(lengths(k)).toLong, codeLengthLengths(lengths(k)))
          k += 1

  // Builds canonical code lengths (≤ `limit`) and codes from frequencies; returns false (and zeroes
  // the arrays) if fewer than two symbols occur.
  private def build(frequencies: Array[Int], lengths: Array[Int], codes: Array[Int], limit: Int)
  :   Boolean =

    java.util.Arrays.fill(lengths, 0)
    java.util.Arrays.fill(codes, 0)

    var used = 0
    var f = 0

    while f < frequencies.length do
      if frequencies(f) > 0 then used += 1
      f += 1

    if used <= 1 then false else
      // A min-heap of (frequency, node index); internal nodes get indices past the leaves.
      val ordering: Ordering[(Int, Int)] = Ordering.by: item =>
        -item(0)
      val nodes = scm.PriorityQueue.empty[(Int, Int)](using ordering)
      val internal = scm.ArrayBuffer[(Int, Int)]()
      var i = 0

      while i < frequencies.length do
        if frequencies(i) > 0 then nodes.enqueue((frequencies(i), i))
        i += 1

      while nodes.size > 1 do
        val (freq1, index1) = nodes.dequeue()
        val (freq2, index2) = nodes.dequeue()
        internal += ((index1, index2))
        nodes.enqueue((freq1 + freq2, frequencies.length + internal.length - 1))

      // Assign a code length to each leaf equal to its depth in the tree.
      val stack = scm.Stack[(Int, Int)]()
      stack.push((nodes.dequeue()(1), 0))

      while stack.nonEmpty do
        val (node, depth) = stack.pop()

        if node < frequencies.length then lengths(node) = depth
        else
          val (left, right) = internal(node - frequencies.length)
          stack.push((left, depth + 1))
          stack.push((right, depth + 1))

      limitLengths(frequencies, lengths, limit)
      assignCodes(lengths, codes, limit)
      true

  // Rebalances code lengths so none exceeds `limit`, preserving a valid (Kraft-complete) tree.
  private def limitLengths(frequencies: Array[Int], lengths: Array[Int], limit: Int): Unit =
    var maxLength = 0
    var i = 0

    while i < lengths.length do
      maxLength = maxLength.max(lengths(i))
      i += 1

    if maxLength > limit then
      val counts = new Array[Int](16)
      i = 0

      while i < lengths.length do
        counts(lengths(i).min(limit)) += 1
        i += 1

      var total = 0
      var len = 1

      while len <= limit do
        total += counts(len) << (limit - len)
        len += 1

      while total > (1 << limit) do
        var j = limit - 1

        while counts(j) == 0 do j -= 1
        counts(j) -= 1
        counts(limit) -= 1
        counts(j + 1) += 2
        total -= 1

      // Reassign lengths shortest-code to most-frequent, longest to least-frequent.
      val order = (0 until frequencies.length).sortBy(frequencies(_)).toArray
      var current = limit
      var k = 0

      while k < order.length do
        val index = order(k)

        if frequencies(index) > 0 then
          while counts(current) == 0 do current -= 1
          lengths(index) = current
          counts(current) -= 1

        k += 1

  // Assigns canonical, bit-reversed codes in increasing length order.
  private def assignCodes(lengths: Array[Int], codes: Array[Int], limit: Int): Unit =
    var code = 0
    var len = 1

    while len <= limit do
      var i = 0

      while i < lengths.length do
        if lengths(i) == len then
          codes(i) = (Integer.reverse(code) >>> 16) >>> (16 - len)
          code += 1

        i += 1

      code <<= 1
      len += 1
