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

import java.io as ji

import anticipation.*
import rudiments.*
import vacuous.*

// The VP8L lossless encoder, ported from image-rs/image-webp (`src/lossless/encoder/mod.rs`,
// MIT/Apache-2.0). It uses a fixed strategy — the subtract-green and (top/left) predictor
// transforms, then length-limited Huffman coding with run-length compression of repeated pixels
// (encoded as distance-1 backward references). Output is a complete simple-container WebP file.
private[hallucination] object WebpEncoder:
  def encode(raster: Raster): Data =
    val width = raster.width
    val height = raster.height
    val alpha = raster.descriptor.hasAlpha

    // Expand the raster to an RGBA byte buffer.
    val pixels = new Array[Byte](width*height*4)
    var i = 0

    while i < width*height do
      val word = raster.word(i)
      val chroma = raster.descriptor.chroma(word)
      pixels(i*4) = chroma.red.toByte
      pixels(i*4 + 1) = chroma.green.toByte
      pixels(i*4 + 2) = chroma.blue.toByte
      val opacity = if alpha then (raster.descriptor.alpha(word)*255 + 0.5).toInt else 255
      pixels(i*4 + 3) = opacity.toByte
      i += 1

    container(encodeFrame(pixels, width, height, alpha))

  // Wraps a VP8L frame in the simple RIFF/WEBP container.
  private def container(frame: Data): Data =
    val padded = frame.length + (frame.length & 1)
    val out = ji.ByteArrayOutputStream()

    def fourcc(text: String): Unit = text.foreach: char =>
      out.write(char.toInt)

    def u32(value: Int): Unit =
      out.write(value & 0xff); out.write((value >>> 8) & 0xff)
      out.write((value >>> 16) & 0xff); out.write((value >>> 24) & 0xff)

    fourcc("RIFF")
    u32(padded + 12)
    fourcc("WEBP")
    fourcc("VP8L")
    u32(frame.length)
    out.write(frame.mutable(using Unsafe))

    if (frame.length & 1) == 1 then out.write(0)
    out.toByteArray.nn.immutable(using Unsafe)

  private def encodeFrame(pixels: Array[Byte], width: Int, height: Int, alpha: Boolean): Data =
    val writer = WebpBitWriter()

    // Header: signature, dimensions, alpha flag, version.
    writer.writeBits(0x2f, 8)
    writer.writeBits((width - 1).toLong, 14)
    writer.writeBits((height - 1).toLong, 14)
    writer.writeBits(if alpha then 1 else 0, 1)
    writer.writeBits(0, 3)

    // Subtract-green transform.
    writer.writeBits(0x5, 3)

    // Predictor transform: a single block using the top predictor everywhere (the first row and
    // column are handled by the decoder's border rules).
    writer.writeBits(0x39, 6)
    writer.writeBits(0, 1)
    WebpHuffmanEncoder.writeSingleEntry(writer, 2)
    var t = 0

    while t < 4 do
      WebpHuffmanEncoder.writeSingleEntry(writer, 0)
      t += 1

    writer.writeBits(0, 1) // transforms done
    writer.writeBits(0, 1) // no colour cache
    writer.writeBits(0, 1) // no meta-Huffman codes

    // Forward subtract-green.
    var i = 0

    while i < pixels.length do
      pixels(i) = (pixels(i) - pixels(i + 1)).toByte
      pixels(i + 2) = (pixels(i + 2) - pixels(i + 1)).toByte
      i += 4

    // Forward predictor: subtract the pixel above (rows ≥ 1), the pixel to the left (row 0), and
    // opaque black from the top-left pixel's alpha.
    val stride = width*4
    var y = height - 1

    while y >= 1 do
      var j = 0

      while j < stride do
        pixels(y*stride + j) = (pixels(y*stride + j) - pixels((y - 1)*stride + j)).toByte
        j += 1

      y -= 1

    var k = stride - 1

    while k >= 4 do
      pixels(k) = (pixels(k) - pixels(k - 4)).toByte
      k -= 1

    pixels(3) = (pixels(3) - 255.toByte).toByte

    // Count symbol frequencies (with run-length compression of repeated pixels), then build and
    // write the codes.
    val freq0 = new Array[Int](256)
    val freq1 = new Array[Int](280)
    val freq2 = new Array[Int](256)
    val freq3 = new Array[Int](256)

    if !alpha then freq3(0) = 1

    val count = width*height
    var p = 0

    while p < count do
      freq0(u(pixels, p*4)) += 1
      freq1(u(pixels, p*4 + 1)) += 1
      freq2(u(pixels, p*4 + 2)) += 1

      if alpha then freq3(u(pixels, p*4 + 3)) += 1
      val run = runLength(pixels, p, count)

      if run > 0 then countRun(run, freq1)
      p += 1 + run

    val lengths0 = new Array[Int](256); val codes0 = new Array[Int](256)
    val lengths1 = new Array[Int](280); val codes1 = new Array[Int](280)
    val lengths2 = new Array[Int](256); val codes2 = new Array[Int](256)
    val lengths3 = new Array[Int](256); val codes3 = new Array[Int](256)

    WebpHuffmanEncoder.writeTree(writer, freq1, lengths1, codes1)
    WebpHuffmanEncoder.writeTree(writer, freq0, lengths0, codes0)
    WebpHuffmanEncoder.writeTree(writer, freq2, lengths2, codes2)

    if alpha then WebpHuffmanEncoder.writeTree(writer, freq3, lengths3, codes3)
    else WebpHuffmanEncoder.writeSingleEntry(writer, 0)

    WebpHuffmanEncoder.writeSingleEntry(writer, 1)

    // Write the image data: each pixel's channel codes packed green, red, blue, (alpha), then any
    // run of following identical pixels as a length symbol (a distance-1 backward reference).
    p = 0

    while p < count do
      val green = u(pixels, p*4 + 1)
      val red = u(pixels, p*4)
      val blue = u(pixels, p*4 + 2)
      var code = codes1(green).toLong
      var bits = lengths1(green)
      code |= codes0(red).toLong << bits; bits += lengths0(red)
      code |= codes2(blue).toLong << bits; bits += lengths2(blue)

      if alpha then
        val a = u(pixels, p*4 + 3)
        code |= codes3(a).toLong << bits
        bits += lengths3(a)

      writer.writeBits(code, bits)
      val run = runLength(pixels, p, count)

      if run > 0 then writeRun(writer, run, codes1, lengths1)
      p += 1 + run

    writer.bytes

  private inline def u(data: Array[Byte], index: Int): Int = data(index) & 0xff

  private def pixelsEqual(data: Array[Byte], a: Int, b: Int): Boolean =
    data(a*4) == data(b*4) && data(a*4 + 1) == data(b*4 + 1) &&
      data(a*4 + 2) == data(b*4 + 2) && data(a*4 + 3) == data(b*4 + 3)

  // The number of pixels immediately after `p` that equal it, capped at 4096.
  private def runLength(data: Array[Byte], p: Int, count: Int): Int =
    var run = 0

    while run < 4096 && p + 1 + run < count && pixelsEqual(data, p, p + 1 + run) do run += 1
    run

  private def countRun(run: Int, freq1: Array[Int]): Unit =
    if run <= 4 then freq1(256 + run - 1) += 1
    else freq1(256 + lengthToSymbol(run)(0)) += 1

  private def writeRun(writer: WebpBitWriter, run: Int, codes1: Array[Int], lengths1: Array[Int])
  :   Unit =

    if run <= 4 then
      val symbol = 256 + run - 1
      writer.writeBits(codes1(symbol).toLong, lengths1(symbol))
    else
      val (symbol, extraBits) = lengthToSymbol(run)
      writer.writeBits(codes1(256 + symbol).toLong, lengths1(256 + symbol))
      writer.writeBits((run.toLong - 1) & ((1L << extraBits) - 1), extraBits)

  // Maps a run length (> 4) to its length prefix symbol and count of extra bits.
  private def lengthToSymbol(length: Int): (Int, Int) =
    val value = length - 1
    val highestBit = 31 - Integer.numberOfLeadingZeros(value)
    val secondHighestBit = (value >>> (highestBit - 1)) & 1
    (2*highestBit + secondHighestBit, highestBit - 1)
