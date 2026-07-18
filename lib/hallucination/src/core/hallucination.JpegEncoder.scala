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

// A pure-Scala baseline JPEG encoder, ported from the jpeg-encoder crate (Apache-2.0/MIT). It
// converts an RGB raster to YCbCr, quantizes with the mozjpeg-derived Annex K tables scaled by a
// quality factor, runs the forward DCT, builds per-image optimized Huffman tables, and writes a
// baseline sequential JPEG with one non-interleaved scan per component. Chroma is subsampled 2x1x1
// (4:2:0) below quality 90 and kept full-resolution (4:4:4) at 90 and above.
private[hallucination] object JpegEncoder:
  // The zig-zag order: `ZigZag(k)` is the natural block index of the k-th coefficient written.
  private val ZigZag: Array[Int] = Array(
    0, 1, 8, 16, 9, 2, 3, 10,
    17, 24, 32, 25, 18, 11, 4, 5,
    12, 19, 26, 33, 40, 48, 41, 34,
    27, 20, 13, 6, 7, 14, 21, 28,
    35, 42, 49, 56, 57, 50, 43, 36,
    29, 22, 15, 23, 30, 37, 44, 51,
    58, 59, 52, 45, 38, 31, 39, 46,
    53, 60, 61, 54, 47, 55, 62, 63)

  // Annex K luminance and chrominance base quantization tables (natural order).
  private val LumaQuant: Array[Int] = Array(
    16, 11, 10, 16, 24, 40, 51, 61,
    12, 12, 14, 19, 26, 58, 60, 55,
    14, 13, 16, 24, 40, 57, 69, 56,
    14, 17, 22, 29, 51, 87, 80, 62,
    18, 22, 37, 56, 68, 109, 103, 77,
    24, 35, 55, 64, 81, 104, 113, 92,
    49, 64, 78, 87, 103, 121, 120, 101,
    72, 92, 95, 98, 112, 100, 103, 99)

  private val ChromaQuant: Array[Int] = Array(
    17, 18, 24, 47, 99, 99, 99, 99,
    18, 21, 26, 66, 99, 99, 99, 99,
    24, 26, 56, 99, 99, 99, 99, 99,
    47, 66, 99, 99, 99, 99, 99, 99,
    99, 99, 99, 99, 99, 99, 99, 99,
    99, 99, 99, 99, 99, 99, 99, 99,
    99, 99, 99, 99, 99, 99, 99, 99,
    99, 99, 99, 99, 99, 99, 99, 99)

  private def clamp(value: Int, low: Int, high: Int): Int = value.max(low).min(high)

  private def ceilDiv(x: Int, y: Int): Int = (x + y - 1)/y

  // The quantization divisors for a quality factor: the base table scaled and clamped to 1..255.
  private def scaledTable(base: Array[Int], quality: Int): Array[Int] =
    val q = clamp(quality, 1, 100)
    val scale = if q < 50 then 5000/q else 200 - q*2

    base.map: value =>
      clamp((value*scale + 50)/100, 1, 255)

  // ITU-R BT.601 RGB-to-YCbCr, scaled by 2^16 (matching the encoder's fixed-point conversion).
  private def rgbToYcbcr(r: Int, g: Int, b: Int): (Int, Int, Int) =
    val y = (19595*r + 38470*g + 7471*b + 0x7fff) >> 16
    val cb = (-11059*r - 21709*g + 32768*b + (128 << 16) + 0x7fff) >> 16
    val cr = (32768*r - 27439*g - 5329*b + (128 << 16) + 0x7fff) >> 16
    (clamp(y, 0, 255), clamp(cb, 0, 255), clamp(cr, 0, 255))

  private def quantizeValue(coefficient: Int, divisor0: Int): Int =
    val divisor = divisor0 << 3 // premultiplied by 8, since the DCT leaves an overall factor of 8
    val magnitude = if coefficient < 0 then -coefficient else coefficient
    val quotient = (magnitude + divisor/2)/divisor
    if coefficient < 0 then -quotient else quotient

  // Extracts one 8x8 block from a plane at the given stride, level-shifting samples by -128.
  private def block
    ( plane: Array[Byte], startX: Int, startY: Int, colStride: Int, rowStride: Int, width: Int )
  :   Array[Int] =

    val result = new Array[Int](64)
    var y = 0

    while y < 8 do
      var x = 0

      while x < 8 do
        val ix = startX + x*colStride
        val iy = startY + y*rowStride
        result(y*8 + x) = (plane(iy*width + ix) & 0xff) - 128
        x += 1

      y += 1

    result

  // Forward-transforms and quantizes a block, returning coefficients in zig-zag order.
  private def transform(samples: Array[Int], quant: Array[Int]): Array[Int] =
    JpegFdct.fdct(samples)
    val result = new Array[Int](64)
    var i = 0

    while i < 64 do
      val z = ZigZag(i)
      result(i) = quantizeValue(samples(z), quant(z))
      i += 1

    result

  def encode(raster: Raster): Data = encode(raster, 90)

  def encode(raster: Raster, quality: Int): Data =
    val width = raster.width
    val height = raster.height
    val hMax = if quality < 90 then 2 else 1
    val vMax = hMax

    // Full-resolution YCbCr planes, padded to whole MCUs and edge-extended.
    val bufWidth = ceilDiv(width, 8*hMax)*hMax*8
    val bufHeight = ceilDiv(height, 8*vMax)*vMax*8
    val planeY = new Array[Byte](bufWidth*bufHeight)
    val planeCb = new Array[Byte](bufWidth*bufHeight)
    val planeCr = new Array[Byte](bufWidth*bufHeight)

    var y = 0

    while y < bufHeight do
      val sy = y.min(height - 1)
      var x = 0

      while x < bufWidth do
        val chroma = raster(x.min(width - 1), sy)
        val (yv, cb, cr) = rgbToYcbcr(chroma.red, chroma.green, chroma.blue)
        val index = y*bufWidth + x
        planeY(index) = yv.toByte
        planeCb(index) = cb.toByte
        planeCr(index) = cr.toByte
        x += 1

      y += 1

    val lumaQuant = scaledTable(LumaQuant, quality)
    val chromaQuant = scaledTable(ChromaQuant, quality)
    val colsBlocks = ceilDiv(width, 8)
    val rowsBlocks = ceilDiv(height, 8)

    def blocksFor(plane: Array[Byte], hScale: Int, vScale: Int, quant: Array[Int])
    :   Array[Array[Int]] =

      val cols = ceilDiv(colsBlocks, hScale)
      val rows = ceilDiv(rowsBlocks, vScale)
      val result = new Array[Array[Int]](cols*rows)
      var blockY = 0

      while blockY < rows do
        var blockX = 0

        while blockX < cols do
          val samples =
            block(plane, blockX*8*hScale, blockY*8*vScale, hScale, vScale, bufWidth)

          result(blockY*cols + blockX) = transform(samples, quant)
          blockX += 1

        blockY += 1

      result

    val yBlocks = blocksFor(planeY, 1, 1, lumaQuant)
    val cbBlocks = blocksFor(planeCb, hMax, vMax, chromaQuant)
    val crBlocks = blocksFor(planeCr, hMax, vMax, chromaQuant)

    val (lumaDc, lumaAc) = optimizeTables(Array(yBlocks))
    val (chromaDc, chromaAc) = optimizeTables(Array(cbBlocks, crBlocks))

    val out = ji.ByteArrayOutputStream()

    def u8(value: Int): Unit = out.write(value & 0xff)
    def u16(value: Int): Unit = { u8(value >> 8); u8(value) }
    def marker(code: Int): Unit = { u8(0xff); u8(code) }

    marker(JpegMarker.Soi)

    // APP0 JFIF header.
    marker(0xe0)
    u16(16)

    "JFIF".foreach: char =>
      u8(char.toInt)

    u8(0)
    u8(1); u8(2)  // version 1.02
    u8(0)         // pixel-aspect-ratio units
    u16(1); u16(1)
    u8(0); u8(0)  // no thumbnail

    writeQuantization(u8, u16, marker, 0, lumaQuant)
    writeQuantization(u8, u16, marker, 1, chromaQuant)

    // SOF0: baseline; components Y (subsampled), Cb, Cr.
    marker(0xc0)
    u16(8 + 3*3)
    u8(8)
    u16(height); u16(width)
    u8(3)
    u8(1); u8((hMax << 4) | vMax); u8(0)
    u8(2); u8(0x11); u8(1)
    u8(3); u8(0x11); u8(1)

    writeHuffman(u8, u16, marker, 0, 0, lumaDc)
    writeHuffman(u8, u16, marker, 1, 0, lumaAc)
    writeHuffman(u8, u16, marker, 0, 1, chromaDc)
    writeHuffman(u8, u16, marker, 1, 1, chromaAc)

    writeScan(out, u8, u16, marker, 1, 0, 0, yBlocks, lumaDc, lumaAc)
    writeScan(out, u8, u16, marker, 2, 1, 1, cbBlocks, chromaDc, chromaAc)
    writeScan(out, u8, u16, marker, 3, 1, 1, crBlocks, chromaDc, chromaAc)

    marker(JpegMarker.Eoi)
    out.toByteArray.nn.immutable(using Unsafe)

  private def writeQuantization
    ( u8: Int => Unit, u16: Int => Unit, marker: Int => Unit, dest: Int, quant: Array[Int] )
  :   Unit =

    marker(JpegMarker.Dqt)
    u16(2 + 1 + 64)
    u8(dest)
    var k = 0

    while k < 64 do
      u8(quant(ZigZag(k)))
      k += 1

  private def writeHuffman
    ( u8: Int => Unit, u16: Int => Unit, marker: Int => Unit, tableClass: Int, dest: Int,
      table: JpegEncodeTable )
  :   Unit =

    marker(JpegMarker.Dht)
    u16(2 + 1 + 16 + table.values.length)
    u8((tableClass << 4) | dest)
    table.lengths.foreach(u8)
    table.values.foreach(u8)

  private def writeScan
    ( out: ji.ByteArrayOutputStream, u8: Int => Unit, u16: Int => Unit, marker: Int => Unit,
      id: Int, dcTable: Int, acTable: Int, blocks: Array[Array[Int]], dc: JpegEncodeTable,
      ac: JpegEncodeTable )
  :   Unit =

    marker(JpegMarker.Sos)
    u16(2 + 1 + 2 + 3)
    u8(1)
    u8(id)
    u8((dcTable << 4) | acTable)
    u8(0); u8(63); u8(0)

    val writer = JpegBitWriter(out)
    var prevDc = 0
    var index = 0

    while index < blocks.length do
      writeBlock(writer, blocks(index), prevDc, dc, ac)
      prevDc = blocks(index)(0)
      index += 1

    writer.flushBits()

  private def writeBlock
    ( writer: JpegBitWriter, block: Array[Int], prevDc: Int, dc: JpegEncodeTable,
      ac: JpegEncodeTable )
  :   Unit =

    val (dcSize, dcValue) = JpegHuffmanEncoder.getCode(block(0) - prevDc)
    writer.encodeValue(dcSize, dcSize, dcValue, dc)

    var zeroRun = 0
    var i = 1

    while i < 64 do
      if block(i) == 0 then zeroRun += 1
      else
        while zeroRun > 15 do
          writer.encode(0xf0, ac)
          zeroRun -= 16

        val (size, value) = JpegHuffmanEncoder.getCode(block(i))
        writer.encodeValue(size, (zeroRun << 4) | size, value, ac)
        zeroRun = 0

      i += 1

    if zeroRun > 0 then writer.encode(0x00, ac)

  // Gathers DC and AC symbol frequencies across the components sharing a table, then builds
  // optimized Huffman tables. The DC predictor resets at the start of each component.
  private def optimizeTables(components: Array[Array[Array[Int]]])
  :   (JpegEncodeTable, JpegEncodeTable) =

    val dcFreq = new Array[Int](257)
    val acFreq = new Array[Int](257)
    dcFreq(256) = 1
    acFreq(256) = 1

    components.foreach: blocks =>
      var prevDc = 0
      var index = 0

      while index < blocks.length do
        val block = blocks(index)
        dcFreq(JpegHuffmanEncoder.numBits(block(0) - prevDc)) += 1
        prevDc = block(0)

        var zeroRun = 0
        var i = 1

        while i < 64 do
          if block(i) == 0 then zeroRun += 1
          else
            while zeroRun > 15 do
              acFreq(0xf0) += 1
              zeroRun -= 16

            acFreq((zeroRun << 4) | JpegHuffmanEncoder.numBits(block(i))) += 1
            zeroRun = 0

          i += 1

        if zeroRun > 0 then acFreq(0) += 1
        index += 1

    (JpegHuffmanEncoder.newOptimized(dcFreq), JpegHuffmanEncoder.newOptimized(acFreq))
