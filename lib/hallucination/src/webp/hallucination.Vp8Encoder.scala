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

import scala.math

import java.io as ji

import anticipation.*
import vacuous.*

import scala.caps

import Vp8Tables.*

// The VP8 lossy (keyframe) encoder, ported from image-rs/image-webp (`src/lossy/encoder.rs`,
// MIT/Apache-2.0), per RFC 6386. It uses the reference's fixed strategy: whole-macroblock DC
// prediction for luma and chroma, a single coefficient partition, and no segmentation, loop-filter
// adjustments or token-probability updates. For each macroblock it predicts, subtracts, forward-
// transforms and quantizes the residual; it also reconstructs each block (dequantize + inverse
// transform) so its prediction borders match what the decoder will see. Produces a raw VP8
// bitstream (the payload of a "VP8 " chunk).
private[hallucination] object Vp8Encoder:
  private val LumaStride = Vp8Predict.LumaStride
  private val ChromaStride = Vp8Predict.ChromaStride

  // Builds the raw VP8 keyframe bitstream from an RGB(A) raster at the given quality (0–100).
  def encode(raster: Raster, quality: Int): Data =
    Encoder(raster, quality).run()

  // Writes actual−predicted for one 4×4 subblock at (bx, by) into `out(outOffset..)`. Hosted on
  // the companion, not `Encoder`: its arguments are `Encoder`-derived arrays, which cannot be
  // passed to a method on the (exclusive) encoder itself.
  private def difference
    ( predicted: scala.Array[Int], plane: scala.Array[Int], planeWidth: Int, stride: Int, px: Int, py: Int,
        bx: Int, by: Int, out: scala.Array[Int], outOffset: Int )
  :   Unit =

    var y = 0

    while y < 4 do
      var x = 0

      while x < 4 do
        val predictedValue = predicted((1 + by*4 + y)*stride + 1 + bx*4 + x)
        val actual = plane((py + by*4 + y)*planeWidth + px + bx*4 + x)
        writable(out)(outOffset + y*4 + x) = actual - predictedValue
        x += 1

      y += 1

  private final class Encoder(raster: Raster, quality: Int) extends caps.Mutable:
    private val width = raster.width
    private val height = raster.height
    private val mbWidth = (width + 15)/16
    private val mbHeight = (height + 15)/16
    private val lumaWidth = mbWidth*16
    private val chromaWidth = mbWidth*8

    private var ybuf: scala.Array[Int]^ = new scala.Array[Int](lumaWidth*mbHeight*16)
    private var ubuf: scala.Array[Int]^ = new scala.Array[Int](chromaWidth*mbHeight*8)
    private var vbuf: scala.Array[Int]^ = new scala.Array[Int](chromaWidth*mbHeight*8)

    private val quantIndex = 127 - quality.max(0).min(100)*127/100
    private val ydc = dcQuant.readable(quantIndex)
    private val yac = acQuant.readable(quantIndex)
    private val y2dc = dcQuant.readable(quantIndex)*2
    private val y2ac = (acQuant.readable(quantIndex)*155/100).max(8)
    private val uvdc = dcQuant.readable(quantIndex)
    private val uvac = acQuant.readable(quantIndex)

    private val tokenProbs = coeffProbs

    private var header: Vp8BoolEncoder^ = Vp8BoolEncoder()
    private var partition: Vp8BoolEncoder^ = Vp8BoolEncoder()

    private var leftBorderY: scala.Array[Int]^ = scala.Array.fill(17)(129)
    private var leftBorderU: scala.Array[Int]^ = scala.Array.fill(9)(129)
    private var leftBorderV: scala.Array[Int]^ = scala.Array.fill(9)(129)
    private var topBorderY: scala.Array[Int]^ = scala.Array.fill(lumaWidth + 4)(127)
    private var topBorderU: scala.Array[Int]^ = scala.Array.fill(chromaWidth)(127)
    private var topBorderV: scala.Array[Int]^ = scala.Array.fill(chromaWidth)(127)

    // Per-column and per-row non-zero "complexity" carried between macroblocks: y2, 4×y, 2×u, 2×v.
    // Built with a loop: nested `Array.fill` trips ClassTag synthesis under cc.
    private val topComplexity: AnyRef =
      val rows = new scala.Array[scala.Array[Int]](mbWidth)
      var i = 0
      while i < rows.length do { rows(i) = new scala.Array[Int](9); i += 1 }
      rows.asInstanceOf[AnyRef]
    
    private inline def topComplexityRows(index: Int): scala.Array[Int] =
      topComplexity.asInstanceOf[scala.Array[scala.Array[Int]]](index)
    private var leftComplexity: scala.Array[Int]^ = new scala.Array[Int](9)

    update def run(): Data =
      rgbToYuv()
      encodeCompressedHeader()

      var mby = 0

      while mby < mbHeight do
        leftComplexity = new scala.Array[Int](9)
        leftBorderY = scala.Array.fill(17)(129)
        leftBorderU = scala.Array.fill(9)(129)
        leftBorderV = scala.Array.fill(9)(129)
        var mbx = 0

        while mbx < mbWidth do
          // Macroblock header: DC luma mode, DC chroma mode (no segment or skip flag).
          header.writeTree(keyframeYmodeTree.asInstanceOf[scala.Array[Int]], keyframeYmodeProbs.asInstanceOf[scala.Array[Int]], 0, DcPred)
          header.writeTree(keyframeUvModeTree.asInstanceOf[scala.Array[Int]], keyframeUvModeProbs.asInstanceOf[scala.Array[Int]], 0, DcPred)

          val yBlocks = transformLuma(mbx, mby)
          val (uBlocks, vBlocks) = transformChroma(mbx, mby)
          encodeResidual(mbx, yBlocks, uBlocks, vBlocks)
          mbx += 1

        mby += 1

      val headerBytes = header.bytes
      wrap(headerBytes, partition.bytes)

    // The full VP8 chunk: uncompressed header (tag, magic, dimensions), then the compressed header
    // and coefficient partition.
    private def wrap(headerBytes: Data, partitionBytes: Data): Data =
      val out = ji.ByteArrayOutputStream()
      val tag = (headerBytes.length << 5) | (1 << 4) // for_display; keyframe, version 0
      out.write(tag & 0xff); out.write((tag >>> 8) & 0xff); out.write((tag >>> 16) & 0xff)
      out.write(0x9d); out.write(0x01); out.write(0x2a)
      val w = width & 0x3fff; val h = height & 0x3fff
      out.write(w & 0xff); out.write((w >>> 8) & 0xff)
      out.write(h & 0xff); out.write((h >>> 8) & 0xff)
      out.write(Array.unsafeJvm(headerBytes))
      out.write(Array.unsafeJvm(partitionBytes))
      Array.unsafeFrozen(out.toByteArray.nn)

    private update def encodeCompressedHeader(): Unit =
      header.writeLiteral(1, 0) // colour space
      header.writeLiteral(1, 0) // pixel type
      header.writeFlag(false)   // segments disabled
      header.writeFlag(false)   // normal (not simple) filter
      header.writeLiteral(6, 63) // filter level
      header.writeLiteral(3, 7)  // sharpness
      header.writeFlag(false)   // no loop-filter adjustments
      header.writeLiteral(2, 0) // one partition

      header.writeLiteral(7, quantIndex)
      var i = 0

      while i < 5 do
        header.writeOptionalSigned(4, Unset)
        i += 1

      header.writeLiteral(1, 0) // refresh entropy probabilities

      // No token-probability updates: signal "no update" for every probability.
      i = 0

      while i < 1056 do
        header.writeBool(false, coeffUpdateProbs.readable(i))
        i += 1

      header.writeLiteral(1, 0) // no macroblock skip-coefficient probability

    private update def encodeResidual
      ( mbx: Int, yBlocks: scala.Array[Int], uBlocks: scala.Array[Int], vBlocks: scala.Array[Int] )
    :   Unit =

      // Y2: the DC coefficient of each of the 16 luma subblocks, Walsh-Hadamard transformed.
      val coeffs0: scala.Array[Int]^ = new scala.Array[Int](16)
      var k = 0

      while k < 16 do
        coeffs0(k) = yBlocks(16*k)
        k += 1

      Vp8Transform.wht4x4(coeffs0)

      val y2Complexity = leftComplexity(0) + topComplexityRows(mbx)(0)
      val y2n = encodeCoefficients(coeffs0, 0, 1, y2Complexity, y2dc, y2ac) // plane Y2
      leftComplexity(0) = if y2n then 1 else 0
      writable(topComplexityRows(mbx))(0) = if y2n then 1 else 0

      var y = 0

      while y < 4 do
        var left = leftComplexity(y + 1)
        var x = 0

        while x < 4 do
          val i = x + y*4
          val n = encodeCoefficients(yBlocks, i*16, 0, left + topComplexityRows(mbx)(x + 1), ydc, yac)
          left = if n then 1 else 0
          writable(topComplexityRows(mbx))(x + 1) = if n then 1 else 0
          x += 1

        leftComplexity(y + 1) = left
        y += 1

      encodeChromaPlane(mbx, uBlocks, 5)
      encodeChromaPlane(mbx, vBlocks, 7)

    private update def encodeChromaPlane(mbx: Int, blocks: scala.Array[Int], base: Int): Unit =
      var y = 0

      while y < 2 do
        var left = leftComplexity(y + base)
        var x = 0

        while x < 2 do
          val i = x + y*2

          val n =
            encodeCoefficients(blocks, i*16, 2, left + topComplexityRows(mbx)(x + base), uvdc, uvac)

          left = if n then 1 else 0
          writable(topComplexityRows(mbx))(x + base) = if n then 1 else 0
          x += 1

        leftComplexity(y + base) = left
        y += 1

    // Quantizes one 4×4 DCT block and entropy-codes its tokens; returns whether it was non-empty.
    private update def encodeCoefficients
      ( block: scala.Array[Int], offset: Int, plane: Int, complexity0: Int, dcq: Int, acq: Int )
    :   Boolean =

      val firstCoeff = if plane == 0 then 1 else 0
      var complexity = complexity0

      // Quantize into zigzag order.
      val zigzagBlock: scala.Array[Int]^ = new scala.Array[Int](16)
      var i = firstCoeff

      while i < 16 do
        val zi = zigzag.readable(i)
        zigzagBlock(i) = block(offset + zi)/(if zi > 0 then acq else dcq)
        i += 1

      // The end-of-block index: one past the last non-zero coefficient.
      var last = 15

      while last >= firstCoeff && zigzagBlock(last) == 0 do last -= 1
      val endOfBlock = last + 1

      var skipEob = false
      i = firstCoeff

      while i < endOfBlock do
        val coeff = zigzagBlock(i)
        val band = coeffBands.readable(i)
        val probOffset = coeffIndex(plane, band, complexity, 0)
        val startIndex = if skipEob then 2 else 0
        val absValue = math.abs(coeff)

        val token =
          if absValue == 0 then
            partition.writeTree(dctTokenTree.asInstanceOf[scala.Array[Int]], tokenProbs.asInstanceOf[scala.Array[Int]], probOffset, Dct0, startIndex)
            skipEob = true
            Dct0
          else if absValue <= 4 then
            partition.writeTree(dctTokenTree.asInstanceOf[scala.Array[Int]], tokenProbs.asInstanceOf[scala.Array[Int]], probOffset, absValue, startIndex)
            skipEob = false
            absValue
          else
            val category =
              if absValue <= 6 then DctCat1 else if absValue <= 10 then DctCat2
              else if absValue <= 18 then DctCat3 else if absValue <= 34 then DctCat4
              else if absValue <= 66 then DctCat5 else DctCat6

            partition.writeTree(dctTokenTree.asInstanceOf[scala.Array[Int]], tokenProbs.asInstanceOf[scala.Array[Int]], probOffset, category, startIndex)
            val extra = absValue - dctCatBase.readable(category - DctCat1)
            var mask = if category == DctCat6 then 1 << 10 else 1 << (category - DctCat1)
            var c = (category - DctCat1)*12

            while probDctCat.readable(c) != 0 do
              partition.writeBool((extra & mask) > 0, probDctCat.readable(c))
              mask >>= 1
              c += 1

            skipEob = false
            category

        if token != Dct0 then partition.writeFlag(coeff < 0)
        complexity = if token == Dct0 then 0 else if token == 1 then 1 else 2
        i += 1

      if endOfBlock < 16 then
        val band = coeffBands.readable(firstCoeff.max(endOfBlock))

        partition.writeTree(dctTokenTree.asInstanceOf[scala.Array[Int]], tokenProbs.asInstanceOf[scala.Array[Int]], coeffIndex(plane, band, complexity, 0),
            DctEob)

      endOfBlock > 0

    // Converts the RGB raster to macroblock-padded Y/U/V planes (edge-clamped into the padding),
    // downsampling chroma by averaging each 2×2 block.
    private update def rgbToYuv(): Unit =
      inline def rgb(sx: Int, sy: Int): Chroma =
        val x = sx.min(width - 1)
        val y = sy.min(height - 1)
        raster.descriptor.chroma(raster.word(y*width + x))

      var y = 0

      while y < mbHeight*16 do
        var x = 0

        while x < lumaWidth do
          val c = rgb(x, y)
          ybuf(y*lumaWidth + x) = rgbToY(c.red, c.green, c.blue)
          x += 1

        y += 1

      var cy = 0

      while cy < mbHeight*8 do
        var cx = 0

        while cx < chromaWidth do
          val a = rgb(cx*2, cy*2); val b = rgb(cx*2 + 1, cy*2)
          val d = rgb(cx*2, cy*2 + 1); val e = rgb(cx*2 + 1, cy*2 + 1)
          ubuf(cy*chromaWidth + cx) = chromaAvg(a, b, d, e, uCoeffs)
          vbuf(cy*chromaWidth + cx) = chromaAvg(a, b, d, e, vCoeffs)
          cx += 1

        cy += 1

    private val uCoeffs = (-9719, -19081, 28800)
    private val vCoeffs = (28800, -24116, -4684)

    private def rgbToY(r: Int, g: Int, b: Int): Int =
      (16839*r + 33059*g + 6420*b + 32768 + (16 << 16)) >> 16

    private def chromaRaw(c: Chroma, coeffs: (Int, Int, Int)): Int =
      coeffs(0)*c.red + coeffs(1)*c.green + coeffs(2)*c.blue + (128 << 16)

    private def chromaAvg(a: Chroma, b: Chroma, d: Chroma, e: Chroma, coeffs: (Int, Int, Int))
    :   Int =

      (chromaRaw(a, coeffs) + chromaRaw(b, coeffs) + chromaRaw(d, coeffs) +
        chromaRaw(e, coeffs)) >> (16 + 2)

    // Predicts (DC), differences and forward-transforms the luma macroblock, returning the 16
    // subblocks' un-quantized DCT coefficients. Also reconstructs the block to update the borders.
    private update def transformLuma(mbx: Int, mby: Int): scala.Array[Int]^ =
      val predicted = Vp8Predict.createBorderLuma(mbx, mby, mbWidth, topBorderY, leftBorderY)
      Vp8Predict.predictDcpred(predicted, 16, LumaStride, mby != 0, mbx != 0)

      val blocks: scala.Array[Int]^ = new scala.Array[Int](256)
      var by = 0

      while by < 4 do
        var bx = 0

        while bx < 4 do
          val i = by*4 + bx
          difference(predicted, ybuf, lumaWidth, LumaStride, mbx*16, mby*16, bx, by, blocks, i*16)
          Vp8Transform.dct4x4Slice(blocks, i*16)
          bx += 1

        by += 1

      reconstructLuma(predicted, blocks, mbx)
      blocks

    private update def reconstructLuma(predicted: scala.Array[Int], blocks: scala.Array[Int], mbx: Int): Unit =
      val recon = blocks.clone()
      val c0: scala.Array[Int]^ = new scala.Array[Int](16)
      var k = 0

      while k < 16 do
        c0(k) = recon(16*k)
        k += 1

      Vp8Transform.wht4x4(c0)

      k = 0

      while k < 16 do
        c0(k) = c0(k)/(if k > 0 then y2ac else y2dc)
        k += 1

      k = 0

      while k < 16 do
        recon(16*k) = 0
        var i = 1

        while i < 16 do
          recon(16*k + i) = recon(16*k + i)/yac
          i += 1

        k += 1

      k = 0

      while k < 16 do
        c0(k) = c0(k)*(if k > 0 then y2ac else y2dc)
        k += 1

      Vp8Transform.iwht4x4(c0)

      k = 0

      while k < 16 do
        var i = 1

        while i < 16 do
          recon(16*k + i) = recon(16*k + i)*yac
          i += 1

        recon(16*k) = c0(k)
        Vp8Transform.idct4x4(recon, 16*k)
        k += 1

      var by = 0

      while by < 4 do
        var bx = 0

        while bx < 4 do
          Vp8Predict.addResidue(predicted, recon, (by*4 + bx)*16, 1 + by*4, 1 + bx*4, LumaStride)
          bx += 1

        by += 1

      k = 0

      while k < 17 do
        leftBorderY(k) = predicted(k*LumaStride + 16)
        k += 1

      k = 0

      while k < 16 do
        topBorderY(mbx*16 + k) = predicted(16*LumaStride + k + 1)
        k += 1

    private update def transformChroma(mbx: Int, mby: Int): (scala.Array[Int], scala.Array[Int]) =
      (transformChromaPlane(mbx, mby, u = true), transformChromaPlane(mbx, mby, u = false))

    // Selects the U or V plane and border fields itself: passing `this`-derived arrays as
    // arguments to a method on `this` is a separation failure while `this` is exclusive.
    private update def transformChromaPlane(mbx: Int, mby: Int, u: Boolean): scala.Array[Int]^ =
      val plane = if u then ubuf else vbuf
      val topBorder = if u then topBorderU else topBorderV
      val leftBorder = if u then leftBorderU else leftBorderV
      val predicted = Vp8Predict.createBorderChroma(mbx, mby, topBorder, leftBorder)
      Vp8Predict.predictDcpred(predicted, 8, ChromaStride, mby != 0, mbx != 0)

      val blocks: scala.Array[Int]^ = new scala.Array[Int](64)
      var by = 0

      while by < 2 do
        var bx = 0

        while bx < 2 do
          val i = by*2 + bx

          difference(predicted, plane, chromaWidth, ChromaStride, mbx*8, mby*8, bx, by, blocks,
              i*16)

          Vp8Transform.dct4x4Slice(blocks, i*16)
          bx += 1

        by += 1

      // Reconstruct: quantize then dequantize (by uvdc/uvac), inverse-transform, add back.
      val recon = blocks.clone()
      var k = 0

      while k < 4 do
        var i = 0

        while i < 16 do
          val q = if i == 0 then uvdc else uvac
          recon(16*k + i) = (recon(16*k + i)/q)*q
          i += 1

        Vp8Transform.idct4x4(recon, 16*k)
        k += 1

      by = 0

      while by < 2 do
        var bx = 0

        while bx < 2 do
          Vp8Predict.addResidue(predicted, recon, (by*2 + bx)*16, 1 + by*4, 1 + bx*4, ChromaStride)
          bx += 1

        by += 1

      k = 0

      while k < 9 do
        leftBorder(k) = predicted(k*ChromaStride + 8)
        k += 1

      k = 0

      while k < 8 do
        topBorder(mbx*8 + k) = predicted(8*ChromaStride + k + 1)
        k += 1

      blocks

