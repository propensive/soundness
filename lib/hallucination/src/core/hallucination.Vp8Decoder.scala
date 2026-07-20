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

import anticipation.*
import contingency.*

import RasterError.Reason
import Vp8Tables.*

// The VP8 lossy (keyframe) decoder, ported from image-rs/image-webp (`src/lossy/mod.rs`,
// MIT/Apache-2.0), per RFC 6386. It parses the frame header, then for each macroblock reads the
// prediction modes and dequantized DCT coefficients, reconstructs the Y/U/V planes by intra
// prediction plus inverse transform, and finally applies the loop filter. Only keyframes are
// supported, as in the reference (and as WebP requires). Inter-frame prediction and motion vectors
// are absent.
private[hallucination] object Vp8Decoder:
  def decode(data: Data, start: Int, end: Int): Vp8Frame raises RasterError =
    Decoder(data, start, end).run()

  private final class Segment:
    var ydc = 0; var yac = 0; var y2dc = 0; var y2ac = 0; var uvdc = 0; var uvac = 0
    var quantizer = 0; var loopfilter = 0; var deltaValues = false

  // The bottom/right edge state carried from already-decoded neighbours: per-subblock non-zero
  // "complexity" (laid out y2, 4×y, 2×u, 2×v) and the four B_PRED modes.
  private final class Edge:
    val complexity = new Array[Int](9)
    val bpred = new Array[Int](4)

  private final class Macroblock:
    val bpred = new Array[Int](16)
    var lumaMode = DcPred
    var chromaMode = DcPred
    var segmentId = 0
    var coeffsSkipped = false
    var nonZeroDct = false

  private final class Decoder(data: Data, start: Int, end: Int):
    private var position = start
    private var mbWidth = 0
    private var mbHeight = 0

    private var width = 0
    private var height = 0
    private var bufferWidth = 0

    private var ybuf: Array[Int] = new Array(0)
    private var ubuf: Array[Int] = new Array(0)
    private var vbuf: Array[Int] = new Array(0)

    private var filterType = false // true: simple filter; false: normal filter
    private var filterLevel = 0
    private var sharpnessLevel = 0

    private var segmentsEnabled = false
    private var segmentsUpdateMap = false
    private val segments = Array.fill(4)(Segment())
    private val segmentProbs = Array(255, 255, 255)

    private var loopFilterAdjustments = false
    private val refDelta = new Array[Int](4)
    private val modeDelta = new Array[Int](4)

    private var numPartitions = 1
    private val partitions = new Array[Vp8Bool](8)
    private var bool: Vp8Bool = Vp8Bool(data, start, start)

    private val tokenProbs = coeffProbs.clone()
    private var probSkipFalse = -1 // −1 means no skip probability

    private var top: Array[Edge] = Array()
    private var left = Edge()

    private var topBorderY: Array[Int] = Array()
    private var leftBorderY: Array[Int] = Array()
    private var topBorderU: Array[Int] = Array()
    private var leftBorderU: Array[Int] = Array()
    private var topBorderV: Array[Int] = Array()
    private var leftBorderV: Array[Int] = Array()

    private var macroblocks: Array[Macroblock] = Array()

    private inline def u8(index: Int): Int = data(index) & 0xff

    private def u16le(index: Int): Int = u8(index) | (u8(index + 1) << 8)
    private def u24le(index: Int): Int = u16le(index) | (u8(index + 2) << 16)

    def run(): Vp8Frame raises RasterError =
      readFrameHeader()
      var mby = 0

      while mby < mbHeight do
        val p = mby % numPartitions
        left = Edge()
        var mbx = 0

        while mbx < mbWidth do
          val mb = readMacroblockHeader(mbx)
          val blocks = new Array[Int](384)

          if !mb.coeffsSkipped then readResidualData(mb, mbx, p, blocks)
          else
            if mb.lumaMode != BPred then
              left.complexity(0) = 0
              top(mbx).complexity(0) = 0

            var i = 1

            while i < 9 do
              left.complexity(i) = 0
              top(mbx).complexity(i) = 0
              i += 1

          intraPredictLuma(mbx, mby, mb, blocks)
          intraPredictChroma(mbx, mby, mb, blocks)
          macroblocks(mby*mbWidth + mbx) = mb
          mbx += 1

        leftBorderY = Array.fill(1 + 16)(129)
        leftBorderU = Array.fill(1 + 8)(129)
        leftBorderV = Array.fill(1 + 8)(129)
        mby += 1

      mby = 0

      while mby < mbHeight do
        var mbx = 0

        while mbx < mbWidth do
          loopFilter(mbx, mby, macroblocks(mby*mbWidth + mbx))
          mbx += 1

        mby += 1

      Vp8Frame(width, height, bufferWidth, ybuf, ubuf, vbuf)

    private def readFrameHeader(): Unit raises RasterError =
      val tag = u24le(position)
      position += 3

      if (tag & 1) != 0 then abort(RasterError(Webp(), Reason.UnsupportedVariant)) // not a keyframe
      val firstPartitionSize = tag >> 5

      if u8(position) != 0x9d || u8(position + 1) != 0x01 || u8(position + 2) != 0x2a
      then abort(RasterError(Webp(), Reason.BadSignature))

      position += 3

      width = u16le(position) & 0x3fff; position += 2
      height = u16le(position) & 0x3fff; position += 2

      if width == 0 || height == 0 then abort(RasterError(Webp(), Reason.UnsupportedVariant))

      mbWidth = (width + 15)/16
      mbHeight = (height + 15)/16
      bufferWidth = mbWidth*16

      top = Array.fill(mbWidth)(Edge())
      left = Edge()
      macroblocks = Array.fill(mbWidth*mbHeight)(Macroblock())

      ybuf = new Array[Int](mbWidth*16*mbHeight*16)
      ubuf = new Array[Int](mbWidth*8*mbHeight*8)
      vbuf = new Array[Int](mbWidth*8*mbHeight*8)

      topBorderY = Array.fill(width + 4 + 16)(127)
      leftBorderY = Array.fill(1 + 16)(129)
      topBorderU = Array.fill(8*mbWidth)(127)
      leftBorderU = Array.fill(1 + 8)(129)
      topBorderV = Array.fill(8*mbWidth)(127)
      leftBorderV = Array.fill(1 + 8)(129)

      val headerStart = position
      bool = Vp8Bool(data, headerStart, headerStart + firstPartitionSize)
      position += firstPartitionSize

      val colorSpace = bool.literal(1)
      bool.literal(1) // pixel type

      if colorSpace != 0 then abort(RasterError(Webp(), Reason.UnsupportedVariant))

      segmentsEnabled = bool.flag

      if segmentsEnabled then readSegmentUpdates()

      filterType = bool.flag
      filterLevel = bool.literal(6)
      sharpnessLevel = bool.literal(3)

      loopFilterAdjustments = bool.flag

      if loopFilterAdjustments then readLoopFilterAdjustments()

      numPartitions = 1 << bool.literal(2)
      initPartitions()

      readQuantizationIndices()
      bool.literal(1) // refresh entropy probabilities (ignored for keyframes)
      updateTokenProbabilities()

      probSkipFalse = if bool.literal(1) == 1 then bool.literal(8) else -1

    private def initPartitions(): Unit =
      if numPartitions > 1 then
        val sizesStart = position
        position += 3*(numPartitions - 1)
        var partitionStart = position

        var i = 0

        while i < numPartitions - 1 do
          val size = u24le(sizesStart + i*3)
          partitions(i) = Vp8Bool(data, partitionStart, partitionStart + size)
          partitionStart += size
          i += 1

        partitions(numPartitions - 1) = Vp8Bool(data, partitionStart, end)
      else
        partitions(0) = Vp8Bool(data, position, end)

    private def readSegmentUpdates(): Unit =
      segmentsUpdateMap = bool.flag
      val updateData = bool.flag

      if updateData then
        val absoluteValues = bool.flag
        var i = 0

        while i < 4 do
          segments(i).deltaValues = !absoluteValues
          i += 1

        i = 0

        while i < 4 do
          segments(i).quantizer = bool.optionalSigned(7)
          i += 1

        i = 0

        while i < 4 do
          segments(i).loopfilter = bool.optionalSigned(6)
          i += 1

      if segmentsUpdateMap then
        var i = 0

        while i < 3 do
          segmentProbs(i) = if bool.flag then bool.literal(8) else 255
          i += 1

    private def readLoopFilterAdjustments(): Unit =
      if bool.flag then
        var i = 0

        while i < 4 do
          refDelta(i) = bool.optionalSigned(6)
          i += 1

        i = 0

        while i < 4 do
          modeDelta(i) = bool.optionalSigned(6)
          i += 1

    private def readQuantizationIndices(): Unit =
      def dcQuant(index: Int): Int = Vp8Tables.dcQuant(index.max(0).min(127))
      def acQuant(index: Int): Int = Vp8Tables.acQuant(index.max(0).min(127))

      val yacAbs = bool.literal(7)
      val ydcDelta = bool.optionalSigned(4)
      val y2dcDelta = bool.optionalSigned(4)
      val y2acDelta = bool.optionalSigned(4)
      val uvdcDelta = bool.optionalSigned(4)
      val uvacDelta = bool.optionalSigned(4)

      val n = if segmentsEnabled then 4 else 1
      var i = 0

      while i < n do
        val base =
          if segmentsEnabled then
            if segments(i).deltaValues then segments(i).quantizer + yacAbs
            else segments(i).quantizer
          else
            yacAbs

        segments(i).ydc = dcQuant(base + ydcDelta)
        segments(i).yac = acQuant(base)
        segments(i).y2dc = dcQuant(base + y2dcDelta)*2
        segments(i).y2ac = acQuant(base + y2acDelta)*155/100
        segments(i).uvdc = dcQuant(base + uvdcDelta)
        segments(i).uvac = acQuant(base + uvacDelta)

        if segments(i).y2ac < 8 then segments(i).y2ac = 8
        if segments(i).uvdc > 132 then segments(i).uvdc = 132
        i += 1

    private def updateTokenProbabilities(): Unit =
      var i = 0

      while i < 4 do
        var j = 0

        while j < 8 do
          var k = 0

          while k < 3 do
            var t = 0

            while t < 11 do
              if bool.bool(coeffUpdateProbs(coeffIndex(i, j, k, t)))
              then tokenProbs(coeffIndex(i, j, k, t)) = bool.literal(8)

              t += 1

            k += 1

          j += 1

        i += 1


    private def readMacroblockHeader(mbx: Int): Macroblock =
      val mb = Macroblock()

      if segmentsEnabled && segmentsUpdateMap then
        mb.segmentId = bool.tree(segmentIdTree, segmentProbs, 0)

      mb.coeffsSkipped = if probSkipFalse >= 0 then bool.bool(probSkipFalse) else false
      mb.lumaMode = bool.tree(keyframeYmodeTree, keyframeYmodeProbs, 0)

      if mb.lumaMode == BPred then
        var y = 0

        while y < 4 do
          var x = 0

          while x < 4 do
            val topMode = top(mbx).bpred(x)
            val leftMode = left.bpred(y)

            val intra = bool.tree(keyframeBpredModeTree, keyframeBpredModeProbs,
                (topMode*10 + leftMode)*9)

            mb.bpred(x + y*4) = intra
            top(mbx).bpred(x) = intra
            left.bpred(y) = intra
            x += 1

          y += 1
      else
        val mode = intraFromLuma(mb.lumaMode)
        var i = 0

        while i < 4 do
          mb.bpred(12 + i) = mode
          left.bpred(i) = mode
          i += 1

      mb.chromaMode = bool.tree(keyframeUvModeTree, keyframeUvModeProbs, 0)
      var i = 0

      while i < 4 do
        top(mbx).bpred(i) = mb.bpred(12 + i)
        i += 1

      mb

    // Maps a whole-macroblock luma mode to the subblock mode stored as neighbour context.
    private def intraFromLuma(luma: Int): Int = luma match
      case `DcPred` => BDcPred
      case `VPred`  => BVePred
      case `HPred`  => BHePred
      case _        => BTmPred

    // Reads one 4×4 block's dequantized coefficients into `block(offset..)`; returns whether
    // it has any non-zero coefficient. For the Y-after-Y2 plane the DC (index 0) is untouched.
    private def readCoefficients
      ( block: Array[Int], offset: Int, p: Int, plane: Int, complexity: Int, dcq: Int, acq: Int )
    :   Boolean =

      val decoder = partitions(p)
      val firstCoeff = if plane == 0 then 1 else 0 // YCoeff1 skips the DC
      var context = complexity
      var hasCoefficients = false
      var skip = false
      var i = firstCoeff
      var stop = false

      while i < 16 && !stop do
        val band = coeffBands(i)
        val probOffset = coeffIndex(plane, band, context, 0)
        val token = decoder.tree(dctTokenTree, tokenProbs, probOffset, 2*(if skip then 1 else 0))

        if token == DctEob then stop = true
        else if token == Dct0 then
          skip = true
          hasCoefficients = true
          context = 0
          i += 1
        else
          var absValue =
            if token <= 4 then token
            else
              val category = token - DctCat1
              var extra = 0
              var c = category*12

              while probDctCat(c) != 0 do
                extra = extra + extra + (if decoder.bool(probDctCat(c)) then 1 else 0)
                c += 1

              dctCatBase(category) + extra

          skip = false
          context = if absValue == 0 then 0 else if absValue == 1 then 1 else 2

          if decoder.flag then absValue = -absValue

          val zigzag = zigzagTable(i)
          block(offset + zigzag) = absValue*(if zigzag > 0 then acq else dcq)
          hasCoefficients = true
          i += 1

      hasCoefficients

    private def zigzagTable(i: Int): Int = zigzag(i)

    private def readResidualData(mb: Macroblock, mbx: Int, p: Int, blocks: Array[Int]): Unit =
      val sindex = mb.segmentId
      var plane = if mb.lumaMode == BPred then 3 else 1 // YCoeff0 or Y2

      if plane == 1 then
        val complexity = top(mbx).complexity(0) + left.complexity(0)
        val block = new Array[Int](16)

        val n = readCoefficients(block, 0, p, 1, complexity, segments(sindex).y2dc,
            segments(sindex).y2ac)

        left.complexity(0) = if n then 1 else 0
        top(mbx).complexity(0) = if n then 1 else 0
        Vp8Transform.iwht4x4(block)
        var k = 0

        while k < 16 do
          blocks(16*k) = block(k)
          k += 1

        plane = 0 // YCoeff1

      var y = 0

      while y < 4 do
        var leftComplexity = left.complexity(y + 1)
        var x = 0

        while x < 4 do
          val i = x + y*4
          val complexity = top(mbx).complexity(x + 1) + leftComplexity

          val n = readCoefficients(blocks, i*16, p, plane, complexity, segments(sindex).ydc,
              segments(sindex).yac)

          if blocks(i*16) != 0 || n then
            mb.nonZeroDct = true
            Vp8Transform.idct4x4(blocks, i*16)

          leftComplexity = if n then 1 else 0
          top(mbx).complexity(x + 1) = if n then 1 else 0
          x += 1

        left.complexity(y + 1) = leftComplexity
        y += 1

      for j <- List(5, 7).stdlib do
        var yy = 0

        while yy < 2 do
          var leftComplexity = left.complexity(yy + j)
          var x = 0

          while x < 2 do
            val i = x + yy*2 + (if j == 5 then 16 else 20)
            val complexity = top(mbx).complexity(x + j) + leftComplexity

            val n = readCoefficients(blocks, i*16, p, 2, complexity, segments(sindex).uvdc,
                segments(sindex).uvac)

            if blocks(i*16) != 0 || n then
              mb.nonZeroDct = true
              Vp8Transform.idct4x4(blocks, i*16)

            leftComplexity = if n then 1 else 0
            top(mbx).complexity(x + j) = if n then 1 else 0
            x += 1

          left.complexity(yy + j) = leftComplexity
          yy += 1

    private def intraPredictLuma(mbx: Int, mby: Int, mb: Macroblock, resdata: Array[Int]): Unit =
      val stride = Vp8Predict.LumaStride
      val ws = Vp8Predict.createBorderLuma(mbx, mby, mbWidth, topBorderY, leftBorderY)

      mb.lumaMode match
        case `VPred`  => Vp8Predict.predictVpred(ws, 16, 1, 1, stride)
        case `HPred`  => Vp8Predict.predictHpred(ws, 16, 1, 1, stride)
        case `TmPred` => Vp8Predict.predictTmpred(ws, 16, 1, 1, stride)
        case `DcPred` => Vp8Predict.predictDcpred(ws, 16, stride, mby != 0, mbx != 0)
        case _        => Vp8Predict.predict4x4(ws, stride, mb.bpred, resdata)

      if mb.lumaMode != BPred then
        var y = 0

        while y < 4 do
          var x = 0

          while x < 4 do
            Vp8Predict.addResidue(ws, resdata, (x + y*4)*16, 1 + y*4, 1 + x*4, stride)
            x += 1

          y += 1

      leftBorderY(0) = ws(16)
      var i = 0

      while i < 16 do
        leftBorderY(1 + i) = ws((i + 1)*stride + 16)
        i += 1

      i = 0

      while i < 16 do
        topBorderY(mbx*16 + i) = ws(16*stride + 1 + i)
        i += 1

      var y = 0

      while y < 16 do
        var x = 0

        while x < 16 do
          ybuf((mby*16 + y)*bufferWidth + mbx*16 + x) = ws((1 + y)*stride + 1 + x)
          x += 1

        y += 1

    private def intraPredictChroma(mbx: Int, mby: Int, mb: Macroblock, resdata: Array[Int]): Unit =
      val stride = Vp8Predict.ChromaStride
      val chromaWidth = bufferWidth/2
      val uws = Vp8Predict.createBorderChroma(mbx, mby, topBorderU, leftBorderU)
      val vws = Vp8Predict.createBorderChroma(mbx, mby, topBorderV, leftBorderV)

      mb.chromaMode match
        case `DcPred` =>
          Vp8Predict.predictDcpred(uws, 8, stride, mby != 0, mbx != 0)
          Vp8Predict.predictDcpred(vws, 8, stride, mby != 0, mbx != 0)

        case `VPred` =>
          Vp8Predict.predictVpred(uws, 8, 1, 1, stride)
          Vp8Predict.predictVpred(vws, 8, 1, 1, stride)

        case `HPred` =>
          Vp8Predict.predictHpred(uws, 8, 1, 1, stride)
          Vp8Predict.predictHpred(vws, 8, 1, 1, stride)

        case _ =>
          Vp8Predict.predictTmpred(uws, 8, 1, 1, stride)
          Vp8Predict.predictTmpred(vws, 8, 1, 1, stride)

      var y = 0

      while y < 2 do
        var x = 0

        while x < 2 do
          val i = x + y*2
          Vp8Predict.addResidue(uws, resdata, (16*16 + i*16), 1 + y*4, 1 + x*4, stride)
          Vp8Predict.addResidue(vws, resdata, (20*16 + i*16), 1 + y*4, 1 + x*4, stride)
          x += 1

        y += 1

      setChromaBorder(leftBorderU, topBorderU, uws, mbx)
      setChromaBorder(leftBorderV, topBorderV, vws, mbx)

      y = 0

      while y < 8 do
        var x = 0

        while x < 8 do
          val index = (mby*8 + y)*chromaWidth + mbx*8 + x
          ubuf(index) = uws((1 + y)*stride + 1 + x)
          vbuf(index) = vws((1 + y)*stride + 1 + x)
          x += 1

        y += 1

    private def setChromaBorder
      ( leftBorder: Array[Int], topBorder: Array[Int], block: Array[Int], mbx: Int )
    :   Unit =

      val stride = Vp8Predict.ChromaStride
      leftBorder(0) = block(8)
      var i = 0

      while i < 8 do
        leftBorder(1 + i) = block((i + 1)*stride + 8)
        i += 1

      i = 0

      while i < 8 do
        topBorder(mbx*8 + i) = block(8*stride + 1 + i)
        i += 1

    private def loopFilter(mbx: Int, mby: Int, mb: Macroblock): Unit =
      val lumaW = bufferWidth
      val chromaW = bufferWidth/2
      val (level, interior, hev) = calculateFilterParameters(mb)

      if level > 0 then
        val mbedge = (level + 2)*2 + interior
        val subEdge = level*2 + interior
        val doSubblock = mb.lumaMode == BPred || (!mb.coeffsSkipped && mb.nonZeroDct)

        // Filter across the left macroblock edge.
        if mbx > 0 then
          if filterType then
            var y = 0

            while y < 16 do
              Vp8Filter.simpleSegmentHorizontal(mbedge, ybuf, (mby*16 + y)*lumaW + mbx*16 - 4)
              y += 1
          else
            var y = 0

            while y < 16 do
              Vp8Filter.macroblockFilterHorizontal(hev, interior, mbedge, ybuf,
                  (mby*16 + y)*lumaW + mbx*16 - 4)

              y += 1

            y = 0

            while y < 8 do
              Vp8Filter.macroblockFilterHorizontal(hev, interior, mbedge, ubuf,
                  (mby*8 + y)*chromaW + mbx*8 - 4)

              Vp8Filter.macroblockFilterHorizontal(hev, interior, mbedge, vbuf,
                  (mby*8 + y)*chromaW + mbx*8 - 4)

              y += 1

        // Filter across the internal vertical subblock edges.
        if doSubblock then
          if filterType then
            for x <- List(4, 8, 12).stdlib do
              var y = 0

              while y < 16 do
                Vp8Filter.simpleSegmentHorizontal(subEdge, ybuf,
                    (mby*16 + y)*lumaW + mbx*16 + x - 4)

                y += 1
          else
            for x <- List(4, 8, 12).stdlib do
              var y = 0

              while y < 16 do
                Vp8Filter.subblockFilterHorizontal(hev, interior, subEdge, ybuf,
                    (mby*16 + y)*lumaW + mbx*16 + x - 4)

                y += 1

            var y = 0

            while y < 8 do
              Vp8Filter.subblockFilterHorizontal(hev, interior, subEdge, ubuf,
                  (mby*8 + y)*chromaW + mbx*8)

              Vp8Filter.subblockFilterHorizontal(hev, interior, subEdge, vbuf,
                  (mby*8 + y)*chromaW + mbx*8)

              y += 1

        // Filter across the top macroblock edge.
        if mby > 0 then
          if filterType then
            var x = 0

            while x < 16 do
              Vp8Filter.simpleSegmentVertical(mbedge, ybuf, (mby*16)*lumaW + mbx*16 + x, lumaW)
              x += 1
          else
            var x = 0

            while x < 16 do
              Vp8Filter.macroblockFilterVertical(hev, interior, mbedge, ybuf,
                  (mby*16)*lumaW + mbx*16 + x, lumaW)

              x += 1

            x = 0

            while x < 8 do
              Vp8Filter.macroblockFilterVertical(hev, interior, mbedge, ubuf,
                  (mby*8)*chromaW + mbx*8 + x, chromaW)

              Vp8Filter.macroblockFilterVertical(hev, interior, mbedge, vbuf,
                  (mby*8)*chromaW + mbx*8 + x, chromaW)

              x += 1

        // Filter across the internal horizontal subblock edges.
        if doSubblock then
          if filterType then
            for y <- List(4, 8, 12).stdlib do
              var x = 0

              while x < 16 do
                Vp8Filter.simpleSegmentVertical(subEdge, ybuf,
                    (mby*16 + y)*lumaW + mbx*16 + x, lumaW)

                x += 1
          else
            for y <- List(4, 8, 12).stdlib do
              var x = 0

              while x < 16 do
                Vp8Filter.subblockFilterVertical(hev, interior, subEdge, ybuf,
                    (mby*16 + y)*lumaW + mbx*16 + x, lumaW)

                x += 1

            var x = 0

            while x < 8 do
              Vp8Filter.subblockFilterVertical(hev, interior, subEdge, ubuf,
                  (mby*8 + 4)*chromaW + mbx*8 + x, chromaW)

              Vp8Filter.subblockFilterVertical(hev, interior, subEdge, vbuf,
                  (mby*8 + 4)*chromaW + mbx*8 + x, chromaW)

              x += 1

    // Returns (filter level, interior limit, high-edge-variance threshold) for a macroblock.
    private def calculateFilterParameters(mb: Macroblock): (Int, Int, Int) =
      val segment = segments(mb.segmentId)
      var level = filterLevel

      if level == 0 then (0, 0, 0) else
        if segmentsEnabled then
          level = if segment.deltaValues then level + segment.loopfilter else segment.loopfilter

        level = level.max(0).min(63)

        if loopFilterAdjustments then
          level += refDelta(0)

          if mb.lumaMode == BPred then level += modeDelta(0)

        level = level.max(0).min(63)

        var interior = level

        if sharpnessLevel > 0 then
          interior >>= (if sharpnessLevel > 4 then 2 else 1)

          if interior > 9 - sharpnessLevel then interior = 9 - sharpnessLevel

        if interior == 0 then interior = 1
        val hev = if level >= 40 then 2 else if level >= 15 then 1 else 0
        (level, interior, hev)
