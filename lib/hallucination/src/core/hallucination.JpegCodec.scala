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
import vacuous.*

import RasterError.Reason

// A pure-Scala JPEG decoder, ported from image-rs/jpeg-decoder (MIT/Apache-2.0). It handles
// baseline and progressive Huffman-coded DCT images with 1, 3 or 4 components (grayscale, YCbCr /
// RGB, and CMYK / YCCK); arithmetic coding, lossless and hierarchical JPEGs are rejected. Unlike
// the reference it is single-threaded and always decodes at full resolution, accumulating every
// scan's coefficients into per-component planes that are dequantized and inverse-transformed once
// the image ends. The JVM keeps using `javax.imageio`; this codec serves Scala.js and WASI.
private[hallucination] object JpegCodec:
  def isJpeg(data: Data): Boolean =
    data.length >= 3 && (data(0) & 0xff) == 0xff && (data(1) & 0xff) == 0xd8 &&
      (data(2) & 0xff) == 0xff

  def decode(data: Data): Raster raises RasterError =
    try JpegDecoder(data).decode()
    catch case _: (IndexOutOfBoundsException | NegativeArraySizeException) =>
      abort(RasterError(Jpeg(), Reason.Truncated))

private[hallucination] enum JpegColor:
  case Grayscale, Rgb, YCbCr, Cmyk, Ycck, Unknown

// ITU-R BT.601 YCbCr-to-RGB, based on libjpeg-turbo's `jdcolext.c` via image-rs/jpeg-decoder.
private[hallucination] object JpegColorConvert:
  inline val FixedPointOffset = 20
  private val Half = (1 << FixedPointOffset)/2
  private def f2f(x: Double): Int = (x*((1 << FixedPointOffset).toDouble) + 0.5).toInt
  private val Cr2R = f2f(1.40200)
  private val Cb2G = f2f(0.34414)
  private val Cr2G = f2f(0.71414)
  private val Cb2B = f2f(1.77200)

  private def clamp(value: Int): Int = (value >> FixedPointOffset).min(255).max(0)

  def ycbcrToRgb(y0: Int, cb0: Int, cr0: Int): (Int, Int, Int) =
    val y = y0*(1 << FixedPointOffset) + Half
    val cb = cb0 - 128
    val cr = cr0 - 128

    ( clamp(y + Cr2R*cr),
      clamp(y - Cb2G*cb - Cr2G*cr),
      clamp(y + Cb2B*cb) )

private[hallucination] final class JpegDecoder(data: Data)(using Tactic[RasterError]):
  private val reader = JpegReader(data, 0)

  private var frame: Optional[JpegFrame] = Unset
  private val dcTables: Array[Optional[JpegHuffmanTable]] = Array(Unset, Unset, Unset, Unset)
  private val acTables: Array[Optional[JpegHuffmanTable]] = Array(Unset, Unset, Unset, Unset)

  // Quantization tables in natural (un-zigzagged) order, indexed by destination identifier.
  private val quantTables: Array[Optional[Array[Int]]] = Array(Unset, Unset, Unset, Unset)

  private var restartInterval = 0
  private var adobeTransform: Optional[Int] = Unset
  private var isJfif = false

  // One coefficient plane per frame component (`blockWidth*blockHeight*64` entries), accumulated
  // across all scans.
  private var coefficients: Array[Array[Int]] = Array()

  // The zig-zag scan order: `Unzigzag(k)` is the natural (row-major) block index of the k-th
  // coefficient in the bitstream.
  private val Unzigzag: Array[Int] = Array(
    0, 1, 8, 16, 9, 2, 3, 10,
    17, 24, 32, 25, 18, 11, 4, 5,
    12, 19, 26, 33, 40, 48, 41, 34,
    27, 20, 13, 6, 7, 14, 21, 28,
    35, 42, 49, 56, 57, 50, 43, 36,
    29, 22, 15, 23, 30, 37, 44, 51,
    58, 59, 52, 45, 38, 31, 39, 46,
    53, 60, 61, 54, 47, 55, 62, 63)

  private def bad(): Nothing = abort(RasterError(Jpeg(), Reason.Bitstream))

  def decode(): Raster =
    if reader.u8() != 0xff || reader.u8() != JpegMarker.Soi then bad()

    var previousMarker = JpegMarker.Soi
    var pendingMarker = -1
    var running = true

    while running do
      val marker =
        if pendingMarker == -1 then readMarker()
        else
          val next = pendingMarker
          pendingMarker = -1
          next

      if JpegMarker.isSof(marker) then
        if frame.present then abort(RasterError(Jpeg(), Reason.UnsupportedVariant))
        val parsed = JpegParser.parseSof(reader, marker)
        frame = parsed

        coefficients = parsed.components.map: component =>
          new Array[Int](component.blockWidth*component.blockHeight*64)

      else if marker == JpegMarker.Sos then
        val current = frame.lay(bad())(identity)
        val scan = JpegParser.parseSos(reader, current)
        pendingMarker = decodeScan(current, scan)

      else if marker == JpegMarker.Dqt then
        loadQuantizationTables()

      else if marker == JpegMarker.Dht then
        loadHuffmanTables()

      else if marker == JpegMarker.Dri then
        restartInterval = JpegParser.parseDri(reader)

      else if marker == JpegMarker.Dac then
        abort(RasterError(Jpeg(), Reason.UnsupportedVariant))

      else if marker == JpegMarker.Com then
        JpegParser.skipSegment(reader)

      else if JpegMarker.isApp(marker) then
        JpegParser.parseApp(reader, marker) match
          case JpegApp.Jfif             => isJfif = true
          case JpegApp.Avi1             => ()
          case JpegApp.Adobe(transform) => adobeTransform = transform
          case JpegApp.None             => ()

      else if JpegMarker.isRst(marker) then
        if previousMarker != JpegMarker.Sos then bad()

      else if marker == JpegMarker.Eoi then
        running = false

      else if marker == JpegMarker.Dnl || marker == JpegMarker.Dhp || marker == JpegMarker.Exp then
        abort(RasterError(Jpeg(), Reason.UnsupportedVariant))

      else
        bad()

      previousMarker = marker

    render(frame.lay(bad())(identity))

  // Skips extraneous bytes and fill bytes to the next marker, returning its code.
  private def readMarker(): Int =
    var marker = 0

    while marker == 0 do
      while reader.u8() != 0xff do ()
      var byte = reader.u8()
      while byte == 0xff do byte = reader.u8()
      if byte != 0x00 then marker = byte

    marker

  private def loadQuantizationTables(): Unit =
    val tables = JpegParser.parseDqt(reader)
    var index = 0

    while index < 4 do
      tables(index).let: table =>
        val natural = new Array[Int](64)
        var j = 0
        while j < 64 do { natural(Unzigzag(j)) = table(j); j += 1 }
        quantTables(index) = natural

      index += 1

  private def loadHuffmanTables(): Unit =
    val baseline = frame.lay(false)(_.isBaseline)
    val (dc, ac) = JpegParser.parseDht(reader, baseline)
    var index = 0

    while index < 4 do
      dc(index).let(dcTables(index) = _)
      ac(index).let(acTables(index) = _)
      index += 1

  // Decodes one scan's entropy-coded data into the coefficient planes; returns the marker that
  // terminated it (or -1).
  private def decodeScan(frame: JpegFrame, scan: JpegScan): Int =
    val count = scan.componentIndices.length
    val components = scan.componentIndices.map(frame.components(_))
    var check = 0

    while check < count do
      if quantTables(components(check).quantizationTableIndex).absent then bad()
      check += 1

    if scan.spectralStart == 0 then
      var index = 0

      while index < count do
        if dcTables(scan.dcTableIndices(index)).absent then bad()
        index += 1

    if scan.spectralEnd > 1 then
      var index = 0

      while index < count do
        if acTables(scan.acTableIndices(index)).absent then bad()
        index += 1

    val isInterleaved = count > 1
    val huffman = JpegHuffmanDecoder()
    val dcPredictors = new Array[Int](count)
    val eobRun = new Array[Int](1)
    var mcusLeftUntilRestart = restartInterval
    var expectedRst = 0

    val mcuHoriz = new Array[Int](count)
    val mcuVert = new Array[Int](count)
    var index = 0

    while index < count do
      mcuHoriz(index) = if isInterleaved then components(index).horizontalSamplingFactor else 1
      mcuVert(index) = if isInterleaved then components(index).verticalSamplingFactor else 1
      index += 1

    val maxMcuX = if isInterleaved then frame.mcuWidth else components(0).blockWidth
    val maxMcuY = if isInterleaved then frame.mcuHeight else components(0).blockHeight

    var mcuY = 0
    var stop = false

    while mcuY < maxMcuY && !stop do
      if mcuY*8 >= frame.imageHeight then stop = true else
        var mcuX = 0
        var rowStop = false

        while mcuX < maxMcuX && !rowStop do
          if mcuX*8 >= frame.imageWidth then rowStop = true else
            if restartInterval > 0 && mcusLeftUntilRestart == 0 then
              val rst = huffman.takeMarker(reader)
              if rst == -1 || !JpegMarker.isRst(rst) || (rst & 7) != expectedRst then bad()
              huffman.reset()
              var reset = 0
              while reset < count do { dcPredictors(reset) = 0; reset += 1 }
              eobRun(0) = 0
              expectedRst = (expectedRst + 1) % 8
              mcusLeftUntilRestart = restartInterval

            var i = 0

            while i < count do
              val component = components(i)
              val globalIndex = scan.componentIndices(i)
              val plane = coefficients(globalIndex)
              var vPos = 0

              while vPos < mcuVert(i) do
                var hPos = 0

                while hPos < mcuHoriz(i) do
                  val blockY = mcuY*mcuVert(i) + vPos
                  val blockX = mcuX*mcuHoriz(i) + hPos
                  val base = (blockY*component.blockWidth + blockX)*64

                  if scan.successiveHigh == 0 then
                    decodeBlock
                      ( huffman, plane, base, scan.dcTableIndices(i), scan.acTableIndices(i),
                        scan.spectralStart, scan.spectralEnd, scan.successiveLow, eobRun,
                        dcPredictors, i )
                  else
                    decodeBlockRefine
                      ( huffman, plane, base, scan.acTableIndices(i), scan.spectralStart,
                        scan.spectralEnd, scan.successiveLow, eobRun )

                  hPos += 1

                vPos += 1

              i += 1

            if restartInterval > 0 then mcusLeftUntilRestart -= 1

            mcuX += 1

        mcuY += 1

    var marker = huffman.takeMarker(reader)
    while JpegMarker.isRst(marker) do marker = readMarker()
    marker

  // Section F.2.2: decodes the coefficients of one block on the first pass over its spectral band.
  private def decodeBlock
    ( huffman:       JpegHuffmanDecoder,
      coeff:         Array[Int],
      base:          Int,
      dcTableIndex:  Int,
      acTableIndex:  Int,
      spectralStart: Int,
      spectralEnd:   Int,
      successiveLow: Int,
      eobRun:        Array[Int],
      dcPredictors:  Array[Int],
      predIndex:     Int )
  :   Unit =

    if spectralStart == 0 then
      val value = huffman.decode(reader, dcTables(dcTableIndex).vouch)

      val diff =
        if value == 0 then 0
        else if value <= 11 then huffman.receiveExtend(reader, value)
        else bad()

      dcPredictors(predIndex) = dcPredictors(predIndex) + diff
      coeff(base) = dcPredictors(predIndex) << successiveLow

    var index = spectralStart.max(1)

    if index < spectralEnd && eobRun(0) > 0 then eobRun(0) -= 1
    else if index < spectralEnd then
      val acTable = acTables(acTableIndex).vouch
      var continue = true

      while continue && index < spectralEnd do
        if huffman.decodeFastAc(reader, acTable) then
          index += huffman.fastAcRun

          if index >= spectralEnd then continue = false else
            coeff(base + Unzigzag(index)) = huffman.fastAcValue << successiveLow
            index += 1
        else
          val byte = huffman.decode(reader, acTable)
          val r = byte >> 4
          val s = byte & 0x0f

          if s == 0 then
            if r == 15 then index += 16
            else
              eobRun(0) = (1 << r) - 1
              if r > 0 then eobRun(0) += huffman.getBits(reader, r)
              continue = false
          else
            index += r

            if index >= spectralEnd then continue = false else
              coeff(base + Unzigzag(index)) = huffman.receiveExtend(reader, s) << successiveLow
              index += 1

  // Section G.1.2: refines coefficients on later (successive-approximation) passes.
  private def decodeBlockRefine
    ( huffman:       JpegHuffmanDecoder,
      coeff:         Array[Int],
      base:          Int,
      acTableIndex:  Int,
      spectralStart: Int,
      spectralEnd:   Int,
      successiveLow: Int,
      eobRun:        Array[Int] )
  :   Unit =

    val bit = 1 << successiveLow

    if spectralStart == 0 then
      if huffman.getBits(reader, 1) == 1 then coeff(base) |= bit
    else
      val acTable = acTables(acTableIndex).vouch

      if eobRun(0) > 0 then
        eobRun(0) -= 1
        refineNonZeroes(huffman, coeff, base, acTable, spectralStart, spectralEnd, 64, bit)
      else
        var index = spectralStart

        while index < spectralEnd do
          val byte = huffman.decode(reader, acTable)
          val r = byte >> 4
          val s = byte & 0x0f
          var zeroRun = r
          var value = 0

          if s == 0 then
            if r != 15 then
              eobRun(0) = (1 << r) - 1
              if r > 0 then eobRun(0) += huffman.getBits(reader, r)
              zeroRun = 64
          else if s == 1 then
            value = if huffman.getBits(reader, 1) == 1 then bit else -bit
          else
            bad()

          index = refineNonZeroes(huffman, coeff, base, acTable, index, spectralEnd, zeroRun, bit)
          if value != 0 then coeff(base + Unzigzag(index)) = value
          index += 1

  // Section G.1.2.3: advances through the band, refining already-nonzero coefficients by one bit,
  // until `zrl` zero coefficients have been skipped; returns the index reached.
  private def refineNonZeroes
    ( huffman:     JpegHuffmanDecoder,
      coeff:       Array[Int],
      base:        Int,
      acTable:     JpegHuffmanTable,
      start:       Int,
      end:         Int,
      zrl:         Int,
      bit:         Int )
  :   Int =

    var zeroRun = zrl
    var index = start
    var result = end - 1
    var settled = false

    while index < end && !settled do
      val position = base + Unzigzag(index)

      if coeff(position) == 0 then
        if zeroRun == 0 then
          result = index
          settled = true
        else
          zeroRun -= 1
      else if huffman.getBits(reader, 1) == 1 && (coeff(position) & bit) == 0 then
        if coeff(position) > 0 then coeff(position) += bit else coeff(position) -= bit

      if !settled then index += 1

    result

  // Dequantizes and inverse-transforms every coefficient plane, then assembles the interleaved,
  // colour-converted image and wraps it as a `Raster`.
  private def render(frame: JpegFrame): Raster =
    val planes = new Array[Array[Byte]](frame.components.length)
    var index = 0

    while index < frame.components.length do
      planes(index) = renderPlane(frame.components(index), coefficients(index))
      index += 1

    val width = frame.imageWidth
    val height = frame.imageHeight

    if frame.components.length == 1
    then grayscaleRaster(frame.components(0), planes(0), width, height)
    else colorRaster(frame, planes, width, height)

  private def renderPlane(component: JpegComponent, coeff: Array[Int]): Array[Byte] =
    val quant = quantTables(component.quantizationTableIndex).vouch
    val stride = component.blockWidth*8
    val plane = new Array[Byte](stride*component.blockHeight*8)
    var blockY = 0

    while blockY < component.blockHeight do
      var blockX = 0

      while blockX < component.blockWidth do
        val base = (blockY*component.blockWidth + blockX)*64
        JpegIdct.dequantizeAndIdct8x8(coeff, base, quant, plane, blockY*8*stride + blockX*8, stride)
        blockX += 1

      blockY += 1

    plane

  private def grayscaleRaster(component: JpegComponent, plane: Array[Byte], width: Int, height: Int)
  :   Raster =

    val stride = component.blockWidth*8

    Raster.build(width, height, Descriptor.rgb): pixel =>
      val x = pixel%width
      val y = pixel/width
      val luma = plane(y*stride + x) & 0xffL
      (luma << 16) | (luma << 8) | luma

  private def colorRaster
    ( frame: JpegFrame, planes: Array[Array[Byte]], width: Int, height: Int )
  :   Raster =

    val transform = determineColorTransform(frame)
    val upsampler = JpegUpsampler(frame.components, width, height)
    val convert = chooseColorConvert(frame.components.length, transform)
    val rgb = new Array[Byte](width*height*3)
    val row = new Array[Byte](width*3)

    var y = 0

    while y < height do
      upsampler.upsampleAndInterleaveRow(planes, y, row, convert)
      System.arraycopy(row, 0, rgb, y*width*3, width*3)
      y += 1

    Raster.build(width, height, Descriptor.rgb): pixel =>
      (rgb(pixel*3) & 0xffL) << 16 | (rgb(pixel*3 + 1) & 0xffL) << 8 | (rgb(pixel*3 + 2) & 0xffL)

  private def chooseColorConvert(count: Int, transform: JpegColor)
  :   (Array[Array[Byte]], Array[Byte]) => Unit =

    (buffers, output) =>
      val width = output.length/3
      var x = 0

      while x < width do
        val a0 = buffers(0)(x) & 0xff
        val a1 = buffers(1)(x) & 0xff
        val a2 = buffers(2)(x) & 0xff

        val (r, g, b) = transform match
          case JpegColor.Rgb   => (a0, a1, a2)
          case JpegColor.YCbCr => JpegColorConvert.ycbcrToRgb(a0, a1, a2)

          case JpegColor.Cmyk =>
            val k = buffers(3)(x) & 0xff
            (a0*k/255, a1*k/255, a2*k/255)

          case JpegColor.Ycck =>
            val k = buffers(3)(x) & 0xff
            val (yr, yg, yb) = JpegColorConvert.ycbcrToRgb(a0, a1, a2)
            (yr*k/255, yg*k/255, yb*k/255)

          case _ =>
            (a0, a0, a0)

        output(x*3) = r.toByte
        output(x*3 + 1) = g.toByte
        output(x*3 + 2) = b.toByte
        x += 1

  // Section on colour determination, following the reference (and the heuristics described at
  // entropymine.wordpress.com/2018/10/22/how-is-a-jpeg-images-color-type-determined).
  private def determineColorTransform(frame: JpegFrame): JpegColor =
    val count = frame.components.length

    if count == 1 then JpegColor.Grayscale
    else if count == 3 && matchesIdentifiers(frame, 1, 2, 3) then JpegColor.YCbCr
    else if count == 3 && matchesIdentifiers(frame, 82, 71, 66) then JpegColor.Rgb
    else if count == 3 && isJfif then JpegColor.YCbCr
    else if adobeTransform.present then adobeColor(count)
    else if count == 4 then JpegColor.Cmyk
    else if count == 3 then JpegColor.YCbCr
    else JpegColor.Unknown

  // The colour transform indicated by an Adobe APP14 marker.
  private def adobeColor(count: Int): JpegColor = adobeTransform.vouch match
    case 0 => if count == 3 then JpegColor.Rgb else JpegColor.Cmyk
    case 1 => JpegColor.YCbCr
    case _ => JpegColor.Ycck

  private def matchesIdentifiers(frame: JpegFrame, a: Int, b: Int, c: Int): Boolean =
    frame.components(0).identifier == a && frame.components(1).identifier == b &&
      frame.components(2).identifier == c
