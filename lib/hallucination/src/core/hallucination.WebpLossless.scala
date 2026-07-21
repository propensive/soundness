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
import vacuous.*

import RasterError.Reason

// The VP8L lossless decoder, ported from image-rs/image-webp (`src/lossless/decoder/mod.rs`,
// MIT/Apache-2.0). It reads the transform list, the meta-Huffman code groups and the entropy-coded
// image stream, producing an RGBA byte buffer which the reverse transforms then un-apply.
private[hallucination] object WebpLossless:
  private val CodeLengthCodes: Int = 19

  private val CodeLengthCodeOrder: Array[Int] =
    Array(17, 18, 0, 1, 2, 3, 4, 5, 16, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15)

  private val HuffmanCodesPerMetaCode: Int = 5
  private val AlphabetSize: Array[Int] = Array(256 + 24, 256, 256, 256, 40)

  // (xoffset, yoffset) pairs, flattened, for short backward-reference distances.
  private val DistanceMap: Array[Int] = Array(
    0, 1, 1, 0, 1, 1, -1, 1, 0, 2, 2, 0, 1, 2, -1, 2, 2, 1,
    -2, 1, 2, 2, -2, 2, 0, 3, 3, 0, 1, 3, -1, 3, 3, 1, -3, 1,
    2, 3, -2, 3, 3, 2, -3, 2, 0, 4, 4, 0, 1, 4, -1, 4, 4, 1,
    -4, 1, 3, 3, -3, 3, 2, 4, -2, 4, 4, 2, -4, 2, 0, 5, 3, 4,
    -3, 4, 4, 3, -4, 3, 5, 0, 1, 5, -1, 5, 5, 1, -5, 1, 2, 5,
    -2, 5, 5, 2, -5, 2, 4, 4, -4, 4, 3, 5, -3, 5, 5, 3, -5, 3,
    0, 6, 6, 0, 1, 6, -1, 6, 6, 1, -6, 1, 2, 6, -2, 6, 6, 2,
    -6, 2, 4, 5, -4, 5, 5, 4, -5, 4, 3, 6, -3, 6, 6, 3, -6, 3,
    0, 7, 7, 0, 1, 7, -1, 7, 5, 5, -5, 5, 7, 1, -7, 1, 4, 6,
    -4, 6, 6, 4, -6, 4, 2, 7, -2, 7, 7, 2, -7, 2, 3, 7, -3, 7,
    7, 3, -7, 3, 5, 6, -5, 6, 6, 5, -6, 5, 8, 0, 4, 7, -4, 7,
    7, 4, -7, 4, 8, 1, 8, 2, 6, 6, -6, 6, 8, 3, 5, 7, -5, 7,
    7, 5, -7, 5, 8, 4, 6, 7, -6, 7, 7, 6, -7, 6, 8, 5, 7, 7,
    -7, 7, 8, 6, 8, 7)

  private final class Group(val trees: Array[WebpHuffman]):
    def allSingle: Boolean =
      trees(0).isSingleNode && trees(1).isSingleNode && trees(2).isSingleNode &&
        trees(3).isSingleNode

  private final class ColorCache(val bits: Int):
    private val entries = new Array[Int](1 << bits)
    def insert(argb: Int): Unit = entries((0x1e35a7bd*argb) >>> (32 - bits)) = argb
    def lookup(index: Int): Int = entries(index)

  private final class HuffmanInfo
    ( val xsize: Int, val cache: Optional[ColorCache], val image: Array[Int], val bits: Int,
      val mask: Int, val groups: Array[Group] ):

    def huffIndex(x: Int, y: Int): Int =
      if bits == 0 then 0 else image((y >> bits)*xsize + (x >> bits))

  private final class Transform(val kind: Int, val sizeBits: Int, val data: Array[Byte],
      val tableSize: Int)

  // Reads a full VP8L frame — its 5-byte header then the transformed image — returning the
  // dimensions and the un-transformed RGBA buffer.
  def decode(reader: WebpBitReader): (Int, Int, Array[Byte]) raises RasterError =
    val signature = reader.readBits(8)

    if signature != 0x2f then abort(RasterError(Webp(), Reason.BadSignature))
    val width = reader.readBits(14) + 1
    val height = reader.readBits(14) + 1
    reader.readBits(1)
    val version = reader.readBits(3)

    if version != 0 then abort(RasterError(Webp(), Reason.UnsupportedVariant))
    (width, height, Decoder(reader, width, height).run())

  // Decodes a VP8L stream whose dimensions are given externally (no 5-byte header), as used for
  // the lossless-compressed alpha plane of a lossy image. Returns the RGBA buffer.
  def decodeRaw(reader: WebpBitReader, width: Int, height: Int): Array[Byte] raises RasterError =
    Decoder(reader, width, height).run()

  private final class Decoder(reader: WebpBitReader, width: Int, height: Int):
    // One optional transform slot per transform type; `order` is reverse-read order, which is the
    // order transforms must be un-applied in.
    private val transforms = new Array[Transform](4)
    private var order: List[Int] = Nil

    def run(): Array[Byte] raises RasterError =
      val transformedWidth = readTransforms()
      val buffer = new Array[Byte](transformedWidth*height*4)
      decodeImageStream(transformedWidth, height, true, buffer, 0)

      var currentWidth = transformedWidth
      var image = buffer

      for index <- order do
        val transform = transforms(index)

        transform.kind match
          case 0 =>
            WebpTransform.predictor(image, currentWidth, height, transform.sizeBits, transform.data)

          case 1 =>
            WebpTransform.color(image, currentWidth, transform.sizeBits, transform.data)

          case 2 =>
            WebpTransform.subtractGreen(image)

          case _ =>
            // Colour indexing restores the full (un-subsampled) width.
            val full = new Array[Byte](width*height*4)
            System.arraycopy(image, 0, full, 0, image.length.min(full.length))
            WebpTransform.colorIndexing(full, width, height, transform.tableSize, transform.data)
            currentWidth = width
            image = full

      image

    private def readTransforms(): Int raises RasterError =
      var xsize = width

      while reader.readBits(1) == 1 do
        val kind = reader.readBits(2)

        if transforms(kind) != null then abort(RasterError(Webp(), Reason.InvalidTransform))
        order = kind :: order

        transforms(kind) = kind match
          case 0 | 1 =>
            val sizeBits = reader.readBits(3) + 2
            val blockWidth = WebpTransform.subsampleSize(xsize, sizeBits)
            val blockHeight = WebpTransform.subsampleSize(height, sizeBits)
            val data = new Array[Byte](blockWidth*blockHeight*4)
            decodeImageStream(blockWidth, blockHeight, false, data, 0)
            Transform(kind, sizeBits, data, 0)

          case 2 =>
            Transform(2, 0, new Array[Byte](0), 0)

          case _ =>
            val tableSize = reader.readBits(8) + 1
            val colorMap = new Array[Byte](tableSize*4)
            decodeImageStream(tableSize, 1, false, colorMap, 0)

            // The palette is stored delta-coded across entries.
            var i = 4

            while i < colorMap.length do
              colorMap(i) = (colorMap(i) + colorMap(i - 4)).toByte
              i += 1

            val bits =
              if tableSize <= 2 then 3
              else if tableSize <= 4 then 2
              else if tableSize <= 16 then 1
              else 0

            xsize = WebpTransform.subsampleSize(xsize, bits)
            Transform(3, 0, colorMap, tableSize)

      xsize

    private def decodeImageStream
      ( xsize: Int, ysize: Int, argb: Boolean, data: Array[Byte], offset: Int )
    :   Unit raises RasterError =

      val cache = readColorCache()
      val info = readHuffmanCodes(argb, xsize, ysize, cache)
      decodeImageData(xsize, ysize, info, data, offset)

    private def readColorCache(): Optional[ColorCache] raises RasterError =
      if reader.readBits(1) != 1 then Unset else
        val codeBits = reader.readBits(4)

        if codeBits < 1 || codeBits > 11 then abort(RasterError(Webp(), Reason.Bitstream))
        ColorCache(codeBits)

    private def readHuffmanCodes
      ( readMeta: Boolean, xsize: Int, ysize: Int, cache: Optional[ColorCache] )
    :   HuffmanInfo raises RasterError =

      var numGroups = 1
      var huffmanBits = 0
      var huffmanXsize = 1
      var entropy = new Array[Int](0)

      if readMeta && reader.readBits(1) == 1 then
        huffmanBits = reader.readBits(3) + 2
        huffmanXsize = WebpTransform.subsampleSize(xsize, huffmanBits)
        val huffmanYsize = WebpTransform.subsampleSize(ysize, huffmanBits)
        val data = new Array[Byte](huffmanXsize*huffmanYsize*4)
        decodeImageStream(huffmanXsize, huffmanYsize, false, data, 0)

        // Each block's meta-Huffman index is packed into the top two bytes of its pixel.
        entropy = new Array[Int](huffmanXsize*huffmanYsize)
        var i = 0

        while i < entropy.length do
          val code = ((data(i*4) & 0xff) << 8) | (data(i*4 + 1) & 0xff)
          entropy(i) = code

          if code + 1 > numGroups then numGroups = code + 1
          i += 1

      val cacheBits = cache.let(_.bits).or(0)
      val groups = new Array[Group](numGroups)
      var g = 0

      while g < numGroups do
        val trees = new Array[WebpHuffman](HuffmanCodesPerMetaCode)
        var j = 0

        while j < HuffmanCodesPerMetaCode do
          val alphabet = AlphabetSize(j) + (if j == 0 && cache.present then 1 << cacheBits else 0)
          trees(j) = readHuffmanCode(alphabet)
          j += 1

        groups(g) = Group(trees)
        g += 1

      val mask = if huffmanBits == 0 then -1 else (1 << huffmanBits) - 1
      HuffmanInfo(huffmanXsize, cache, entropy, huffmanBits, mask, groups)

    private def readHuffmanCode(alphabetSize: Int): WebpHuffman raises RasterError =
      if reader.readBits(1) == 1 then
        // Simple code: one or two explicitly-listed symbols.
        val numSymbols = reader.readBits(1) + 1
        val firstIs8Bit = reader.readBits(1)
        val zero = reader.readBits(1 + 7*firstIs8Bit)

        if zero >= alphabetSize then abort(RasterError(Webp(), Reason.Bitstream))

        if numSymbols == 1 then WebpHuffman.single(zero) else
          val one = reader.readBits(8)

          if one >= alphabetSize then abort(RasterError(Webp(), Reason.Bitstream))
          WebpHuffman.twoNode(zero, one)
      else
        val codeLengthCodeLengths = new Array[Int](CodeLengthCodes)
        val numCodeLengths = 4 + reader.readBits(4)
        var i = 0

        while i < numCodeLengths do
          codeLengthCodeLengths(CodeLengthCodeOrder(i)) = reader.readBits(3)
          i += 1

        WebpHuffman.buildImplicit(readHuffmanCodeLengths(codeLengthCodeLengths, alphabetSize))

    private def readHuffmanCodeLengths
      ( codeLengthCodeLengths: Array[Int], numSymbols: Int )
    :   Array[Int] raises RasterError =

      val table = WebpHuffman.buildImplicit(codeLengthCodeLengths)

      var maxSymbol =
        if reader.readBits(1) == 1 then
          val lengthBits = 2 + 2*reader.readBits(3)
          val maxMinusTwo = reader.readBits(lengthBits)

          if maxMinusTwo > numSymbols - 2 then abort(RasterError(Webp(), Reason.Bitstream))
          2 + maxMinusTwo
        else
          numSymbols

      val lengths = new Array[Int](numSymbols)
      var prevLength = 8
      var symbol = 0

      while symbol < numSymbols && maxSymbol > 0 do
        maxSymbol -= 1
        reader.fill()
        val codeLen = table.readSymbol(reader)

        if codeLen < 16 then
          lengths(symbol) = codeLen
          symbol += 1

          if codeLen != 0 then prevLength = codeLen
        else
          val slot = codeLen - 16
          val extraBits = if slot == 0 then 2 else if slot == 1 then 3 else 7
          val repeatOffset = if slot <= 1 then 3 else 11
          var repeat = reader.readBits(extraBits) + repeatOffset

          if symbol + repeat > numSymbols then abort(RasterError(Webp(), Reason.Bitstream))
          val length = if codeLen == 16 then prevLength else 0

          while repeat > 0 do
            repeat -= 1
            lengths(symbol) = length
            symbol += 1

      lengths

    private def decodeImageData
      ( width: Int, height: Int, info: HuffmanInfo, data: Array[Byte], offset: Int )
    :   Unit raises RasterError =

      val numValues = width*height
      var group = info.groups(info.huffIndex(0, 0))
      var index = 0
      var nextBlockStart = 0

      while index < numValues do
        reader.fill()
        var handled = false

        if index >= nextBlockStart then
          val x = index%width
          val y = index/width
          nextBlockStart = ((x | info.mask).min(width - 1)) + y*width + 1
          group = info.groups(info.huffIndex(x, y))

          // Fast path: a block whose four colour codes are all single-symbol writes no per-pixel
          // data, so the whole run is one repeated pixel. (Re-reading a single-node tree on the
          // fall-through path below consumes no bits, so a non-literal code is handled there.)
          if group.allSingle then
            val green = group.trees(0).readSymbol(reader)

            if green < 256 then
              val count = if info.bits == 0 then numValues - index else nextBlockStart - index
              val red = group.trees(1).readSymbol(reader)
              val blue = group.trees(2).readSymbol(reader)
              val alpha = group.trees(3).readSymbol(reader)
              var i = 0

              while i < count do
                store(data, offset, index + i, red, green, blue, alpha)
                i += 1

              info.cache.let(_.insert(pack(red, green, blue, alpha)))
              index += count
              handled = true

        if !handled then
          val green = group.trees(0).readSymbol(reader)

          if green < 256 then
            val red = group.trees(1).readSymbol(reader)
            val blue = group.trees(2).readSymbol(reader)

            if reader.nbits < 15 then reader.fill()
            val alpha = group.trees(3).readSymbol(reader)
            store(data, offset, index, red, green, blue, alpha)
            info.cache.let(_.insert(pack(red, green, blue, alpha)))
            index += 1
          else if green < 256 + 24 then
            val length = copyDistance(green - 256)
            val dist = planeCodeToDistance(width, copyDistance(group.trees(4).readSymbol(reader)))

            if index < dist || numValues - index < length then
              abort(RasterError(Webp(), Reason.Bitstream))

            var i = 0

            while i < length do
              val from = offset + (index - dist + i)*4
              val to = offset + (index + i)*4
              data(to) = data(from); data(to + 1) = data(from + 1)
              data(to + 2) = data(from + 2); data(to + 3) = data(from + 3)
              i += 1

            // A distance-1 run repeats the previous pixel, already in the cache at the same slot;
            // the reference skips re-inserting it (which matters when it came from a cache lookup).
            if dist != 1 then info.cache.let: cache =>
              var j = 0

              while j < length do
                val p = offset + (index + j)*4

                cache.insert(pack(data(p) & 0xff, data(p + 1) & 0xff, data(p + 2) & 0xff,
                    data(p + 3) & 0xff))

                j += 1

            index += length
          else
            val cache = info.cache.or(abort(RasterError(Webp(), Reason.Bitstream)))
            writeArgb(data, offset, index, cache.lookup(green - 280))
            index += 1

    private def store
      ( data: Array[Byte], offset: Int, index: Int, red: Int, green: Int, blue: Int, alpha: Int )
    :   Unit =

      val p = offset + index*4
      data(p) = red.toByte; data(p + 1) = green.toByte
      data(p + 2) = blue.toByte; data(p + 3) = alpha.toByte

    private def writeArgb(data: Array[Byte], offset: Int, index: Int, argb: Int): Unit =
      val p = offset + index*4
      data(p) = (argb >>> 16).toByte; data(p + 1) = (argb >>> 8).toByte
      data(p + 2) = argb.toByte; data(p + 3) = (argb >>> 24).toByte

    private def pack(red: Int, green: Int, blue: Int, alpha: Int): Int =
      ((red & 0xff) << 16) | ((green & 0xff) << 8) | (blue & 0xff) | ((alpha & 0xff) << 24)

    private def copyDistance(prefix: Int): Int raises RasterError =
      if prefix < 4 then prefix + 1 else
        val extraBits = (prefix - 2) >> 1
        val distOffset = (2 + (prefix & 1)) << extraBits
        val bits = reader.peek(extraBits).toInt
        reader.consume(extraBits)
        distOffset + bits + 1

    private def planeCodeToDistance(xsize: Int, planeCode: Int): Int =
      if planeCode > 120 then planeCode - 120 else
        val dist = DistanceMap((planeCode - 1)*2) + DistanceMap((planeCode - 1)*2 + 1)*xsize

        if dist < 1 then 1 else dist
