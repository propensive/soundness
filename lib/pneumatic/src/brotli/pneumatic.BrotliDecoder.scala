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
package pneumatic

import scala.caps

// A pure-Scala Brotli decoder (RFC 7932), ported faithfully from Google's `org.brotli.dec`
// (MIT-licensed, Copyright 2015 Google Inc.). Two deliberate simplifications versus the reference:
//
//  - The whole compressed input is decoded in one pass from a complete byte array, so the streaming
//    `InputStream`/refill machinery of the reference `BitReader` collapses to a straightforward
//    64-bit accumulator over a zero-padded buffer.
//  - Backward references index directly into one growable output array rather than a windowed ring
//    buffer, so the reference's ring-buffer wrap and paused-output (`WRITE`) states disappear. This
//    is valid because a backward distance never exceeds the window, which is already emitted.
//
// The bit-stream parsing, Huffman-table construction, context modelling, distance decoding and
// static-dictionary transforms are otherwise a direct translation of the reference.
private[pneumatic] object BrotliDecoder:
  final val HuffmanMaxTableSize = 1080

  private[pneumatic] final val DefaultCodeLength = 8
  private[pneumatic] final val CodeLengthRepeatCode = 16
  private[pneumatic] final val NumLiteralCodes = 256
  private[pneumatic] final val NumInsertAndCopyCodes = 704
  private[pneumatic] final val NumBlockLengthCodes = 26
  private[pneumatic] final val LiteralContextBits = 6
  private[pneumatic] final val DistanceContextBits = 2
  private[pneumatic] final val HuffmanTableBits = 8
  private[pneumatic] final val HuffmanTableMask = 0xff
  private[pneumatic] final val CodeLengthCodes = 18
  private[pneumatic] final val NumDistanceShortCodes = 16
  private final val MaxLength = 15

  private[pneumatic] val codeLengthCodeOrder: Array[Int]^{} =
    Array.unsafeFrozen:
      scala.Array(1, 2, 3, 4, 0, 5, 17, 6, 16, 7, 8, 9, 10, 11, 12, 13, 14, 15)

  private[pneumatic] val distanceShortCodeIndexOffset: Array[Int]^{} =
    Array.unsafeFrozen:
      scala.Array(3, 2, 1, 0, 3, 3, 3, 3, 3, 3, 2, 2, 2, 2, 2, 2)

  private[pneumatic] val distanceShortCodeValueOffset: Array[Int]^{} =
    Array.unsafeFrozen:
      scala.Array(0, 0, 0, 0, -1, 1, -2, 2, -3, 3, -1, 1, -2, 2, -3, 3)

  // Static Huffman code for the code-length code lengths.
  private[pneumatic] val fixedTable: Array[Int]^{} =
    Array.unsafeFrozen:
      scala.Array(0x020000, 0x020004, 0x020003, 0x030002, 0x020000, 0x020004, 0x020003, 0x040001,
          0x020000, 0x020004, 0x020003, 0x030002, 0x020000, 0x020004, 0x020003, 0x040005)

  private[pneumatic] def corrupt(message: String): Nothing =
    throw IllegalStateException("the Brotli data is corrupt: "+message)

  // Builds a Huffman lookup table assuming code lengths are in symbol order (reference `Huffman`).
  private[pneumatic] def buildHuffmanTable
    ( rootTable: scala.Array[Int]^, tableOffset: Int, rootBits: Int, codeLengths: scala.Array[Int],
      codeLengthsSize: Int )
  :   Unit =

    val sorted: scala.Array[Int]^ = new scala.Array[Int](codeLengthsSize)
    val count: scala.Array[Int]^ = new scala.Array[Int](MaxLength + 1)
    val offset: scala.Array[Int]^ = new scala.Array[Int](MaxLength + 1)

    var symbol = 0
    while symbol < codeLengthsSize do { count(codeLengths(symbol)) += 1; symbol += 1 }

    offset(1) = 0
    var len = 1
    while len < MaxLength do { offset(len + 1) = offset(len) + count(len); len += 1 }

    symbol = 0

    while symbol < codeLengthsSize do
      if codeLengths(symbol) != 0 then
        sorted(offset(codeLengths(symbol))) = symbol
        offset(codeLengths(symbol)) += 1

      symbol += 1

    var tableBits = rootBits
    var tableSize = 1 << tableBits
    var totalSize = tableSize

    if offset(MaxLength) == 1 then
      var key = 0
      while key < totalSize do { rootTable(tableOffset + key) = sorted(0); key += 1 }
    else
      var key = 0
      symbol = 0
      len = 1
      var step = 2

      while len <= rootBits do
        while count(len) > 0 do
          replicateValue(rootTable, tableOffset + key, step, tableSize, len << 16 | sorted(symbol))
          symbol += 1
          key = nextKey(key, len)
          count(len) -= 1

        len += 1
        step <<= 1

      val mask = totalSize - 1
      var low = -1
      var currentOffset = tableOffset
      len = rootBits + 1
      step = 2

      while len <= MaxLength do
        while count(len) > 0 do
          if (key & mask) != low then
            currentOffset += tableSize
            tableBits = nextTableBitSize(count, len, rootBits)
            tableSize = 1 << tableBits
            totalSize += tableSize
            low = key & mask

            rootTable(tableOffset + low) =
              (tableBits + rootBits) << 16 | (currentOffset - tableOffset - low)

          replicateValue(rootTable, currentOffset + (key >> rootBits), step, tableSize,
              (len - rootBits) << 16 | sorted(symbol))

          symbol += 1
          key = nextKey(key, len)
          count(len) -= 1

        len += 1
        step <<= 1

  private def nextKey(key0: Int, len: Int): Int =
    var step = 1 << (len - 1)
    while (key0 & step) != 0 do step >>= 1
    (key0 & (step - 1)) + step

  private def replicateValue(table: scala.Array[Int]^, offset: Int, step: Int, end0: Int, item: Int)
  :   Unit =

    var end = end0

    while
      end -= step
      table(offset + end) = item
      end > 0
    do ()

  private def nextTableBitSize(count: scala.Array[Int], len0: Int, rootBits: Int): Int =
    var len = len0
    var left = 1 << (len - rootBits)
    var settled = false

    while len < MaxLength && !settled do
      left -= count(len)
      if left <= 0 then settled = true else { len += 1; left <<= 1 }

    len - rootBits

  private[pneumatic] def moveToFront(v: scala.Array[Int]^, index0: Int): Unit =
    var index = index0
    val value = v(index)
    while index > 0 do { v(index) = v(index - 1); index -= 1 }
    v(0) = value

  private[pneumatic] def inverseMoveToFrontTransform(v: scala.Array[Byte]^, vLen: Int): Unit =
    val mtf: scala.Array[Int]^ = new scala.Array[Int](256)
    var i = 0
    while i < 256 do { mtf(i) = i; i += 1 }
    i = 0

    while i < vLen do
      val index = v(i) & 0xff
      v(i) = mtf(index).toByte
      if index != 0 then moveToFront(mtf, index)
      i += 1

  def decode(input: scala.Array[Byte], inputLength: Int): scala.Array[Byte] =
    BrotliDecode(input, inputLength).run()

// A single-shot decoder instance holding all mutable decoding state. Not reusable; create one per
// decode. `run()` returns exactly the decoded bytes.
private[pneumatic] final class BrotliDecode(source: scala.Array[Byte], sourceLength: Int)
extends caps.Mutable:
  import BrotliDecoder.*
  import BrotliTables.*

  // --- Bit reader (LSB-first, over a zero-padded copy of the whole input) ------------------------
  private var data: scala.Array[Byte]^ =
    val buffer: scala.Array[Byte]^ = new scala.Array[Byte](sourceLength + 16)
    System.arraycopy(source, 0, buffer, 0, sourceLength)
    buffer

  private var bytePos: Int = 0
  private var accumulator: Long = 0L
  private var bitOffset: Int = 64

  private update def nextInt(): Int =
    val value =
      (data(bytePos) & 0xff) | ((data(bytePos + 1) & 0xff) << 8) |
        ((data(bytePos + 2) & 0xff) << 16) | ((data(bytePos + 3) & 0xff) << 24)

    bytePos += 4
    value

  private update def fillBitWindow(): Unit =
    if bitOffset >= 32 then
      accumulator = (nextInt().toLong << 32) | (accumulator >>> 32)
      bitOffset -= 32

  private update def readBits(n: Int): Int =
    fillBitWindow()
    val value = ((accumulator >>> bitOffset).toInt) & ((1 << n) - 1)
    bitOffset += n
    value

  private update def jumpToByteBoundary(): Unit =
    val padding = (64 - bitOffset) & 7
    if padding != 0 && readBits(padding) != 0 then corrupt("non-zero padding bits")

  private update def prime(): Unit =
    accumulator = 0L
    bitOffset = 64
    fillBitWindow()
    fillBitWindow()

  // Copy `length` raw (byte-aligned) bytes into `dst`; valid only right after a byte boundary.
  private update def copyRawBytes(dst: scala.Array[Byte]^{this}, offset: Int, length: Int): Unit =
    val streamPos = bytePos - ((64 - bitOffset) >> 3)
    System.arraycopy(data, streamPos, dst, offset, length)
    bytePos = streamPos + length
    prime()

  // --- Output (one growable array; `pos` is the count of decoded bytes) --------------------------
  private var output: scala.Array[Byte]^ =
    new scala.Array[Byte](if sourceLength < 64 then 256 else sourceLength*4)

  private var pos: Int = 0

  private update def ensureCapacity(needed: Int): Unit =
    if needed > output.length then
      var size = output.length
      while size < needed do size <<= 1
      val grown: scala.Array[Byte]^ = new scala.Array[Byte](size)
      System.arraycopy(output, 0, grown, 0, pos)
      output = grown

  // --- Meta-block and Huffman state --------------------------------------------------------------
  private var metaBlockLength = 0
  private var inputEnd = false
  private var isUncompressed = false
  private var isMetadata = false

  private var blockTypeTrees: scala.Array[Int]^ = new scala.Array[Int](3*HuffmanMaxTableSize)
  private var blockLenTrees: scala.Array[Int]^ = new scala.Array[Int](3*HuffmanMaxTableSize)
  private var blockLength: scala.Array[Int]^ = new scala.Array[Int](3)
  private var numBlockTypes: scala.Array[Int]^ = new scala.Array[Int](3)
  private var blockTypeRb: scala.Array[Int]^ = new scala.Array[Int](6)
  private var distRb: scala.Array[Int]^ = scala.Array(16, 15, 11, 4)

  // The Huffman work table for `decodeContextMap`. A per-instance work area rather than a local,
  // because a fresh local array cannot flow into the `^{this}`-typed table parameters of
  // `readHuffmanCode` and `readSymbol`; it is fully rebuilt before each use.
  private var contextTable: scala.Array[Int]^ = new scala.Array[Int](HuffmanMaxTableSize)

  private var hGroup0Codes: scala.Array[Int]^ = new scala.Array[Int](0)
  private var hGroup0Trees: scala.Array[Int]^ = new scala.Array[Int](0)
  private var hGroup1Codes: scala.Array[Int]^ = new scala.Array[Int](0)
  private var hGroup1Trees: scala.Array[Int]^ = new scala.Array[Int](0)
  private var hGroup2Codes: scala.Array[Int]^ = new scala.Array[Int](0)
  private var hGroup2Trees: scala.Array[Int]^ = new scala.Array[Int](0)

  private var maxDistance = 0
  private var distRbIdx = 0
  private var trivialLiteralContext = false
  private var literalTreeIndex = 0
  private var literalTree = 0
  private var insertLength = 0
  private var contextModes: scala.Array[Byte]^ = new scala.Array[Byte](0)
  private var contextMap: scala.Array[Byte]^ = new scala.Array[Byte](0)
  private var distContextMap: scala.Array[Byte]^ = new scala.Array[Byte](0)
  private var contextMapSlice = 0
  private var distContextMapSlice = 0
  private var contextLookupOffset1 = 0
  private var contextLookupOffset2 = 0
  private var treeCommandOffset = 0
  private var distanceCode = 0
  private var numDirectDistanceCodes = 0
  private var distancePostfixMask = 0
  private var distancePostfixBits = 0
  private var distance = 0
  private var copyLength = 0
  private var copyDst = 0
  private var maxBackwardDistance = 0

  // --- Header ------------------------------------------------------------------------------------
  private update def decodeWindowBits(): Int =
    if readBits(1) == 0 then 16 else
      val n = readBits(3)

      if n != 0 then 17 + n else
        val m = readBits(3)
        if m != 0 then 8 + m else 17

  private update def decodeVarLenUnsignedByte(): Int =
    if readBits(1) != 0 then
      val n = readBits(3)
      if n == 0 then 1 else readBits(n) + (1 << n)
    else
      0

  private update def decodeMetaBlockLength(): Unit =
    inputEnd = readBits(1) == 1
    metaBlockLength = 0
    isUncompressed = false
    isMetadata = false

    if inputEnd && readBits(1) != 0 then () // ISLASTEMPTY: an empty final meta-block
    else
      val sizeNibbles = readBits(2) + 4
      var earlyReturn = false

      if sizeNibbles == 7 then
        isMetadata = true
        if readBits(1) != 0 then corrupt("reserved bit set")
        val sizeBytes = readBits(2)

        if sizeBytes == 0 then earlyReturn = true // empty metadata block: length stays 0
        else
          var i = 0

          while i < sizeBytes do
            val bits = readBits(8)
            if bits == 0 && i + 1 == sizeBytes && sizeBytes > 1 then corrupt("exuberant nibble")
            metaBlockLength |= bits << (i*8)
            i += 1
      else
        var i = 0

        while i < sizeNibbles do
          val bits = readBits(4)
          if bits == 0 && i + 1 == sizeNibbles && sizeNibbles > 4 then corrupt("exuberant nibble")
          metaBlockLength |= bits << (i*4)
          i += 1

      if !earlyReturn then
        metaBlockLength += 1
        if !inputEnd then isUncompressed = readBits(1) == 1

  // --- Symbol reading ----------------------------------------------------------------------------
  private update def readSymbol(table: scala.Array[Int]^{this}, offset0: Int): Int =
    var offset = offset0
    val value = (accumulator >>> bitOffset).toInt
    offset += value & HuffmanTableMask
    val bits = table(offset) >> 16
    val sym = table(offset) & 0xffff

    if bits <= HuffmanTableBits then { bitOffset += bits; sym } else
      offset += sym
      offset += (value & ((1 << bits) - 1)) >>> HuffmanTableBits
      bitOffset += (table(offset) >> 16) + HuffmanTableBits
      table(offset) & 0xffff

  private update def readBlockLength(table: scala.Array[Int]^{this}, offset: Int): Int =
    fillBitWindow()
    val code = readSymbol(table, offset)
    blockLengthOffset.readable(code) + readBits(blockLengthNBits.readable(code))

  private def translateShortCodes(code: Int): Int =
    if code < NumDistanceShortCodes then
      val index = (distRbIdx + distanceShortCodeIndexOffset.readable(code)) & 3
      distRb(index) + distanceShortCodeValueOffset.readable(code)
    else
      code - NumDistanceShortCodes + 1

  private update def readHuffmanCodeLengths
    ( codeLengthCodeLengths: scala.Array[Int], numSymbols: Int, codeLengths: scala.Array[Int]^ )
  :   Unit =

    var symbol = 0
    var prevCodeLen = DefaultCodeLength
    var repeat = 0
    var repeatCodeLen = 0
    var space = 32768
    val table: scala.Array[Int]^ = new scala.Array[Int](32)

    buildHuffmanTable(table, 0, 5, codeLengthCodeLengths, CodeLengthCodes)

    while symbol < numSymbols && space > 0 do
      fillBitWindow()
      val p = ((accumulator >>> bitOffset).toInt) & 31
      bitOffset += table(p) >> 16
      val codeLen = table(p) & 0xffff

      if codeLen < CodeLengthRepeatCode then
        repeat = 0
        codeLengths(symbol) = codeLen
        symbol += 1
        if codeLen != 0 then { prevCodeLen = codeLen; space -= 32768 >> codeLen }
      else
        val extraBits = codeLen - 14
        val newLen = if codeLen == CodeLengthRepeatCode then prevCodeLen else 0
        if repeatCodeLen != newLen then { repeat = 0; repeatCodeLen = newLen }
        val oldRepeat = repeat
        if repeat > 0 then { repeat -= 2; repeat <<= extraBits }
        repeat += readBits(extraBits) + 3
        val repeatDelta = repeat - oldRepeat
        if symbol + repeatDelta > numSymbols then corrupt("code length repeat overflow")
        var i = 0
        while i < repeatDelta do { codeLengths(symbol) = repeatCodeLen; symbol += 1; i += 1 }
        if repeatCodeLen != 0 then space -= repeatDelta << (15 - repeatCodeLen)

    if space != 0 then corrupt("unused code-length space")

  private update def readHuffmanCode(alphabetSize: Int, table: scala.Array[Int]^{this}, offset: Int)
  :   Unit =
    var ok = true
    val codeLengths: scala.Array[Int]^ = new scala.Array[Int](alphabetSize)
    val simpleCodeOrSkip = readBits(2)

    if simpleCodeOrSkip == 1 then
      var maxBitsCounter = alphabetSize - 1
      var maxBits = 0
      val symbols: scala.Array[Int]^ = new scala.Array[Int](4)
      val numSymbols = readBits(2) + 1
      while maxBitsCounter != 0 do { maxBitsCounter >>= 1; maxBits += 1 }
      var i = 0

      while i < numSymbols do
        symbols(i) = readBits(maxBits) % alphabetSize
        codeLengths(symbols(i)) = 2
        i += 1

      codeLengths(symbols(0)) = 1

      numSymbols match
        case 1 => ()

        case 2 =>
          ok = symbols(0) != symbols(1)
          codeLengths(symbols(1)) = 1

        case 3 =>
          ok = symbols(0) != symbols(1) && symbols(0) != symbols(2) && symbols(1) != symbols(2)

        case _ =>
          ok = symbols(0) != symbols(1) && symbols(0) != symbols(2) && symbols(0) != symbols(3) &&
            symbols(1) != symbols(2) && symbols(1) != symbols(3) && symbols(2) != symbols(3)

          if readBits(1) == 1 then
            codeLengths(symbols(2)) = 3
            codeLengths(symbols(3)) = 3
          else
            codeLengths(symbols(0)) = 2
    else
      val codeLengthCodeLengths: scala.Array[Int]^ = new scala.Array[Int](CodeLengthCodes)
      var space = 32
      var numCodes = 0
      var i = simpleCodeOrSkip

      while i < CodeLengthCodes && space > 0 do
        val codeLenIdx = codeLengthCodeOrder.readable(i)
        fillBitWindow()
        val p = ((accumulator >>> bitOffset).toInt) & 15
        bitOffset += fixedTable.readable(p) >> 16
        val v = fixedTable.readable(p) & 0xffff
        codeLengthCodeLengths(codeLenIdx) = v
        if v != 0 then { space -= 32 >> v; numCodes += 1 }
        i += 1

      ok = numCodes == 1 || space == 0
      readHuffmanCodeLengths(codeLengthCodeLengths, alphabetSize, codeLengths)

    if !ok then corrupt("invalid Huffman code")
    buildHuffmanTable(table, offset, HuffmanTableBits, codeLengths, alphabetSize)

  private update def decodeContextMap(contextMapSize: Int, contextMap: scala.Array[Byte]^{this}): Int =
    val numTrees = decodeVarLenUnsignedByte() + 1

    if numTrees == 1 then numTrees else
      val useRleForZeros = readBits(1) == 1
      val maxRunLengthPrefix = if useRleForZeros then readBits(4) + 1 else 0
      readHuffmanCode(numTrees + maxRunLengthPrefix, contextTable, 0)
      var i = 0

      while i < contextMapSize do
        fillBitWindow()
        val code = readSymbol(contextTable, 0)

        if code == 0 then { contextMap(i) = 0; i += 1 }
        else if code <= maxRunLengthPrefix then
          var reps = (1 << code) + readBits(code)

          while reps != 0 do
            if i >= contextMapSize then corrupt("context map overflow")
            contextMap(i) = 0
            i += 1
            reps -= 1
        else
          contextMap(i) = (code - maxRunLengthPrefix).toByte
          i += 1

      if readBits(1) == 1 then inverseMoveToFrontTransform(contextMap, contextMapSize)
      numTrees

  private update def decodeBlockTypeAndLength(treeType: Int): Unit =
    val offset = treeType*2
    fillBitWindow()
    var blockType = readSymbol(blockTypeTrees, treeType*HuffmanMaxTableSize)
    blockLength(treeType) = readBlockLength(blockLenTrees, treeType*HuffmanMaxTableSize)

    if blockType == 1 then blockType = blockTypeRb(offset + 1) + 1
    else if blockType == 0 then blockType = blockTypeRb(offset)
    else blockType -= 2

    if blockType >= numBlockTypes(treeType) then blockType -= numBlockTypes(treeType)
    blockTypeRb(offset) = blockTypeRb(offset + 1)
    blockTypeRb(offset + 1) = blockType

  private update def decodeLiteralBlockSwitch(): Unit =
    decodeBlockTypeAndLength(0)
    val literalBlockType = blockTypeRb(1)
    contextMapSlice = literalBlockType << LiteralContextBits
    literalTreeIndex = contextMap(contextMapSlice) & 0xff
    literalTree = hGroup0Trees(literalTreeIndex)
    val contextMode = contextModes(literalBlockType).toInt
    contextLookupOffset1 = contextLookupOffsets.readable(contextMode)
    contextLookupOffset2 = contextLookupOffsets.readable(contextMode + 1)

  private update def decodeCommandBlockSwitch(): Unit =
    decodeBlockTypeAndLength(1)
    treeCommandOffset = hGroup1Trees(blockTypeRb(3))

  private update def decodeDistanceBlockSwitch(): Unit =
    decodeBlockTypeAndLength(2)
    distContextMapSlice = blockTypeRb(5) << DistanceContextBits

  private update def huffmanTreeGroupDecode
    ( codes: scala.Array[Int]^{this}, trees: scala.Array[Int]^{this}, alphabetSize: Int )
  :   Unit =

    var next = 0
    var i = 0

    while i < trees.length do
      trees(i) = next
      readHuffmanCode(alphabetSize, codes, next)
      next += HuffmanMaxTableSize
      i += 1

  private update def readMetablockHuffmanCodesAndContextMaps(): Unit =
    var i = 0

    while i < 3 do
      numBlockTypes(i) = decodeVarLenUnsignedByte() + 1
      blockLength(i) = 1 << 28

      if numBlockTypes(i) > 1 then
        readHuffmanCode(numBlockTypes(i) + 2, blockTypeTrees, i*HuffmanMaxTableSize)
        readHuffmanCode(NumBlockLengthCodes, blockLenTrees, i*HuffmanMaxTableSize)
        blockLength(i) = readBlockLength(blockLenTrees, i*HuffmanMaxTableSize)

      i += 1

    distancePostfixBits = readBits(2)
    numDirectDistanceCodes = NumDistanceShortCodes + (readBits(4) << distancePostfixBits)
    distancePostfixMask = (1 << distancePostfixBits) - 1
    val numDistanceCodes = numDirectDistanceCodes + (48 << distancePostfixBits)

    contextModes = new scala.Array[Byte](numBlockTypes(0))
    i = 0

    while i < numBlockTypes(0) do
      contextModes(i) = (readBits(2) << 1).toByte
      i += 1

    contextMap = new scala.Array[Byte](numBlockTypes(0) << LiteralContextBits)
    val numLiteralTrees = decodeContextMap(numBlockTypes(0) << LiteralContextBits, contextMap)
    trivialLiteralContext = true
    var j = 0

    while j < (numBlockTypes(0) << LiteralContextBits) do
      if contextMap(j) != (j >> LiteralContextBits).toByte then
        trivialLiteralContext = false
        j = numBlockTypes(0) << LiteralContextBits
      else
        j += 1

    distContextMap = new scala.Array[Byte](numBlockTypes(2) << DistanceContextBits)
    val numDistTrees = decodeContextMap(numBlockTypes(2) << DistanceContextBits, distContextMap)

    hGroup0Codes = new scala.Array[Int](numLiteralTrees*HuffmanMaxTableSize)
    hGroup0Trees = new scala.Array[Int](numLiteralTrees)
    hGroup1Codes = new scala.Array[Int](numBlockTypes(1)*HuffmanMaxTableSize)
    hGroup1Trees = new scala.Array[Int](numBlockTypes(1))
    hGroup2Codes = new scala.Array[Int](numDistTrees*HuffmanMaxTableSize)
    hGroup2Trees = new scala.Array[Int](numDistTrees)

    huffmanTreeGroupDecode(hGroup0Codes, hGroup0Trees, NumLiteralCodes)
    huffmanTreeGroupDecode(hGroup1Codes, hGroup1Trees, NumInsertAndCopyCodes)
    huffmanTreeGroupDecode(hGroup2Codes, hGroup2Trees, numDistanceCodes)

    contextMapSlice = 0
    distContextMapSlice = 0
    contextLookupOffset1 = contextLookupOffsets.readable(contextModes(0).toInt)
    contextLookupOffset2 = contextLookupOffsets.readable(contextModes(0).toInt + 1)
    literalTreeIndex = 0
    literalTree = hGroup0Trees(0)
    treeCommandOffset = hGroup1Trees(0)

    blockTypeRb(0) = 1; blockTypeRb(2) = 1; blockTypeRb(4) = 1
    blockTypeRb(1) = 0; blockTypeRb(3) = 0; blockTypeRb(5) = 0

  // --- Command loop ------------------------------------------------------------------------------
  private update def runCommands(): Unit =
    while metaBlockLength > 0 do
      if blockLength(1) == 0 then decodeCommandBlockSwitch()
      blockLength(1) -= 1
      fillBitWindow()
      val cmdCode = readSymbol(hGroup1Codes, treeCommandOffset)
      var rangeIdx = cmdCode >>> 6
      distanceCode = 0
      if rangeIdx >= 2 then { rangeIdx -= 2; distanceCode = -1 }
      val insertCode = insertRangeLut.readable(rangeIdx) + ((cmdCode >>> 3) & 7)
      val copyCode = copyRangeLut.readable(rangeIdx) + (cmdCode & 7)
      insertLength = insertLengthOffset.readable(insertCode) + readBits(insertLengthNBits.readable(insertCode))
      copyLength = copyLengthOffset.readable(copyCode) + readBits(copyLengthNBits.readable(copyCode))

      ensureCapacity(pos + insertLength)

      if trivialLiteralContext then
        var j = 0

        while j < insertLength do
          if blockLength(0) == 0 then decodeLiteralBlockSwitch()
          blockLength(0) -= 1
          fillBitWindow()
          output(pos) = readSymbol(hGroup0Codes, literalTree).toByte
          pos += 1
          j += 1
      else
        var prevByte1 = if pos >= 1 then output(pos - 1) & 0xff else 0
        var prevByte2 = if pos >= 2 then output(pos - 2) & 0xff else 0
        var j = 0

        while j < insertLength do
          if blockLength(0) == 0 then decodeLiteralBlockSwitch()

          val treeIndex = contextMap(contextMapSlice +
            (contextLookup.readable(contextLookupOffset1 + prevByte1) |
              contextLookup.readable(contextLookupOffset2 + prevByte2))) & 0xff

          blockLength(0) -= 1
          prevByte2 = prevByte1
          fillBitWindow()
          prevByte1 = readSymbol(hGroup0Codes, hGroup0Trees(treeIndex))
          output(pos) = prevByte1.toByte
          pos += 1
          j += 1

      metaBlockLength -= insertLength

      if metaBlockLength <= 0 then () else
        if distanceCode < 0 then
          if blockLength(2) == 0 then decodeDistanceBlockSwitch()
          blockLength(2) -= 1
          fillBitWindow()

          distanceCode = readSymbol(hGroup2Codes, hGroup2Trees(
              distContextMap(distContextMapSlice +
                (if copyLength > 4 then 3 else copyLength - 2)) & 0xff))

          if distanceCode >= numDirectDistanceCodes then
            distanceCode -= numDirectDistanceCodes
            val postfix = distanceCode & distancePostfixMask
            distanceCode >>>= distancePostfixBits
            val n = (distanceCode >>> 1) + 1
            val offset = ((2 + (distanceCode & 1)) << n) - 4

            distanceCode =
              numDirectDistanceCodes + postfix + ((offset + readBits(n)) << distancePostfixBits)

        distance = translateShortCodes(distanceCode)
        if distance < 0 then corrupt("negative distance")

        if maxDistance != maxBackwardDistance && pos < maxBackwardDistance then maxDistance = pos
        else maxDistance = maxBackwardDistance

        copyDst = pos

        if distance > maxDistance then
          if copyLength >= minWordLength && copyLength <= maxWordLength then
            var offset = offsetsByLength.readable(copyLength)
            val wordId = distance - maxDistance - 1
            val shift = sizeBitsByLength.readable(copyLength)
            val mask = (1 << shift) - 1
            val wordIdx = wordId & mask
            val transformIdx = wordId >>> shift
            offset += wordIdx*copyLength
            if transformIdx >= transforms.length then corrupt("invalid dictionary reference")
            ensureCapacity(pos + maxTransformedWordLength)

            val len = BrotliDictionary.transformDictionaryWord(output, copyDst, dictData, offset,
                copyLength, transforms.readable(transformIdx))

            copyDst += len
            pos += len
            metaBlockLength -= len
          else
            corrupt("invalid dictionary reference")
        else
          if distanceCode > 0 then { distRb(distRbIdx & 3) = distance; distRbIdx += 1 }
          if copyLength > metaBlockLength then corrupt("invalid backward reference")
          ensureCapacity(pos + copyLength)
          var j = 0

          while j < copyLength do
            output(pos) = output(pos - distance)
            pos += 1
            metaBlockLength -= 1
            j += 1

  // --- Driver ------------------------------------------------------------------------------------
  private val minWordLength = BrotliDictionary.minWordLength
  private val maxWordLength = BrotliDictionary.maxWordLength
  private val maxTransformedWordLength = BrotliDictionary.maxTransformedWordLength
  private val transforms = BrotliDictionary.transforms
  private val offsetsByLength = BrotliDictionary.offsetsByLength
  private val sizeBitsByLength = BrotliDictionary.sizeBitsByLength
  private val dictData = BrotliDictionary.data

  update def run(): scala.Array[Byte] =
    prime()
    val windowBits = decodeWindowBits()
    if windowBits == 9 then corrupt("invalid window-bits code")
    maxBackwardDistance = (1 << windowBits) - 16

    var done = false

    while !done do
      decodeMetaBlockLength()

      if metaBlockLength == 0 && !isMetadata then { if inputEnd then done = true }
      else if isMetadata then
        jumpToByteBoundary()
        var k = 0
        while k < metaBlockLength do { readBits(8); k += 1 }
        if inputEnd then done = true
      else if isUncompressed then
        jumpToByteBoundary()
        ensureCapacity(pos + metaBlockLength)
        copyRawBytes(output, pos, metaBlockLength)
        pos += metaBlockLength
        if inputEnd then done = true
      else
        readMetablockHuffmanCodesAndContextMaps()
        runCommands()
        if inputEnd then done = true

    jumpToByteBoundary()
    val result: scala.Array[Byte]^ = new scala.Array[Byte](pos)
    System.arraycopy(output, 0, result, 0, pos)
    result
