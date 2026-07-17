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
package turbulence

// A little-endian (LSB-first) bit-stream writer, the mirror of the decoder's `BitReader`. Bits
// accumulate into a 64-bit register and are flushed a byte at a time; `align` pads the current byte
// with zero bits, after which `writeBytes` may append raw byte-aligned data.
private[turbulence] final class BrotliBitWriter:
  private var out: Array[Byte] = new Array[Byte](256)
  private var size: Int = 0
  private var accumulator: Long = 0L
  private var bitCount: Int = 0

  private def ensure(extra: Int): Unit =
    if size + extra > out.length then
      var grown = out.length
      while size + extra > grown do grown <<= 1
      val fresh = new Array[Byte](grown)
      System.arraycopy(out, 0, fresh, 0, size)
      out = fresh

  def writeBits(value: Int, n: Int): Unit =
    accumulator |= (value.toLong & ((1L << n) - 1)) << bitCount
    bitCount += n

    while bitCount >= 8 do
      ensure(1)
      out(size) = (accumulator & 0xff).toByte
      size += 1
      accumulator >>>= 8
      bitCount -= 8

  def align(): Unit =
    if bitCount > 0 then
      ensure(1)
      out(size) = (accumulator & 0xff).toByte
      size += 1
      accumulator = 0L
      bitCount = 0

  def writeBytes(bytes: Array[Byte], offset: Int, length: Int): Unit =
    ensure(length)
    System.arraycopy(bytes, offset, out, size, length)
    size += length

  def result(): Array[Byte] =
    val array = new Array[Byte](size)
    System.arraycopy(out, 0, array, 0, size)
    array

// Brotli encoder (RFC 7932). Because there is no trusted pure-Java Brotli encoder to port, this is
// written from scratch against the specification, reusing the trusted Huffman-tree construction
// from Google's reference C encoder (`entropy_encode.c`, MIT-licensed). It is a fast,
// single-block-type encoder: greedy hash-chain LZ77 backward references plus a per-meta-block
// literal / insert-and-copy / distance Huffman code. It emits valid Brotli any decoder accepts,
// at a ratio between raw deflate and full-quality brotli. Incompressible or oversized inputs fall
// back to stored (uncompressed) meta-blocks. The parameters (single block type, no context
// modelling, no distance
// cache, NPOSTFIX/NDIRECT = 0) keep the meta-block structure simple while remaining spec-compliant.
private[turbulence] object BrotliEncoder:
  import BrotliTables.*

  private final val MaxMetaBlock = 1 << 24
  private final val MinMatch = 4
  private final val HashBits = 17
  private final val HashSize = 1 << HashBits
  private final val RingSize = 1 << 18 // hash-chain ring; bounds chain memory for large inputs
  private final val MaxChain = 64      // longest chain walk per position
  private final val NiceLength = 128   // stop searching at a match this good

  def encode(input: Array[Byte], length: Int): Array[Byte] =
    if length == 0 then
      val writer = BrotliBitWriter()
      writer.writeBits(0, 1) // WBITS = 16
      writer.writeBits(1, 1) // ISLAST = 1
      writer.writeBits(1, 1) // ISLASTEMPTY = 1
      writer.align()
      writer.result()
    else if length > MaxMetaBlock then
      val storedWriter = BrotliBitWriter()
      storeAll(storedWriter, input, length)
      storedWriter.result()
    else
      val writer = BrotliBitWriter()
      compressBlock(writer, input, length)
      val compressed = writer.result()

      // Never expand beyond the stored framing: for tiny or incompressible inputs the
      // Huffman-tree headers can cost more than they save. The stored size is computed
      // analytically, so the stored form is only materialized when it wins.
      if compressed.length < storedSize(length) then compressed
      else
        val storedWriter = BrotliBitWriter()
        storeAll(storedWriter, input, length)
        storedWriter.result()

  // The exact byte length `storeAll` would produce, without producing it.
  private def storedSize(length: Int): Int =
    var bits = 1 // WBITS
    var pos = 0

    while pos < length do
      val chunk = Math.min(length - pos, MaxMetaBlock)
      val value = chunk - 1
      val nibbles = if value < (1 << 16) then 4 else if value < (1 << 20) then 5 else 6
      bits += 1 + 2 + nibbles*4 + 1
      bits = (bits + 7) & ~7 // align
      bits += chunk*8
      pos += chunk

    bits += 2
    bits = (bits + 7) & ~7 // final align

    bits >>> 3

  // Fallback: frame the payload as uncompressed meta-blocks (see the class comment on RFC framing).
  private def storeAll(writer: BrotliBitWriter, input: Array[Byte], length: Int): Unit =
    writer.writeBits(0, 1) // WBITS = 16
    var pos = 0

    while pos < length do
      val chunk = Math.min(length - pos, MaxMetaBlock)
      writer.writeBits(0, 1) // ISLAST = 0
      val value = chunk - 1
      val nibbles = if value < (1 << 16) then 4 else if value < (1 << 20) then 5 else 6
      writer.writeBits(nibbles - 4, 2)
      var i = 0
      while i < nibbles do { writer.writeBits((value >>> (i*4)) & 0xf, 4); i += 1 }
      writer.writeBits(1, 1) // ISUNCOMPRESSED = 1
      writer.align()
      writer.writeBytes(input, pos, chunk)
      pos += chunk

    writer.writeBits(1, 1) // ISLAST = 1
    writer.writeBits(1, 1) // ISLASTEMPTY = 1
    writer.align()

  // --- Huffman construction, ported from the reference C encoder (entropy_encode.c) --------------
  private def reverseBits(nBits: Int, value: Int): Int =
    var retval = 0
    var v = value
    var i = 0
    while i < nBits do { retval = (retval << 1) | (v & 1); v >>= 1; i += 1 }
    retval

  private def convertBitDepthsToSymbols(depth: Array[Byte], len: Int, bits: Array[Int]): Unit =
    val blCount = new Array[Int](16)
    val nextCode = new Array[Int](16)
    var i = 0
    while i < len do { blCount(depth(i) & 0xff) += 1; i += 1 }
    blCount(0) = 0
    var code = 0
    nextCode(0) = 0
    i = 1
    while i < 16 do { code = (code + blCount(i - 1)) << 1; nextCode(i) = code; i += 1 }
    i = 0

    while i < len do
      val d = depth(i) & 0xff
      if d != 0 then { bits(i) = reverseBits(d, nextCode(d)); nextCode(d) += 1 }
      i += 1

  private def setDepth
    ( p0: Int, total: Array[Int], left: Array[Int], right: Array[Int], depth: Array[Byte],
      maxDepth: Int )
  :   Boolean =

    val stack = new Array[Int](16)
    var level = 0
    var p = p0
    stack(0) = -1
    var result = 0 // 0 = running, 1 = success, 2 = failure

    while result == 0 do
      if left(p) >= 0 then
        level += 1

        if level > maxDepth then result = 2
        else { stack(level) = right(p); p = left(p) }
      else
        depth(right(p)) = level.toByte
        while level >= 0 && stack(level) == -1 do level -= 1

        if level < 0 then result = 1
        else { p = stack(level); stack(level) = -1 }

    result == 1

  // Sort leaf nodes [0, n) least popular first, ties broken by larger value (reference comparator).
  private def sortLeaves(total: Array[Int], right: Array[Int], n: Int): Unit =
    var i = 1

    while i < n do
      val t = total(i)
      val r = right(i)
      var j = i - 1

      while j >= 0 && (total(j) > t || (total(j) == t && right(j) < r)) do
        total(j + 1) = total(j)
        right(j + 1) = right(j)
        j -= 1

      total(j + 1) = t
      right(j + 1) = r
      i += 1

  private def createHuffmanTree
    ( data: Array[Int], length: Int, treeLimit: Int, depth: Array[Byte] )
  :   Unit =

    val total = new Array[Int](2*length + 1)
    val left = new Array[Int](2*length + 1)
    val right = new Array[Int](2*length + 1)
    var i = 0
    while i < length do { depth(i) = 0; i += 1 }

    var countLimit = 1
    var done = false

    while !done do
      var n = 0
      i = length

      while i != 0 do
        i -= 1

        if data(i) != 0 then
          val count = if data(i) > countLimit then data(i) else countLimit
          total(n) = count; left(n) = -1; right(n) = i; n += 1

      if n == 1 then
        depth(right(0)) = 1
        done = true
      else
        sortLeaves(total, right, n)
        // Leaves have left = -1; re-establish that after the sort's partial arrays.
        var s = 0
        while s < n do { left(s) = -1; s += 1 }

        total(n) = Int.MaxValue; left(n) = -1; right(n) = -1
        total(n + 1) = Int.MaxValue; left(n + 1) = -1; right(n + 1) = -1

        var ii = 0
        var jj = n + 1
        var k = n - 1

        while k != 0 do
          // Take the two smallest-count nodes (next leaf or internal node), advancing each cursor.
          val l = if total(ii) <= total(jj) then ii else jj
          if l == ii then ii += 1 else jj += 1
          val r = if total(ii) <= total(jj) then ii else jj
          if r == ii then ii += 1 else jj += 1
          val jEnd = 2*n - k
          total(jEnd) = total(l) + total(r)
          left(jEnd) = l
          right(jEnd) = r
          total(jEnd + 1) = Int.MaxValue; left(jEnd + 1) = -1; right(jEnd + 1) = -1
          k -= 1

        if setDepth(2*n - 1, total, left, right, depth, treeLimit) then done = true
        else countLimit <<= 1

  // --- Huffman-tree serialization (inverse of the decoder's readHuffmanCode) ---------------------
  private val codeLengthCodeOrder: Array[Int] =
    Array(1, 2, 3, 4, 0, 5, 17, 6, 16, 7, 8, 9, 10, 11, 12, 13, 14, 15)

  private def writeCodeLengthCodeLength(writer: BrotliBitWriter, v: Int): Unit = v match
    case 0 => writer.writeBits(0, 2)
    case 1 => writer.writeBits(7, 4)
    case 2 => writer.writeBits(3, 3)
    case 3 => writer.writeBits(2, 2)
    case 4 => writer.writeBits(1, 2)
    case _ => writer.writeBits(15, 4)

  private def bitsForCount(count: Int): Int =
    var v = count
    var bits = 0
    while v != 0 do { v >>= 1; bits += 1 }
    bits

  // Emit the Huffman code for `depth`/`codes` over `alphabetSize` symbols. Trees with a single used
  // symbol use the compact "simple" form (0 bits per symbol); all others use the "complex" form,
  // writing the code-length symbol stream without run-length compression (valid, and negligible
  // overhead for large blocks).
  private def storeHuffmanTree
    ( writer: BrotliBitWriter, depth: Array[Byte], codes: Array[Int], alphabetSize: Int )
  :   Unit =

    var used = 0
    var lastNonZero = -1
    var onlySymbol = 0
    var i = 0

    while i < alphabetSize do
      if depth(i) != 0 then { used += 1; lastNonZero = i; onlySymbol = i }
      i += 1

    if used <= 1 then
      writer.writeBits(1, 2) // simple code
      writer.writeBits(0, 2) // NSYM - 1 = 0
      writer.writeBits(onlySymbol, bitsForCount(alphabetSize - 1))
      // A single-symbol tree is decoded as a zero-length code, so the payload must spend no bits on
      // it; clear the depth accordingly for the data-writing pass.
      depth(onlySymbol) = 0
    else
      val streamLen = lastNonZero + 1
      val histogram = new Array[Int](18)
      i = 0
      while i < streamLen do { histogram(depth(i) & 0xff) += 1; i += 1 }
      val clcDepth = new Array[Byte](18)
      createHuffmanTree(histogram, 18, 5, clcDepth)
      val clcCodes = new Array[Int](18)
      convertBitDepthsToSymbols(clcDepth, 18, clcCodes)

      // When a single code-length value dominates, its code-length-code has one symbol and the
      // decoder reads the stream with zero bits per symbol, so we must write zero bits too.
      var clcUsed = 0
      var s = 0
      while s < 18 do { if clcDepth(s) != 0 then clcUsed += 1; s += 1 }
      val singleClc = clcUsed == 1

      writer.writeBits(0, 2) // HSKIP = 0 (complex code, read all code-length codes)
      var space = 32
      i = 0

      while i < 18 && space > 0 do
        val sym = codeLengthCodeOrder(i)
        val v = clcDepth(sym) & 0xff
        writeCodeLengthCodeLength(writer, v)
        if v != 0 then space -= 32 >> v
        i += 1

      i = 0

      while i < streamLen do
        val len = depth(i) & 0xff
        if !singleClc then writer.writeBits(clcCodes(len), clcDepth(len) & 0xff)
        i += 1

  // --- Length/distance prefix codes --------------------------------------------------------------
  // Scanning upward exits immediately for the common short lengths.
  private def lengthCode(offsets: Array[Int], nbits: Array[Int], length: Int): Int =
    var i = 0
    while i + 1 < offsets.length && offsets(i + 1) <= length do i += 1
    i

  // Distance symbol and extra bits for a backward distance, with NPOSTFIX = 0, NDIRECT = 0. The
  // symbol for distance d covers d+3 in [(2+b) << n, (3+b) << n), located directly from the bit
  // width of d+3.
  private def distanceCode(distance: Int): Long =
    val x = distance + 3
    val n = 30 - Integer.numberOfLeadingZeros(x) // 31 - nlz(x) - 1
    val b = if x >= (3 << n) then 1 else 0
    val j = ((n - 1) << 1) + b
    val low = ((2 + b) << n) - 3
    val extra = distance - low
    ((16 + j).toLong << 40) | (n.toLong << 32) | (extra.toLong & 0xffffffffL)

  // --- Compressed meta-block ---------------------------------------------------------------------
  private def compressBlock(writer: BrotliBitWriter, input: Array[Byte], length: Int): Unit =
    val windowBits = if length <= (1 << 22) then 22 else 24
    val maxDistance = (1 << windowBits) - 16

    // LZ77 command generation: greedy hash-chain search over 4-byte prefixes. The chain links
    // live in a ring of positions (like zlib's `prev` array), bounded by `RingSize`; entries that
    // alias across the ring appear as non-decreasing positions and terminate the walk. Commands
    // accumulate in growable parallel `Int` arrays (unboxed, unlike an `ArrayBuffer[Int]`).
    val head = new Array[Int](HashSize)
    var h = 0
    while h < HashSize do { head(h) = -1; h += 1 }

    val ringMask = RingSize - 1
    val chain = new Array[Int](Math.min(length, RingSize))

    var capacity = 1024
    var cmdInsert = new Array[Int](capacity)
    var cmdLitPos = new Array[Int](capacity)
    var cmdCopy = new Array[Int](capacity)
    var cmdDist = new Array[Int](capacity)
    var commands = 0

    def push(insert: Int, litPos: Int, copy: Int, dist: Int): Unit =
      if commands == capacity then
        capacity <<= 1
        val ni = new Array[Int](capacity); System.arraycopy(cmdInsert, 0, ni, 0, commands)
        val nl = new Array[Int](capacity); System.arraycopy(cmdLitPos, 0, nl, 0, commands)
        val nc = new Array[Int](capacity); System.arraycopy(cmdCopy, 0, nc, 0, commands)
        val nd = new Array[Int](capacity); System.arraycopy(cmdDist, 0, nd, 0, commands)
        cmdInsert = ni; cmdLitPos = nl; cmdCopy = nc; cmdDist = nd

      cmdInsert(commands) = insert
      cmdLitPos(commands) = litPos
      cmdCopy(commands) = copy
      cmdDist(commands) = dist
      commands += 1

    def hashAt(p: Int): Int =
      val v = (input(p) & 0xff) | ((input(p + 1) & 0xff) << 8) | ((input(p + 2) & 0xff) << 16) |
        ((input(p + 3) & 0xff) << 24)

      (v*0x1e35a7bd) >>> (32 - HashBits)

    var pos = 0
    var literalStart = 0

    while pos < length do
      var matched = false

      if pos + MinMatch <= length then
        val hv = hashAt(pos)
        var candidate = head(hv)
        chain(pos & ringMask) = candidate
        head(hv) = pos

        // Walk the chain for the longest match, newest candidate first.
        var bestLen = 0
        var bestDist = 0
        var depth = MaxChain

        while candidate >= 0 && depth > 0 && pos - candidate <= maxDistance do
          // Check the byte one past the current best first: cheap rejection of shorter matches.
          if bestLen == 0 ||
            (pos + bestLen < length && input(candidate + bestLen) == input(pos + bestLen))
          then
            var len = 0
            while pos + len < length && input(candidate + len) == input(pos + len) do len += 1

            if len > bestLen then
              bestLen = len
              bestDist = pos - candidate
              if len >= NiceLength then depth = 0

          val next = chain(candidate & ringMask)
          // A non-decreasing link means the ring entry was overwritten by a newer position.
          candidate = if next >= candidate then -1 else next
          depth -= 1

        if bestLen >= MinMatch then
          push(pos - literalStart, literalStart, bestLen, bestDist)
          var q = pos + 1
          val end = pos + bestLen

          while q < end do
            if q + MinMatch <= length then
              val qh = hashAt(q)
              chain(q & ringMask) = head(qh)
              head(qh) = q

            q += 1

          pos = end
          literalStart = pos
          matched = true

      if !matched then pos += 1

    if literalStart < length then push(length - literalStart, literalStart, 0, 0)

    // Histograms.
    val litHist = new Array[Int](256)
    val cmdHist = new Array[Int](704)
    val distHist = new Array[Int](64)
    var c = 0

    while c < commands do
      val insert = cmdInsert(c)
      val litPos = cmdLitPos(c)
      var t = 0
      while t < insert do { litHist(input(litPos + t) & 0xff) += 1; t += 1 }
      val copy = cmdCopy(c)
      val insertCode = lengthCode(insertLengthOffset, insertLengthNBits, insert)
      val copyCode = if copy > 0 then lengthCode(copyLengthOffset, copyLengthNBits, copy) else 0
      cmdHist(commandCode(insertCode, copyCode)) += 1

      if copy > 0 then
        val dc = distanceCode(cmdDist(c))
        distHist((dc >>> 40).toInt) += 1

      c += 1

    if isAllZero(litHist) then litHist(0) = 1
    if isAllZero(distHist) then distHist(0) = 1

    val litDepth = new Array[Byte](256); val litCodes = new Array[Int](256)
    val cmdDepth = new Array[Byte](704); val cmdCodes = new Array[Int](704)
    val distDepth = new Array[Byte](64); val distCodes = new Array[Int](64)
    createHuffmanTree(litHist, 256, 15, litDepth)
    convertBitDepthsToSymbols(litDepth, 256, litCodes)
    createHuffmanTree(cmdHist, 704, 15, cmdDepth)
    convertBitDepthsToSymbols(cmdDepth, 704, cmdCodes)
    createHuffmanTree(distHist, 64, 15, distDepth)
    convertBitDepthsToSymbols(distDepth, 64, distCodes)

    // --- Write the meta-block ---
    writeWindowBits(writer, windowBits)
    writer.writeBits(1, 1) // ISLAST = 1
    writer.writeBits(0, 1) // ISLASTEMPTY = 0
    val value = length - 1
    val nibbles = if value < (1 << 16) then 4 else if value < (1 << 20) then 5 else 6
    writer.writeBits(nibbles - 4, 2)
    var nb = 0
    while nb < nibbles do { writer.writeBits((value >>> (nb*4)) & 0xf, 4); nb += 1 }
    // ISUNCOMPRESSED is absent because ISLAST = 1.
    writer.writeBits(0, 1) // NBLTYPESL = 1
    writer.writeBits(0, 1) // NBLTYPESI = 1
    writer.writeBits(0, 1) // NBLTYPESD = 1
    writer.writeBits(0, 2) // NPOSTFIX = 0
    writer.writeBits(0, 4) // NDIRECT = 0
    writer.writeBits(0, 2) // literal context mode 0
    writer.writeBits(0, 1) // literal context map: 1 tree
    writer.writeBits(0, 1) // distance context map: 1 tree
    storeHuffmanTree(writer, litDepth, litCodes, 256)
    storeHuffmanTree(writer, cmdDepth, cmdCodes, 704)
    storeHuffmanTree(writer, distDepth, distCodes, 64)

    c = 0

    while c < commands do
      val insert = cmdInsert(c)
      val litPos = cmdLitPos(c)
      val copy = cmdCopy(c)
      val insertCode = lengthCode(insertLengthOffset, insertLengthNBits, insert)
      val copyCode = if copy > 0 then lengthCode(copyLengthOffset, copyLengthNBits, copy) else 0
      val cmd = commandCode(insertCode, copyCode)
      writer.writeBits(cmdCodes(cmd), cmdDepth(cmd) & 0xff)
      writer.writeBits(insert - insertLengthOffset(insertCode), insertLengthNBits(insertCode))

      if copy > 0
      then writer.writeBits(copy - copyLengthOffset(copyCode), copyLengthNBits(copyCode))
      else writer.writeBits(0, copyLengthNBits(copyCode))

      var t = 0

      while t < insert do
        val b = input(litPos + t) & 0xff
        writer.writeBits(litCodes(b), litDepth(b) & 0xff)
        t += 1

      if copy > 0 then
        val dc = distanceCode(cmdDist(c))
        val sym = (dc >>> 40).toInt
        val n = ((dc >>> 32) & 0xff).toInt
        val extra = (dc & 0xffffffffL).toInt
        writer.writeBits(distCodes(sym), distDepth(sym) & 0xff)
        writer.writeBits(extra, n)

      c += 1

    writer.align()

  private def isAllZero(histogram: Array[Int]): Boolean =
    var i = 0
    while i < histogram.length do { if histogram(i) != 0 then return false; i += 1 }
    true

  // Insert group and copy group determine the range index; see the decoder's command decoding.
  private val rangeIndex: Array[Int] =
    // indexed by insGroup*3 + copGroup, giving the base range index (0..8)
    Array(0, 1, 4, 2, 3, 6, 5, 7, 8)

  private def commandCode(insertCode: Int, copyCode: Int): Int =
    val insGroup = insertCode / 8
    val copGroup = copyCode / 8
    val base = rangeIndex(insGroup*3 + copGroup)
    ((base + 2) << 6) | ((insertCode & 7) << 3) | (copyCode & 7)

  private def writeWindowBits(writer: BrotliBitWriter, windowBits: Int): Unit =
    if windowBits == 16 then writer.writeBits(0, 1)
    else if windowBits == 17 then
      writer.writeBits(1, 1); writer.writeBits(0, 3); writer.writeBits(0, 3)
    else if windowBits >= 18 then
      writer.writeBits(1, 1); writer.writeBits(windowBits - 17, 3)
    else
      writer.writeBits(1, 1); writer.writeBits(0, 3); writer.writeBits(windowBits - 8, 3)
