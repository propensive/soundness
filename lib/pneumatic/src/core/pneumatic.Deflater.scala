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

import Flate.*
import FlateTables.*

// A streaming DEFLATE compressor, ported faithfully from JZlib (BSD 3-clause, Copyright (c)
// 2000-2011 ymnk, JCraft, Inc.), itself a port of zlib 1.1.3 by Jean-loup Gailly and Mark Adler:
// hash-chain longest-match search with lazy evaluation, dynamic Huffman trees per block, and the
// full flush protocol (`ZNoFlush`, `ZSyncFlush`, `ZFinish`), streaming with bounded memory. Only
// the zlib and raw framings are supported (`nowrap`); gzip framing is added by `Deflation`.
private[pneumatic] object Deflater:
  final val MinMatch = 3
  final val MaxMatch = 258
  final val MinLookahead = MaxMatch + MinMatch + 1

  final val MaxBits = 15
  final val DCodes = 30
  final val BlCodes = 19
  final val LengthCodes = 29
  final val Literals = 256
  final val LCodes = Literals + 1 + LengthCodes
  final val HeapSize = 2*LCodes + 1
  final val EndBlock = 256
  final val MaxBlBits = 7

  final val BufSize = 16

  final val Rep36 = 16     // repeat previous bit length 3-6 times (2 bits of repeat count)
  final val Repz310 = 17   // repeat a zero length 3-10 times (3 bits of repeat count)
  final val Repz11138 = 18 // repeat a zero length 11-138 times (7 bits of repeat count)

  final val StoredBlock = 0
  final val StaticTrees = 1
  final val DynTrees = 2

  final val ZBinary = 0
  final val ZAscii = 1
  final val ZUnknown = 2

  // block states
  final val NeedMore = 0      // block not completed, need more input or more output
  final val BlockDone = 1     // block flush performed
  final val FinishStarted = 2 // finish started, need only more output at next deflate
  final val FinishDone = 3    // finish done, accept no more input or output

  final val InitState = 42
  final val BusyState = 113
  final val FinishState = 666

  final val StoredFunc = 0
  final val FastFunc = 1
  final val SlowFunc = 2

  final val DefMemLevel = 8

  // Per-level parameters: reduce lazy search above goodLength; do not perform lazy search above
  // maxLazy; quit search above niceLength; never search chains longer than maxChain.
  val configGoodLength: Array[Int] = Array(0, 4, 4, 4, 4, 8, 8, 8, 32, 32)
  val configMaxLazy: Array[Int] = Array(0, 4, 5, 6, 4, 16, 16, 32, 128, 258)
  val configNiceLength: Array[Int] = Array(0, 8, 16, 32, 16, 32, 128, 128, 258, 258)
  val configMaxChain: Array[Int] = Array(0, 4, 8, 32, 16, 32, 128, 256, 1024, 4096)

  val configFunc: Array[Int] =
    Array(StoredFunc, FastFunc, FastFunc, FastFunc, SlowFunc, SlowFunc, SlowFunc, SlowFunc,
        SlowFunc, SlowFunc)

  // Mapping from a distance to a distance code, where dist is the distance - 1.
  def dCode(dist: Int): Int =
    if dist < 256 then distCode(dist) else distCode(256 + (dist >>> 7))

  def smaller(tree: Array[Short], n: Int, m: Int, depth: Array[Byte]): Boolean =
    val tn2 = tree(n*2)
    val tm2 = tree(m*2)
    tn2 < tm2 || (tn2 == tm2 && depth(n) <= depth(m))

  // Reverse the first len bits of a code.
  def biReverse(code0: Int, len0: Int): Int =
    var code = code0
    var len = len0
    var res = 0

    while
      res |= code & 1
      code >>>= 1
      res <<= 1
      len -= 1
      len > 0
    do ()

    res >>> 1

  // Generate the codes for a given tree and bit counts (which need not be optimal).
  def genCodes(tree: Array[Short], maxCode: Int, blCount: Array[Short], nextCode: Array[Short])
  :   Unit =

    var code: Int = 0
    nextCode(0) = 0
    var bits = 1

    while bits <= MaxBits do
      code = (code + (blCount(bits - 1) & 0xffff)) << 1
      nextCode(bits) = code.toShort
      bits += 1

    var n = 0

    while n <= maxCode do
      val len = tree(n*2 + 1).toInt

      if len != 0 then
        tree(n*2) = biReverse(nextCode(len) & 0xffff, len).toShort
        nextCode(len) = ((nextCode(len) & 0xffff) + 1).toShort

      n += 1

private[pneumatic] object TreeConfig:
  import Deflater.*

  val staticL: TreeConfig = TreeConfig(staticLtree, extraLbits, Literals + 1, LCodes, MaxBits)
  val staticD: TreeConfig = TreeConfig(staticDtree, extraDbits, 0, DCodes, MaxBits)
  val staticBl: TreeConfig = TreeConfig(emptyShorts, extraBlbits, 0, BlCodes, MaxBlBits)

// The static configuration of one of the three Huffman trees: its static counterpart (empty for
// the bit-length tree), extra-bit tables and size limits (JZlib's `StaticTree`).
private[pneumatic] final class TreeConfig
  ( val staticTree: Array[Short], val extraBits: Array[Int], val extraBase: Int, val elems: Int,
    val maxLength: Int )

// One dynamic Huffman tree under construction (JZlib's `Tree`): frequency and code arrays
// interleaved in `dynTree`, built against a `TreeConfig`.
private[pneumatic] final class FlateTree(val dynTree: Array[Short], val statDesc: TreeConfig):
  import Deflater.*

  var maxCode: Int = 0 // largest code with non-zero frequency

  // Compute the optimal bit lengths for a tree and update the total bit length for the current
  // block.
  private def genBitlen(s: Deflater): Unit =
    val tree = dynTree
    val stree = statDesc.staticTree
    val extra = statDesc.extraBits
    val base = statDesc.extraBase
    val maxLength = statDesc.maxLength
    var h = 0        // heap index
    var n = 0        // iterate over the tree elements
    var m = 0
    var bits = 0     // bit length
    var xbits = 0    // extra bits
    var f: Short = 0 // frequency
    var overflow = 0 // number of elements with bit length too large

    bits = 0
    while bits <= MaxBits do { s.blCount(bits) = 0; bits += 1 }

    // In a first pass, compute the optimal bit lengths (which may overflow in the case of the
    // bit length tree).
    tree(s.heap(s.heapMax)*2 + 1) = 0 // root of the heap

    h = s.heapMax + 1

    while h < HeapSize do
      n = s.heap(h)
      bits = tree(tree(n*2 + 1)*2 + 1) + 1

      if bits > maxLength then
        bits = maxLength
        overflow += 1

      tree(n*2 + 1) = bits.toShort // we overwrite tree(n*2 + 1) which is no longer needed

      if n <= maxCode then // a leaf node
        s.blCount(bits) = (s.blCount(bits) + 1).toShort
        xbits = 0
        if n >= base then xbits = extra(n - base)
        f = tree(n*2)
        s.optLen += f*(bits + xbits)
        if stree.length != 0 then s.staticLen += f*(stree(n*2 + 1) + xbits)

      h += 1

    if overflow != 0 then
      // This happens for example on obj2 and pic of the Calgary corpus. Find the first bit
      // length which could increase:
      while overflow > 0 do
        bits = maxLength - 1
        while s.blCount(bits) == 0 do bits -= 1
        s.blCount(bits) = (s.blCount(bits) - 1).toShort    // move one leaf down the tree
        s.blCount(bits + 1) = (s.blCount(bits + 1) + 2).toShort // an overflow item's brother
        s.blCount(maxLength) = (s.blCount(maxLength) - 1).toShort
        overflow -= 2

      bits = maxLength

      while bits != 0 do
        n = s.blCount(bits)

        while n != 0 do
          h -= 1
          m = s.heap(h)

          if m <= maxCode then
            if tree(m*2 + 1) != bits then
              s.optLen += (bits - tree(m*2 + 1))*tree(m*2)
              tree(m*2 + 1) = bits.toShort

            n -= 1

        bits -= 1

  // Construct one Huffman tree and assign the code bit strings and lengths. Update the total bit
  // length for the current block.
  def buildTree(s: Deflater): Unit =
    val tree = dynTree
    val stree = statDesc.staticTree
    val elems = statDesc.elems
    var n = 0 // iterate over heap elements
    var m = 0
    var max = -1 // largest code with non zero frequency
    var node = 0 // new node being created

    // Construct the initial heap, with least frequent element in heap(1). The sons of heap(n)
    // are heap(2*n) and heap(2*n+1). heap(0) is not used.
    s.heapLen = 0
    s.heapMax = HeapSize

    n = 0

    while n < elems do
      if tree(n*2) != 0 then
        s.heapLen += 1
        s.heap(s.heapLen) = n
        max = n
        s.depth(n) = 0
      else
        tree(n*2 + 1) = 0

      n += 1

    // The pkzip format requires that at least one distance code exists, and that at least one
    // bit should be sent even if there is only one possible code. So to avoid special checks
    // later on we force at least two codes of non zero frequency.
    while s.heapLen < 2 do
      s.heapLen += 1
      node = if max < 2 then { max += 1; max } else 0
      s.heap(s.heapLen) = node
      tree(node*2) = 1
      s.depth(node) = 0
      s.optLen -= 1
      if stree.length != 0 then s.staticLen -= stree(node*2 + 1)
      // node is 0 or 1 so it does not have extra bits

    maxCode = max

    // The elements heap(heapLen/2+1 .. heapLen) are leaves of the tree; establish sub-heaps of
    // increasing lengths:
    n = s.heapLen/2

    while n >= 1 do
      s.pqdownheap(tree, n)
      n -= 1

    // Construct the Huffman tree by repeatedly combining the least two frequent nodes.
    node = elems // next internal node of the tree

    while
      n = s.heap(1)
      s.heap(1) = s.heap(s.heapLen)
      s.heapLen -= 1
      s.pqdownheap(tree, 1)
      m = s.heap(1) // m = node of next least frequency

      s.heapMax -= 1
      s.heap(s.heapMax) = n // keep the nodes sorted by frequency
      s.heapMax -= 1
      s.heap(s.heapMax) = m

      // Create a new node father of n and m
      tree(node*2) = (tree(n*2) + tree(m*2)).toShort
      s.depth(node) = (Math.max(s.depth(n), s.depth(m)) + 1).toByte
      tree(n*2 + 1) = node.toShort
      tree(m*2 + 1) = node.toShort

      // and insert the new node in the heap
      s.heap(1) = node
      node += 1
      s.pqdownheap(tree, 1)

      s.heapLen >= 2
    do ()

    s.heapMax -= 1
    s.heap(s.heapMax) = s.heap(1)

    // At this point, the fields freq and dad are set; generate the bit lengths, then the codes.
    genBitlen(s)
    Deflater.genCodes(tree, max, s.blCount, s.nextCode)

// A streaming deflater with the same call pattern as `java.util.zip.Deflater`: feed input with
// `setInput`, drain with `deflate` (which consumes input into the sliding window and returns the
// number of bytes produced), then `finish` and drain until `finished`.
private[pneumatic] final class Deflater(level0: Int, nowrap: Boolean) extends DeflateEngine:
  import Deflater.*

  private[pneumatic] var nextIn: Array[Byte] = empty
  private[pneumatic] var nextInIndex: Int = 0
  private[pneumatic] var availIn: Int = 0
  private[pneumatic] var totalIn: Long = 0
  private[pneumatic] var nextOut: Array[Byte] = empty
  private[pneumatic] var nextOutIndex: Int = 0
  private[pneumatic] var availOut: Int = 0
  private[pneumatic] var totalOut: Long = 0
  private[pneumatic] var msg: String = ""
  private[pneumatic] val adler: FlateChecksum = Adler32()

  private val level: Int = if level0 == -1 then 6 else level0
  private val strategy: Int = 0 // Z_DEFAULT_STRATEGY
  private var wrap: Int = if nowrap then 0 else 1

  private var status: Int = 0
  private var dataType: Int = ZUnknown
  private var lastFlush: Int = ZNoFlush

  private val wBits: Int = MaxWbits
  private val wSize: Int = 1 << wBits
  private val wMask: Int = wSize - 1

  private val window: Array[Byte] = new Array[Byte](wSize*2)
  private val windowSize: Int = 2*wSize
  private val prev: Array[Short] = new Array[Short](wSize)
  private val head: Array[Short] = new Array[Short](1 << (DefMemLevel + 7))

  private val hashBits: Int = DefMemLevel + 7
  private val hashSize: Int = 1 << hashBits
  private val hashMask: Int = hashSize - 1
  private val hashShift: Int = (hashBits + MinMatch - 1)/MinMatch

  private val litBufsize: Int = 1 << (DefMemLevel + 6)
  private val pendingBuf: Array[Byte] = new Array[Byte](litBufsize*3)
  private val pendingBufSize: Int = litBufsize*3
  private val dBuf: Int = litBufsize
  private val lBuf: Array[Byte] = new Array[Byte](litBufsize)

  private var pending: Int = 0    // number of bytes in the pending buffer
  private var pendingOut: Int = 0 // next pending byte to output to the stream

  private var insH: Int = 0
  private var blockStart: Int = 0
  private var matchLength: Int = 0
  private var prevMatch: Int = 0
  private var matchAvailable: Int = 0
  private var strstart: Int = 0
  private var matchStart: Int = 0
  private var lookahead: Int = 0
  private var prevLength: Int = 0

  private var maxChainLength: Int = 0
  private var maxLazyMatch: Int = 0
  private var goodMatch: Int = 0
  private var niceMatch: Int = 0

  private val dynLtree: Array[Short] = new Array[Short](HeapSize*2)
  private val dynDtree: Array[Short] = new Array[Short]((2*DCodes + 1)*2)
  private val blTree: Array[Short] = new Array[Short]((2*BlCodes + 1)*2)

  private val lDesc: FlateTree = FlateTree(dynLtree, TreeConfig.staticL)
  private val dDesc: FlateTree = FlateTree(dynDtree, TreeConfig.staticD)
  private val blDesc: FlateTree = FlateTree(blTree, TreeConfig.staticBl)

  private[pneumatic] val blCount: Array[Short] = new Array[Short](MaxBits + 1)
  private[pneumatic] val nextCode: Array[Short] = new Array[Short](MaxBits + 1)
  private[pneumatic] val heap: Array[Int] = new Array[Int](2*LCodes + 1)
  private[pneumatic] var heapLen: Int = 0
  private[pneumatic] var heapMax: Int = 0
  private[pneumatic] val depth: Array[Byte] = new Array[Byte](2*LCodes + 1)

  private var lastLit: Int = 0
  private[pneumatic] var optLen: Int = 0
  private[pneumatic] var staticLen: Int = 0
  private var matches: Int = 0
  private var lastEobLen: Int = 0

  private var biBuf: Int = 0   // output bit buffer (low 16 bits)
  private var biValid: Int = 0 // number of valid bits in biBuf

  private var finishing: Boolean = false
  private var streamEnded: Boolean = false

  deflateReset()

  private def lmInit(): Unit =
    var i = 0
    while i < hashSize do { head(i) = 0; i += 1 }

    maxLazyMatch = configMaxLazy(level)
    goodMatch = configGoodLength(level)
    niceMatch = configNiceLength(level)
    maxChainLength = configMaxChain(level)

    strstart = 0
    blockStart = 0
    lookahead = 0
    matchLength = MinMatch - 1
    prevLength = MinMatch - 1
    matchAvailable = 0
    insH = 0

  // Initialize the tree data structures for a new zlib stream.
  private def trInit(): Unit =
    biBuf = 0
    biValid = 0
    lastEobLen = 8 // enough lookahead for inflate
    initBlock()

  private def initBlock(): Unit =
    var i = 0
    while i < LCodes do { dynLtree(i*2) = 0; i += 1 }
    i = 0
    while i < DCodes do { dynDtree(i*2) = 0; i += 1 }
    i = 0
    while i < BlCodes do { blTree(i*2) = 0; i += 1 }

    dynLtree(EndBlock*2) = 1
    optLen = 0
    staticLen = 0
    lastLit = 0
    matches = 0

  // Restore the heap property by moving down the tree starting at node k.
  private[pneumatic] def pqdownheap(tree: Array[Short], k0: Int): Unit =
    var k = k0
    val v = heap(k)
    var j = k << 1 // left son of k
    var go = true

    while go && j <= heapLen do
      // Set j to the smallest of the two sons:
      if j < heapLen && smaller(tree, heap(j + 1), heap(j), depth) then j += 1

      // Exit if v is smaller than both sons
      if smaller(tree, v, heap(j), depth) then go = false
      else
        // Exchange v with the smallest son
        heap(k) = heap(j)
        k = j
        j <<= 1

    heap(k) = v

  // Scan a literal or distance tree to determine the frequencies of the codes in the bit length
  // tree.
  private def scanTree(tree: Array[Short], maxCode: Int): Unit =
    var prevlen = -1              // last emitted length
    var curlen = 0                // length of current code
    var nextlen = tree(1).toInt   // length of next code
    var count = 0                 // repeat count of the current code
    var maxCount = 7              // max repeat count
    var minCount = 4              // min repeat count

    if nextlen == 0 then
      maxCount = 138
      minCount = 3

    tree((maxCode + 1)*2 + 1) = 0xffff.toShort // guard

    var n = 0

    while n <= maxCode do
      curlen = nextlen
      nextlen = tree((n + 1)*2 + 1).toInt
      count += 1

      if count < maxCount && curlen == nextlen then () // keep counting
      else
        if count < minCount then blTree(curlen*2) = (blTree(curlen*2) + count).toShort
        else if curlen != 0 then
          if curlen != prevlen then blTree(curlen*2) = (blTree(curlen*2) + 1).toShort
          blTree(Rep36*2) = (blTree(Rep36*2) + 1).toShort
        else if count <= 10 then
          blTree(Repz310*2) = (blTree(Repz310*2) + 1).toShort
        else
          blTree(Repz11138*2) = (blTree(Repz11138*2) + 1).toShort

        count = 0
        prevlen = curlen

        if nextlen == 0 then
          maxCount = 138
          minCount = 3
        else if curlen == nextlen then
          maxCount = 6
          minCount = 3
        else
          maxCount = 7
          minCount = 4

      n += 1

  // Construct the Huffman tree for the bit lengths and return the index in blOrder of the last
  // bit length code to send.
  private def buildBlTree(): Int =
    // Determine the bit length frequencies for literal and distance trees
    scanTree(dynLtree, lDesc.maxCode)
    scanTree(dynDtree, dDesc.maxCode)

    // Build the bit length tree:
    blDesc.buildTree(this)
    // optLen now includes the length of the tree representations, except the lengths of the bit
    // lengths codes and the 5+5+4 bits for the counts.

    // Determine the number of bit length codes to send. The pkzip format requires that at least
    // 4 bit length codes be sent.
    var maxBlindex = BlCodes - 1

    while maxBlindex >= 3 && blTree(blOrder(maxBlindex)*2 + 1) == 0 do maxBlindex -= 1

    // Update optLen to include the bit length tree and counts
    optLen += 3*(maxBlindex + 1) + 5 + 5 + 4

    maxBlindex

  // Send the header for a block using dynamic Huffman trees.
  private def sendAllTrees(lcodes: Int, dcodes: Int, blcodes: Int): Unit =
    sendBits(lcodes - 257, 5)
    sendBits(dcodes - 1, 5)
    sendBits(blcodes - 4, 4)

    var rank = 0

    while rank < blcodes do
      sendBits(blTree(blOrder(rank)*2 + 1).toInt, 3)
      rank += 1

    sendTree(dynLtree, lcodes - 1) // literal tree
    sendTree(dynDtree, dcodes - 1) // distance tree

  // Send a literal or distance tree in compressed form, using the codes in blTree.
  private def sendTree(tree: Array[Short], maxCode: Int): Unit =
    var prevlen = -1            // last emitted length
    var curlen = 0              // length of current code
    var nextlen = tree(1).toInt // length of next code
    var count = 0               // repeat count of the current code
    var maxCount = 7            // max repeat count
    var minCount = 4            // min repeat count

    if nextlen == 0 then
      maxCount = 138
      minCount = 3

    var n = 0

    while n <= maxCode do
      curlen = nextlen
      nextlen = tree((n + 1)*2 + 1).toInt
      count += 1

      if count < maxCount && curlen == nextlen then () // keep counting
      else
        if count < minCount then
          while { sendCode(curlen, blTree); count -= 1; count != 0 } do ()
        else if curlen != 0 then
          if curlen != prevlen then
            sendCode(curlen, blTree)
            count -= 1

          sendCode(Rep36, blTree)
          sendBits(count - 3, 2)
        else if count <= 10 then
          sendCode(Repz310, blTree)
          sendBits(count - 3, 3)
        else
          sendCode(Repz11138, blTree)
          sendBits(count - 11, 7)

        count = 0
        prevlen = curlen

        if nextlen == 0 then
          maxCount = 138
          minCount = 3
        else if curlen == nextlen then
          maxCount = 6
          minCount = 3
        else
          maxCount = 7
          minCount = 4

      n += 1

  // Output bytes and bits on the stream; there is always enough room in pendingBuf.
  private def putByteRange(p: Array[Byte], start: Int, len: Int): Unit =
    System.arraycopy(p, start, pendingBuf, pending, len)
    pending += len

  private def putByte(c: Byte): Unit =
    pendingBuf(pending) = c
    pending += 1

  private def putShort(w: Int): Unit =
    putByte(w.toByte)
    putByte((w >>> 8).toByte)

  private def putShortMsb(b: Int): Unit =
    putByte((b >> 8).toByte)
    putByte(b.toByte)

  private def sendCode(c: Int, tree: Array[Short]): Unit =
    val c2 = c*2
    sendBits(tree(c2) & 0xffff, tree(c2 + 1) & 0xffff)

  private def sendBits(value: Int, length: Int): Unit =
    if biValid > BufSize - length then
      biBuf = (biBuf | ((value << biValid) & 0xffff)) & 0xffff
      putShort(biBuf)
      biBuf = (value >>> (BufSize - biValid)) & 0xffff
      biValid += length - BufSize
    else
      biBuf = (biBuf | ((value << biValid) & 0xffff)) & 0xffff
      biValid += length

  // Send one empty static block to give enough lookahead for inflate.
  private def trAlign(): Unit =
    sendBits(StaticTrees << 1, 3)
    sendCode(EndBlock, staticLtree)
    biFlush()

    // Of the 10 bits for the empty block, we have already sent (10 - biValid) bits.
    if 1 + lastEobLen + 10 - biValid < 9 then
      sendBits(StaticTrees << 1, 3)
      sendCode(EndBlock, staticLtree)
      biFlush()

    lastEobLen = 7

  // Save the match info and tally the frequency counts. Return true if the current block must be
  // flushed.
  private def trTally(dist0: Int, lc: Int): Boolean =
    var dist = dist0
    pendingBuf(dBuf + lastLit*2) = (dist >>> 8).toByte
    pendingBuf(dBuf + lastLit*2 + 1) = dist.toByte
    lBuf(lastLit) = lc.toByte
    lastLit += 1

    if dist == 0 then
      // lc is the unmatched char
      dynLtree(lc*2) = (dynLtree(lc*2) + 1).toShort
    else
      matches += 1
      // Here, lc is the match length - MinMatch
      dist -= 1 // dist = match distance - 1

      dynLtree((lengthCode(lc) + Literals + 1)*2) =
        (dynLtree((lengthCode(lc) + Literals + 1)*2) + 1).toShort

      dynDtree(dCode(dist)*2) = (dynDtree(dCode(dist)*2) + 1).toShort

    if (lastLit & 0x1fff) == 0 && level > 2 then
      // Compute an upper bound for the compressed length
      var outLength = lastLit*8
      val inLength = strstart - blockStart
      var dcode = 0

      while dcode < DCodes do
        outLength = (outLength + dynDtree(dcode*2)*(5L + extraDbits(dcode))).toInt
        dcode += 1

      outLength >>>= 3
      if matches < lastLit/2 && outLength < inLength/2 then return true

    lastLit == litBufsize - 1
    // We avoid equality with litBufsize because of wraparound at 64K and because stored blocks
    // are restricted to 64K-1 bytes.

  // Send the block data compressed using the given Huffman trees
  private def compressBlock(ltree: Array[Short], dtree: Array[Short]): Unit =
    var dist = 0 // distance of matched string
    var lc = 0   // match length or unmatched char (if dist == 0)
    var lx = 0   // running index in lBuf
    var code = 0 // the code to send

    if lastLit != 0 then
      while
        dist = ((pendingBuf(dBuf + lx*2) << 8) & 0xff00) | (pendingBuf(dBuf + lx*2 + 1) & 0xff)
        lc = lBuf(lx) & 0xff
        lx += 1

        if dist == 0 then sendCode(lc, ltree) // send a literal byte
        else
          // Here, lc is the match length - MinMatch
          code = lengthCode(lc)
          sendCode(code + Literals + 1, ltree) // send the length code
          var extra = extraLbits(code)

          if extra != 0 then
            lc -= baseLength(code)
            sendBits(lc, extra) // send the extra length bits

          dist -= 1 // dist is now the match distance - 1
          code = dCode(dist)
          sendCode(code, dtree) // send the distance code
          extra = extraDbits(code)

          if extra != 0 then
            dist -= baseDist(code)
            sendBits(dist, extra) // send the extra distance bits

        lx < lastLit
      do ()

    sendCode(EndBlock, ltree)
    lastEobLen = ltree(EndBlock*2 + 1).toInt

  // Set the data type to ASCII or BINARY, using a crude approximation: binary if more than 20%
  // of the bytes are <= 6 or >= 128, ascii otherwise.
  private def setDataType(): Unit =
    var n = 0
    var asciiFreq = 0
    var binFreq = 0

    while n < 7 do { binFreq += dynLtree(n*2); n += 1 }
    while n < 128 do { asciiFreq += dynLtree(n*2); n += 1 }
    while n < Literals do { binFreq += dynLtree(n*2); n += 1 }

    dataType = if binFreq > (asciiFreq >>> 2) then ZBinary else ZAscii

  // Flush the bit buffer, keeping at most 7 bits in it.
  private def biFlush(): Unit =
    if biValid == 16 then
      putShort(biBuf)
      biBuf = 0
      biValid = 0
    else if biValid >= 8 then
      putByte(biBuf.toByte)
      biBuf >>>= 8
      biValid -= 8

  // Flush the bit buffer and align the output on a byte boundary
  private def biWindup(): Unit =
    if biValid > 8 then putShort(biBuf)
    else if biValid > 0 then putByte(biBuf.toByte)

    biBuf = 0
    biValid = 0

  // Copy a stored block, storing first the length and its one's complement if requested.
  private def copyBlock(buf: Int, len: Int, header: Boolean): Unit =
    biWindup()        // align on byte boundary
    lastEobLen = 8    // enough lookahead for inflate

    if header then
      putShort(len)
      putShort(~len)

    putByteRange(window, buf, len)

  private def flushBlockOnly(eof: Boolean): Unit =
    trFlushBlock(if blockStart >= 0 then blockStart else -1, strstart - blockStart, eof)
    blockStart = strstart
    flushPending()

  // Copy without compression as much as possible from the input stream: used only for level 0.
  private def deflateStored(flush: Int): Int =
    // Stored blocks are limited to 0xffff bytes, pendingBuf to pendingBufSize, and each stored
    // block has a 5 byte header:
    var maxBlockSize = 0xffff
    if maxBlockSize > pendingBufSize - 5 then maxBlockSize = pendingBufSize - 5

    var flushCurrent = false
    var bail = -1

    // Copy as much as possible from input to output:
    while bail == -1 && !flushCurrent do
      // Fill the window as much as possible:
      if lookahead <= 1 then
        fillWindow()

        if lookahead == 0 && flush == ZNoFlush then bail = NeedMore
        else if lookahead == 0 then flushCurrent = true

      if bail == -1 && !flushCurrent then
        strstart += lookahead
        lookahead = 0

        // Emit a stored block if pendingBuf will be full:
        val maxStart = blockStart + maxBlockSize

        if strstart == 0 || strstart >= maxStart then
          lookahead = strstart - maxStart
          strstart = maxStart
          flushBlockOnly(false)
          if availOut == 0 then bail = NeedMore

        // Flush if we may have to slide, otherwise blockStart may become negative:
        if bail == -1 && strstart - blockStart >= wSize - MinLookahead then
          flushBlockOnly(false)
          if availOut == 0 then bail = NeedMore

    if bail != -1 then bail else
      flushBlockOnly(flush == ZFinish)

      if availOut == 0 then (if flush == ZFinish then FinishStarted else NeedMore)
      else if flush == ZFinish then FinishDone
      else BlockDone

  // Send a stored block
  private def trStoredBlock(buf: Int, storedLen: Int, eof: Boolean): Unit =
    sendBits((StoredBlock << 1) + (if eof then 1 else 0), 3) // send block type
    copyBlock(buf, storedLen, true)                          // with header

  // Determine the best encoding for the current block: dynamic trees, static trees or store, and
  // output the encoded block.
  private def trFlushBlock(buf: Int, storedLen: Int, eof: Boolean): Unit =
    var optLenb = 0
    var staticLenb = 0
    var maxBlindex = 0 // index of last bit length code of non zero freq

    // Build the Huffman trees unless a stored block is forced
    if level > 0 then
      // Check if the file is ascii or binary
      if dataType == ZUnknown then setDataType()

      // Construct the literal and distance trees
      lDesc.buildTree(this)
      dDesc.buildTree(this)

      // At this point, optLen and staticLen are the total bit lengths of the compressed block
      // data, excluding the tree representations. Build the bit length tree for the above two
      // trees, and get the index in blOrder of the last bit length code to send.
      maxBlindex = buildBlTree()

      // Determine the best encoding. Compute first the block length in bytes
      optLenb = (optLen + 3 + 7) >>> 3
      staticLenb = (staticLen + 3 + 7) >>> 3
      if staticLenb <= optLenb then optLenb = staticLenb
    else
      optLenb = storedLen + 5 // force a stored block
      staticLenb = optLenb

    if storedLen + 4 <= optLenb && buf != -1 then
      // 4: two words for the lengths. Transforming a block into a stored block is never too
      // late, since litBufsize <= wSize.
      trStoredBlock(buf, storedLen, eof)
    else if staticLenb == optLenb then
      sendBits((StaticTrees << 1) + (if eof then 1 else 0), 3)
      compressBlock(staticLtree, staticDtree)
    else
      sendBits((DynTrees << 1) + (if eof then 1 else 0), 3)
      sendAllTrees(lDesc.maxCode + 1, dDesc.maxCode + 1, maxBlindex + 1)
      compressBlock(dynLtree, dynDtree)

    initBlock()
    if eof then biWindup()

  // Read a new buffer from the current input stream, update the adler32 and total number of
  // bytes read.
  private def readBuf(buf: Array[Byte], start: Int, size: Int): Int =
    var len = availIn

    if len > size then len = size

    if len == 0 then 0 else
      availIn -= len
      if wrap != 0 then adler.update(nextIn, nextInIndex, len)
      System.arraycopy(nextIn, nextInIndex, buf, start, len)
      nextInIndex += len
      totalIn += len
      len

  // Fill the window when the lookahead becomes insufficient. Updates strstart and lookahead.
  private def fillWindow(): Unit =
    var n = 0
    var m = 0
    var p = 0
    var more = 0 // Amount of free space at the end of the window.

    while
      more = windowSize - lookahead - strstart

      // Deal with the 64K limit:
      if more == 0 && strstart == 0 && lookahead == 0 then more = wSize
      else if more == -1 then more -= 1
      else if strstart >= wSize + wSize - MinLookahead then
        // The window is almost full and there is insufficient lookahead: move the upper half to
        // the lower one to make room in the upper half.
        System.arraycopy(window, wSize, window, 0, wSize)
        matchStart -= wSize
        strstart -= wSize // we now have strstart >= MaxDist
        blockStart -= wSize

        // Slide the hash table
        n = hashSize
        p = n

        while
          p -= 1
          m = head(p) & 0xffff
          head(p) = if m >= wSize then (m - wSize).toShort else 0
          n -= 1
          n != 0
        do ()

        n = wSize
        p = n

        while
          p -= 1
          m = prev(p) & 0xffff
          prev(p) = if m >= wSize then (m - wSize).toShort else 0
          n -= 1
          n != 0
        do ()

        more += wSize

      if availIn == 0 then return

      // In all cases, more >= 2.
      n = readBuf(window, strstart + lookahead, more)
      lookahead += n

      // Initialize the hash value now that we have some input:
      if lookahead >= MinMatch then
        insH = window(strstart) & 0xff
        insH = ((insH << hashShift) ^ (window(strstart + 1) & 0xff)) & hashMask

      lookahead < MinLookahead && availIn != 0
    do ()

  // Compress as much as possible from the input stream, returning the current block state. This
  // function does not perform lazy evaluation of matches: used for the fast compression levels.
  private def deflateFast(flush: Int): Int =
    var hashHead = 0     // head of the hash chain
    var bflush = false   // set if current block must be flushed
    var result = -1

    while result == -1 do
      // Make sure that we always have enough lookahead, except at the end of the input file.
      if lookahead < MinLookahead then
        fillWindow()

        if lookahead < MinLookahead && flush == ZNoFlush then result = NeedMore
        else if lookahead == 0 then
          // flush the current block
          flushBlockOnly(flush == ZFinish)

          result =
            if availOut == 0 then (if flush == ZFinish then FinishStarted else NeedMore)
            else if flush == ZFinish then FinishDone
            else BlockDone

      if result == -1 then
        // Insert the string window(strstart .. strstart+2) in the dictionary, and set hashHead
        // to the head of the hash chain:
        if lookahead >= MinMatch then
          insH = ((insH << hashShift) ^ (window(strstart + MinMatch - 1) & 0xff)) & hashMask
          hashHead = head(insH) & 0xffff
          prev(strstart & wMask) = head(insH)
          head(insH) = strstart.toShort

        // Find the longest match, discarding those <= prevLength. At this point we have always
        // matchLength < MinMatch
        if hashHead != 0 && ((strstart - hashHead) & 0xffff) <= wSize - MinLookahead then
          // To simplify the code, we prevent matches with the string of window index 0.
          matchLength = longestMatch(hashHead)
          // longestMatch sets matchStart

        if matchLength >= MinMatch then
          bflush = trTally(strstart - matchStart, matchLength - MinMatch)
          lookahead -= matchLength

          // Insert new strings in the hash table only if the match length is not too large.
          if matchLength <= maxLazyMatch && lookahead >= MinMatch then
            matchLength -= 1 // string at strstart already in hash table

            while
              strstart += 1
              insH = ((insH << hashShift) ^ (window(strstart + MinMatch - 1) & 0xff)) & hashMask
              hashHead = head(insH) & 0xffff
              prev(strstart & wMask) = head(insH)
              head(insH) = strstart.toShort
              // strstart never exceeds wSize-MaxMatch, so there are always MinMatch bytes ahead.
              matchLength -= 1
              matchLength != 0
            do ()

            strstart += 1
          else
            strstart += matchLength
            matchLength = 0
            insH = window(strstart) & 0xff
            insH = ((insH << hashShift) ^ (window(strstart + 1) & 0xff)) & hashMask
            // If lookahead < MinMatch, insH is garbage, but it does not matter since it will be
            // recomputed at next deflate call.
        else
          // No match, output a literal byte
          bflush = trTally(0, window(strstart) & 0xff)
          lookahead -= 1
          strstart += 1

        if bflush then
          flushBlockOnly(false)
          if availOut == 0 then result = NeedMore

    result

  // Same as above, but achieves better compression: a match is finally adopted only if there is
  // no better match at the next window position.
  private def deflateSlow(flush: Int): Int =
    var hashHead = 0   // head of hash chain
    var bflush = false // set if current block must be flushed
    var result = -1

    // Process the input block.
    while result == -1 do
      // Make sure that we always have enough lookahead, except at the end of the input file.
      if lookahead < MinLookahead then
        fillWindow()

        if lookahead < MinLookahead && flush == ZNoFlush then result = NeedMore
        else if lookahead == 0 then
          // flush the current block
          if matchAvailable != 0 then
            bflush = trTally(0, window(strstart - 1) & 0xff)
            matchAvailable = 0

          flushBlockOnly(flush == ZFinish)

          result =
            if availOut == 0 then (if flush == ZFinish then FinishStarted else NeedMore)
            else if flush == ZFinish then FinishDone
            else BlockDone

      if result == -1 then
        // Insert the string window(strstart .. strstart+2) in the dictionary, and set hashHead
        // to the head of the hash chain:
        if lookahead >= MinMatch then
          insH = ((insH << hashShift) ^ (window(strstart + MinMatch - 1) & 0xff)) & hashMask
          hashHead = head(insH) & 0xffff
          prev(strstart & wMask) = head(insH)
          head(insH) = strstart.toShort

        // Find the longest match, discarding those <= prevLength.
        prevLength = matchLength
        prevMatch = matchStart
        matchLength = MinMatch - 1

        if hashHead != 0 && prevLength < maxLazyMatch &&
          ((strstart - hashHead) & 0xffff) <= wSize - MinLookahead
        then
          // To simplify the code, we prevent matches with the string of window index 0.
          matchLength = longestMatch(hashHead)
          // longestMatch sets matchStart

          if matchLength == MinMatch && strstart - matchStart > 4096
          then matchLength = MinMatch - 1
          // If prevMatch is also MinMatch, matchStart is garbage but we will ignore the current
          // match anyway.

        // If there was a match at the previous step and the current match is not better, output
        // the previous match:
        if prevLength >= MinMatch && matchLength <= prevLength then
          val maxInsert = strstart + lookahead - MinMatch
          // Do not insert strings in hash table beyond this.

          bflush = trTally(strstart - 1 - prevMatch, prevLength - MinMatch)

          // Insert in hash table all strings up to the end of the match. strstart-1 and strstart
          // are already inserted. If there is not enough lookahead, the last two strings are not
          // inserted in the hash table.
          lookahead -= prevLength - 1
          prevLength -= 2

          while
            strstart += 1

            if strstart <= maxInsert then
              insH = ((insH << hashShift) ^ (window(strstart + MinMatch - 1) & 0xff)) & hashMask
              hashHead = head(insH) & 0xffff
              prev(strstart & wMask) = head(insH)
              head(insH) = strstart.toShort

            prevLength -= 1
            prevLength != 0
          do ()

          matchAvailable = 0
          matchLength = MinMatch - 1
          strstart += 1

          if bflush then
            flushBlockOnly(false)
            if availOut == 0 then result = NeedMore
        else if matchAvailable != 0 then
          // If there was no match at the previous position, output a single literal. If there
          // was a match but the current match is longer, truncate the previous match to a single
          // literal.
          bflush = trTally(0, window(strstart - 1) & 0xff)
          if bflush then flushBlockOnly(false)
          strstart += 1
          lookahead -= 1
          if availOut == 0 then result = NeedMore
        else
          // There is no previous match to compare with, wait for the next step to decide.
          matchAvailable = 1
          strstart += 1
          lookahead -= 1

    result

  private def longestMatch(curMatch0: Int): Int =
    var curMatch = curMatch0
    var chainLength = maxChainLength // max hash chain length
    var scan = strstart              // current string
    var matchIndex = 0               // matched string
    var len = 0                      // length of current match
    var bestLen = prevLength         // best match length so far

    val limit = if strstart > wSize - MinLookahead then strstart - (wSize - MinLookahead) else 0
    var nice = niceMatch

    // Stop when curMatch becomes <= limit. To simplify the code, we prevent matches with the
    // string of window index 0.
    val strend = strstart + MaxMatch
    var scanEnd1 = window(scan + bestLen - 1)
    var scanEnd = window(scan + bestLen)

    // Do not waste too much time if we already have a good match:
    if prevLength >= goodMatch then chainLength >>= 2

    // Do not look for matches beyond the end of the input. This is necessary to make deflate
    // deterministic.
    if nice > lookahead then nice = lookahead

    var done = false

    while !done do
      matchIndex = curMatch

      // Skip to next match if the match length cannot increase or if the match length is less
      // than 2:
      val skip =
        window(matchIndex + bestLen) != scanEnd ||
          window(matchIndex + bestLen - 1) != scanEnd1 ||
          window(matchIndex) != window(scan) ||
          { matchIndex += 1; window(matchIndex) != window(scan + 1) }

      if !skip then
        // It is not necessary to compare the first two bytes again, since they are always equal.
        scan += 2
        matchIndex += 1

        // We check for insufficient lookahead only every 8th comparison; the 256th check will be
        // made at strstart+258. Written as an explicit loop (not a closure) so `scan` and
        // `matchIndex` stay unboxed.
        var go = true

        while go do
          var group = 0
          var mismatch = false

          while group < 8 && !mismatch do
            scan += 1
            matchIndex += 1
            if window(scan) != window(matchIndex) then mismatch = true
            group += 1

          go = !mismatch && scan < strend

        len = MaxMatch - (strend - scan)
        scan = strend - MaxMatch

        if len > bestLen then
          matchStart = curMatch
          bestLen = len

          if len >= nice then done = true
          else
            scanEnd1 = window(scan + bestLen - 1)
            scanEnd = window(scan + bestLen)

      if !done then
        curMatch = prev(curMatch & wMask) & 0xffff
        chainLength -= 1
        if curMatch <= limit || chainLength == 0 then done = true

    if bestLen <= lookahead then bestLen else lookahead

  private def deflateReset(): Unit =
    totalIn = 0
    totalOut = 0
    msg = ""
    dataType = ZUnknown
    pending = 0
    pendingOut = 0
    status = if wrap == 0 then BusyState else InitState
    adler.reset()
    lastFlush = ZNoFlush
    trInit()
    lmInit()

  // Flush as much pending output as possible.
  private def flushPending(): Unit =
    var len = pending

    if len > availOut then len = availOut

    if len != 0 then
      System.arraycopy(pendingBuf, pendingOut, nextOut, nextOutIndex, len)
      nextOutIndex += len
      pendingOut += len
      totalOut += len
      availOut -= len
      pending -= len
      if pending == 0 then pendingOut = 0

  private def deflateInternal(flush: Int): Int =
    if flush > ZFinish || flush < 0 then return ZStreamError

    if status == FinishState && flush != ZFinish then
      msg = "stream error"
      return ZStreamError

    if availOut == 0 then
      msg = "buffer error"
      return ZBufError

    val oldFlush = lastFlush
    lastFlush = flush

    // Write the zlib header
    if status == InitState then
      var header = (ZDeflated + ((wBits - 8) << 4)) << 8
      var levelFlags = ((level - 1) & 0xff) >> 1

      if levelFlags > 3 then levelFlags = 3
      header |= levelFlags << 6
      header += 31 - (header%31)

      status = BusyState
      putShortMsb(header)
      adler.reset()

    // Flush as much pending output as possible
    if pending != 0 then
      flushPending()

      if availOut == 0 then
        // Since availOut is 0, deflate will be called again with more output space, but possibly
        // with both pending and availIn equal to zero: make sure we return Ok instead of
        // BufError at the next call.
        lastFlush = -1
        return ZOk
    else if availIn == 0 && flush <= oldFlush && flush != ZFinish then
      msg = "buffer error"
      return ZBufError

    // User must not provide more input after the first finish:
    if status == FinishState && availIn != 0 then
      msg = "buffer error"
      return ZBufError

    // Start a new block or continue the current one.
    if availIn != 0 || lookahead != 0 || (flush != ZNoFlush && status != FinishState) then
      val bstate = configFunc(level) match
        case StoredFunc => deflateStored(flush)
        case FastFunc   => deflateFast(flush)
        case _          => deflateSlow(flush)

      if bstate == FinishStarted || bstate == FinishDone then status = FinishState

      if bstate == NeedMore || bstate == FinishStarted then
        if availOut == 0 then lastFlush = -1 // avoid BufError next call
        return ZOk

      if bstate == BlockDone then
        if flush == ZPartialFlush then trAlign()
        else // FullFlush or SyncFlush
          trStoredBlock(0, 0, false)

          // For a full flush, this empty block will be recognized as a special marker by
          // inflate_sync().
          if flush == ZFullFlush then
            var i = 0
            while i < hashSize do { head(i) = 0; i += 1 } // forget history

        flushPending()

        if availOut == 0 then
          lastFlush = -1 // avoid BufError at next call
          return ZOk

    if flush != ZFinish then ZOk
    else if wrap <= 0 then ZStreamEnd
    else
      // Write the zlib trailer (adler32)
      val checksum = adler.value
      putShortMsb((checksum >>> 16).toInt)
      putShortMsb((checksum & 0xffff).toInt)
      flushPending()

      // If availOut is zero, the application will call deflate again to flush the rest.
      if wrap > 0 then wrap = -wrap // write the trailer only once!

      if pending != 0 then ZOk else ZStreamEnd

  def setInput(buffer: Array[Byte]): Unit = setInput(buffer, 0, buffer.length)

  def setInput(buffer: Array[Byte], offset: Int, length: Int): Unit =
    nextIn = buffer
    nextInIndex = offset
    availIn = length

  def getBytesRead: Long = totalIn

  def finish(): Unit = finishing = true

  def finished: Boolean = streamEnded

  def deflate(target: Array[Byte], offset: Int, space: Int): Int =
    deflate(target, offset, space, if finishing then ZFinish else ZNoFlush)

  def deflate(target: Array[Byte], offset: Int, space: Int, flush: Int): Int =
    nextOut = target
    nextOutIndex = offset
    availOut = space
    val result = deflateInternal(flush)
    val produced = space - availOut
    nextOut = empty

    if result == ZStreamEnd then streamEnded = true
    else if result == ZStreamError then throw IllegalStateException("deflate failure: "+msg)

    produced

  def end(): Unit = ()
