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
package gastronomy

import scala.{caps, math}

import java.nio.charset.StandardCharsets

import scala.reflect.Selectable.reflectiveSelectable

import anticipation.*
import corpuscular.*
import fulminate.*
import gossamer.*
import prepositional.*

object Blake3:
  private final val OutLen   = 32
  private final val KeyLen   = 32
  private final val BlockLen = 64
  private final val ChunkLen = 1024

  private final val ChunkStart        = 1
  private final val ChunkEnd          = 2
  private final val ParentFlag        = 4
  private final val RootFlag          = 8
  private final val KeyedHashFlag     = 16
  private final val DeriveKeyContext  = 32
  private final val DeriveKeyMaterial = 64

  private final val Iv: Array[Int]^{} =
    scala.Array      ( 0x6a09e667, 0xbb67ae85, 0x3c6ef372, 0xa54ff53a,
        0x510e527f, 0x9b05688c, 0x1f83d9ab, 0x5be0cd19 )

    . asInstanceOf[Array[Int]^{}]

  private final val MsgPermutation: Array[Int]^{} =
    scala.Array(2, 6, 3, 10, 7, 0, 4, 13, 1, 11, 12, 5, 9, 14, 15, 8).asInstanceOf[Array[Int]^{}]

  // The message schedule, flattened: entry 16r + i is the permutation applied r times to i, so
  // each round reads its words straight from the block through its row, and the block is never
  // permuted or copied. Frozen, like `Iv` and `MsgPermutation`, so the object stays pure.
  private final val Schedule: Array[Int]^{} =
    @tailrec
    def permuted(times: Int, index: Int): Int =
      if times == 0 then index else permuted(times - 1, MsgPermutation.readable(index))

    scala.Array.tabulate(112) { entry => permuted(entry/16, entry%16) }.asInstanceOf[Array[Int]^{}]

  // The schedule as the JVM array, read natively in `round`; `Array.unsafeJvm` asserts only that
  // the rounds do not write to it. A `def`, as `ivWords` is, since a JVM-array field would make
  // the object impure.
  private def schedule: scala.Array[Int] = Array.unsafeJvm(Schedule)

  private def mix(state: scala.Array[Int]^, a: Int, b: Int, c: Int, d: Int, mx: Int, my: Int): Unit =
    state(a) = state(a) + state(b) + mx
    state(d) = Integer.rotateRight(state(d) ^ state(a), 16)
    state(c) = state(c) + state(d)
    state(b) = Integer.rotateRight(state(b) ^ state(c), 12)
    state(a) = state(a) + state(b) + my
    state(d) = Integer.rotateRight(state(d) ^ state(a), 8)
    state(c) = state(c) + state(d)
    state(b) = Integer.rotateRight(state(b) ^ state(c), 7)

  private def round(state: scala.Array[Int]^, m: scala.Array[Int], row: Int): Unit =
    val s = schedule
    val o = 16*row
    // SIMD: these four column mixes operate on disjoint quadruples of state and would be
    //       issued as a single 4-lane vector instruction by an SSE/NEON backend.
    mix(state, 0, 4,  8, 12, m(s(o)), m(s(o + 1)))
    mix(state, 1, 5,  9, 13, m(s(o + 2)), m(s(o + 3)))
    mix(state, 2, 6, 10, 14, m(s(o + 4)), m(s(o + 5)))
    mix(state, 3, 7, 11, 15, m(s(o + 6)), m(s(o + 7)))
    // SIMD: the diagonal mixes form the second 4-lane batch, with the same shape.
    mix(state, 0, 5, 10, 15, m(s(o + 8)), m(s(o + 9)))
    mix(state, 1, 6, 11, 12, m(s(o + 10)), m(s(o + 11)))
    mix(state, 2, 7,  8, 13, m(s(o + 12)), m(s(o + 13)))
    mix(state, 3, 4,  9, 14, m(s(o + 14)), m(s(o + 15)))

  // Reads `chainingValue` and `blockWords`, and never writes them.
  private def compress
    ( chainingValue: scala.Array[Int],
      blockWords:    scala.Array[Int],
      counter:       Long,
      blockLen:      Int,
      flags:         Int )
  :   scala.Array[Int]^ =

    val state: scala.Array[Int]^ = new scala.Array[Int](16)
    System.arraycopy(chainingValue, 0, state, 0, 8)
    state(8)  = Iv.readable(0); state(9)  = Iv.readable(1); state(10) = Iv.readable(2); state(11) = Iv.readable(3)
    state(12) = counter.toInt
    state(13) = (counter >>> 32).toInt
    state(14) = blockLen
    state(15) = flags

    round(state, blockWords, 0)
    round(state, blockWords, 1)
    round(state, blockWords, 2)
    round(state, blockWords, 3)
    round(state, blockWords, 4)
    round(state, blockWords, 5)
    round(state, blockWords, 6)

    var i = 0

    while i < 8 do
      state(i)     = state(i) ^ state(i + 8)
      state(i + 8) = state(i + 8) ^ chainingValue(i)
      i += 1

    state

  private def wordsFromBytes(bytes: scala.Array[Byte], offset: Int, words: scala.Array[Int]^): Unit =
    var i = 0

    while i < words.length do
      val o = offset + 4*i

      words(i) =
        (bytes(o)     & 0xff) |
          ((bytes(o + 1) & 0xff) <<  8) |
          ((bytes(o + 2) & 0xff) << 16) |
          ((bytes(o + 3) & 0xff) << 24)

      i += 1

  // Pure: both word arrays are frozen, so an output is a value, shared freely between the
  // hasher's stack and the parent nodes built from it.
  private final class Output
    ( val inputChainingValue: Array[Int]^{},
      val blockWords:         Array[Int]^{},
      val counter:            Long,
      val blockLen:           Int,
      val flags:              Int )
  extends caps.Pure:

    // `compress` only reads its words, so the frozen arrays may cross as JVM arrays.
    private def compressed(counter: Long, flags: Int): scala.Array[Int]^ =
      compress
        ( Array.unsafeJvm(inputChainingValue), Array.unsafeJvm(blockWords), counter, blockLen,
          flags )

    def chainingValue(): Array[Int]^{} =
      val out = compressed(counter, flags)
      val cv = Array.allocate[Int](8)
      System.arraycopy(out, 0, cv.raw, 0, 8)
      Array.freeze(cv)

    // Interior scratch, as the block buffers are: the generic `Array.allocate`/`update` would
    // allocate reflectively and box every byte, which costs more than the hash itself.
    def rootOutputBytes(outLen: Int): Array[Byte]^{} =
      val result = new scala.Array[Byte](outLen)
      var blockCounter = 0L
      var pos = 0

      while pos < outLen do
        val words = compressed(blockCounter, flags | RootFlag)

        val take = math.min(2*OutLen, outLen - pos)
        var i = 0

        while i < take do
          result(pos + i) = (words(i/4) >>> (8*(i%4))).toByte
          i += 1

        pos += take
        blockCounter += 1

      // Fresh and never escaping before this point, so no writer can alias it.
      Array.unsafeFrozen(result)

  private def parentOutput
    ( leftCv: Array[Int]^{}, rightCv: Array[Int]^{}, keyWords: Array[Int]^{}, flags: Int )
  :   Output =

    val blockWords = Array.allocate[Int](16)
    blockWords.place(leftCv, 0, 0, 8)
    blockWords.place(rightCv, 0, 8, 8)
    Output(keyWords, Array.freeze(blockWords), 0L, BlockLen, ParentFlag | flags)

  private def parentCv
    ( leftCv: Array[Int]^{}, rightCv: Array[Int]^{}, keyWords: Array[Int]^{}, flags: Int )
  :   Array[Int]^{} =

    parentOutput(leftCv, rightCv, keyWords, flags).chainingValue()

  private final class ChunkState(keyWordsInit: Array[Int]^{}, var chunkCounter: Long, val flags: Int)
  extends caps.Mutable:
    // Cloning only reads the frozen key words.
    private var chainingValue: scala.Array[Int]^ = Array.unsafeJvm(keyWordsInit).clone()
    private var block:         scala.Array[Byte]^ = new scala.Array[Byte](BlockLen)

    var blockLen:         Int = 0
    var blocksCompressed: Int = 0

    def len: Int = BlockLen*blocksCompressed + blockLen

    private def startFlag: Int = if blocksCompressed == 0 then ChunkStart else 0

    update def update(input: Array[Byte]^{caps.any.rd}, start: Int, end: Int): Unit =
      // The window is copied out of, never into, so one named JVM view covers the whole call.
      val source = Array.unsafeJvm(input)
      var pos = start

      while pos < end do
        if blockLen == BlockLen then
          // Allocated only when a block actually compresses: a short input never does.
          val blockWords = new scala.Array[Int](16)
          wordsFromBytes(block, 0, blockWords)

          val out =
            compress(chainingValue, blockWords, chunkCounter, BlockLen, flags | startFlag)

          System.arraycopy(out, 0, chainingValue, 0, 8)
          blocksCompressed += 1
          java.util.Arrays.fill(block, 0.toByte)
          blockLen = 0

        val want = BlockLen - blockLen
        val take = math.min(want, end - pos)
        System.arraycopy(source, pos, block, blockLen, take)
        blockLen += take
        pos += take

    def output(): Output =
      val blockWords = Array.allocate[Int](16)
      wordsFromBytes(block, 0, blockWords.raw)

      val cv = Array.allocate[Int](8)
      System.arraycopy(chainingValue, 0, cv.raw, 0, 8)

      Output
        ( Array.freeze(cv),
          Array.freeze(blockWords),
          chunkCounter,
          blockLen,
          flags | startFlag | ChunkEnd )

  // The key words are frozen, so the hasher, its chunks and its outputs share them uncopied.
  private final class Hasher(keyWords: Array[Int]^{}, val flags: Int) extends caps.Mutable:
    private var chunkState: ChunkState^ = ChunkState(keyWords, 0L, flags)
    // The stack of chaining values, one per level of the tree, so at most 54 deep; grown on
    // demand, since an input of one chunk — a key, a path, a short message — never pushes.
    private var cvStack: scala.Array[Array[Int]^{}]^ = new scala.Array[Array[Int]^{}](0)
    private var cvStackLen: Int = 0

    private update def pushStack(cv: Array[Int]^{}): Unit =
      if cvStackLen == cvStack.length then
        val bigger = new scala.Array[Array[Int]^{}]((cvStack.length*2).max(8).min(54))
        System.arraycopy(cvStack, 0, bigger, 0, cvStackLen)
        cvStack = bigger

      cvStack(cvStackLen) = cv
      cvStackLen += 1

    private update def popStack(): Array[Int]^{} =
      cvStackLen -= 1
      cvStack(cvStackLen)

    private update def addChunkCv(initialCv: Array[Int]^{}, initialTotal: Long): Unit =
      var cv = initialCv
      var totalChunks = initialTotal

      while (totalChunks & 1L) == 0L do
        cv = parentCv(popStack(), cv, keyWords, flags)
        totalChunks >>= 1

      pushStack(cv)

    update def update(data: Array[Byte]^{}): Unit = update(data, 0, data.length)

    update def update(input: Array[Byte]^{caps.any.rd}, start: Int, end: Int): Unit =
      // SIMD: AVX2 / AVX-512 backends process 4 / 8 / 16 chunks at a time here using interleaved
      //       state; the scalar path below handles one chunk per iteration.
      var pos = start

      while pos < end do
        if chunkState.len == ChunkLen then
          val chunkCv = chunkState.output().chainingValue()
          val totalChunks = chunkState.chunkCounter + 1L
          addChunkCv(chunkCv, totalChunks)
          chunkState = ChunkState(keyWords, totalChunks, flags)

        val want = ChunkLen - chunkState.len
        val take = math.min(want, end - pos)
        chunkState.update(input, pos, pos + take)
        pos += take

    update def complete(outLen: Int): Array[Byte]^{} =
      var current = chunkState.output()
      var i = cvStackLen

      while i > 0 do
        i -= 1
        current = parentOutput(cvStack(i), current.chainingValue(), keyWords, flags)

      current.rootOutputBytes(outLen)

  given hash: (hashing: Hashing { def blake3: Hashing.Function }) => Hash in Blake3 =
    Hash(t"BLAKE3", t"HMAC-BLAKE3", hashing.blake3)

  // The pure-Scala BLAKE3 `Digestion`, used by the Soundness hashing provider.
  def digestion(): Digestion^ = new Digestion:
    private var hasher: Hasher^ = Hasher(Iv, 0)
    update def append(bytes: Data): Unit = hasher.update(bytes)

    override update def append(array: Array[Byte]^{caps.any.rd}, start: Int, count: Int): Unit =
      hasher.update(array, start, start + count)

    update def digest(): Data = hasher.complete(OutLen)

  def hashOf(input: Array[Byte]^{}, length: Int = OutLen): Array[Byte]^{} =
    val hasher: Hasher^ = Hasher(Iv, 0)
    hasher.update(input)
    hasher.complete(length)

  def keyedHash(key: Array[Byte]^{}, input: Array[Byte]^{}, length: Int = OutLen): Array[Byte]^{} =
    if key.length != KeyLen
    then panic(m"BLAKE3 key must be $KeyLen bytes (got ${key.length})")

    val keyWords = Array.allocate[Int](8)
    // `wordsFromBytes` only reads its bytes.
    wordsFromBytes(Array.unsafeJvm(key), 0, keyWords.raw)

    val hasher: Hasher^ = Hasher(Array.freeze(keyWords), KeyedHashFlag)
    hasher.update(input)
    hasher.complete(length)

  def deriveKey(context: Text, material: Array[Byte]^{}, length: Int = OutLen): Array[Byte]^{} =
    val ctxBytes = Array.unsafeFrozen(context.s.getBytes(StandardCharsets.UTF_8).nn)
    val ctxHasher: Hasher^ = Hasher(Iv, DeriveKeyContext)
    ctxHasher.update(ctxBytes)

    val ctxKey = ctxHasher.complete(KeyLen)
    val ctxKeyWords = Array.allocate[Int](8)
    wordsFromBytes(Array.unsafeJvm(ctxKey), 0, ctxKeyWords.raw)

    val matHasher: Hasher^ = Hasher(Array.freeze(ctxKeyWords), DeriveKeyMaterial)
    matHasher.update(material)
    matHasher.complete(length)

sealed trait Blake3 extends Algorithm:
  type Bits = 256
