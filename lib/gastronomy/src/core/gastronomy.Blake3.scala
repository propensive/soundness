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
┃    Soundness, version 0.54.0.                                                                    ┃
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

import java.nio.charset.StandardCharsets

import scala.reflect.Selectable.reflectiveSelectable

import anticipation.*
import fulminate.*
import gossamer.*
import prepositional.*
import rudiments.*
import vacuous.*

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

  private final val Iv: Array[Int] =
    Array
      ( 0x6a09e667, 0xbb67ae85, 0x3c6ef372, 0xa54ff53a,
        0x510e527f, 0x9b05688c, 0x1f83d9ab, 0x5be0cd19 )

  private final val MsgPermutation: Array[Int] =
    Array(2, 6, 3, 10, 7, 0, 4, 13, 1, 11, 12, 5, 9, 14, 15, 8)

  private def mix(state: Array[Int], a: Int, b: Int, c: Int, d: Int, mx: Int, my: Int): Unit =
    state(a) = state(a) + state(b) + mx
    state(d) = Integer.rotateRight(state(d) ^ state(a), 16)
    state(c) = state(c) + state(d)
    state(b) = Integer.rotateRight(state(b) ^ state(c), 12)
    state(a) = state(a) + state(b) + my
    state(d) = Integer.rotateRight(state(d) ^ state(a), 8)
    state(c) = state(c) + state(d)
    state(b) = Integer.rotateRight(state(b) ^ state(c), 7)

  private def round(state: Array[Int], m: Array[Int]): Unit =
    // SIMD: these four column mixes operate on disjoint quadruples of state and would be
    //       issued as a single 4-lane vector instruction by an SSE/NEON backend.
    mix(state, 0, 4,  8, 12, m(0), m(1))
    mix(state, 1, 5,  9, 13, m(2), m(3))
    mix(state, 2, 6, 10, 14, m(4), m(5))
    mix(state, 3, 7, 11, 15, m(6), m(7))
    // SIMD: the diagonal mixes form the second 4-lane batch, with the same shape.
    mix(state, 0, 5, 10, 15, m(8), m(9))
    mix(state, 1, 6, 11, 12, m(10), m(11))
    mix(state, 2, 7,  8, 13, m(12), m(13))
    mix(state, 3, 4,  9, 14, m(14), m(15))

  private def permute(m: Array[Int]): Unit =
    val out = new Array[Int](16)
    var i = 0

    while i < 16 do
      out(i) = m(MsgPermutation(i))
      i += 1

    System.arraycopy(out, 0, m, 0, 16)

  private def compress
    ( chainingValue: Array[Int],
      blockWords:    Array[Int],
      counter:       Long,
      blockLen:      Int,
      flags:         Int )
  :   Array[Int] =

    val state = new Array[Int](16)
    System.arraycopy(chainingValue, 0, state, 0, 8)
    state(8)  = Iv(0); state(9)  = Iv(1); state(10) = Iv(2); state(11) = Iv(3)
    state(12) = counter.toInt
    state(13) = (counter >>> 32).toInt
    state(14) = blockLen
    state(15) = flags

    val block = blockWords.clone()

    round(state, block); permute(block)
    round(state, block); permute(block)
    round(state, block); permute(block)
    round(state, block); permute(block)
    round(state, block); permute(block)
    round(state, block); permute(block)
    round(state, block)

    var i = 0

    while i < 8 do
      state(i)     = state(i) ^ state(i + 8)
      state(i + 8) = state(i + 8) ^ chainingValue(i)
      i += 1

    state

  private def wordsFromBytes(bytes: Array[Byte], offset: Int, words: Array[Int]): Unit =
    var i = 0

    while i < words.length do
      val o = offset + 4*i

      words(i) =
        (bytes(o)     & 0xff)         |
          ((bytes(o + 1) & 0xff) <<  8) |
          ((bytes(o + 2) & 0xff) << 16) |
          ((bytes(o + 3) & 0xff) << 24)

      i += 1

  private final class Output
    ( val inputChainingValue: Array[Int],
      val blockWords:         Array[Int],
      val counter:            Long,
      val blockLen:           Int,
      val flags:              Int ):

    def chainingValue(): Array[Int] =
      val out = compress(inputChainingValue, blockWords, counter, blockLen, flags)
      val cv = new Array[Int](8)
      System.arraycopy(out, 0, cv, 0, 8)
      cv

    def rootOutputBytes(outLen: Int): Array[Byte] =
      val result = new Array[Byte](outLen)
      var blockCounter = 0L
      var pos = 0

      while pos < outLen do
        val words =
          compress(inputChainingValue, blockWords, blockCounter, blockLen, flags | RootFlag)

        val take = math.min(2*OutLen, outLen - pos)
        var i = 0

        while i < take do
          result(pos + i) = (words(i/4) >>> (8*(i%4))).toByte
          i += 1

        pos += take
        blockCounter += 1

      result

  private def parentOutput
    ( leftCv: Array[Int], rightCv: Array[Int], keyWords: Array[Int], flags: Int )
  :   Output =

    val blockWords = new Array[Int](16)
    System.arraycopy(leftCv, 0, blockWords, 0, 8)
    System.arraycopy(rightCv, 0, blockWords, 8, 8)
    Output(keyWords.clone(), blockWords, 0L, BlockLen, ParentFlag | flags)

  private def parentCv
    ( leftCv: Array[Int], rightCv: Array[Int], keyWords: Array[Int], flags: Int )
  :   Array[Int] =

    parentOutput(leftCv, rightCv, keyWords, flags).chainingValue()

  private final class ChunkState(keyWordsInit: Array[Int], var chunkCounter: Long, val flags: Int):
    val chainingValue:    Array[Int]  = keyWordsInit.clone()
    val block:            Array[Byte] = new Array[Byte](BlockLen)
    var blockLen:         Int         = 0
    var blocksCompressed: Int         = 0

    def len: Int = BlockLen*blocksCompressed + blockLen

    private def startFlag: Int = if blocksCompressed == 0 then ChunkStart else 0

    def update(input: Array[Byte], start: Int, end: Int): Unit =
      val blockWords = new Array[Int](16)
      var pos = start

      while pos < end do
        if blockLen == BlockLen then
          wordsFromBytes(block, 0, blockWords)

          val out =
            compress(chainingValue, blockWords, chunkCounter, BlockLen, flags | startFlag)

          System.arraycopy(out, 0, chainingValue, 0, 8)
          blocksCompressed += 1
          java.util.Arrays.fill(block, 0.toByte)
          blockLen = 0

        val want = BlockLen - blockLen
        val take = math.min(want, end - pos)
        System.arraycopy(input, pos, block, blockLen, take)
        blockLen += take
        pos += take

    def output(): Output =
      val blockWords = new Array[Int](16)
      wordsFromBytes(block, 0, blockWords)

      Output
        ( chainingValue.clone(),
          blockWords,
          chunkCounter,
          blockLen,
          flags | startFlag | ChunkEnd )

  private final class Hasher(keyWordsInit: Array[Int], val flags: Int):
    private val keyWords:   Array[Int]        = keyWordsInit.clone()
    private var chunkState: ChunkState        = ChunkState(keyWords, 0L, flags)
    private val cvStack:    Array[Array[Int]] = new Array[Array[Int]](54)
    private var cvStackLen: Int               = 0

    private def pushStack(cv: Array[Int]): Unit =
      cvStack(cvStackLen) = cv
      cvStackLen += 1

    private def popStack(): Array[Int] =
      cvStackLen -= 1
      cvStack(cvStackLen)

    private def addChunkCv(initialCv: Array[Int], initialTotal: Long): Unit =
      var cv = initialCv
      var totalChunks = initialTotal

      while (totalChunks & 1L) == 0L do
        cv = parentCv(popStack(), cv, keyWords, flags)
        totalChunks >>= 1

      pushStack(cv)

    def update(data: IArray[Byte]): Unit =
      // SIMD: AVX2 / AVX-512 backends process 4 / 8 / 16 chunks at a time here using interleaved
      //       state; the scalar path below handles one chunk per iteration.
      val input = data.mutable(using Unsafe)
      var pos = 0
      val end = input.length

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

    def complete(outLen: Int): IArray[Byte] =
      var current = chunkState.output()
      var i = cvStackLen

      while i > 0 do
        i -= 1
        current = parentOutput(cvStack(i), current.chainingValue(), keyWords, flags)

      current.rootOutputBytes(outLen).immutable(using Unsafe)

  given hash: (hashing: Hashing { def blake3: Hashing.Function }) => Hash in Blake3 =
    Hash(t"BLAKE3", t"HMAC-BLAKE3", hashing.blake3)

  // The pure-Scala BLAKE3 `Digestion`, used by the Soundness hashing provider.
  def digestion(): Digestion = new Digestion:
    private val hasher: Hasher = Hasher(Iv, 0)
    def append(bytes: Data): Unit = hasher.update(bytes)
    def digest(): Data = hasher.complete(OutLen)

  def hashOf(input: IArray[Byte], length: Int = OutLen): IArray[Byte] =
    val hasher = Hasher(Iv, 0)
    hasher.update(input)
    hasher.complete(length)

  def keyedHash(key: IArray[Byte], input: IArray[Byte], length: Int = OutLen): IArray[Byte] =
    if key.length != KeyLen
    then panic(m"BLAKE3 key must be $KeyLen bytes (got ${key.length})")

    val keyBytes = key.mutable(using Unsafe)
    val keyWords = new Array[Int](8)
    wordsFromBytes(keyBytes, 0, keyWords)

    val hasher = Hasher(keyWords, KeyedHashFlag)
    hasher.update(input)
    hasher.complete(length)

  def deriveKey(context: Text, material: IArray[Byte], length: Int = OutLen): IArray[Byte] =
    val ctxBytes: Array[Byte] = context.s.getBytes(StandardCharsets.UTF_8).nn
    val ctxHasher = Hasher(Iv, DeriveKeyContext)
    ctxHasher.update(ctxBytes.immutable(using Unsafe))

    val ctxKey = ctxHasher.complete(KeyLen).mutable(using Unsafe)
    val ctxKeyWords = new Array[Int](8)
    wordsFromBytes(ctxKey, 0, ctxKeyWords)

    val matHasher = Hasher(ctxKeyWords, DeriveKeyMaterial)
    matHasher.update(material)
    matHasher.complete(length)

sealed trait Blake3 extends Algorithm:
  type Bits = 256
