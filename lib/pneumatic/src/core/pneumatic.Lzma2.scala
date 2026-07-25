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

import scala.collection.mutable as scm

import anticipation.*
import rudiments.*
import turbulence.*
import vacuous.*
import zephyrine.*

// LZMA2 wraps the raw LZMA symbol stream in self-describing chunks: each carries its uncompressed
// size and, for compressed chunks, its compressed size plus flags saying whether the dictionary,
// the LZMA state, and the properties (lc/lp/pb) reset at the chunk boundary. This is what makes
// LZMA2 seekable and resettable where plain LZMA is not. A control byte of 0x00 ends the stream.
//
// The decompressor is a streaming state machine: it accumulates just enough input to parse a chunk
// header, then (for compressed chunks) buffers the ≤ 64 KiB compressed payload before handing to
// the range decoder. Consumed input is compacted away, so buffered compressed data stays bounded to
// roughly one chunk. Decoded bytes are appended to `output`, which the enclosing engine drains.

object Lzma2:
  inline val UncompressedSizeMax = 1 << 21 // 2 MiB per chunk
  inline val CompressedSizeMax = 1 << 16   // 64 KiB per LZMA chunk
  inline val DefaultPreset = 6

  private def compressorEngine(preset: Int): XzEngine = Lzma2CompressorEngine(preset)

  private def decompressorEngine(dictSize: Int): XzEngine = Lzma2DecompressorEngine(dictSize)

  private def defaultDictSize: Int = Lzma2Options.preset(DefaultPreset).dictSize

  given compression: Lzma2 is Compression:
    def compressor()(using Buffering): Duct[Data, Data] {
      type Transport = Credit
      type Upstream = Credit } =

      XzStage(compressorEngine(DefaultPreset))

    def decompressor()(using Buffering): Duct[Data, Data] {
      type Transport = Credit
      type Upstream = Credit } =

      XzStage(decompressorEngine(defaultDictSize))

    override def compress(stream: Progression[Data]): Progression[Data] =
      Xz.drive(compressorEngine(DefaultPreset), stream)

    override def decompress(stream: Progression[Data]): Progression[Data] =
      Xz.drive(decompressorEngine(defaultDictSize), stream)

  // Compress raw LZMA2 with an explicit preset (0..9).
  def compressor(preset: Int)(using Buffering): Duct[Data, Data] {
    type Transport = Credit
    type Upstream = Credit } =

    XzStage(compressorEngine(preset))

  def compress(stream: Progression[Data], preset: Int): Progression[Data] =
    Xz.drive(compressorEngine(preset), stream)

  // Decompress raw LZMA2 with an explicit dictionary size — the size the producer used, as a raw
  // LZMA2 stream (unlike `.xz`) does not record it. The default forms assume the preset-6 size.
  def decompressor(dictSize: Int)(using Buffering): Duct[Data, Data] {
    type Transport = Credit
    type Upstream = Credit } =

    XzStage(decompressorEngine(dictSize))

  def decompress(stream: Progression[Data], dictSize: Int): Progression[Data] =
    Xz.drive(decompressorEngine(dictSize), stream)

private enum Lzma2State:
  case Control, UncompressedHeader, UncompressedData, LzmaHeader, LzmaData, Ended

private[pneumatic] final class Lzma2Decompressor(dictSize: Int):
  private val lz = LzDecoder(dictSize)
  private val rc = RangeDecoder()

  // Bounded output scratch: decode/copy passes are capped to this size so a large dictionary does
  // not force a correspondingly large flush buffer.
  private val flushCap =
    if dictSize >= (1 << 16) then 1 << 16 else if dictSize > 0 then dictSize else 1

  private val flushBuffer = new Array[Byte](flushCap)
  private var lzma: LzmaDecoder | Null = null
  private var haveProperties = false
  private var propsLc = -1
  private var propsLp = -1
  private var propsPb = -1

  private var state = Lzma2State.Control

  // Buffered, not-yet-consumed compressed input, with a read cursor.
  private var input: Array[Byte] = new Array[Byte](1 << 16)
  private var readPos = 0
  private var writePos = 0

  // Per-chunk header fields.
  private var control = 0
  private var chunkReset = 0
  private var chunkUncompressed = 0
  private var chunkCompressed = 0
  private var uncompressedRemaining = 0
  private var dictResetPending = true

  val output: scm.ArrayBuffer[Byte] = scm.ArrayBuffer()

  private var consumedTotal = 0

  def ended: Boolean = state == Lzma2State.Ended

  // Total bytes drawn from the fed input, so the container can locate where a block's LZMA2 data
  // ends and its padding/check begin.
  def consumed: Int = consumedTotal

  private def available: Int = writePos - readPos

  private def compact(): Unit =
    if readPos > 0 then
      System.arraycopy(input, readPos, input, 0, writePos - readPos)
      writePos -= readPos
      readPos = 0

  private def ensureCapacity(extra: Int): Unit =
    if writePos + extra > input.length then
      compact()

      if writePos + extra > input.length then
        var size = input.length*2
        while writePos + extra > size do size *= 2
        val grown = new Array[Byte](size)
        System.arraycopy(input, 0, grown, 0, writePos)
        input = grown

  def accept(bytes: Array[Byte], offset: Int, length: Int): Unit =
    ensureCapacity(length)
    System.arraycopy(bytes, offset, input, writePos, length)
    writePos += length
    process()

  private def readByte(): Int =
    val byte = input(readPos) & 0xff
    readPos += 1
    consumedTotal += 1
    byte

  private def process(): Unit =
    var progressing = true

    while progressing do
      progressing = false

      state match
        case Lzma2State.Ended => ()

        case Lzma2State.Control =>
          if available >= 1 then
            control = readByte()

            if control == 0x00 then state = Lzma2State.Ended
            else if control >= 0x80 then
              chunkUncompressed = (control & 0x1f) << 16
              chunkReset = (control >> 5) & 0x03
              state = Lzma2State.LzmaHeader
              progressing = true
            else if control <= 2 then
              if control == 1 then { lz.reset(); dictResetPending = false; haveProperties = false }
              state = Lzma2State.UncompressedHeader
              progressing = true
            else
              throw IllegalStateException("the LZMA2 data is corrupt: bad control byte")

        case Lzma2State.UncompressedHeader =>
          if available >= 2 then
            chunkUncompressed = ((readByte() << 8) | readByte()) + 1
            uncompressedRemaining = chunkUncompressed
            state = Lzma2State.UncompressedData
            progressing = true

        case Lzma2State.UncompressedData =>
          if available >= 1 then
            val take =
              if available < uncompressedRemaining then available else uncompressedRemaining

            var offset = readPos
            var remaining = take

            while remaining > 0 do
              val step = if remaining < flushCap then remaining else flushCap
              lz.copyUncompressed(input, offset, step)
              val flushed = lz.flush(flushBuffer, 0)
              appendOutput(flushed)
              offset += flushed
              remaining -= flushed

            readPos += take
            consumedTotal += take
            uncompressedRemaining -= take
            if uncompressedRemaining == 0 then state = Lzma2State.Control
            progressing = uncompressedRemaining == 0

        case Lzma2State.LzmaHeader =>
          val headerSize = 4 + (if chunkReset >= 2 then 1 else 0)

          if available >= headerSize then
            chunkUncompressed += ((readByte() << 8) | readByte()) + 1
            chunkCompressed = ((readByte() << 8) | readByte()) + 1

            if chunkReset >= 2 then configureProperties(readByte())
            else if chunkReset == 1 then
              (lzma.nn).reset()
            else if lzma == null then
              throw IllegalStateException("the LZMA2 data is corrupt: no properties before chunk")

            if chunkReset == 3 then lz.reset()
            uncompressedRemaining = chunkUncompressed
            state = Lzma2State.LzmaData
            progressing = true

        case Lzma2State.LzmaData =>
          if available >= chunkCompressed then
            rc.prepare(input, readPos, chunkCompressed)
            var remaining = uncompressedRemaining

            while remaining > 0 do
              lz.setLimit(if remaining < flushCap then remaining else flushCap)
              (lzma.nn).decode()
              val flushed = lz.flush(flushBuffer, 0)
              appendOutput(flushed)
              remaining -= flushed

            if rc.inError then
              throw IllegalStateException("the LZMA2 data is corrupt: range coder overran chunk")

            readPos += chunkCompressed
            consumedTotal += chunkCompressed
            state = Lzma2State.Control
            progressing = true

    compact()

  private def configureProperties(propsByte: Int): Unit =
    val (lc, lp, pb) = Lzma2Options.decodeProperties(propsByte)

    if lzma == null || lc != propsLc || lp != propsLp || pb != propsPb then
      propsLc = lc; propsLp = lp; propsPb = pb
      lzma = LzmaDecoder(lz, rc, lc, lp, pb)
    else
      (lzma.nn).reset()

    haveProperties = true

  private def appendOutput(count: Int): Unit =
    var i = 0
    while i < count do { output += flushBuffer(i); i += 1 }

  def finish(): Unit =
    if state != Lzma2State.Ended && available > 0 then process()

// Compresses a fully-buffered payload into an LZMA2 chunk stream. The single LZMA model persists
// across chunks (only the first chunk resets the dictionary, state and properties); the range coder
// is finished and restarted at each chunk boundary, which is cut when the chunk's uncompressed size
// approaches 2 MiB or its compressed size approaches the 64 KiB ceiling. A trailing 0x00 control
// byte ends the stream. Incompressible spans still emit valid (if not smaller) LZMA chunks.
private[pneumatic] final class Lzma2Compressor(data: Array[Byte], options: Lzma2Options):
  private val rc = RangeEncoder()

  private val effectiveDict =
    if options.dictSize < data.length then options.dictSize else data.length

  private val depth = if options.depthLimit > 0 then options.depthLimit else 32
  private val finder = HashChain(data, if effectiveDict > 0 then effectiveDict else 1, depth)

  private val lzma = LzmaEncoder(data, rc, options.lc, options.lp, options.pb, finder,
      options.niceLen, options.mode == Lzma2Options.ModeNormal)

  def compress(): Array[Byte] =
    val out = scm.ArrayBuffer[Byte]()

    if data.length == 0 then out += 0x00.toByte else
      var firstChunk = true

      while lzma.hasMore do
        rc.reset()
        val startPos = lzma.pos
        val uncompressedCeiling = Lzma2.UncompressedSizeMax - Lzma.MatchLenMax
        val compressedCeiling = Lzma2.CompressedSizeMax - 64

        while lzma.hasMore &&
          (lzma.pos - startPos) < uncompressedCeiling &&
          rc.pendingSize < compressedCeiling
        do lzma.encodeSymbol()

        val uncompressedSize = lzma.pos - startPos
        val compressedSize = rc.finish()
        val reset = if firstChunk then 3 else 0
        val u = uncompressedSize - 1
        val c = compressedSize - 1

        out += (0x80 | (reset << 5) | ((u >>> 16) & 0x1f)).toByte
        out += ((u >>> 8) & 0xff).toByte
        out += (u & 0xff).toByte
        out += ((c >>> 8) & 0xff).toByte
        out += (c & 0xff).toByte

        if reset >= 2 then
          out += Lzma2Options.propertiesByte(options.lc, options.lp, options.pb).toByte

        val payload = rc.bytes
        var i = 0
        while i < payload.length do { out += payload(i); i += 1 }
        firstChunk = false

      out += 0x00.toByte

    out.toArray

sealed trait Lzma2 extends Compressor
