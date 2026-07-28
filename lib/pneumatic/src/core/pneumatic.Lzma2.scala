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

import proscenium.compat.*

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

  private def compressorEngine(preset: Int): XzEngine^ = Lzma2CompressorEngine(preset)

  private def decompressorEngine(dictSize: Int): XzEngine^ = Lzma2DecompressorEngine(dictSize)

  private def defaultDictSize: Int = Lzma2Options.preset(DefaultPreset).dictSize

  given compression: Lzma2 is Compression:
    def compressor()(using Buffering): (Duct[Data, Data] {
      type Transport = Credit
      type Upstream = Credit })^ =

      XzStage(compressorEngine(DefaultPreset))

    def decompressor()(using Buffering): (Duct[Data, Data] {
      type Transport = Credit
      type Upstream = Credit })^ =

      XzStage(decompressorEngine(defaultDictSize))

    override def compress(stream: Progression[Data]): Progression[Data] =
      Xz.drive(compressorEngine(DefaultPreset), stream)

    override def decompress(stream: Progression[Data]): Progression[Data] =
      Xz.drive(decompressorEngine(defaultDictSize), stream)

  // Compress raw LZMA2 with an explicit preset (0..9).
  def compressor(preset: Int)(using Buffering): (Duct[Data, Data] {
    type Transport = Credit
    type Upstream = Credit })^ =

    XzStage(compressorEngine(preset))

  def compress(stream: Progression[Data], preset: Int): Progression[Data] =
    Xz.drive(compressorEngine(preset), stream)

  // Decompress raw LZMA2 with an explicit dictionary size — the size the producer used, as a raw
  // LZMA2 stream (unlike `.xz`) does not record it. The default forms assume the preset-6 size.
  def decompressor(dictSize: Int)(using Buffering): (Duct[Data, Data] {
    type Transport = Credit
    type Upstream = Credit })^ =

    XzStage(decompressorEngine(dictSize))

  def decompress(stream: Progression[Data], dictSize: Int): Progression[Data] =
    Xz.drive(decompressorEngine(dictSize), stream)

private enum Lzma2State:
  case Control, UncompressedHeader, UncompressedData, LzmaHeader, LzmaData, Ended

// The streaming LZMA2 decompressor. The original XZ-for-Java shape is a graph of
// mutually-referencing objects — the chunk parser, the `LzDecoder` sliding window, the
// `RangeDecoder` and the `LzmaDecoder` symbol decoder all hold each other — which separation
// checking cannot express, so they are flattened into this single `caps.Mutable` state machine.
// Each original object survives as a contiguous block of fields and methods (`dict*` is the former
// `LzDecoder`, `rc*` the former `RangeDecoder`, and the probability model plus `decodeSymbols` the
// former `LzmaDecoder`/`LzmaCoder`/`LengthDecoder`s). The 2-D probability matrices are flattened
// into single arrays indexed by computed offsets, so every adaptive probability lives in a tracked
// `Array[Short]^` field.
private[pneumatic] final class Lzma2Decompressor(dictSize: Int) extends caps.Mutable:
  import Lzma.*
  import RangeCoder.{TopMask, BitModelTotalBits, BitModelTotal, MoveBits, ProbInit}

  // Bounded output scratch: decode/copy passes are capped to this size so a large dictionary does
  // not force a correspondingly large flush buffer.
  private val flushCap =
    if dictSize >= (1 << 16) then 1 << 16 else if dictSize > 0 then dictSize else 1

  private val flushBuffer: Array[Byte]^ = new Array[Byte](flushCap)

  // --- The sliding-window dictionary (the former `LzDecoder`): a flat buffer of recently-decoded
  // bytes, from which match copies read. `dictStart..dictPos` is the not-yet-flushed run;
  // `dictFull` tracks the history available for distance references. `dictSetLimit` bounds how far
  // a single decode pass may advance `dictPos` before a flush, keeping match copies in the buffer.
  private val dictBuffer: Array[Byte]^ = new Array[Byte](dictSize)
  private var dictStart: Int = 0
  private var dictPos: Int = 0
  private var dictFull: Int = 0
  private var dictLimit: Int = 0
  private var pendingLength: Int = 0
  private var pendingDist: Int = 0

  private update def dictReset(): Unit =
    dictStart = 0
    dictPos = 0
    dictFull = 0
    dictLimit = 0
    dictBuffer(dictSize - 1) = 0 // so the very first back-reference reads a defined zero

  private update def dictSetLimit(outMax: Int): Unit =
    dictLimit = if dictSize - dictPos <= outMax then dictSize else dictPos + outMax

  private def dictHasSpace: Boolean = dictPos < dictLimit

  private def dictGetByte(dist: Int): Int =
    var offset = dictPos - dist - 1
    if dist >= dictPos then offset += dictSize
    dictBuffer(offset) & 0xff

  private update def dictPutByte(byte: Byte): Unit =
    dictBuffer(dictPos) = byte
    dictPos += 1
    if dictFull < dictPos then dictFull = dictPos

  private update def dictRepeat(dist: Int, len: Int): Unit =
    if dist < 0 || dist >= dictFull then
      throw IllegalStateException("the LZMA data is corrupt: invalid match distance")

    var left = if dictLimit - dictPos < len then dictLimit - dictPos else len
    pendingLength = len - left
    pendingDist = dist

    var back = dictPos - dist - 1
    if dist >= dictPos then back += dictSize

    while left > 0 do
      dictBuffer(dictPos) = dictBuffer(back)
      dictPos += 1
      back += 1
      if back == dictSize then back = 0
      left -= 1

    if dictFull < dictPos then dictFull = dictPos

  private update def dictRepeatPending(): Unit =
    if pendingLength > 0 then dictRepeat(pendingDist, pendingLength)

  // Copy `len` literal bytes straight from the buffered input (an LZMA2 uncompressed chunk).
  private update def dictCopyUncompressed(sourceOffset: Int, len: Int): Unit =
    val copySize = if dictSize - dictPos < len then dictSize - dictPos else len
    System.arraycopy(input, sourceOffset, dictBuffer, dictPos, copySize)
    dictPos += copySize
    if dictFull < dictPos then dictFull = dictPos

  // Emit everything decoded since the last flush into `flushBuffer`, wrapping `dictPos` when it
  // reaches the buffer end.
  private update def dictFlush(): Int =
    val copySize = dictPos - dictStart
    if dictPos == dictSize then dictPos = 0
    System.arraycopy(dictBuffer, dictStart, flushBuffer, 0, copySize)
    dictStart = dictPos
    copySize

  // --- The range decoder (the former `RangeDecoder`), decoding an LZMA2 chunk's compressed
  // payload in place from the buffered `input`. The first byte of the payload must be zero; the
  // following four are the initial `code`. `range` starts saturated. LZMA2 signals each chunk's
  // uncompressed length out of band, so an exact symbol count is decoded with no end marker.
  private var rcRange: Int = 0
  private var rcCode: Int = 0
  private var rcPos: Int = 0
  private var rcLimit: Int = 0

  private update def rcPrepare(offset: Int, length: Int): Unit =
    rcPos = offset + 1 // the leading byte is always zero
    rcLimit = offset + length
    rcRange = 0xffffffff
    rcCode = 0
    var i = 0
    while i < 4 do { rcCode = (rcCode << 8) | (input(rcPos) & 0xff); rcPos += 1; i += 1 }

  private def rcInError: Boolean = rcPos > rcLimit

  private update def rcNormalize(): Unit =
    if (rcRange & TopMask) == 0 then
      rcCode = (rcCode << 8) | (rcNextByte() & 0xff)
      rcRange <<= 8

  private update def rcNextByte(): Int =
    val byte = if rcPos < rcLimit then input(rcPos).toInt else 0
    rcPos += 1
    byte

  private update def rcDecodeBit(probs: Array[Short]^{this}, index: Int): Int =
    rcNormalize()
    val prob = probs(index).toInt
    val bound = (rcRange >>> BitModelTotalBits)*prob

    if (rcCode ^ 0x80000000) < (bound ^ 0x80000000) then
      rcRange = bound
      probs(index) = (prob + ((BitModelTotal - prob) >>> MoveBits)).toShort
      0
    else
      rcRange -= bound
      rcCode -= bound
      probs(index) = (prob - (prob >>> MoveBits)).toShort
      1

  // An MSB-first probability tree of `size` entries at `offset` within `probs`.
  private update def rcDecodeBitTree(probs: Array[Short]^{this}, offset: Int, size: Int): Int =
    var symbol = 1

    while
      symbol = (symbol << 1) | rcDecodeBit(probs, offset + symbol)
      symbol < size
    do ()

    symbol - size

  private update def rcDecodeBitTreeReverse(probs: Array[Short]^{this}, offset: Int, size: Int)
  :   Int =

    var symbol = 1
    var result = 0
    var i = 0

    while
      val bit = rcDecodeBit(probs, offset + symbol)
      symbol = (symbol << 1) | bit
      result |= bit << i
      i += 1
      symbol < size
    do ()

    result

  private update def rcDecodeDirectBits(count0: Int): Int =
    var result = 0
    var count = count0

    while count != 0 do
      rcNormalize()
      rcRange >>>= 1
      val t = (rcCode - rcRange) >>> 31
      rcCode -= rcRange & (t - 1)
      result = (result << 1) | (1 - t)
      count -= 1

    result

  // --- The LZMA probability model (the former `LzmaCoder`, `State` and the two
  // `LengthDecoder`s), reconfigured per chunk header. The 12-state context is `lzmaState`; the
  // matrices are flat, row-major (`state*PosStatesMax + posState` etc.).
  private var propsLc = -1
  private var propsLp = -1
  private var propsPb = -1
  private var modelReady = false

  private var posMask: Int = 0
  private var literalPosMask: Int = 0
  private var literalContextBits: Int = 0

  private var lzmaState: Int = 0
  private val reps: Array[Int]^ = new Array[Int](Reps)

  private val isMatch: Array[Short]^ = new Array[Short](States*PosStatesMax)
  private val isRep: Array[Short]^ = new Array[Short](States)
  private val isRep0: Array[Short]^ = new Array[Short](States)
  private val isRep1: Array[Short]^ = new Array[Short](States)
  private val isRep2: Array[Short]^ = new Array[Short](States)
  private val isRep0Long: Array[Short]^ = new Array[Short](States*PosStatesMax)
  private val distSlots: Array[Short]^ = new Array[Short](DistStates*DistSlots)
  private val distSpecial: Array[Short]^ = new Array[Short](DistSpecialTotal)
  private val distAlign: Array[Short]^ = new Array[Short](AlignSize)
  private var literalProbs: Array[Short]^ = new Array[Short](0x300)

  private val matchLenChoice: Array[Short]^ = new Array[Short](2)
  private val matchLenLow: Array[Short]^ = new Array[Short](PosStatesMax*LenLowSymbols)
  private val matchLenMid: Array[Short]^ = new Array[Short](PosStatesMax*LenMidSymbols)
  private val matchLenHigh: Array[Short]^ = new Array[Short](LenHighSymbols)
  private val repLenChoice: Array[Short]^ = new Array[Short](2)
  private val repLenLow: Array[Short]^ = new Array[Short](PosStatesMax*LenLowSymbols)
  private val repLenMid: Array[Short]^ = new Array[Short](PosStatesMax*LenMidSymbols)
  private val repLenHigh: Array[Short]^ = new Array[Short](LenHighSymbols)

  private update def resetProbs(probs: Array[Short]^{this}): Unit =
    var i = 0
    while i < probs.length do { probs(i) = ProbInit; i += 1 }

  private update def resetModel(): Unit =
    var i = 0
    while i < Reps do { reps(i) = 0; i += 1 }
    lzmaState = 0
    resetProbs(isMatch)
    resetProbs(isRep)
    resetProbs(isRep0)
    resetProbs(isRep1)
    resetProbs(isRep2)
    resetProbs(isRep0Long)
    resetProbs(distSlots)
    resetProbs(distSpecial)
    resetProbs(distAlign)
    resetProbs(literalProbs)
    resetProbs(matchLenChoice)
    resetProbs(matchLenLow)
    resetProbs(matchLenMid)
    resetProbs(matchLenHigh)
    resetProbs(repLenChoice)
    resetProbs(repLenLow)
    resetProbs(repLenMid)
    resetProbs(repLenHigh)

  private def literalIndex(prevByte: Int, position: Int): Int =
    val contextLow = prevByte >>> (8 - literalContextBits)
    val contextHigh = (position & literalPosMask) << literalContextBits
    (contextLow + contextHigh)*0x300

  private def stateIsLiteral: Boolean = lzmaState < 7

  private update def updateLiteralState(): Unit =
    lzmaState =
      if lzmaState < 4 then 0 else if lzmaState < 10 then lzmaState - 3 else lzmaState - 6

  private update def updateMatchState(): Unit = lzmaState = if lzmaState < 7 then 7 else 10
  private update def updateRepState(): Unit = lzmaState = if lzmaState < 7 then 8 else 11
  private update def updateShortRepState(): Unit = lzmaState = if lzmaState < 7 then 9 else 11

  // The length model (the former `LengthDecoder`): a choice bit selects the low (8), mid (8) or
  // high (256) symbol range, each a bit-tree; low/mid are indexed by position-state.
  private update def decodeLength
    ( choice: Array[Short]^{this},
      low: Array[Short]^{this},
      mid: Array[Short]^{this},
      high: Array[Short]^{this},
      posState: Int )
  :   Int =

    if rcDecodeBit(choice, 0) == 0
    then rcDecodeBitTree(low, posState*LenLowSymbols, LenLowSymbols) + MatchLenMin
    else if rcDecodeBit(choice, 1) == 0 then
      rcDecodeBitTree(mid, posState*LenMidSymbols, LenMidSymbols) + MatchLenMin + LenLowSymbols
    else
      rcDecodeBitTree(high, 0, LenHighSymbols) + MatchLenMin + LenLowSymbols + LenMidSymbols

  // --- The LZMA symbol decoder (the former `LzmaDecoder`): reads literals and matches from the
  // range decoder, writing bytes (and match copies) into the sliding window. Driven a window's
  // worth at a time, bounded by `dictSetLimit`. Any match left dangling at the limit finishes
  // first.
  private update def decodeSymbols(): Unit =
    dictRepeatPending()

    while dictHasSpace do
      val posState = dictPos & posMask

      if rcDecodeBit(isMatch, lzmaState*PosStatesMax + posState) == 0 then decodeLiteral()
      else
        val len =
          if rcDecodeBit(isRep, lzmaState) == 0 then decodeMatch(posState)
          else decodeRepMatch(posState)

        dictRepeat(reps(0), len)

  private update def decodeLiteral(): Unit =
    val prevByte = dictGetByte(0)
    val base = literalIndex(prevByte, dictPos)
    var symbol = 1

    if stateIsLiteral then
      while symbol < 0x100 do
        symbol = (symbol << 1) | rcDecodeBit(literalProbs, base + symbol)
    else
      var matchByte = dictGetByte(reps(0)) << 1
      var continue = true

      while continue && symbol < 0x100 do
        val matchBit = (matchByte >> 8) & 1
        matchByte <<= 1
        val bit = rcDecodeBit(literalProbs, base + ((1 + matchBit) << 8) + symbol)
        symbol = (symbol << 1) | bit
        if matchBit != bit then continue = false

      while symbol < 0x100 do
        symbol = (symbol << 1) | rcDecodeBit(literalProbs, base + symbol)

    dictPutByte(symbol.toByte)
    updateLiteralState()

  private update def decodeMatch(posState: Int): Int =
    updateMatchState()
    reps(3) = reps(2)
    reps(2) = reps(1)
    reps(1) = reps(0)

    val len = decodeLength(matchLenChoice, matchLenLow, matchLenMid, matchLenHigh, posState)
    val distSlot = rcDecodeBitTree(distSlots, distState(len)*DistSlots, DistSlots)

    if distSlot < DistModelStart then reps(0) = distSlot
    else
      val footerBits = (distSlot >> 1) - 1
      reps(0) = (2 | (distSlot & 1)) << footerBits

      if distSlot < DistModelEnd then
        val index = distSlot - DistModelStart
        reps(0) += rcDecodeBitTreeReverse(distSpecial, distSpecialOffsets(index),
            distSpecialSize(index))
      else
        reps(0) += rcDecodeDirectBits(footerBits - AlignBits) << AlignBits
        reps(0) += rcDecodeBitTreeReverse(distAlign, 0, AlignSize)

    len

  private update def decodeRepMatch(posState: Int): Int =
    if rcDecodeBit(isRep0, lzmaState) == 0 then
      if rcDecodeBit(isRep0Long, lzmaState*PosStatesMax + posState) == 0 then
        updateShortRepState()
        return 1
    else
      val distance =
        if rcDecodeBit(isRep1, lzmaState) == 0 then reps(1)
        else if rcDecodeBit(isRep2, lzmaState) == 0 then
          val d = reps(2)
          reps(2) = reps(1)
          d
        else
          val d = reps(3)
          reps(3) = reps(2)
          reps(2) = reps(1)
          d

      reps(1) = reps(0)
      reps(0) = distance

    updateRepState()
    decodeLength(repLenChoice, repLenLow, repLenMid, repLenHigh, posState)

  // --- The chunk state machine.
  private var stage = Lzma2State.Control

  // Buffered, not-yet-consumed compressed input, with a read cursor.
  private var input: Array[Byte]^ = new Array[Byte](1 << 16)
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

  def ended: Boolean = stage == Lzma2State.Ended

  // Total bytes drawn from the fed input, so the container can locate where a block's LZMA2 data
  // ends and its padding/check begin.
  def consumed: Int = consumedTotal

  private def available: Int = writePos - readPos

  private update def compact(): Unit =
    if readPos > 0 then
      System.arraycopy(input, readPos, input, 0, writePos - readPos)
      writePos -= readPos
      readPos = 0

  private update def ensureCapacity(extra: Int): Unit =
    if writePos + extra > input.length then
      compact()

      if writePos + extra > input.length then
        var size = input.length*2
        while writePos + extra > size do size *= 2
        val grown: Array[Byte]^ = new Array[Byte](size)
        System.arraycopy(input, 0, grown, 0, writePos)
        input = grown

  update def accept(bytes: Array[Byte]^{caps.any.rd}, offset: Int, length: Int): Unit =
    ensureCapacity(length)
    System.arraycopy(bytes.asInstanceOf[Array[Byte]], offset, input, writePos, length)
    writePos += length
    process()

  private update def readByte(): Int =
    val byte = input(readPos) & 0xff
    readPos += 1
    consumedTotal += 1
    byte

  private update def process(): Unit =
    var progressing = true

    while progressing do
      progressing = false

      stage match
        case Lzma2State.Ended => ()

        case Lzma2State.Control =>
          if available >= 1 then
            control = readByte()

            if control == 0x00 then stage = Lzma2State.Ended
            else if control >= 0x80 then
              chunkUncompressed = (control & 0x1f) << 16
              chunkReset = (control >> 5) & 0x03
              stage = Lzma2State.LzmaHeader
              progressing = true
            else if control <= 2 then
              if control == 1 then { dictReset(); dictResetPending = false }
              stage = Lzma2State.UncompressedHeader
              progressing = true
            else
              throw IllegalStateException("the LZMA2 data is corrupt: bad control byte")

        case Lzma2State.UncompressedHeader =>
          if available >= 2 then
            chunkUncompressed = ((readByte() << 8) | readByte()) + 1
            uncompressedRemaining = chunkUncompressed
            stage = Lzma2State.UncompressedData
            progressing = true

        case Lzma2State.UncompressedData =>
          if available >= 1 then
            val take =
              if available < uncompressedRemaining then available else uncompressedRemaining

            var offset = readPos
            var remaining = take

            while remaining > 0 do
              val step = if remaining < flushCap then remaining else flushCap
              dictCopyUncompressed(offset, step)
              val flushed = dictFlush()
              appendOutput(flushBuffer, flushed)
              offset += flushed
              remaining -= flushed

            readPos += take
            consumedTotal += take
            uncompressedRemaining -= take
            if uncompressedRemaining == 0 then stage = Lzma2State.Control
            progressing = uncompressedRemaining == 0

        case Lzma2State.LzmaHeader =>
          val headerSize = 4 + (if chunkReset >= 2 then 1 else 0)

          if available >= headerSize then
            chunkUncompressed += ((readByte() << 8) | readByte()) + 1
            chunkCompressed = ((readByte() << 8) | readByte()) + 1

            if chunkReset >= 2 then configureProperties(readByte())
            else if !modelReady then
              throw IllegalStateException("the LZMA2 data is corrupt: no properties before chunk")
            else if chunkReset == 1 then resetModel()

            if chunkReset == 3 then dictReset()
            uncompressedRemaining = chunkUncompressed
            stage = Lzma2State.LzmaData
            progressing = true

        case Lzma2State.LzmaData =>
          if available >= chunkCompressed then
            rcPrepare(readPos, chunkCompressed)
            var remaining = uncompressedRemaining

            while remaining > 0 do
              dictSetLimit(if remaining < flushCap then remaining else flushCap)
              decodeSymbols()
              val flushed = dictFlush()
              appendOutput(flushBuffer, flushed)
              remaining -= flushed

            if rcInError then
              throw IllegalStateException("the LZMA2 data is corrupt: range coder overran chunk")

            readPos += chunkCompressed
            consumedTotal += chunkCompressed
            stage = Lzma2State.Control
            progressing = true

    compact()


  private update def configureProperties(propsByte: Int): Unit =
    val (lc, lp, pb) = Lzma2Options.decodeProperties(propsByte)

    if !modelReady || lc != propsLc || lp != propsLp || pb != propsPb then
      propsLc = lc; propsLp = lp; propsPb = pb
      posMask = (1 << pb) - 1
      literalPosMask = (1 << lp) - 1
      literalContextBits = lc
      val literalSize = 0x300 << (lc + lp)
      if literalProbs.length != literalSize then literalProbs = new Array[Short](literalSize)
      modelReady = true

    resetModel()

  // Append the first `count` bytes of the flush scratch (passed in as `this`-scoped so the field
  // array flows through the exclusive-parameter shape) to the decoded output.
  private update def appendOutput(flushed: Array[Byte]^{this}, count: Int): Unit =
    var i = 0
    while i < count do { output += flushed(i); i += 1 }

  update def finish(): Unit =
    if stage != Lzma2State.Ended && available > 0 then process()

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
  // The match finder privately owns its buffers; the construction capture is erased.
  private val finder: HashChain =
    scala.caps.unsafe.unsafeAssumePure
      (HashChain(data, if effectiveDict > 0 then effectiveDict else 1, depth))

  // Likewise: the encoder's state is reached only through this compressor.
  private val lzma: LzmaEncoder =
    scala.caps.unsafe.unsafeAssumePure
      (LzmaEncoder(data, rc, options.lc, options.lp, options.pb, finder,
          options.niceLen, options.mode == Lzma2Options.ModeNormal))

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
