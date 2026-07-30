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

  private val flushBuffer: scala.Array[Byte]^ = new scala.Array[Byte](flushCap)

  // --- The sliding-window dictionary (the former `LzDecoder`): a flat buffer of recently-decoded
  // bytes, from which match copies read. `dictStart..dictPos` is the not-yet-flushed run;
  // `dictFull` tracks the history available for distance references. `dictSetLimit` bounds how far
  // a single decode pass may advance `dictPos` before a flush, keeping match copies in the buffer.
  private val dictBuffer: scala.Array[Byte]^ = new scala.Array[Byte](dictSize)
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

  private update def rcDecodeBit(probs: scala.Array[Short]^{this}, index: Int): Int =
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
  private update def rcDecodeBitTree(probs: scala.Array[Short]^{this}, offset: Int, size: Int): Int =
    var symbol = 1

    while
      symbol = (symbol << 1) | rcDecodeBit(probs, offset + symbol)
      symbol < size
    do ()

    symbol - size

  private update def rcDecodeBitTreeReverse(probs: scala.Array[Short]^{this}, offset: Int, size: Int)
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
  private val reps: scala.Array[Int]^ = new scala.Array[Int](Reps)

  private val isMatch: scala.Array[Short]^ = new scala.Array[Short](States*PosStatesMax)
  private val isRep: scala.Array[Short]^ = new scala.Array[Short](States)
  private val isRep0: scala.Array[Short]^ = new scala.Array[Short](States)
  private val isRep1: scala.Array[Short]^ = new scala.Array[Short](States)
  private val isRep2: scala.Array[Short]^ = new scala.Array[Short](States)
  private val isRep0Long: scala.Array[Short]^ = new scala.Array[Short](States*PosStatesMax)
  private val distSlots: scala.Array[Short]^ = new scala.Array[Short](DistStates*DistSlots)
  private val distSpecial: scala.Array[Short]^ = new scala.Array[Short](DistSpecialTotal)
  private val distAlign: scala.Array[Short]^ = new scala.Array[Short](AlignSize)
  private var literalProbs: scala.Array[Short]^ = new scala.Array[Short](0x300)

  private val matchLenChoice: scala.Array[Short]^ = new scala.Array[Short](2)
  private val matchLenLow: scala.Array[Short]^ = new scala.Array[Short](PosStatesMax*LenLowSymbols)
  private val matchLenMid: scala.Array[Short]^ = new scala.Array[Short](PosStatesMax*LenMidSymbols)
  private val matchLenHigh: scala.Array[Short]^ = new scala.Array[Short](LenHighSymbols)
  private val repLenChoice: scala.Array[Short]^ = new scala.Array[Short](2)
  private val repLenLow: scala.Array[Short]^ = new scala.Array[Short](PosStatesMax*LenLowSymbols)
  private val repLenMid: scala.Array[Short]^ = new scala.Array[Short](PosStatesMax*LenMidSymbols)
  private val repLenHigh: scala.Array[Short]^ = new scala.Array[Short](LenHighSymbols)

  private update def resetProbs(probs: scala.Array[Short]^{this}): Unit =
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
    ( choice: scala.Array[Short]^{this},
      low: scala.Array[Short]^{this},
      mid: scala.Array[Short]^{this},
      high: scala.Array[Short]^{this},
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
  private var input: scala.Array[Byte]^ = new scala.Array[Byte](1 << 16)
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
        val grown: scala.Array[Byte]^ = new scala.Array[Byte](size)
        System.arraycopy(input, 0, grown, 0, writePos)
        input = grown

  update def accept(bytes: Array[Byte]^{caps.any.rd}, offset: Int, length: Int): Unit =
    ensureCapacity(length)
    System.arraycopy(bytes.asInstanceOf[scala.Array[Byte]], offset, input, writePos, length)
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
      if literalProbs.length != literalSize then literalProbs = new scala.Array[Short](literalSize)
      modelReady = true

    resetModel()

  // Append the first `count` bytes of the flush scratch (passed in as `this`-scoped so the field
  // array flows through the exclusive-parameter shape) to the decoded output.
  private update def appendOutput(flushed: scala.Array[Byte]^{this}, count: Int): Unit =
    var i = 0
    while i < count do { output += flushed(i); i += 1 }

  update def finish(): Unit =
    if stage != Lzma2State.Ended && available > 0 then process()

// Compresses a fully-buffered payload into an LZMA2 chunk stream. The single LZMA model persists
// across chunks (only the first chunk resets the dictionary, state and properties); the range coder
// is finished and restarted at each chunk boundary, which is cut when the chunk's uncompressed size
// approaches 2 MiB or its compressed size approaches the 64 KiB ceiling. A trailing 0x00 control
// byte ends the stream. Incompressible spans still emit valid (if not smaller) LZMA chunks.
//
// As with the decompressor above, the original mutually-referencing objects — the chunk framer,
// the `RangeEncoder` (shared between framer and symbol encoder), the `HashChain` match finder and
// the `LzmaEncoder` with its `LzmaCoder`/`LengthEncoder` model — are flattened into one
// `caps.Mutable` state machine, with the probability matrices flattened into single tracked
// arrays indexed by computed offsets.
private[pneumatic] final class Lzma2Compressor(data: scala.Array[Byte], options: Lzma2Options)
extends caps.Mutable:
  import Lzma.*
  import RangeCoder.{TopMask, BitModelTotalBits, BitModelTotal, MoveBits, ProbInit}

  // --- The range encoder (the former `RangeEncoder`): encodes single bits, direct bits and
  // probability trees into a growable buffer. `rcLow` is a 33-bit carry accumulator; `rcShiftLow`
  // propagates carries into already-buffered bytes through the one-byte `rcCache` plus a run of
  // `rcCacheSize` deferred `0xff`s.
  private var rcBuffer: scala.Array[Byte]^ = new scala.Array[Byte](1 << 16)
  private var rcCount: Int = 0
  private var rcLow: Long = 0
  private var rcRange: Int = 0
  private var rcCache: Int = 0
  private var rcCacheSize: Long = 0

  private update def rcReset(): Unit =
    rcLow = 0
    rcRange = 0xffffffff
    rcCache = 0
    rcCacheSize = 1
    rcCount = 0

  // A conservative upper bound on how many bytes finishing now would append: the already-buffered
  // bytes, the deferred cache run, and the five flush bytes. Used to keep LZMA2 chunks within
  // their 64 KiB compressed limit.
  private def rcPendingSize: Int = rcCount + rcCacheSize.toInt + 5 - 1

  private update def rcWriteByte(byte: Int): Unit =
    if rcCount == rcBuffer.length then
      val grown: scala.Array[Byte]^ = new scala.Array[Byte](rcBuffer.length*2)
      System.arraycopy(rcBuffer, 0, grown, 0, rcCount)
      rcBuffer = grown

    rcBuffer(rcCount) = byte.toByte
    rcCount += 1

  private update def rcShiftLow(): Unit =
    val lowHigh = (rcLow >>> 32).toInt

    if lowHigh != 0 || rcLow < 0xff000000L then
      var temp = rcCache
      var continue = true

      while continue do
        rcWriteByte((temp + lowHigh) & 0xff)
        temp = 0xff
        rcCacheSize -= 1
        continue = rcCacheSize != 0

      rcCache = (rcLow >>> 24).toInt & 0xff

    rcCacheSize += 1
    rcLow = (rcLow & 0x00ffffff) << 8

  private update def rcEncodeBit(probs: scala.Array[Short]^{this}, index: Int, bit: Int): Unit =
    val prob = probs(index).toInt
    val bound = (rcRange >>> BitModelTotalBits)*prob

    if bit == 0 then
      rcRange = bound
      probs(index) = (prob + ((BitModelTotal - prob) >>> MoveBits)).toShort
    else
      rcLow += bound & 0xffffffffL
      rcRange -= bound
      probs(index) = (prob - (prob >>> MoveBits)).toShort

    if (rcRange & TopMask) == 0 then
      rcRange <<= 8
      rcShiftLow()

  // An MSB-first probability tree of `size` entries at `offset` within `probs`.
  private update def rcEncodeBitTree
    ( probs: scala.Array[Short]^{this}, offset: Int, size: Int, symbol: Int )
  :   Unit =

    var index = 1
    var mask = size
    var continue = true

    while continue do
      mask >>>= 1
      val bit = symbol & mask
      rcEncodeBit(probs, offset + index, if bit != 0 then 1 else 0)
      index <<= 1
      if bit != 0 then index |= 1
      continue = mask != 1

  private update def rcEncodeBitTreeReverse
    ( probs: scala.Array[Short]^{this}, offset: Int, size: Int, symbol0: Int )
  :   Unit =

    var index = 1
    var symbol = symbol0
    var continue = true

    while continue do
      val bit = symbol & 1
      symbol >>>= 1
      rcEncodeBit(probs, offset + index, bit)
      index = (index << 1) | bit
      continue = index < size

  private update def rcEncodeDirectBits(value: Int, count0: Int): Unit =
    var i = count0

    while i != 0 do
      i -= 1
      rcRange >>>= 1
      if ((value >>> i) & 1) != 0 then rcLow += rcRange.toLong & 0xffffffffL

      if (rcRange & TopMask) == 0 then
        rcRange <<= 8
        rcShiftLow()

  // Flush the last five bytes of `rcLow`; `rcBuffer(0 ..< rcCount)` then holds a complete
  // range-coded payload.
  private update def rcFinish(): Int =
    var i = 0
    while i < 5 do { rcShiftLow(); i += 1 }
    rcCount

  // --- The match finder (the former `HashChain`) over the fully-buffered input. Every position
  // is keyed by a hash of its four leading bytes; positions sharing a hash are threaded
  // newest-first through `hashChain`, so a search walks recent candidates in the dictionary
  // window up to a bounded depth — the hash-chain (HC4) family used by the fast LZMA presets.
  private val length = data.length

  private val effectiveDict =
    val bounded = if options.dictSize < data.length then options.dictSize else data.length
    if bounded > 0 then bounded else 1

  private val depth = if options.depthLimit > 0 then options.depthLimit else 32

  private final val HashBits = 17
  private val hashHead: scala.Array[Int]^ = new scala.Array[Int](1 << HashBits)
  private val hashChain: scala.Array[Int]^ = new scala.Array[Int](if length > 0 then length else 1)

  private def hashAt(pos: Int): Int =
    val value = (data(pos) & 0xff) | ((data(pos + 1) & 0xff) << 8) |
      ((data(pos + 2) & 0xff) << 16) | ((data(pos + 3) & 0xff) << 24)

    (value*0x9e3779b1) >>> (32 - HashBits)

  private update def finderInsert(pos: Int): Unit =
    if pos + 4 <= length then
      val h = hashAt(pos)
      hashChain(pos) = hashHead(h)
      hashHead(h) = pos

  // The longest match at `pos`, as (length, distanceValue) where the LZMA distance is one less
  // than the byte offset. Returns length 0 if nothing usable is found.
  private def finderFind(pos: Int, lenLimit: Int): Long =
    if pos + 4 > length || lenLimit < MatchLenMin then 0L else
      var bestLen = 0
      var bestDist = 0
      var candidate = hashHead(hashAt(pos))
      var remaining = depth

      while candidate >= 0 && (pos - candidate) <= effectiveDict && remaining > 0 do
        var len = 0
        while len < lenLimit && data(candidate + len) == data(pos + len) do len += 1

        if len > bestLen then
          bestLen = len
          bestDist = pos - candidate - 1
          if len >= lenLimit then remaining = 1

        candidate = hashChain(candidate)
        remaining -= 1

      (bestLen.toLong << 32) | (bestDist.toLong & 0xffffffffL)

  // The full candidate set at `pos`: for each length reachable (strictly increasing), the nearest
  // distance achieving it, so the caller can weigh the length/distance trade-off by price.
  // Results fill `candidateLen`/`candidateDist`; the return value is the count. Read-only with
  // respect to the finder (does not insert `pos`).
  private update def finderFindAll
    ( pos: Int, lenLimit: Int, outLen: scala.Array[Int]^{this}, outDist: scala.Array[Int]^{this} )
  :   Int =

    if pos + 4 > length || lenLimit < MatchLenMin then 0 else
      var count = 0
      var maxLen = MatchLenMin - 1
      var candidate = hashHead(hashAt(pos))
      var remaining = depth

      while candidate >= 0 && (pos - candidate) <= effectiveDict && remaining > 0 do
        var len = 0
        while len < lenLimit && data(candidate + len) == data(pos + len) do len += 1

        if len > maxLen then
          outLen(count) = len
          outDist(count) = pos - candidate - 1
          count += 1
          maxLen = len
          if len >= lenLimit then remaining = 1

        candidate = hashChain(candidate)
        remaining -= 1

      count

  // The hash heads start empty (no candidate positions).
  private update def finderReset(): Unit =
    var i = 0
    while i < (1 << HashBits) do { hashHead(i) = -1; i += 1 }

  // --- The LZMA probability model (the former `LzmaCoder`, `State` and the two
  // `LengthEncoder`s), flat and row-major exactly as in `Lzma2Decompressor` above.
  private val posMask: Int = (1 << options.pb) - 1
  private val literalPosMask: Int = (1 << options.lp) - 1
  private val literalContextBits: Int = options.lc
  private val niceLen: Int = options.niceLen
  private val normal: Boolean = options.mode == Lzma2Options.ModeNormal

  private var lzmaState: Int = 0
  private val reps: scala.Array[Int]^ = new scala.Array[Int](Reps)

  private val isMatch: scala.Array[Short]^ = new scala.Array[Short](States*PosStatesMax)
  private val isRep: scala.Array[Short]^ = new scala.Array[Short](States)
  private val isRep0: scala.Array[Short]^ = new scala.Array[Short](States)
  private val isRep1: scala.Array[Short]^ = new scala.Array[Short](States)
  private val isRep2: scala.Array[Short]^ = new scala.Array[Short](States)
  private val isRep0Long: scala.Array[Short]^ = new scala.Array[Short](States*PosStatesMax)
  private val distSlotProbs: scala.Array[Short]^ = new scala.Array[Short](DistStates*DistSlots)
  private val distSpecial: scala.Array[Short]^ = new scala.Array[Short](DistSpecialTotal)
  private val distAlign: scala.Array[Short]^ = new scala.Array[Short](AlignSize)

  private val literalProbs: scala.Array[Short]^ =
    new scala.Array[Short](0x300 << (options.lc + options.lp))

  private val matchLenChoice: scala.Array[Short]^ = new scala.Array[Short](2)
  private val matchLenLow: scala.Array[Short]^ = new scala.Array[Short](PosStatesMax*LenLowSymbols)
  private val matchLenMid: scala.Array[Short]^ = new scala.Array[Short](PosStatesMax*LenMidSymbols)
  private val matchLenHigh: scala.Array[Short]^ = new scala.Array[Short](LenHighSymbols)
  private val repLenChoice: scala.Array[Short]^ = new scala.Array[Short](2)
  private val repLenLow: scala.Array[Short]^ = new scala.Array[Short](PosStatesMax*LenLowSymbols)
  private val repLenMid: scala.Array[Short]^ = new scala.Array[Short](PosStatesMax*LenMidSymbols)
  private val repLenHigh: scala.Array[Short]^ = new scala.Array[Short](LenHighSymbols)

  private update def resetProbs(probs: scala.Array[Short]^{this}): Unit =
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
    resetProbs(distSlotProbs)
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

  // The length model (the former `LengthEncoder`): a choice bit chooses the low/mid/high range,
  // then a bit-tree codes the symbol within it. Lengths are offset by `MatchLenMin`.
  private update def encodeLength
    ( choice: scala.Array[Short]^{this},
      low: scala.Array[Short]^{this},
      mid: scala.Array[Short]^{this},
      high: scala.Array[Short]^{this},
      len: Int,
      posState: Int )
  :   Unit =

    val value = len - MatchLenMin

    if value < LenLowSymbols then
      rcEncodeBit(choice, 0, 0)
      rcEncodeBitTree(low, posState*LenLowSymbols, LenLowSymbols, value)
    else
      rcEncodeBit(choice, 0, 1)
      val mid0 = value - LenLowSymbols

      if mid0 < LenMidSymbols then
        rcEncodeBit(choice, 1, 0)
        rcEncodeBitTree(mid, posState*LenMidSymbols, LenMidSymbols, mid0)
      else
        rcEncodeBit(choice, 1, 1)
        rcEncodeBitTree(high, 0, LenHighSymbols, mid0 - LenMidSymbols)

  // --- The LZMA symbol encoder (the former `LzmaEncoder`). It walks the input once, at each step
  // choosing between a literal, an ordinary match, and — in normal mode — a repeat-match reusing
  // one of the four recent distances. Two strategies: `fast` (presets 0..3) greedily takes the
  // longest match, subject to a distance-dependent minimum length; `normal` (presets 4..9)
  // additionally prefers repeat-matches when competitive and applies one-step lazy evaluation.
  private var readPos = 0

  // Reusable candidate buffers for the price-based normal parser.
  private val candidateLen: scala.Array[Int]^ = new scala.Array[Int](MatchLenMax + 1)
  private val candidateDist: scala.Array[Int]^ = new scala.Array[Int](MatchLenMax + 1)

  private def pos: Int = readPos
  private def hasMore: Boolean = readPos < length

  private def byteAt(dist: Int): Int = data(readPos - dist - 1) & 0xff

  private def distSlot(dist: Int): Int =
    if dist < DistModelStart then dist
    else
      val n = 31 - java.lang.Integer.numberOfLeadingZeros(dist)
      (n << 1) | ((dist >>> (n - 1)) & 1)

  private def minLenFor(dist: Int): Int =
    if dist >= 32768 then 4 else if dist >= 512 then 3 else 2

  // The match length obtainable at repeat distance `repDist` from `at`, capped at `lenLimit`.
  private def repMatchLen(at: Int, repDist: Int, lenLimit: Int): Int =
    val back = at - repDist - 1

    if back < 0 then 0 else
      var l = 0
      while l < lenLimit && data(back + l) == data(at + l) do l += 1
      l

  // The best of the four repeat distances at `at`: the packed (length << 32 | index).
  private def bestRep(at: Int, lenLimit: Int): Long =
    var bestLen = 0
    var bestIndex = 0
    var i = 0

    while i < Reps do
      val l = repMatchLen(at, reps(i), lenLimit)
      if l > bestLen then { bestLen = l; bestIndex = i }
      i += 1

    (bestLen.toLong << 32) | bestIndex.toLong

  private update def encodeSymbol(): Int =
    val posState = readPos & posMask
    val lenLimit = if length - readPos < MatchLenMax then length - readPos else MatchLenMax

    if normal then
      // Gather the candidate set (read-only) before inserting this position, so it is never a
      // candidate for itself.
      val count = finderFindAll(readPos, lenLimit, candidateLen, candidateDist)
      finderInsert(readPos)
      encodeNormal(posState, lenLimit, count)
    else
      val found = finderFind(readPos, lenLimit)
      finderInsert(readPos)
      encodeFast(posState, (found >>> 32).toInt, (found & 0xffffffffL).toInt)

  private update def emitMatch(dist: Int, len: Int, posState: Int): Int =
    rcEncodeBit(isMatch, lzmaState*PosStatesMax + posState, 1)
    rcEncodeBit(isRep, lzmaState, 0)
    encodeMatch(dist, len, posState)
    advance(len)

  private update def emitRep(index: Int, len: Int, posState: Int): Int =
    rcEncodeBit(isMatch, lzmaState*PosStatesMax + posState, 1)
    rcEncodeBit(isRep, lzmaState, 1)
    encodeRepMatch(index, len, posState)
    advance(len)

  private update def emitLiteral(posState: Int): Int =
    rcEncodeBit(isMatch, lzmaState*PosStatesMax + posState, 0)
    encodeLiteral()
    readPos += 1
    1

  private update def advance(len: Int): Int =
    var k = 1
    while k < len do { finderInsert(readPos + k); k += 1 }
    readPos += len
    len

  private update def encodeFast(posState: Int, matchLen: Int, matchDist: Int): Int =
    if matchLen >= MatchLenMin && matchLen >= minLenFor(matchDist) then
      emitMatch(matchDist, matchLen, posState)
    else
      emitLiteral(posState)

  // Price-based per-step parsing: cost every candidate (literal, each repeat-match, and each
  // fresh-match length/distance trade-off from the finder) in fixed-point bits using the current
  // probability model, and take the option with the lowest cost-per-byte, applying one-step lazy
  // evaluation to fresh matches. Reps are cheap, so they win whenever competitive.
  private update def encodeNormal(posState: Int, lenLimit: Int, count: Int): Int =
    val rep = bestRep(readPos, lenLimit)
    val repLen = (rep >>> 32).toInt
    val repIndex = (rep & 0xffffffffL).toInt

    // Start from the literal option; matches must beat it on cost-per-byte.
    var bestKind = 0 // 0 literal, 1 match, 2 rep
    var bestLen = 1
    var bestArg = 0
    var bestCost = literalPrice(readPos)

    if repLen >= MatchLenMin then
      val cost = repPrice(repIndex, repLen, posState)

      if cheaperPerByte(cost, repLen, bestCost, bestLen) then
        bestKind = 2; bestLen = repLen; bestArg = repIndex; bestCost = cost

    var k = 0

    while k < count do
      val len = candidateLen(k)
      val dist = candidateDist(k)

      if len >= MatchLenMin && len >= minLenFor(dist) then
        val cost = matchPrice(dist, len, posState)

        if cheaperPerByte(cost, len, bestCost, bestLen) then
          bestKind = 1; bestLen = len; bestArg = dist; bestCost = cost

      k += 1

    if bestKind == 0 then emitLiteral(posState)
    else if bestKind == 1 && bestLen < niceLen && lazyDefer(bestLen) then emitLiteral(posState)
    else if bestKind == 2 then emitRep(bestArg, bestLen, posState)
    else emitMatch(bestArg, bestLen, posState)

  // Is (costA over lenA) strictly cheaper per byte than (costB over lenB)? Cross-multiplied to
  // keep the comparison exact in integers.
  private def cheaperPerByte(costA: Int, lenA: Int, costB: Int, lenB: Int): Boolean =
    costA.toLong*lenB < costB.toLong*lenA

  // One-step lazy evaluation: if the next position offers a strictly longer fresh match, defer by
  // emitting a literal now. `readPos` has already been inserted, so the lookahead sees it.
  private def lazyDefer(currentLen: Int): Boolean =
    val next = readPos + 1

    if next + 4 > length then false else
      val nextLimit = if length - next < MatchLenMax then length - next else MatchLenMax
      val nextFound = finderFind(next, nextLimit)
      (nextFound >>> 32).toInt > currentLen

  // Pricing only reads the model, so these take read-only views of the probability arrays.
  private def priceBit(probs: scala.Array[Short]^{caps.any.rd}, index: Int, bit: Int): Int =
    RangeCoder.bitPrice(probs(index).toInt, bit)

  private def lengthPrice
    ( choice: scala.Array[Short]^{caps.any.rd},
      low: scala.Array[Short]^{caps.any.rd},
      mid: scala.Array[Short]^{caps.any.rd},
      high: scala.Array[Short]^{caps.any.rd},
      len: Int,
      posState: Int )
  :   Int =

    val l = len - MatchLenMin

    if l < LenLowSymbols then
      priceBit(choice, 0, 0) +
        RangeCoder.bitTreePrice(low, posState*LenLowSymbols, LenLowSymbols, l)
    else if l < LenLowSymbols + LenMidSymbols then
      priceBit(choice, 0, 1) + priceBit(choice, 1, 0) +
        RangeCoder.bitTreePrice(mid, posState*LenMidSymbols, LenMidSymbols, l - LenLowSymbols)
    else
      priceBit(choice, 0, 1) + priceBit(choice, 1, 1) +
        RangeCoder.bitTreePrice(high, 0, LenHighSymbols, l - LenLowSymbols - LenMidSymbols)

  private def distancePrice(dist: Int, lenState: Int): Int =
    val slot = distSlot(dist)
    var price = RangeCoder.bitTreePrice(distSlotProbs, lenState*DistSlots, DistSlots, slot)

    if slot >= DistModelStart then
      val footerBits = (slot >> 1) - 1
      val base = (2 | (slot & 1)) << footerBits

      if slot < DistModelEnd then
        val index = slot - DistModelStart
        price += RangeCoder.bitTreeReversePrice(distSpecial, distSpecialOffsets(index),
            distSpecialSize(index), dist - base)
      else
        price += RangeCoder.directBitsPrice(footerBits - AlignBits)
        price += RangeCoder.bitTreeReversePrice(distAlign, 0, AlignSize, (dist - base) & AlignMask)

    price

  private def matchPrice(dist: Int, len: Int, posState: Int): Int =
    priceBit(isMatch, lzmaState*PosStatesMax + posState, 1) + priceBit(isRep, lzmaState, 0) +
      lengthPrice(matchLenChoice, matchLenLow, matchLenMid, matchLenHigh, len, posState) +
      distancePrice(dist, distState(len))

  private def repPrice(index: Int, len: Int, posState: Int): Int =
    var price =
      priceBit(isMatch, lzmaState*PosStatesMax + posState, 1) + priceBit(isRep, lzmaState, 1)

    if index == 0 then
      price += priceBit(isRep0, lzmaState, 0) +
        priceBit(isRep0Long, lzmaState*PosStatesMax + posState, 1)
    else
      price += priceBit(isRep0, lzmaState, 1)

      if index == 1 then price += priceBit(isRep1, lzmaState, 0)
      else
        price += priceBit(isRep1, lzmaState, 1) +
          priceBit(isRep2, lzmaState, if index == 2 then 0 else 1)

    price + lengthPrice(repLenChoice, repLenLow, repLenMid, repLenHigh, len, posState)

  private def literalPrice(at: Int): Int =
    val prevByte = if at > 0 then data(at - 1) & 0xff else 0
    val base = literalIndex(prevByte, at)
    val target = data(at) & 0xff
    var price = 0
    var context = 1

    if stateIsLiteral then
      var i = 7

      while i >= 0 do
        val bit = (target >>> i) & 1
        price += priceBit(literalProbs, base + context, bit)
        context = (context << 1) | bit
        i -= 1
    else
      var matchByte = (data(at - reps(0) - 1) & 0xff) << 1
      var stillMatched = true
      var i = 7

      while i >= 0 do
        val bit = (target >>> i) & 1

        if stillMatched then
          val matchBit = (matchByte >>> 8) & 1
          matchByte <<= 1
          price += priceBit(literalProbs, base + ((1 + matchBit) << 8) + context, bit)
          if matchBit != bit then stillMatched = false
        else
          price += priceBit(literalProbs, base + context, bit)

        context = (context << 1) | bit
        i -= 1

    price

  private update def encodeLiteral(): Unit =
    val prevByte = if readPos > 0 then data(readPos - 1) & 0xff else 0
    val base = literalIndex(prevByte, readPos)
    val target = data(readPos) & 0xff
    var context = 1

    if stateIsLiteral then
      var i = 7

      while i >= 0 do
        val bit = (target >>> i) & 1
        rcEncodeBit(literalProbs, base + context, bit)
        context = (context << 1) | bit
        i -= 1
    else
      var matchByte = byteAt(reps(0)) << 1
      var stillMatched = true
      var i = 7

      while i >= 0 do
        val bit = (target >>> i) & 1

        if stillMatched then
          val matchBit = (matchByte >>> 8) & 1
          matchByte <<= 1
          rcEncodeBit(literalProbs, base + ((1 + matchBit) << 8) + context, bit)
          if matchBit != bit then stillMatched = false
        else
          rcEncodeBit(literalProbs, base + context, bit)

        context = (context << 1) | bit
        i -= 1

    updateLiteralState()

  private update def encodeMatch(dist: Int, len: Int, posState: Int): Unit =
    updateMatchState()
    reps(3) = reps(2)
    reps(2) = reps(1)
    reps(1) = reps(0)
    reps(0) = dist

    encodeLength(matchLenChoice, matchLenLow, matchLenMid, matchLenHigh, len, posState)

    val slot = distSlot(dist)
    rcEncodeBitTree(distSlotProbs, distState(len)*DistSlots, DistSlots, slot)

    if slot >= DistModelStart then
      val footerBits = (slot >> 1) - 1
      val base = (2 | (slot & 1)) << footerBits

      if slot < DistModelEnd then
        val index = slot - DistModelStart
        rcEncodeBitTreeReverse(distSpecial, distSpecialOffsets(index), distSpecialSize(index),
            dist - base)
      else
        rcEncodeDirectBits((dist - base) >>> AlignBits, footerBits - AlignBits)
        rcEncodeBitTreeReverse(distAlign, 0, AlignSize, (dist - base) & AlignMask)

  // Encode a repeat-match reusing recent distance `index` (0..3), length `len` (at least 2, never
  // a short-rep). The rep-index bits and the reps rotation mirror the decoder's `decodeRepMatch`
  // exactly, and all sub-bits are coded against the pre-update `lzmaState`.
  private update def encodeRepMatch(index: Int, len: Int, posState: Int): Unit =
    if index == 0 then
      rcEncodeBit(isRep0, lzmaState, 0)
      rcEncodeBit(isRep0Long, lzmaState*PosStatesMax + posState, 1)
    else
      rcEncodeBit(isRep0, lzmaState, 1)

      if index == 1 then rcEncodeBit(isRep1, lzmaState, 0)
      else
        rcEncodeBit(isRep1, lzmaState, 1)
        rcEncodeBit(isRep2, lzmaState, if index == 2 then 0 else 1)

      val dist = reps(index)
      var j = index
      while j > 0 do { reps(j) = reps(j - 1); j -= 1 }
      reps(0) = dist

    updateRepState()
    encodeLength(repLenChoice, repLenLow, repLenMid, repLenHigh, len, posState)

  // --- The chunk framer.
  finderReset()
  resetModel()
  rcReset()

  update def compress(): scala.Array[Byte] =
    val out = scm.ArrayBuffer[Byte]()

    if data.length == 0 then out += 0x00.toByte else
      var firstChunk = true

      while hasMore do
        rcReset()
        val startPos = pos
        val uncompressedCeiling = Lzma2.UncompressedSizeMax - Lzma.MatchLenMax
        val compressedCeiling = Lzma2.CompressedSizeMax - 64

        while hasMore &&
          (pos - startPos) < uncompressedCeiling &&
          rcPendingSize < compressedCeiling
        do encodeSymbol()

        val uncompressedSize = pos - startPos
        val compressedSize = rcFinish()
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

        appendPayload(rcBuffer, compressedSize, out)
        firstChunk = false

      out += 0x00.toByte

    out.toArray

  // Append the freshly-encoded chunk payload (passed in as `this`-scoped so the field array flows
  // through the exclusive-parameter shape) to the output.
  private update def appendPayload
    ( payload: scala.Array[Byte]^{this}, count: Int, out: scm.ArrayBuffer[Byte] )
  :   Unit =

    var i = 0
    while i < count do { out += payload(i); i += 1 }

sealed trait Lzma2 extends Compressor
