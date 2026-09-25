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


import scala.collection.mutable as scm

import anticipation.*
import contingency.*
import denominative.*
import prepositional.*
import rudiments.*
import vacuous.*
import zephyrine.*

// XZ (LZMA2 inside the `.xz` container), the high-ratio codec, as a pure-Scala port of the
// public-domain XZ Utils algorithm. It shares the streaming-engine shape of Brotli and LZW: an
// engine buffers into a `pending` array which a thin `Duct` stage drains. The default preset is 6
// with a CRC-64 check (matching the `xz` command-line tool); `Xz.compress(stream, preset)` and
// `Xz.compressor(preset)` select any preset 0..9.
//
// `Lzma2` (in `pneumatic.Lzma2.scala`) is the container-free counterpart, exposing the same LZMA2
// codec without the `.xz` framing — analogous to raw `Deflate` beside `Gzip`/`Zlib`.

// The output-draining half of an engine: mirrors `BrotliEngine`, staging bytes flat in `pending`
// (a `ByteSink`) and handing them out in whatever space each `deliver` offers.
private[pneumatic] trait XzEngine extends caps.Mutable:
  protected val pending: ByteSink^ = ByteSink()

  update def accept(bytes: Array[Byte]^{caps.any.rd}, offset: Int, length: Int): Unit
  update def finish(): Unit

  update def deliver(target: scala.Array[Byte]^, offset: Int, space: Int): Int =
    pending.drainInto(target, offset, space)

  update def gather(): Data = Array.unsafeFrozen(pending.take())

// Accumulates the whole input, then applies a one-shot byte transform (encode or container-decode).
// The transform is a method (not a stored function value) so the engine carries no capability
// capture, per this module's capture-checked discipline.
private[pneumatic] abstract class BufferedEngine extends XzEngine:
  private val input: ByteSink^ = ByteSink()
  private var finished = false

  protected def transform(bytes: scala.Array[Byte]): scala.Array[Byte]

  update def accept(bytes: Array[Byte]^{caps.any.rd}, offset: Int, length: Int): Unit =
    input.append(Array.unsafeJvm(bytes), offset, length)

  update def finish(): Unit =
    if !finished then
      finished = true
      // `take()` yields a fresh exact-size array, which nothing else holds: the transform owns it.
      val output = transform(input.take())
      pending.append(output, 0, output.length)

// The `.xz` container encoder, streaming with bounded memory: it buffers at most one dictionary's
// worth of input, emits that as a complete block as soon as it fills, and closes with the index and
// footer on finish. Peak working memory is therefore ~one segment regardless of total input size.
// (Small inputs still form a single block, byte-identical to the whole-value encoder.) Each block
// is self-contained, which the multi-block decoder handles transparently.
private[pneumatic] final class XzCompressorEngine(preset: Int, checkType: Int) extends XzEngine:
  private val options = Lzma2Options.preset(preset)
  private val segmentSize = options.dictSize
  private val segment: ByteSink^ =
    ByteSink(if segmentSize < (1 << 20) then segmentSize else 1 << 20)

  private val records: scm.ArrayBuffer[(Long, Long)] = scm.ArrayBuffer()
  private var headerEmitted = false
  private var finished = false

  private update def emit(bytes: scala.Array[Byte]): Unit = pending.append(bytes, 0, bytes.length)

  private update def ensureHeader(): Unit =
    if !headerEmitted then
      emit(XzContainer.streamHeader(checkType))
      headerEmitted = true

  // `toArray` then `clear()` rather than `take()`, so the segment keeps its capacity for the next
  // block.
  private update def emitSegment(): Unit =
    if segment.length > 0 then
      val data = segment.toArray
      segment.clear()
      val (blockBytes, unpadded) = XzContainer.block(data, checkType, options)
      emit(blockBytes)
      records += ((unpadded, data.length.toLong))

  update def accept(bytes: Array[Byte]^{caps.any.rd}, offset: Int, length: Int): Unit =
    ensureHeader()
    val raw = Array.unsafeJvm(bytes)
    var position = offset
    var remaining = length

    // Fill the segment in bulk, cutting a block at each dictionary-sized boundary.
    while remaining > 0 do
      val room = segmentSize - segment.length
      val step = if remaining < room then remaining else room
      segment.append(raw, position, step)
      position += step
      remaining -= step
      if segment.length >= segmentSize then emitSegment()

  update def finish(): Unit =
    if !finished then
      finished = true
      ensureHeader()
      emitSegment()
      emit(XzContainer.indexAndFooter(records, checkType))

private enum XzPhase:
  case StreamHeader, BlockHeader, BlockData, BlockCheck, Index

// The `.xz` container decoder, streaming: framing is parsed from the compressed bytes as they
// arrive, and each block's LZMA2 payload is fed to an `Lzma2Decompressor` incrementally, whose
// output is checksummed and moved to `pending` as it is produced. Working memory is therefore a
// dictionary plus whatever one `accept` decodes, not the whole stream — and the decoded bytes are
// copied once, not staged whole and copied again. Offsets are stream-absolute (`Long`), with
// `base` the stream offset of `buffer(0)`; parsed bytes are compacted away on each `accept`.
private[pneumatic] final class XzDecompressorEngine extends XzEngine:
  private var buffer: scala.Array[Byte]^ = new scala.Array[Byte](1 << 16)
  private var end = 0
  private var base = 0L
  private var cursor = 0L         // the next byte to parse
  private var fed = 0L            // the block decoder has been fed up to here
  private var blockDataStart = 0L
  private var phase = XzPhase.StreamHeader
  private var checkType = 0
  private var checkSize = 0
  private var finished = false

  // Replaced per block; the placeholders decode nothing.
  private var decompressor: Lzma2Decompressor^ = Lzma2Decompressor(0)
  private var checker: XzChecker^ = NoChecker()

  private def available: Int = end - (cursor - base).toInt
  private def at(offset: Long): Int = buffer((offset - base).toInt) & 0xff

  update def accept(bytes: Array[Byte]^{caps.any.rd}, offset: Int, length: Int): Unit =
    // Everything before `cursor` has been parsed or consumed by the block decoder.
    val drop = (cursor - base).toInt

    if drop > 0 then
      System.arraycopy(buffer, drop, buffer, 0, end - drop)
      end -= drop
      base += drop

    if end + length > buffer.length then
      var size = buffer.length*2
      while size < end + length do size *= 2
      val grown: scala.Array[Byte]^ = new scala.Array[Byte](size)
      System.arraycopy(buffer, 0, grown, 0, end)
      buffer = grown

    System.arraycopy(bytes.asInstanceOf[scala.Array[Byte]], offset, buffer, end, length)
    end += length
    process()

  private update def process(): Unit =
    var progressing = true

    while progressing do
      progressing = false

      phase match
        case XzPhase.StreamHeader =>
          if available >= 12 then
            checkType = XzContainer.streamCheckType(buffer, (cursor - base).toInt)
            checkSize = XzCheck.size(checkType)
            cursor += 12
            phase = XzPhase.BlockHeader
            progressing = true

        case XzPhase.BlockHeader =>
          if available >= 1 then
            val first = at(cursor)

            if first == XzContainer.IndexIndicator then phase = XzPhase.Index
            else
              val headerSize = (first + 1)*4

              if available >= headerSize then
                val dictSize = XzContainer.blockDictSize(buffer, (cursor - base).toInt)
                decompressor = Lzma2Decompressor(dictSize)
                checker = XzCheck.checker(checkType)
                cursor += headerSize
                blockDataStart = cursor
                fed = cursor
                phase = XzPhase.BlockData
                progressing = true

        case XzPhase.BlockData =>
          val fresh = (base + end - fed).toInt

          if fresh > 0 then
            // The decoder retains what it does not consume, so it is fed each byte once; the
            // buffer is read-only to it, hence the frozen view.
            decompressor.accept(Array.unsafeFrozen(buffer), (fed - base).toInt, fresh)
            fed += fresh
            val produced = decompressor.produced

            if produced > 0 then
              if checkSize > 0 then checker.absorb(decompressor.output, 0, produced)
              pending.append(decompressor.output, 0, produced)
              decompressor.resetOutput()

          cursor = blockDataStart + decompressor.consumed

          if decompressor.ended then
            cursor += (-decompressor.consumed) & 3 // four-byte alignment padding
            phase = XzPhase.BlockCheck
            progressing = true

        case XzPhase.BlockCheck =>
          if available >= checkSize then
            if checkSize > 0 then
              val expected: scala.Array[Byte]^ = checker.bytes
              var c = 0

              while c < checkSize do
                if at(cursor + c) != (expected(c) & 0xff) then
                  throw IllegalStateException("the XZ data is corrupt: integrity check failed")

                c += 1

            cursor += checkSize
            phase = XzPhase.BlockHeader
            progressing = true

        // The index and stream footer that follow are not needed to reproduce the payload.
        case XzPhase.Index => ()

  update def finish(): Unit =
    if !finished then
      finished = true

      if phase == XzPhase.BlockData then
        decompressor.finish()
        val produced = decompressor.produced

        if produced > 0 then
          if checkSize > 0 then checker.absorb(decompressor.output, 0, produced)
          pending.append(decompressor.output, 0, produced)
          decompressor.resetOutput()

        if !decompressor.ended then
          throw IllegalStateException("the XZ data is corrupt: block did not terminate")

        cursor = blockDataStart + decompressor.consumed + ((-decompressor.consumed) & 3)
        phase = XzPhase.BlockCheck
        process()

      phase match
        case XzPhase.StreamHeader =>
          throw IllegalStateException("the XZ data is corrupt: truncated header")

        case XzPhase.BlockCheck | XzPhase.BlockData =>
          throw IllegalStateException("the XZ data is corrupt: truncated")

        case XzPhase.BlockHeader =>
          if available > 0 then throw IllegalStateException("the XZ data is corrupt: truncated")

        case XzPhase.Index => ()

// The container-free raw-LZMA2 encoder.
private[pneumatic] final class Lzma2CompressorEngine(preset: Int) extends BufferedEngine:
  protected def transform(bytes: scala.Array[Byte]): scala.Array[Byte] =
    Lzma2Compressor(bytes, Lzma2Options.preset(preset)).compress()

// Streams LZMA2 decompression: feeds input into the chunk decoder as it arrives and drains the
// decoder's freshly-produced bytes into `pending`, keeping the buffered compressed data bounded.
private[pneumatic] final class Lzma2DecompressorEngine(dictSize: Int) extends XzEngine:
  private val decompressor: Lzma2Decompressor^ = Lzma2Decompressor(dictSize)

  // Move the decoder's freshly-produced bytes into `pending`. Written out in each caller rather
  // than as an `update` helper: a call to a method updating `this` after `decompressor` has been
  // touched in the same body is what the separation checker rejects as a hidden access.
  update def accept(bytes: Array[Byte]^{caps.any.rd}, offset: Int, length: Int): Unit =
    decompressor.accept(bytes, offset, length)
    val count = decompressor.produced

    if count > 0 then
      pending.append(decompressor.output, 0, count)
      decompressor.resetOutput()

  update def finish(): Unit =
    decompressor.finish()
    val count = decompressor.produced

    if count > 0 then
      pending.append(decompressor.output, 0, count)
      decompressor.resetOutput()

// The `Duct` stage wrapping an engine, identical in shape to `BrotliStage`. The engine is
// created by the by-name argument inside the stage, so the stage owns it exclusively.
private[pneumatic] class XzStage(engine0: => XzEngine^) extends Duct[Data, Data]:
  type Transport = Credit
  type Upstream = Credit

  private val engine: XzEngine^ = engine0
  private var finishing = false

  def regulation: Credit is Regulation = summon[Credit is Regulation]
  def translate(demand: Credit): Credit = demand

  update def step(source: Region[Data])(range: Interval in source.type)
    ( target: Slate[Data] )(space: Interval in target.type)
  :   Duct.Progress =

    val sourceInterval: Interval = range
    val targetInterval: Interval = space
    val bytes = unsafely(source.unsafeRaw.asInstanceOf[scala.Array[Byte]])
    val out: scala.Array[Byte]^ =
      unsafely(target.unsafeRaw.asInstanceOf[scala.Array[Byte]]).asInstanceOf[scala.Array[Byte]^]

    engine.accept(bytes.asInstanceOf[Array[Byte]^{caps.any.rd}], sourceInterval.start.n0,
        sourceInterval.size)

    Duct.Progress
      ( sourceInterval.size,
        engine.deliver(out, targetInterval.start.n0, targetInterval.size) )

  override update def flush(target: Slate[Data])(space: Interval in target.type): Int =
    if !finishing then
      engine.finish()
      finishing = true

    val targetInterval: Interval = space
    val out: scala.Array[Byte]^ =
      unsafely(target.unsafeRaw.asInstanceOf[scala.Array[Byte]]).asInstanceOf[scala.Array[Byte]^]
    engine.deliver(out, targetInterval.start.n0, targetInterval.size)

object Xz:
  inline val DefaultPreset = 6

  private def encoderEngine(preset: Int): XzEngine^ = XzCompressorEngine(preset, XzCheck.Crc64Type)
  private def decoderEngine(): XzEngine^ = XzDecompressorEngine()

  given compression: Xz is Compression:
    def compressor()(using Buffering): (Duct[Data, Data] {
      type Transport = Credit
      type Upstream = Credit })^ =

      XzStage(encoderEngine(DefaultPreset))

    def decompressor()(using Buffering): (Duct[Data, Data] {
      type Transport = Credit
      type Upstream = Credit })^ =

      XzStage(decoderEngine())

    override def compress(stream: Chain[Data]): Chain[Data] =
      drive(encoderEngine(DefaultPreset), stream)

    override def decompress(stream: Chain[Data]): Chain[Data] =
      drive(decoderEngine(), stream)

  // Compress with an explicit preset level (0..9); presets 0..3 favour speed, 4..9 favour ratio.
  def compressor(preset: Int)(using Buffering): (Duct[Data, Data] {
    type Transport = Credit
    type Upstream = Credit })^ =

    XzStage(encoderEngine(preset))

  def compress(stream: Chain[Data], preset: Int): Chain[Data] =
    drive(encoderEngine(preset), stream)

  def decompress(stream: Chain[Data]): Chain[Data] = drive(decoderEngine(), stream)

  // Drives an engine over a lazy stream chunk by chunk, then collects its finished tail. The
  // engine argument is by-name, so the (exclusive, mutable) engine is minted inside the deferred
  // block and never escapes it.
  private[pneumatic] def drive(engine0: => XzEngine^, stream: Chain[Data])
  :   Chain[Data] =

    def recur(engine: XzEngine^, stream: Chain[Data]): Chain[Data] = stream match
      case head #:: tail =>
        engine.accept(head, 0, head.length)
        recur(engine, tail)

      case _ =>
        engine.finish()
        val data = engine.gather()
        if data.length > 0 then Chain(data) else Chain.empty

    Chain.defer(recur(engine0, stream))

sealed trait Xz extends Compressor
