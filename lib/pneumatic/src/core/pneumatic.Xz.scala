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

// XZ (LZMA2 inside the `.xz` container), the high-ratio codec, as a pure-Scala port of the
// public-domain XZ Utils algorithm. It shares the streaming-engine shape of Brotli and LZW: an
// engine buffers into a `pending` array which a thin `Duct` stage drains. The default preset is 6
// with a CRC-64 check (matching the `xz` command-line tool); `Xz.compress(stream, preset)` and
// `Xz.compressor(preset)` select any preset 0..9.
//
// `Lzma2` (in `pneumatic.Lzma2.scala`) is the container-free counterpart, exposing the same LZMA2
// codec without the `.xz` framing — analogous to raw `Deflate` beside `Gzip`/`Zlib`.

// The output-draining half of an engine: mirrors `BrotliEngine`, staging bytes in `pending` and
// handing them out in whatever space each `deliver` offers.
private[pneumatic] trait XzEngine extends caps.Mutable:
  protected val pending: scm.ArrayBuffer[Byte] = scm.ArrayBuffer()
  private var delivered: Int = 0

  update def accept(bytes: scala.Array[Byte]^{caps.any.rd}, offset: Int, length: Int): Unit
  update def finish(): Unit

  update def deliver(target: scala.Array[Byte]^, offset: Int, space: Int): Int =
    var produced = 0

    while delivered < pending.length && produced < space do
      target(offset + produced) = pending(delivered)
      delivered += 1
      produced += 1

    if delivered == pending.length then
      pending.clear()
      delivered = 0

    produced

  update def gather(): Data =
    val result = Buffer[Byte](pending.length - delivered)
    var i = 0

    while delivered < pending.length do
      result(i) = pending(delivered)
      i += 1
      delivered += 1

    pending.clear()
    delivered = 0
    Buffer.freeze(result)

// Accumulates the whole input, then applies a one-shot byte transform (encode or container-decode).
// The transform is a method (not a stored function value) so the engine carries no capability
// capture, per this module's capture-checked discipline.
private[pneumatic] abstract class BufferedEngine extends XzEngine:
  private val input: scm.ArrayBuffer[Byte] = scm.ArrayBuffer()
  private var finished = false

  protected def transform(bytes: scala.Array[Byte]): scala.Array[Byte]

  update def accept(bytes: scala.Array[Byte]^{caps.any.rd}, offset: Int, length: Int): Unit =
    var i = 0
    while i < length do { input += bytes(offset + i); i += 1 }

  update def finish(): Unit =
    if !finished then
      finished = true
      val output = transform(input.toArray)
      var i = 0
      while i < output.length do { pending += output(i); i += 1 }

// The `.xz` container encoder, streaming with bounded memory: it buffers at most one dictionary's
// worth of input, emits that as a complete block as soon as it fills, and closes with the index and
// footer on finish. Peak working memory is therefore ~one segment regardless of total input size.
// (Small inputs still form a single block, byte-identical to the whole-value encoder.) Each block
// is self-contained, which the multi-block decoder handles transparently.
private[pneumatic] final class XzCompressorEngine(preset: Int, checkType: Int) extends XzEngine:
  private val options = Lzma2Options.preset(preset)
  private val segmentSize = options.dictSize
  private val segment: scm.ArrayBuffer[Byte] = scm.ArrayBuffer()
  private val records: scm.ArrayBuffer[(Long, Long)] = scm.ArrayBuffer()
  private var headerEmitted = false
  private var finished = false

  private def emit(bytes: scala.Array[Byte]): Unit =
    var i = 0
    while i < bytes.length do { pending += bytes(i); i += 1 }

  private update def ensureHeader(): Unit =
    if !headerEmitted then
      emit(XzContainer.streamHeader(checkType))
      headerEmitted = true

  private def emitSegment(): Unit =
    if segment.nonEmpty then
      val data = segment.toArray
      segment.clear()
      val (blockBytes, unpadded) = XzContainer.block(data, checkType, options)
      emit(blockBytes)
      records += ((unpadded, data.length.toLong))

  update def accept(bytes: scala.Array[Byte]^{caps.any.rd}, offset: Int, length: Int): Unit =
    ensureHeader()
    var i = 0

    while i < length do
      segment += bytes(offset + i)
      i += 1
      if segment.length >= segmentSize then emitSegment()

  update def finish(): Unit =
    if !finished then
      finished = true
      ensureHeader()
      emitSegment()
      emit(XzContainer.indexAndFooter(records, checkType))

// The `.xz` container decoder.
private[pneumatic] final class XzDecompressorEngine extends BufferedEngine:
  protected def transform(bytes: scala.Array[Byte]): scala.Array[Byte] =
    val out = scm.ArrayBuffer[Byte]()
    XzContainer.decode(bytes, out)
    out.toArray

// The container-free raw-LZMA2 encoder.
private[pneumatic] final class Lzma2CompressorEngine(preset: Int) extends BufferedEngine:
  protected def transform(bytes: scala.Array[Byte]): scala.Array[Byte] =
    Lzma2Compressor(bytes, Lzma2Options.preset(preset)).compress()

// Streams LZMA2 decompression: feeds input into the chunk decoder as it arrives and drains the
// decoder's freshly-produced bytes into `pending`, keeping the buffered compressed data bounded.
private[pneumatic] final class Lzma2DecompressorEngine(dictSize: Int) extends XzEngine:
  private val decompressor: Lzma2Decompressor^ = Lzma2Decompressor(dictSize)

  update def accept(bytes: scala.Array[Byte]^{caps.any.rd}, offset: Int, length: Int): Unit =
    decompressor.accept(bytes, offset, length)
    drain()

  update def finish(): Unit =
    decompressor.finish()
    drain()

  private def drain(): Unit =
    val output = decompressor.output
    var i = 0
    while i < output.length do { pending += output(i); i += 1 }
    output.clear()

// The `Duct` stage wrapping an engine, identical in shape to `BrotliStage`. The engine is
// created by the by-name argument inside the stage, so the stage owns it exclusively.
private[pneumatic] class XzStage(engine0: => XzEngine^) extends Duct[Data, Data]:
  type Transport = Credit
  type Upstream = Credit

  private val engine: XzEngine^ = engine0
  private var finishing = false

  def regulation: Credit is Regulation = summon[Credit is Regulation]
  def translate(demand: Credit): Credit = demand

  update def step
    ( source: input.Storage,
      sourceOffset: Int,
      sourceLength: Int,
      target: output.Storage,
      targetOffset: Int,
      targetSpace: Int )
  :   Duct.Progress =

    engine.accept(source.asInstanceOf[scala.Array[Byte]], sourceOffset, sourceLength)

    Duct.Progress
      ( sourceLength,
        engine.deliver(target.asInstanceOf[scala.Array[Byte]], targetOffset, targetSpace) )

  override update def flush(target: output.Storage, targetOffset: Int, targetSpace: Int): Int =
    if !finishing then
      engine.finish()
      finishing = true

    engine.deliver(target.asInstanceOf[scala.Array[Byte]], targetOffset, targetSpace)

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

    override def compress(stream: Progression[Data]): Progression[Data] =
      drive(encoderEngine(DefaultPreset), stream)

    override def decompress(stream: Progression[Data]): Progression[Data] =
      drive(decoderEngine(), stream)

  // Compress with an explicit preset level (0..9); presets 0..3 favour speed, 4..9 favour ratio.
  def compressor(preset: Int)(using Buffering): (Duct[Data, Data] {
    type Transport = Credit
    type Upstream = Credit })^ =

    XzStage(encoderEngine(preset))

  def compress(stream: Progression[Data], preset: Int): Progression[Data] =
    drive(encoderEngine(preset), stream)

  def decompress(stream: Progression[Data]): Progression[Data] = drive(decoderEngine(), stream)

  // Drives an engine over a lazy stream chunk by chunk, then collects its finished tail. The
  // engine argument is by-name, so the (exclusive, mutable) engine is minted inside the deferred
  // block and never escapes it.
  private[pneumatic] def drive(engine0: => XzEngine^, stream: Progression[Data])
  :   Progression[Data] =

    def recur(engine: XzEngine^, stream: Progression[Data]): Progression[Data] = stream match
      case head #:: tail =>
        engine.accept(head.mutable(using Unsafe), 0, head.length)
        recur(engine, tail)

      case _ =>
        engine.finish()
        val data = engine.gather()
        if data.length > 0 then Progression(data) else Progression.empty

    Progression.defer(recur(engine0, stream))

sealed trait Xz extends Compressor
