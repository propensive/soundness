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

import anticipation.*
import contingency.*
import denominative.*
import prepositional.*
import rudiments.*
import turbulence.*
import vacuous.*
import zephyrine.*

// Brotli (RFC 7932), implemented natively in pure Scala: the decoder is a faithful port of Google's
// reference `org.brotli.dec` (see `BrotliDecoder`); the encoder is the fast, spec-compliant path in
// `BrotliEncoder`. As with LZW, the algorithm lives in plain engine classes that buffer output
// in a `pending` array, and the `Duct` stage is a thin wrapper. Both the encoder and decoder need
// the whole value before producing output (the decoder because backward references may reach across
// the entire window; the encoder because it chooses its framing from the total length), so `accept`
// accumulates input and `finish` produces the transformed bytes in one pass.
private[pneumatic] trait BrotliEngine extends caps.Mutable:
  // The engine's produced-but-undelivered output: a flat byte array drained by bulk
  // copies. This was a `scala.collection.mutable.ArrayBuffer[Byte]`, which is not
  // specialized — its backing store is an `Object[]` — so a 4 MB decode built a 32 MB
  // pointer array through millions of boxed appends, then unboxed every byte again on
  // the way out: an envelope that cost more than the Brotli decode inside it.
  private var pending: scala.Array[Byte] = new scala.Array[Byte](0)
  private var limit: Int = 0
  private var delivered: Int = 0

  // Installs the engine's finished output, taken whole from the codec.
  protected update def install(consume bytes: scala.Array[Byte]): Unit =
    pending = bytes
    limit = bytes.length
    delivered = 0

  update def accept(bytes: Array[Byte]^{caps.any.rd}, offset: Int, length: Int): Unit
  update def finish(): Unit

  update def deliver(target: scala.Array[Byte]^, offset: Int, space: Int): Int =
    val count = (limit - delivered).min(space)

    if count > 0 then
      System.arraycopy(pending, delivered, target, offset, count)
      delivered += count

      if delivered == limit then
        pending = new scala.Array[Byte](0)
        limit = 0
        delivered = 0

    count

  update def gather(): Data =
    val count = limit - delivered
    val result = new scala.Array[Byte](count)
    System.arraycopy(pending, delivered, result, 0, count)
    pending = new scala.Array[Byte](0)
    limit = 0
    delivered = 0
    Array.unsafeFrozen(result)

// Accumulates its input as a flat, doubling byte array: the counterpart of the
// output side above, replacing another boxing `ArrayBuffer` (and a per-byte
// generic `readUnchecked`, an unspecialized `ScalaRunTime.array_apply`) with two
// bulk copies per accepted window.
private[pneumatic] trait BrotliAccumulator extends caps.Mutable:
  private var input: scala.Array[Byte] = new scala.Array[Byte](8192)
  private var length0: Int = 0

  protected def accumulated: scala.Array[Byte] = input
  protected def accumulatedLength: Int = length0

  protected update def accumulate(bytes: Array[Byte]^{caps.any.rd}, offset: Int, length: Int)
  :   Unit =

    if length0 + length > input.length then
      var size = input.length*2
      while size < length0 + length do size *= 2
      val grown = new scala.Array[Byte](size)
      System.arraycopy(input, 0, grown, 0, length0)
      input = grown

    System.arraycopy(bytes.asInstanceOf[scala.Array[Byte]], offset, input, length0, length)
    length0 += length

// Accumulates the whole compressed stream, then decodes it in one pass (see `BrotliDecoder`).
private[pneumatic] class BrotliDecoderEngine extends BrotliEngine, BrotliAccumulator:
  private var finished = false

  update def accept(bytes: Array[Byte]^{caps.any.rd}, offset: Int, length: Int): Unit =
    accumulate(bytes, offset, length)

  update def finish(): Unit =
    if !finished then
      finished = true
      install(BrotliDecoder.decode(accumulated, accumulatedLength))

// Accumulates the whole payload, then emits it as Brotli (see `BrotliEncoder`).
private[pneumatic] class BrotliEncoderEngine extends BrotliEngine, BrotliAccumulator:
  private var finished = false

  update def accept(bytes: Array[Byte]^{caps.any.rd}, offset: Int, length: Int): Unit =
    accumulate(bytes, offset, length)

  update def finish(): Unit =
    if !finished then
      finished = true
      install(BrotliEncoder.encode(accumulated, accumulatedLength))

// The `Duct` stage: a thin wrapper presenting a Brotli engine to the streaming kernel, draining the
// engine's retained `pending` buffer into whatever space each step or flush offers. The shape
// mirrors `LzwStage`. The engine is created by the by-name argument inside the stage, so the stage
// owns it exclusively.
private[pneumatic] class BrotliStage(engine0: => BrotliEngine^) extends Duct[Data, Data]:
  type Transport = Credit
  type Upstream = Credit

  private val engine: BrotliEngine^ = engine0
  private var finishing = false

  def regulation: Credit is Regulation = summon[Credit is Regulation]
  def translate(demand: Credit): Credit = demand

  update def step(source: Region[Data])(range: Interval in source.type)
    ( target: Slate[Data] )(space: Interval in target.type)
  :   Duct.Progress =

    val sourceInterval: Interval = range
    val targetInterval: Interval = space
    val bytes = unsafely(source.raw.asInstanceOf[scala.Array[Byte]])
    val out: scala.Array[Byte]^ =
      unsafely(target.raw.asInstanceOf[scala.Array[Byte]]).asInstanceOf[scala.Array[Byte]^]

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
      unsafely(target.raw.asInstanceOf[scala.Array[Byte]]).asInstanceOf[scala.Array[Byte]^]
    engine.deliver(out, targetInterval.start.n0, targetInterval.size)

object Brotli:
  given compression: Brotli is Compression:
    def compressor()(using Buffering): (Duct[Data, Data] {
      type Transport = Credit
      type Upstream = Credit })^ =

      BrotliStage(BrotliEncoderEngine())

    def decompressor()(using Buffering): (Duct[Data, Data] {
      type Transport = Credit
      type Upstream = Credit })^ =

      BrotliStage(BrotliDecoderEngine())

    override def compress(stream: Chain[Data]): Chain[Data] =
      drive(BrotliEncoderEngine(), stream)

    override def decompress(stream: Chain[Data]): Chain[Data] =
      drive(BrotliDecoderEngine(), stream)

  // Drives an engine over a lazy stream chunk by chunk, then collects its finished tail. The
  // engine argument is by-name, so the (exclusive, mutable) engine is minted inside the deferred
  // block and never escapes it.
  private def drive(engine0: => BrotliEngine^, stream: Chain[Data]): Chain[Data] =
    def recur(engine: BrotliEngine^, stream: Chain[Data]): Chain[Data] = stream match
      case head #:: tail =>
        engine.accept(head, 0, head.length)
        recur(engine, tail)

      case _ =>
        engine.finish()
        val data = engine.gather()
        if data.length > 0 then Chain(data) else Chain.empty

    Chain.defer(recur(engine0, stream))

sealed trait Brotli extends Compressor
