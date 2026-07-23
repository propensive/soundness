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
package pneumatic

import anticipation.*
import proscenium.compat.*
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
private[pneumatic] trait BrotliEngine:
  protected val pending: scala.collection.mutable.ArrayBuffer[Byte] =
    scala.collection.mutable.ArrayBuffer()

  private var delivered: Int = 0

  def accept(bytes: Array[Byte], offset: Int, length: Int): Unit
  def finish(): Unit

  def deliver(target: Array[Byte], offset: Int, space: Int): Int =
    var produced = 0

    while delivered < pending.length && produced < space do
      target(offset + produced) = pending(delivered)
      delivered += 1
      produced += 1

    if delivered == pending.length then
      pending.clear()
      delivered = 0

    produced

  def gather(): Data =
    val result = new Array[Byte](pending.length - delivered)
    var i = 0

    while delivered < pending.length do
      result(i) = pending(delivered)
      i += 1
      delivered += 1

    pending.clear()
    delivered = 0
    result.immutable(using Unsafe)

// Accumulates the whole compressed stream, then decodes it in one pass (see `BrotliDecoder`).
private[pneumatic] class BrotliDecoderEngine extends BrotliEngine:
  private val input: scala.collection.mutable.ArrayBuffer[Byte] =
    scala.collection.mutable.ArrayBuffer()

  private var finished = false

  def accept(bytes: Array[Byte], offset: Int, length: Int): Unit =
    var i = 0
    while i < length do { input += bytes(offset + i); i += 1 }

  def finish(): Unit =
    if !finished then
      finished = true
      val array = new Array[Byte](input.length)
      var k = 0
      while k < input.length do { array(k) = input(k); k += 1 }
      val decoded = BrotliDecoder.decode(array, array.length)
      var i = 0
      while i < decoded.length do { pending += decoded(i); i += 1 }

// Accumulates the whole payload, then emits it as Brotli (see `BrotliEncoder`).
private[pneumatic] class BrotliEncoderEngine extends BrotliEngine:
  private val input: scala.collection.mutable.ArrayBuffer[Byte] =
    scala.collection.mutable.ArrayBuffer()

  private var finished = false

  def accept(bytes: Array[Byte], offset: Int, length: Int): Unit =
    var i = 0
    while i < length do { input += bytes(offset + i); i += 1 }

  def finish(): Unit =
    if !finished then
      finished = true
      val array = new Array[Byte](input.length)
      var k = 0
      while k < input.length do { array(k) = input(k); k += 1 }
      val encoded = BrotliEncoder.encode(array, array.length)
      var i = 0
      while i < encoded.length do { pending += encoded(i); i += 1 }

// The `Duct` stage: a thin wrapper presenting a Brotli engine to the streaming kernel, draining the
// engine's retained `pending` buffer into whatever space each step or flush offers. The shape
// mirrors `LzwStage`.
private[pneumatic] class BrotliStage(engine: BrotliEngine) extends Duct[Data, Data]:
  type Transport = Credit
  type Upstream = Credit

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

    engine.accept(source.asInstanceOf[Array[Byte]], sourceOffset, sourceLength)

    Duct.Progress
      ( sourceLength,
        engine.deliver(target.asInstanceOf[Array[Byte]], targetOffset, targetSpace) )

  override update def flush(target: output.Storage, targetOffset: Int, targetSpace: Int): Int =
    if !finishing then
      engine.finish()
      finishing = true

    engine.deliver(target.asInstanceOf[Array[Byte]], targetOffset, targetSpace)

object Brotli:
  given compression: Brotli is Compression:
    def compressor()(using Buffering): Duct[Data, Data] {
      type Transport = Credit
      type Upstream = Credit } =

      BrotliStage(BrotliEncoderEngine())

    def decompressor()(using Buffering): Duct[Data, Data] {
      type Transport = Credit
      type Upstream = Credit } =

      BrotliStage(BrotliDecoderEngine())

    def compress(stream: Progression[Data]): Progression[Data] = drive(BrotliEncoderEngine(), stream)
    def decompress(stream: Progression[Data]): Progression[Data] = drive(BrotliDecoderEngine(), stream)

  // Drives an engine over a lazy stream chunk by chunk, then collects its finished tail.
  private def drive(engine: BrotliEngine, stream: Progression[Data]): Progression[Data] =
    def recur(stream: Progression[Data]): Progression[Data] = stream match
      case head #:: tail =>
        engine.accept(head.mutable(using Unsafe), 0, head.length)
        recur(tail)

      case _ =>
        engine.finish()
        val data = engine.gather()
        if data.length > 0 then Progression(data) else Progression.empty

    Progression.defer(recur(stream))

sealed trait Brotli extends Compressor
