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

import anticipation.*
import rudiments.*
import vacuous.*
import zephyrine.*

// LZW, as used by TIFF and PDF: MSB-first codes of 9 to 12 bits, code 256 clearing the
// table and 257 ending the data. The algorithm lives in plain engine classes — the moral
// equivalent of `juz.Deflater`, so the lazy `LazyList` drivers may close over them exactly
// as `Zlib`'s do over a deflater — and the `Duct` stages are thin wrappers.
//
// With `earlyChange` — the TIFF/PDF default — both sides widen their codes one table entry
// sooner. The decoder's table trails the encoder's by one entry, so the encoder widens (and
// clears) at a `nextCode` threshold one higher than the decoder's table-length threshold,
// which is what keeps the two in step.
private[pneumatic] trait LzwEngine:
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

  // Everything not yet delivered, drained in one immutable piece: the whole-value
  // counterpart of `deliver`, and — being a method of an untracked engine with a pure
  // result — safe to call from within a lazy stream's thunks.
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

private[pneumatic] class LzwEncoder(earlyChange: Boolean) extends LzwEngine:
  private val codes: scala.collection.mutable.HashMap[(Int, Byte), Int] =
    scala.collection.mutable.HashMap()

  private var nextCode = 258
  private var width = 9
  private var prefix = -1
  private var bits = 0L
  private var bitCount = 0
  private var begun = false
  private var ended = false

  private val early: Int = if earlyChange then 1 else 0

  private def emit(code: Int): Unit =
    bits = (bits << width) | code
    bitCount += width

    while bitCount >= 8 do
      pending += ((bits >> (bitCount - 8)) & 0xff).toByte
      bitCount -= 8

  def accept(bytes: Array[Byte], offset: Int, length: Int): Unit =
    if !begun then
      emit(256)
      begun = true

    var i = 0

    while i < length do
      val byte = bytes(offset + i)

      if prefix < 0 then prefix = byte & 0xff else codes.at((prefix, byte)) match
        case code: Int =>
          prefix = code

        case _ =>
          emit(prefix)
          codes((prefix, byte)) = nextCode
          nextCode += 1

          // The decoder's table trails by one entry, so its thresholds sit one lower.
          if nextCode >= (1 << width) + 1 - early && width < 12 then width += 1

          if nextCode >= 4096 - early then
            emit(256)
            codes.clear()
            nextCode = 258
            width = 9

          prefix = byte & 0xff

      i += 1

  def finish(): Unit =
    if !ended then
      if !begun then
        emit(256)
        begun = true

      if prefix >= 0 then
        emit(prefix)
        prefix = -1

      emit(257)
      if bitCount > 0 then emit(0) // pad the final code out to a byte boundary
      bitCount = 0
      ended = true

private[pneumatic] class LzwDecoder(earlyChange: Boolean) extends LzwEngine:
  private val table: scala.collection.mutable.ArrayBuffer[Array[Byte]] =
    scala.collection.mutable.ArrayBuffer()

  private var width = 9
  private var bits = 0L
  private var bitCount = 0
  private var finished = false

  private var previous: Array[Byte] = new Array[Byte](0)

  private val early: Int = if earlyChange then 1 else 0

  reset()

  private def reset(): Unit =
    table.clear()
    var byte = 0

    while byte < 256 do
      table += Array(byte.toByte)
      byte += 1

    table += new Array[Byte](0) // the clear code
    table += new Array[Byte](0) // the end-of-data code
    width = 9
    previous = new Array[Byte](0)

  private def interpret(): Unit =
    val code = ((bits >> (bitCount - width)) & ((1L << width) - 1)).toInt
    bitCount -= width

    code match
      case 256 =>
        reset()

      case 257 =>
        finished = true

      case code =>
        val entry: Array[Byte] =
          if code < table.length then table(code).nn
          else if code == table.length && previous.length > 0 then previous :+ previous(0)
          else throw IllegalStateException("the LZW data is corrupt")

        var i = 0

        while i < entry.length do
          pending += entry(i)
          i += 1

        if previous.length > 0 then table += previous :+ entry(0)
        previous = entry
        if table.length >= (1 << width) - early && width < 12 then width += 1

  def accept(bytes: Array[Byte], offset: Int, length: Int): Unit =
    var consumed = 0

    while consumed < length do
      while bitCount <= 56 && consumed < length do
        bits = (bits << 8) | (bytes(offset + consumed) & 0xff)
        bitCount += 8
        consumed += 1

      while bitCount >= width && !finished do interpret()
      if finished then consumed = length // trailing bytes after end-of-data are noise

  def finish(): Unit = while bitCount >= width && !finished do interpret()

// The `Duct` stages: thin wrappers presenting an engine to the streaming kernel. All output
// is staged through the engine's retained `pending` buffer, drained into whatever target
// space each step or flush offers.
private[pneumatic] class LzwStage(engine: LzwEngine) extends Duct[Data, Data]:
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
