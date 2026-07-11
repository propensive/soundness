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
package turbulence

import java.util.zip as juz

import anticipation.*
import zephyrine.*

// Streaming compression as a pipeline stage, over `juz.Deflater`. The three
// compression formats differ only in framing: `Zlib` is the deflater's own
// wrapper, `Deflate` is the raw stream, and `Gzip` adds a manual header,
// CRC-32 and trailer around a raw stream. Input is staged in a duct-owned
// buffer, since the deflater holds a reference to (not a copy of) its input
// array, which would otherwise outlive the source window's validity.
//
// Compression ratios are unknowable, so `translate` is a pass-through: the
// duct's own bounded buffer and the retained-surplus rule absorb expansion.
private[turbulence] class Deflation(gzip: Boolean, nowrap: Boolean)(using Buffering)
extends Duct[Data, Data]:
  type Transport = Credit
  type Upstream = Credit

  private val deflater: juz.Deflater =
    juz.Deflater(juz.Deflater.DEFAULT_COMPRESSION, nowrap || gzip)

  private val crc: juz.CRC32 = juz.CRC32()
  private val staging: Array[Byte] = new Array[Byte](summon[Buffering].capacity(Substrate.Bytes))
  private var headerDone: Boolean = !gzip
  private var size: Long = 0
  private var finishing: Boolean = false
  private var trailer: Int = 0

  def regulation: Credit is Regulation = summon[Credit is Regulation]
  def translate(demand: Credit): Credit = demand

  // The gzip header (10 bytes) must fit in one step's output space.
  override def quantum: Int = if gzip then 10 else 1

  private update def header(target: Array[Byte], offset: Int): Unit =
    target(offset) = 0x1f
    target(offset + 1) = 0x8b.toByte
    target(offset + 2) = 8

    for index <- 3 to 8 do target(offset + index) = 0

    target(offset + 9) = -1

  update def step
    ( source: input.Storage,
      sourceOffset: Int,
      sourceLength: Int,
      target: output.Storage,
      targetOffset: Int,
      targetSpace: Int )
  :   Duct.Progress =

    val bytes = source.asInstanceOf[Array[Byte]]
    val out = target.asInstanceOf[Array[Byte]]
    var consumed: Int = 0
    var produced: Int = 0

    if !headerDone then
      header(out, targetOffset)
      produced += 10
      headerDone = true

    if deflater.needsInput && sourceLength > 0 then
      consumed = sourceLength.min(staging.length)
      System.arraycopy(bytes, sourceOffset, staging, 0, consumed)
      deflater.setInput(staging, 0, consumed)

      if gzip then
        crc.update(staging, 0, consumed)
        size += consumed

    var run: Int = 1

    while run > 0 && produced < targetSpace do
      run = deflater.deflate(out, targetOffset + produced, targetSpace - produced)
      produced += run

    Duct.Progress(consumed, produced)

  override update def flush(target: output.Storage, targetOffset: Int, targetSpace: Int): Int =
    val out = target.asInstanceOf[Array[Byte]]
    var produced: Int = 0

    if !headerDone && targetSpace >= 10 then
      header(out, targetOffset)
      produced += 10
      headerDone = true

    if headerDone then
      if !finishing then
        deflater.finish()
        finishing = true

      var run: Int = 1

      while run > 0 && produced < targetSpace do
        run = deflater.deflate(out, targetOffset + produced, targetSpace - produced)
        produced += run

      if deflater.finished && gzip then
        val value = crc.getValue

        while trailer < 8 && produced < targetSpace do
          val datum =
            if trailer < 4 then (value >>> (trailer*8)) & 0xff
            else (size >>> ((trailer - 4)*8)) & 0xff

          out(targetOffset + produced) = datum.toByte
          produced += 1
          trailer += 1

    produced

// Streaming decompression, the inverse of `Deflation`, over `juz.Inflater`,
// with the gzip header parsed by a small state machine ahead of the inflater
// and the 8-byte trailer consumed and ignored after it finishes.
private[turbulence] class Inflation(gzip: Boolean, nowrap: Boolean)(using Buffering)
extends Duct[Data, Data]:
  type Transport = Credit
  type Upstream = Credit

  private enum Header:
    case Fixed(remaining: Int)
    case ExtraLength(byte: Int, low: Int)
    case Extra(remaining: Int)
    case Name, Comment
    case Checksum(remaining: Int)
    case Done

  private val inflater: juz.Inflater = juz.Inflater(nowrap || gzip)
  private val staging: Array[Byte] = new Array[Byte](summon[Buffering].capacity(Substrate.Bytes))
  private var header: Header = if gzip then Header.Fixed(10) else Header.Done
  private var flags: Int = 0
  private var headerPosition: Int = 0
  private var trailer: Int = if gzip then 8 else 0

  def regulation: Credit is Regulation = summon[Credit is Regulation]
  def translate(demand: Credit): Credit = demand

  // Consume one header byte, returning the successor state.
  private update def advance(byte: Int, position: Int): Header = header match
    case Header.Fixed(remaining) =>
      if position == 3 then flags = byte

      if remaining > 1 then Header.Fixed(remaining - 1)
      else if (flags & 4) != 0 then Header.ExtraLength(0, 0)
      else if (flags & 8) != 0 then Header.Name
      else if (flags & 16) != 0 then Header.Comment
      else if (flags & 2) != 0 then Header.Checksum(2)
      else Header.Done

    case Header.ExtraLength(0, _) =>
      Header.ExtraLength(1, byte)

    case Header.ExtraLength(_, low) =>
      val length = (byte << 8) | low
      if length == 0 then afterExtra else Header.Extra(length)

    case Header.Extra(remaining) =>
      if remaining > 1 then Header.Extra(remaining - 1) else afterExtra

    case Header.Name =>
      if byte == 0 then afterName else Header.Name

    case Header.Comment =>
      if byte == 0 then afterComment else Header.Comment

    case Header.Checksum(remaining) =>
      if remaining > 1 then Header.Checksum(remaining - 1) else Header.Done

    case Header.Done =>
      Header.Done

  private update def afterExtra: Header =
    if (flags & 8) != 0 then Header.Name
    else if (flags & 16) != 0 then Header.Comment
    else if (flags & 2) != 0 then Header.Checksum(2)
    else Header.Done

  private update def afterName: Header =
    if (flags & 16) != 0 then Header.Comment
    else if (flags & 2) != 0 then Header.Checksum(2)
    else Header.Done

  private update def afterComment: Header =
    if (flags & 2) != 0 then Header.Checksum(2) else Header.Done

  update def step
    ( source: input.Storage,
      sourceOffset: Int,
      sourceLength: Int,
      target: output.Storage,
      targetOffset: Int,
      targetSpace: Int )
  :   Duct.Progress =

    val bytes = source.asInstanceOf[Array[Byte]]
    val out = target.asInstanceOf[Array[Byte]]
    var consumed: Int = 0
    var produced: Int = 0

    while header != Header.Done && consumed < sourceLength do
      header = advance(bytes(sourceOffset + consumed) & 0xff, headerPosition)
      headerPosition += 1
      consumed += 1

    if header == Header.Done then
      if !inflater.finished then
        if inflater.needsInput && consumed < sourceLength then
          val copy = (sourceLength - consumed).min(staging.length)
          System.arraycopy(bytes, sourceOffset + consumed, staging, 0, copy)
          inflater.setInput(staging, 0, copy)
          consumed += copy

        var run: Int = 1

        while run > 0 && produced < targetSpace && !inflater.finished do
          run =
            try inflater.inflate(out, targetOffset + produced, targetSpace - produced)
            catch case error: juz.DataFormatException => throw IllegalStateException(error)

          produced += run

      if inflater.finished && trailer > 0 then
        // Any input the inflater over-read belongs to the trailer.
        trailer -= inflater.getRemaining.min(trailer)
        inflater.setInput(staging, 0, 0)

        val skip = trailer.min(sourceLength - consumed)
        consumed += skip
        trailer -= skip

    Duct.Progress(consumed, produced)

  // The inflater may hold far more pending output than one step's space, so
  // it must keep draining after the upstream ends.
  override def flush(target: output.Storage, targetOffset: Int, targetSpace: Int): Int =
    val out = target.asInstanceOf[Array[Byte]]
    var produced: Int = 0
    var run: Int = 1

    while run > 0 && produced < targetSpace && !inflater.finished do
      run =
        try inflater.inflate(out, targetOffset + produced, targetSpace - produced)
        catch case error: juz.DataFormatException => throw IllegalStateException(error)

      produced += run

    produced
