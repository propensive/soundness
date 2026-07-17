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

import anticipation.*
import zephyrine.*

// Streaming compression as a pipeline stage, over the pure-Scala `Deflater`. The three
// compression formats differ only in framing: `Zlib` is the deflater's own
// wrapper, `Deflate` is the raw stream, and `Gzip` adds a manual header,
// CRC-32 and trailer around a raw stream. The deflater is fed zero-copy from
// the source window: it holds a reference to (not a copy of) its input array,
// but that reference is released before each step returns, so it never
// outlives the window's validity — the same discipline as `Inflation`.
//
// Compression ratios are unknowable, so `translate` is a pass-through: the
// duct's own bounded buffer and the retained-surplus rule absorb expansion.
private[turbulence] class Deflation(gzip: Boolean, nowrap: Boolean)(using Buffering)
extends Duct[Data, Data]:
  type Transport = Credit
  type Upstream = Credit

  private val deflater: Deflater = Deflater(-1, nowrap || gzip)

  private val crc: Crc32 = Crc32()
  private val empty: Array[Byte] = new Array[Byte](0)
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

    // Feed the deflater directly from the source window — no staging copy. As
    // in `Inflation`, the reference is released before this step returns, and
    // only the bytes the deflater actually consumed (`getBytesRead` measures
    // them; `Deflater` has no `getRemaining`) are claimed; the remainder stays
    // in the window for the next step to re-feed.
    if sourceLength > 0 then deflater.setInput(bytes, sourceOffset, sourceLength)

    val before: Long = deflater.getBytesRead
    var run: Int = 1

    while run > 0 && produced < targetSpace do
      run = deflater.deflate(out, targetOffset + produced, targetSpace - produced)
      produced += run

    consumed = (deflater.getBytesRead - before).toInt
    deflater.setInput(empty)

    // The consumed range is a contiguous prefix of the window, so the checksum
    // sees exactly the stream the deflater compressed, in order.
    if gzip && consumed > 0 then
      crc.update(bytes, sourceOffset, consumed)
      size += consumed

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
        val value = crc.value

        while trailer < 8 && produced < targetSpace do
          val datum =
            if trailer < 4 then (value >>> (trailer*8)) & 0xff
            else (size >>> ((trailer - 4)*8)) & 0xff

          out(targetOffset + produced) = datum.toByte
          produced += 1
          trailer += 1

    produced

// Streaming decompression, the inverse of `Deflation`, over the pure-Scala `Inflater`,
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

  private val inflater: Inflater = Inflater(nowrap || gzip)
  private val empty: Array[Byte] = new Array[Byte](0)
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
        // Feed the inflater directly from the source window — no staging copy.
        // The reference cannot outlive the window's validity, because the input
        // is unconditionally released before this step returns: only the bytes
        // the inflater actually consumed are claimed, and the remainder stays
        // in the window for the next step to re-feed. (zlib's own position is
        // held bit-exactly in the inflater's state, so re-supplying the unread
        // whole bytes later is the same stream.)
        val fed = sourceLength - consumed
        if fed > 0 then inflater.setInput(bytes, sourceOffset + consumed, fed)

        var run: Int = 1

        while run > 0 && produced < targetSpace && !inflater.finished do
          run = inflater.inflate(out, targetOffset + produced, targetSpace - produced)
          produced += run

        consumed += fed - inflater.getRemaining
        inflater.setInput(empty)

      if inflater.finished && trailer > 0 then
        // Unconsumed input was never claimed, so the trailer begins exactly at
        // `consumed`; skip as much of it as this window holds.
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
      run = inflater.inflate(out, targetOffset + produced, targetSpace - produced)
      produced += run

    produced
