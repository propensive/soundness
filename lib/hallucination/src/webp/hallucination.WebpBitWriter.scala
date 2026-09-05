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
package hallucination

import java.io as ji

import anticipation.*

import scala.caps

// A little-endian bit writer for the VP8L lossless bitstream, ported from image-rs/image-webp
// (`src/lossless/encoder/mod.rs`, MIT/Apache-2.0). Bits accumulate least-significant first into a
// 64-bit buffer, flushed eight bytes at a time.
private[hallucination] final class WebpBitWriter extends caps.Mutable:
  // Field-held, and more strongly so than the other cases: `Vp8Encoder` keeps two of these in
  // `var` fields, writing to separate buffers which are combined afterwards, so the buffer
  // outlives any single expression. `Array.collect` lends a scribe and freezes it when the
  // lender returns, and that confinement is what makes the freeze sound, so this cannot use it
  // without either exposing growable construction outside a lender — which would give the
  // soundness argument away — or restructuring the codec.
  private val out = ji.ByteArrayOutputStream()
  private var buffer: Long = 0L
  private var count: Int = 0

  update def writeBits(bits: Long, nbits: Int): Unit =
    val previous = count
    buffer |= bits << previous
    count = previous + nbits

    if count >= 64 then
      writeLong(buffer)
      count -= 64
      val shift = 64 - previous
      buffer = if shift >= 64 then 0L else bits >>> shift

  private def writeLong(value: Long): Unit =
    var i = 0

    while i < 8 do
      out.write(((value >>> (i*8)) & 0xff).toInt)
      i += 1

  // Pads to a byte boundary and returns the accumulated bytes.
  update def bytes: Data =
    if count%8 != 0 then writeBits(0, 8 - count%8)

    var i = 0

    while i < count/8 do
      out.write(((buffer >>> (i*8)) & 0xff).toInt)
      i += 1

    buffer = 0
    count = 0
    Array.unsafeFrozen(out.toByteArray.nn)
