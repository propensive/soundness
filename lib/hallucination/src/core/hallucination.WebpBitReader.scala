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

import anticipation.*
import contingency.*

import RasterError.Reason

// A little-endian bit reader for the VP8L lossless bitstream, ported from image-rs/image-webp
// (`src/lossless/decoder/mod.rs`, MIT/Apache-2.0). Bits are consumed least-significant first; the
// buffer holds up to 64 bits, refilled eight bytes at a time. `available` is a guaranteed lower
// bound: the fast refill path reads eight bytes but only commits seven, so the eighth's bits
// overlap and are re-OR'd identically on the next refill — which is why it reads eight and consumes
// seven.
private[hallucination] final class WebpBitReader(data: Data, start: Int, end: Int):
  private var position: Int = start
  private var buffer: Long = 0L
  private var available: Int = 0

  def nbits: Int = available

  private inline def byte(index: Int): Long = (data(index)&0xff).toLong

  // Fills the buffer so it holds 64 bits or the input is exhausted.
  def fill(): Unit =
    if end - position >= 8 then
      var lookahead = 0L
      var i = 0

      while i < 8 do
        lookahead |= byte(position + i) << (i*8)
        i += 1

      position += (63 - available)/8
      buffer |= lookahead << available
      available |= 56
    else
      while position < end && available < 56 do
        buffer |= byte(position) << available
        available += 8
        position += 1

  // The next `num` bits without consuming them.
  def peek(num: Int): Long = buffer & ((1L << num) - 1)

  // The whole buffer without consuming.
  def peekFull: Long = buffer

  def consume(num: Int): Unit raises RasterError =
    if available < num then abort(RasterError(Webp(), Reason.Truncated))
    buffer >>>= num
    available -= num

  // Reads `num` bits (at most 32), refilling if necessary.
  def readBits(num: Int): Int raises RasterError =
    if available < num then fill()
    val value = peek(num)
    consume(num)
    value.toInt
