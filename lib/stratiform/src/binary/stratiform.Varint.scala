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
package stratiform

import scala.language.unsafeNulls
import proscenium.compat.*

import anticipation.*
import contingency.*
import fulminate.*

// §4 of the BinTEL spec: variable-length unsigned integer encoding.
// Seven payload bits per byte; bit 7 set marks a continuation byte;
// bytes are emitted least-significant-first.

object Varint:

  // Result of decoding one varint from a byte stream: the value, and the
  // index *past* the last consumed byte.
  case class Decoded(value: Long, next: Int)

  // Encode a non-negative integer to its BinTEL §4 byte sequence. Long
  // accommodates every value BinTEL might emit — counts, byte lengths,
  // and keyword indices in a single schema.
  def encode(value: Long): Data =
    if value < 0L then throw IllegalArgumentException(s"varint value must be non-negative: $value")
    val buf = new scala.Array[Byte](10)
    var n = value
    var i = 0

    while n >= 0x80L do
      buf(i) = ((n & 0x7fL) | 0x80L).toByte
      n >>>= 7
      i += 1

    buf(i) = n.toByte
    val out = new scala.Array[Byte](i + 1)
    System.arraycopy(buf, 0, out, 0, i + 1)
    out.asInstanceOf[Array[Byte]^{}]

  // Decode a varint from `data` starting at `offset`. Returns the decoded
  // value and the next read position. Raises `Base256.Error.Reason` is not
  // applicable here — the equivalent failure for varint truncation /
  // overflow lives in the BinTEL-level error type which this module does
  // not yet introduce; we use a single dedicated exception for the
  // foundational codec and let callers wrap it.
  def decode(data: Data, offset: Int): Decoded raises Varint.Error =
    var shift = 0
    var acc = 0L
    var i = offset

    while true do
      if i >= data.length then abort(Varint.Error(Varint.Error.Reason.Truncated))
      val b = data(i) & 0xff

      if shift >= 64 || (shift == 63 && (b & 0x7f) > 1)
      then abort(Varint.Error(Varint.Error.Reason.Overflow))

      acc |= (b & 0x7fL) << shift
      i += 1
      if (b & 0x80) == 0 then return Decoded(acc, i)
      shift += 7

    Decoded(0L, offset)

  // VarintError → Varint.Error
  object Error:

    object Reason:
      given communicable: Reason is Communicable =
        case Truncated => m"the varint is truncated — a continuation byte was expected"
        case Overflow  => m"the varint encodes a value that does not fit in 64 bits"

    enum Reason(val number: Int) extends Clarification:
      case Truncated extends Reason(1)
      case Overflow  extends Reason(2)

  case class Error(reason: Varint.Error.Reason)(using Diagnostics)
  extends fulminate.Error(608, reason.number)(m"the varint is invalid because $reason")

