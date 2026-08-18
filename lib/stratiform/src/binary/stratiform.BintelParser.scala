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

import anticipation.*
import contingency.*

// Reads BinTEL body bytes for direct parsing (`Bintel.Parsable`): the
// document structure is fully count-driven and self-delimiting, so the
// parser is a bare offset over the input. Each method mirrors one step of
// `Bintel.decode`'s recursive descent — the varint reads carry the same
// `Varint.Error`/`UnexpectedEoi` mapping, and scalar payloads the same
// truncation check — so failures agree with the AST decoder exactly.
//
// The class is public — generated parsers, spliced into user modules, bind
// it once per read and step through its direct rim — but only stratiform's
// read path can construct one.
final class BintelParser private[stratiform] (input: Data):
  @scala.caps.unsafe.untrackedCaptures
  private[stratiform] val data: scala.Array[Byte] = input.asInstanceOf[scala.Array[Byte]]

  @scala.caps.unsafe.untrackedCaptures
  private[stratiform] var offset: Int = 0

  def directVarint()(using Tactic[Bintel.Error]): Long =
    if offset >= data.length then abort(Bintel.Error(Bintel.Error.Reason.UnexpectedEoi))

    var result = 0L
    var shift = 0
    var continue = true

    while continue do
      if offset >= data.length then abort(Bintel.Error(Bintel.Error.Reason.VarintError))
      val byte = data(offset) & 0xff

      if shift >= 64 || (shift == 63 && (byte & 0x7f) > 1)
      then abort(Bintel.Error(Bintel.Error.Reason.VarintError))

      offset += 1
      result |= (byte.toLong & 0x7f) << shift
      shift += 7
      if (byte & 0x80) == 0 then continue = false

    result

  // A child count or keyword index, bounded to `Int`.
  def directCount()(using Tactic[Bintel.Error]): Int =
    val value = directVarint()

    if value < 0 || value > Int.MaxValue then abort(Bintel.Error(Bintel.Error.Reason.VarintError))

    value.toInt

  // One scalar payload: `length` varint then UTF-8 bytes, exactly as
  // `decodeElement`'s Scalar case reads it.
  def directScalar()(using Tactic[Bintel.Error]): String =
    val length = directCount()

    if offset + length > data.length then abort(Bintel.Error(Bintel.Error.Reason.ValueTruncated))

    val result = java.lang.String(data, offset, length, java.nio.charset.StandardCharsets.UTF_8)
    offset += length
    result

  // One scalar payload's raw bytes, without UTF-8 decoding: the leaf a
  // codec-aware staged parser would use for encoded scalars (§21.7).
  // The staged layer derives its plans from Scala types, which declare
  // no encodings, so nothing generates calls to this yet; full staged
  // codec support is deferred until derived schemas can. Framing is
  // codec-independent, so `directSkipScalar` already skips encoded
  // scalars correctly.
  def directScalarBytes()(using Tactic[Bintel.Error]): scala.Array[Byte] =
    val length = directCount()

    if offset + length > data.length then abort(Bintel.Error(Bintel.Error.Reason.ValueTruncated))

    val result = java.util.Arrays.copyOfRange(data, offset, offset + length).nn
    offset += length
    result

  // Skips one scalar payload without materializing it.
  def directSkipScalar()(using Tactic[Bintel.Error]): Unit =
    val length = directCount()

    if offset + length > data.length then abort(Bintel.Error(Bintel.Error.Reason.ValueTruncated))

    offset += length

  def directAtEnd: Boolean = offset >= data.length
