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
import vacuous.*

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
final class BintelParser private[stratiform]
  ( input: Data,
    codecs: Optional[Tel.Codec.Resolver] = Unset,
    checkCanonical: Boolean = false ):

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

  // One scalar payload through the §21.7 codec named `encoding`: the raw
  // bytes are read via `directScalarBytes` and decoded by the bound codec
  // — B13 when no binding is configured or the name does not resolve, B14
  // when the codec rejects the bytes, and B15 under the OPTIONAL
  // re-encode canonicality check. Generated parsers call this at leaves
  // whose type declares an encoding via the `Tel.Encoded` marker.
  def directEncodedScalar(encoding: String)(using Tactic[Bintel.Error]): String =
    val bytes = directScalarBytes()

    val codec = codecs.let(_(Text(encoding)))
    . or(abort(Bintel.Error(Bintel.Error.Reason.CodecUnresolved)))

    val frozen = bytes.asInstanceOf[Data]

    codec.decode(frozen) match
      case Tel.Codec.Decoded.Failure(_) =>
        abort(Bintel.Error(Bintel.Error.Reason.CodecDecodeFailed))

      case Tel.Codec.Decoded.Value(text) =>
        if checkCanonical then codec.encode(text) match
          case Tel.Codec.Encoded.Bytes(re) =>
            if !java.util.Arrays.equals(re.asInstanceOf[scala.Array[Byte]], bytes)
            then abort(Bintel.Error(Bintel.Error.Reason.CodecNoncanonical))

          case Tel.Codec.Encoded.Invalid(_) =>
            abort(Bintel.Error(Bintel.Error.Reason.CodecNoncanonical))

        text.s

  // One scalar payload's raw bytes, without UTF-8 decoding: the leaf
  // `directEncodedScalar` reads before applying its codec. Framing is
  // codec-independent, so `directSkipScalar` skips encoded scalars
  // correctly too.
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
