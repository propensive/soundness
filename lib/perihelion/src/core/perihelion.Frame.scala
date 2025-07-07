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
package perihelion

import anticipation.*
import contingency.*
import hypotenuse.*
import vacuous.*
import zephyrine.*

object Frame:
  // A per-frame payload cap (16 MiB); larger frames close the connection (1009).
  val maxPayload: Long = 1L << 24
  val maxControlPayload: Int = 125

  def closeData(code: Int, reason: Data): Data =
    Data((code >> 8).toByte, code.toByte) ++ reason

  // Close codes a client may legitimately send (RFC 6455 §7.4.1 plus the
  // registered application range). Everything else — including 1004/1005/1006,
  // which never appear on the wire, and the unassigned 1012–2999 band — is a
  // protocol error.
  def validCloseCode(code: Int): Boolean =
    (code >= 1000 && code <= 1003) || (code >= 1007 && code <= 1011) ||
      (code >= 3000 && code <= 4999)

  // Decode one frame off `cursor` — consuming exactly its bytes and unmasking the
  // payload — or `Unset` at a clean end of stream. The byte-level counterpart of
  // `Frame.encode`. `Masking` fixes the direction: a server requires the frame to be
  // masked (a client's), a client requires it to be unmasked (a server's). Uses
  // `peek`/`next`/`take` (not `lay`/`seek`), which don't reference the cursor's erased
  // `Operand` type, so it works on a bare `Cursor[Data, ?]` parameter.
  def parse(cursor: Cursor[Data, ?])(using masking: Masking)
  :   Optional[Frame] raises WebsocketError =
    if cursor.finished then Unset else
      val byte0 = cursor.peek.asInt
      cursor.next()
      val fin = (byte0 & 0x80) != 0

      // RFC 6455 §5.2: RSV1/2/3 must be zero unless an extension negotiated them,
      // and we negotiate none.
      if (byte0 & 0x70) != 0 then abort(WebsocketError(WebsocketError.Reason.ReservedBits))

      val opcode = byte0 & 0x0f

      val byte1 = cursor.peek.asInt
      cursor.next()
      val masked = (byte1 & 0x80) != 0

      // RFC 6455 §5.1: a server must reject an unmasked client frame; a client must
      // likewise reject a masked server frame.
      if masking.inbound && !masked then abort(WebsocketError(WebsocketError.Reason.Unmasked))
      if !masking.inbound && masked then abort(WebsocketError(WebsocketError.Reason.Masked))

      val length: Long = (byte1 & 0x7f) match
        case 126 => B16(cursor.take(Data())(2)).u16.long
        case 127 => B64(cursor.take(Data())(8)).s64.long
        case n   => n.toLong

      // A 64-bit length with the high bit set decodes negative; treat it, and any
      // length past the cap, as too large to buffer.
      if length < 0 || length > maxPayload
      then abort(WebsocketError(WebsocketError.Reason.TooLarge(length)))

      // Control frames must not be fragmented and carry at most 125 bytes.
      if opcode >= 0x8 && (!fin || length > maxControlPayload)
      then abort(WebsocketError(WebsocketError.Reason.BadControl))

      val mask = if masked then cursor.take(Data())(4) else Data()
      val payload = unmask(cursor.take(Data())(length.toInt), mask)

      opcode match
        case 0x0 => Continuation(fin, payload)
        case 0x1 => Text(fin, payload)
        case 0x2 => Binary(fin, payload)
        case 0x9 => Ping(payload)
        case 0xa => Pong(payload)

        case 0x8 =>
          // A close payload is either empty or a 2-byte code plus an optional
          // reason; a lone byte is malformed. `1005` is the internal "no code
          // present" sentinel and must never travel on the wire.
          if payload.length == 1 then abort(WebsocketError(WebsocketError.Reason.BadClose))
          val code = if payload.length >= 2 then B16(payload.take(2)).u16.int else 1005

          if payload.length >= 2 && !validCloseCode(code)
          then abort(WebsocketError(WebsocketError.Reason.BadClose))

          val reason = if payload.length > 2 then payload.drop(2) else Data()
          Close(code, reason)

        case other => abort(WebsocketError(WebsocketError.Reason.BadOpcode(other)))

  def unmask(bytes: Data, mask: Data): Data =
    if mask.length == 0 then bytes
    else Data.fill(bytes.length): index => (bytes(index)^mask(index%4)).toByte

enum Frame(val opcode: Int, val payload: Data):
  case Continuation(last: Boolean, data: Data) extends Frame(0x0, data)
  case Text(last: Boolean, data: Data)         extends Frame(0x1, data)
  case Binary(last: Boolean, data: Data)       extends Frame(0x2, data)
  case Close(code: Int, reason: Data)          extends Frame(0x8, Frame.closeData(code, reason))
  case Ping(data: Data)                        extends Frame(0x9, data)
  case Pong(data: Data)                        extends Frame(0xa, data)

  def fin: Boolean = this match
    case Continuation(last, _) => last
    case Text(last, _)         => last
    case Binary(last, _)       => last
    case _                     => true

  // Encode as a server-to-client (unmasked) frame.
  def encode: Data =
    val length = payload.length
    val flags = ((if fin then 0x80 else 0x00) | opcode).toByte

    val header: Data =
      if length <= 125 then Data(flags, length.toByte)
      else if length <= 0xffff then Data(flags, 126.toByte, (length >> 8).toByte, length.toByte)
      else
        Data
          ( flags, 127.toByte, 0.toByte, 0.toByte, 0.toByte, 0.toByte, (length >> 24).toByte,
            (length >> 16).toByte, (length >> 8).toByte, length.toByte )

    header ++ payload
