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
package obligatory

import anticipation.*
import contingency.*
import gossamer.*
import prepositional.*
import turbulence.*
import vacuous.*
import zephyrine.*
import pneumatic.*

// gRPC's length-prefixed message framing (the same wire shape for every codec):
// each message is a 1-byte compression flag, a 4-byte big-endian length, then that
// many payload bytes. A flag of 1 means the payload is compressed with the call's
// `grpc-encoding` (gzip here). The `Framable` instance reassembles whole messages
// from the arbitrarily-chunked response body, reusing `Framable.frames` exactly as
// `LengthPrefix` does for the JSON-RPC stream framing.
object GrpcFraming:
  private def gzip(message: Data): Data = Gzip.compression.compress(Progression(message)).read[Data]
  private def gunzip(message: Data): Data =
    Gzip.compression.decompress(Progression(message)).read[Data]

  // Prefix one message for the wire, optionally gzip-compressing the payload.
  def encode(message: Data, compress: Boolean = false): Data =
    val payload = if compress then gzip(message) else message
    val length = payload.length

    val header: Data =
      IArray
        ( (if compress then 1 else 0).toByte,
          (length >>> 24).toByte,
          (length >>> 16).toByte,
          (length >>> 8).toByte,
          length.toByte )

    header ++ payload

  given framable: (tactic: Tactic[GrpcError])
  =>  ((Data is Framable by GrpcFraming)^{tactic}) = input =>
    def truncated(): Nothing =
      abort(GrpcError(Grpc.Status.Internal, t"the gRPC message frame was truncated"))

    val cursor = Cursor(input)

    // Read the 5-byte prefix: a compression flag plus a big-endian length. `Unset`
    // at a clean message boundary (the stream is exhausted) ends the iterator.
    def header: Optional[(Boolean, Int)] =
      cursor.lay(Unset): flag =>
        cursor.next()

        cursor.lay(truncated()): byte0 =>
          cursor.next()

          cursor.lay(truncated()): byte1 =>
            cursor.next()

            cursor.lay(truncated()): byte2 =>
              cursor.next()

              cursor.lay(truncated()): byte3 =>
                cursor.next()
                ( flag != 0,
                  byte0.asInstanceOf[Byte] << 24 | byte1.asInstanceOf[Byte] << 16
                    | byte2.asInstanceOf[Byte] << 8 | byte3.asInstanceOf[Byte] )

    Framable.frames[Data]:
      header.let: (compressed, length) =>
        val payload = cursor.take(truncated())(length)
        if compressed then gunzip(payload) else payload

sealed trait GrpcFraming
