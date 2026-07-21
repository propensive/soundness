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
package perihelion

import java.security.SecureRandom

import anticipation.*
import rudiments.*
import vacuous.*

// Which side of a WebSocket connection this endpoint is, and hence how frames are
// masked. RFC 6455 §5.3: a client MUST mask every frame it sends with a fresh 32-bit
// key; a server MUST send unmasked and reject an unmasked client frame, and — the
// mirror image — a client MUST reject a masked server frame. Masking is applied once,
// on the outgoing side, at the `Channel` boundary (every element spooled there is
// already exactly one complete, unmasked frame); the incoming side (`Frame.parse`)
// enforces the expected direction.
object Masking:
  // The server side: send unmasked, require inbound frames to be masked.
  object Server extends Masking:
    def outbound(frame: Data): Data = frame
    def inbound: Boolean = true

  // The client side: mask each outgoing frame with a fresh key, and reject a masked
  // inbound (server) frame. One `SecureRandom` is kept per connection for the keys.
  class Client() extends Masking:
    private val random: SecureRandom = SecureRandom()

    def inbound: Boolean = false

    def outbound(frame: Data): Data =
      // A frame we generated is unmasked, so its header is 2 bytes plus the 0/2/8 bytes
      // of extended length; the mask bit (byte 1, high bit) is clear. Set it, splice in
      // a fresh 4-byte key, and XOR the payload (`Frame.unmask` is its own inverse).
      val headerLength = (frame(1).toInt & 0x7f) match
        case 126 => 4
        case 127 => 10
        case _   => 2

      val key: Data =
        val bytes = new Array[Byte](4)
        random.nextBytes(bytes)
        bytes.immutable(using Unsafe)

      val header: Data = Data.fill(headerLength): index =>
        if index == 1 then (frame(1).toInt | 0x80).toByte else frame(index)

      val prefix: Data = header ++ key
      val unmasked = Frame.unmask(frame.drop(headerLength), key)
      prefix ++ unmasked

trait Masking:
  // Mask a complete, self-generated (and therefore well-formed and unmasked) frame, or
  // return it unchanged for the server. `inbound` reports whether frames read from the
  // peer must be masked.
  def outbound(frame: Data): Data
  def inbound: Boolean
