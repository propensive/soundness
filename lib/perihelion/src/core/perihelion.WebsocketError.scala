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
import fulminate.*

object WebsocketError:
  // Each reason carries the RFC 6455 close code the server sends before closing.
  enum Reason(val number: Int, val closeCode: Int) extends Clarification:
    case Unmasked                extends Reason(1, 1002)
    case BadOpcode(code: Int)    extends Reason(2, 1002)
    case BadControl              extends Reason(3, 1002)
    case BadFragmentation        extends Reason(4, 1002)
    case TooLarge(size: Long)    extends Reason(5, 1009)
    case InvalidText             extends Reason(6, 1007)
    case ReservedBits            extends Reason(7, 1002)
    case BadClose                extends Reason(8, 1002)
    case Masked                  extends Reason(9, 1002)
    case Handshake(detail: Text) extends Reason(10, 1002)

  given communicable: Reason is Communicable =
    case Reason.Unmasked          => m"the client sent an unmasked frame"
    case Reason.BadOpcode(code)   => m"the frame used the reserved opcode $code"
    case Reason.BadControl        => m"a control frame was fragmented or exceeded 125 bytes"
    case Reason.BadFragmentation  => m"the message fragmentation was invalid"
    case Reason.TooLarge(size)    => m"the frame payload of $size bytes exceeded the limit"
    case Reason.InvalidText       => m"a text frame contained invalid UTF-8"
    case Reason.ReservedBits      => m"a reserved header bit (RSV1/2/3) was set"
    case Reason.BadClose          => m"the close frame had a malformed payload or invalid code"
    case Reason.Masked            => m"the server sent a masked frame"
    case Reason.Handshake(detail) => m"the WebSocket handshake failed because $detail"

case class WebsocketError(reason: WebsocketError.Reason)(using Diagnostics)
extends Error(368, reason.number)(m"the WebSocket protocol was violated because $reason")
