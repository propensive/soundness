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

import scala.caps

import anticipation.*
import coaxial.*
import contingency.*
import parasite.*
import prepositional.*
import rudiments.*
import zephyrine.*

// A live WebSocket client connection: the underlying byte `Duplex`, the outgoing frame
// `Channel` (masking each frame with `Masking.Client`), the reassembled inbound frame
// stream left after the `101` handshake, and the background pump copying spooled frames
// onto the socket. It is the `Connection` type of the `WsUrl is Duplexable` instance
// (`wsClient`), so a client is driven by Coaxial's `react`/`exchange` — or lent directly
// by a session scope (`url.session`), whose free-form send-and-read style these methods
// serve.
class WsConnection
  ( private[perihelion] val duplex:  Duplex,
    private[perihelion] val channel: Channel,
    private[perihelion] val masking: Masking,
    // The connection's pull endpoint (a neutral `AnyRef` carrier for the exclusive
    // `Stream[Data] over Credit`), already advanced past the `101` handshake.
    private[perihelion] val inbound: AnyRef,
    private[perihelion] val pump:    Daemon ):

  // The reassembled inbound messages, one element per complete message: Ping/Pong and
  // Close are handled by the shared `Reader`, and chunk boundaries frame messages.
  def messages(using Tactic[WebsocketError]): (Stream[Data] over Credit)^{this, caps.any} =
    given Masking = masking

    val stream =
      Reader(() => inbound.asInstanceOf[(Stream[Data] over Credit)^], channel)
      . messages.map(_.bytes)

    Stream(stream.stdlib.iterator)

  // Sends one message as one complete frame, masked at the `Channel` boundary.
  def send(consume message: (Stream[Data] over Credit)^): Unit =
    channel.enqueue(message.memoize)

  def close()(using Monitor^): Unit =
    given Masking = masking
    safely(channel.enqueue(Frame.Close(1000, Data()).encode))
    channel.stop()
    safely(pump.attend())
    duplex.close()
