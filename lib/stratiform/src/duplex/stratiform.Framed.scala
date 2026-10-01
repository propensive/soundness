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
import coaxial.*
import contingency.*
import obligatory.*
import prepositional.*
import rudiments.*
import zephyrine.*

// BinTEL messages over a `Duplex`: each a record framed by its four-byte big-endian length
// (`obligatory.LengthPrefix`) whose body is a value's BinTEL encoding — the framing a daemon
// speaks over TLS to its peers and over a UNIX domain socket to a local tool. A `Framed from
// inbound to outbound` reads `inbound` values and sends `outbound` ones, so one end's `Framed
// from Request to Reply` meets the other's `Framed from Reply to Request`, and a symmetric
// protocol is `Framed from T to T`. Both schemas are derived once, when the connection is
// framed, rather than per message as `Bintel.read` and `value.bintel` derive them.
//
// The `Duplex` contract is a single reader, so `messages` is opened once and the same iterator
// is handed back thereafter. The tactics are taken when the connection is framed, not per
// message, because a framing or decoding failure surfaces lazily, as the iterator is pulled;
// the connection therefore captures them, and says so.
object Framed:
  def apply[inbound: Tel.Decodable, outbound: Tel.Encodable](duplex: Duplex)
    ( using inboundSchematic:  inbound is TelSchematic over Tels.Type,
            outboundSchematic: outbound is TelSchematic over Tels.Type )
    ( using buffering: Buffering,
            framing:   Tactic[Framing.Error],
            bintel:    Tactic[Bintel.Error],
            tel:       Tactic[Tel.Error] )
  :   (Framed from inbound to outbound)^{framing, bintel, tel} =

    new Framed:
      type Origin = inbound
      type Result = outbound

      private val inboundSchema: Tels = Tels.tels[inbound](Text("root"))
      private val outboundSchema: Tels = Tels.tels[outbound](Text("root"))

      // The one writer: `Duplex.send` leaves serialization to its caller, and two messages'
      // frames interleaved on the wire would be unreadable.
      private val writes: Mutex = Mutex()

      def send(message: outbound): Unit =
        val body: Data = message.encode.bintel(outboundSchema)
        writes(duplex.send(Stream(LengthPrefix.encode(body))))

      lazy val messages: Iterator[inbound]^{this} =
        duplex.source.chunks.frames[LengthPrefix].map: frame =>
          Bintel.present(Bintel.decode(frame, inboundSchema), inboundSchema).as[inbound]

      def close(): Unit = duplex.close()

trait Framed extends Original, Resultant:
  def send(message: Result): Unit
  def messages: Iterator[Origin]^{this}
  def close(): Unit
