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
┃    Soundness, version 0.39.0.                                                                    ┃
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
import gastronomy.*
import gossamer.*
import hypotenuse.*
import monotonous.*
import parasite.*
import prepositional.*
import proscenium.*
import rudiments.*
import symbolism.*
import telekinesis.*
import turbulence.*
import vacuous.*
import zephyrine.*

import alphabets.base64.standard

object Websocket:
  val magic: Text = t"258EAFA5-E914-47DA-95CA-C5AB0DC85B11"
  enum Opcode:
    case Continuation, Text, Binary, Reserved0, Reserved1, Reserved2, Reserved3, Reserved4, Close,
         Ping, Pong

  given servable: [ResultType] => Websocket[ResultType] is Servable:
    def serve(websocket: Websocket[ResultType]): Http.Response =
      given prefix: ("secWebsocketAccept" is Prefixable of Text) = identity(_)
      given prefix2: ("secWebsocketVersion" is Prefixable of Int) = _.toString.tt

      Http.Response
       (Http.SwitchingProtocols,
        secWebsocketAccept  = (websocket.key+Websocket.magic)
                              . digest[Sha1]
                              . serialize[Base64]
                              . keep(28),
        secWebsocketVersion = 13,
        transferEncoding    = TransferEncoding.Chunked,
        connection          = t"Upgrade",
        upgrade             = t"websocket")
       (websocket.spool.stream.map(_.bytes))

class Websocket[ResultType](request: Http.Request, handle: Stream[Frame] => ResultType)
       (using Monitor, Codicil):

  given prefix3: ("secWebsocketKey" is Prefixable of Text) = identity(_)
  val key: Text = request.headers.secWebsocketKey.prim.or(panic(m"Missing header"))
  private val spool: Spool[Frame] = Spool()

  val task: Task[ResultType] = async(handle(events()))

  def unmask(bytes: Bytes, mask: Bytes): Bytes = Bytes.fill(bytes.length): index =>
    (bytes(index)^mask(index%4)).toByte

  def events(): Stream[Frame] =
    lazy val conduit: Conduit = Conduit(request.body())

    def recur(): Stream[Frame] =
      val head = conduit.datum
      val fin = (head & 128) == 128
      val opcode = Websocket.Opcode.fromOrdinal(head & 16)
      conduit.next()

      val length = (conduit.datum&bin"01111111") match
        case 127   => B64(conduit.take(8)).s64.long.toInt
        case 126   => B16(conduit.take(2)).s16.int
        case count => count

      val mask = conduit.take(4)
      val payload = unmask(conduit.take(length), mask)

      opcode match
        case Websocket.Opcode.Continuation =>
          Frame.Continuation(fin, payload) #:: recur()
        case Websocket.Opcode.Text         =>
          Frame.Text(fin, payload) #:: recur()
        case Websocket.Opcode.Binary       =>
          Frame.Binary(fin, payload) #:: recur()
        case Websocket.Opcode.Ping         =>
          spool.put(Frame.Pong(payload))
          recur()
        case Websocket.Opcode.Pong         =>
          recur()
        case Websocket.Opcode.Close        =>
          spool.put(Frame.Close(1000))
          spool.stop()
          Stream()

    recur()
