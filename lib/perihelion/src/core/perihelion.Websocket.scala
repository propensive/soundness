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
┃    Soundness, version 0.54.0.                                                                    ┃
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
import coaxial.*
import contingency.*
import fulminate.*
import gastronomy.*
import gossamer.*
import hieroglyph.*
import monotonous.*
import parasite.*
import prepositional.*
import rudiments.*
import telekinesis.*
import turbulence.*
import vacuous.*
import zephyrine.*

import Control.*
import alphabets.base64.standard
import charEncoders.utf8
import crypto.permitDeprecatedCrypto
import hashProviders.javaStdlibHashing

object Message:
  // A `Message` serialises to a complete (unmasked) WebSocket frame, so it can
  // flow through Coaxial's `Control.Reply`/`Conclude` and be written verbatim.
  given transmissible: Message is Transmissible =
    case Text(text)   => Stream(Frame.Text(true, text.data).encode)
    case Binary(data) => Stream(Frame.Binary(true, data).encode)

// A complete WebSocket message: text frames are reassembled and UTF-8-decoded;
// binary frames are reassembled as raw bytes. Control frames (Ping/Pong/Close)
// and fragmentation are handled by the library and never surface here.
enum Message:
  case Text(text: anticipation.Text)
  case Binary(data: Data)

// The outgoing side of a connection: a thread-safe spool of encoded frames that
// the server pumps to the socket as the `101` response body (mirroring Coaxial's
// `Duplex.send`). The reader and the handler both enqueue here.
class Channel():
  private val spool: Spool[Data] = Spool()

  private[perihelion] def enqueue(frame: Data): Unit = spool.put(frame)
  private[perihelion] def stream: Stream[Data] = spool.stream

  def send(message: Message): Unit = Message.transmissible.serialize(message).each(spool.put(_))
  def stop(): Unit = spool.stop()

  def close(code: Int = 1000): Unit =
    spool.put(Frame.Close(code, Data()).encode)
    spool.stop()

// Reads client frames off the connection, reassembles fragmented messages,
// answers Ping with Pong, and ends (stopping the outgoing side) when the peer
// sends Close. Protocol violations raise `WebsocketError`.
class Reader(body: () => Stream[Data], channel: Channel)(using Tactic[WebsocketError]):
  def messages: Stream[Message] =
    val cursor = Cursor(body().filter(_.nonEmpty).iterator)

    def message(text: Boolean, data: Data): Message =
      if text then Message.Text(data.utf8) else Message.Binary(data)

    def recur(partial: Optional[(Boolean, Data)]): Stream[Message] =
      Frame.parse(cursor) match
        case Unset =>
          Stream()

        case Frame.Ping(data) =>
          channel.enqueue(Frame.Pong(data).encode)
          recur(partial)

        case Frame.Pong(_) =>
          recur(partial)

        case Frame.Close(code, _) =>
          channel.enqueue(Frame.Close(if code == 1005 then 1000 else code, Data()).encode)
          channel.stop()
          Stream()

        case Frame.Text(fin, data) =>
          if fin then message(true, data) #:: recur(Unset) else recur((true, data))

        case Frame.Binary(fin, data) =>
          if fin then message(false, data) #:: recur(Unset) else recur((false, data))

        case Frame.Continuation(fin, data) =>
          partial.lay(abort(WebsocketError(WebsocketError.Reason.BadFragmentation))):
            (text, accumulated) =>
              val joined = accumulated ++ data
              if fin then message(text, joined) #:: recur(Unset) else recur((text, joined))

    recur(Unset)

object Websocket:
  val magic: Text = t"258EAFA5-E914-47DA-95CA-C5AB0DC85B11"

  given servable: [state] => Websocket[state] is Servable:
    def serve(websocket: Websocket[state]): Http.Response =
      given accept: ("secWebsocketAccept" is Directive of Text) = identity(_)
      given version: ("secWebsocketVersion" is Directive of Int) = _.toString.tt

      val acceptKey: Text =
        t"${websocket.key}${Websocket.magic}".digest[Sha1].serialize[Base64].keep(28)

      Http.Response
        ( Http.SwitchingProtocols,
          secWebsocketAccept  = acceptKey,
          secWebsocketVersion = 13,
          connection          = t"Upgrade",
          upgrade             = t"websocket" )
        ( Http.Body.Streaming(websocket.channel.stream) )

// The `Servable` carrier for a WebSocket handler. On serve it produces the `101`
// handshake response whose body is the outgoing frame stream; the handler runs
// concurrently on `task`, consuming reassembled messages and replying via
// Coaxial's `Control` until the peer closes, the handler concludes, or it errors.
class Websocket[state]
  ( request: Http.Request,
    initial: state,
    handle:  (state: state) ?=> Message => Control[state] )
  ( using Monitor, Codicil ):

  given key0: ("secWebsocketKey" is Directive of Text) = identity(_)

  val key: Text =
    request.headers.secWebsocketKey.prim.or(panic(m"the Sec-WebSocket-Key was missing"))

  val channel: Channel = Channel()

  private def loop(messages: Stream[Message], state: state): state = messages.flow(close(state)):
    handle(using state)(next) match
      case Continue(state2) =>
        loop(more, state2.or(state))

      case Terminate =>
        channel.stop()
        state

      case Reply(frame, state2) =>
        channel.enqueue(frame)
        loop(more, state2.or(state))

      case Conclude(frame, state2) =>
        channel.enqueue(frame)
        channel.stop()
        state2.or(state)

  private def close(state: state): state =
    channel.stop()
    state

  val task: Task[state] = async:
    whereas:
      case error: WebsocketError =>
        safely(channel.close(error.reason.closeCode))
        initial

    . recover:
        loop(Reader(() => request.body(), channel).messages, initial)
