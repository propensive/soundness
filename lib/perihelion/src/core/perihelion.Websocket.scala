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
import alphabets.base64Standard
import charEncoders.utf8Encoder
import crypto.permitDeprecatedCrypto
import providers.javaStdlibProvider

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

  // The raw payload of a message: a Text message's UTF-8 bytes, or a Binary
  // message's bytes verbatim. Used to decode an incoming message to a typed value.
  private[perihelion] def bytes: Data = this match
    case Text(text)   => text.data
    case Binary(data) => data

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

    // Validate `data` as UTF-8. `whole` marks a complete message, where a
    // trailing partial multi-byte sequence is an error; for a fragment prefix it
    // is tolerated, since a code point may straddle a fragment boundary. Used
    // incrementally so invalid bytes fail the connection at once (RFC 6455 §8.1),
    // not only once the whole message is in.
    def validUtf8(data: Data, whole: Boolean): Boolean =
      val decoder = java.nio.charset.StandardCharsets.UTF_8.nn.newDecoder().nn
      val in = java.nio.ByteBuffer.wrap(data.mutable(using Unsafe)).nn
      val out = java.nio.CharBuffer.allocate(data.length + 1).nn
      !decoder.decode(in, out, whole).nn.isError

    def emit(text: Boolean, data: Data): Message =
      if text then Message.Text(data.utf8) else Message.Binary(data)

    // Extend a (new or in-progress) message by `data`, validating a text message
    // incrementally and emitting it once `fin` is seen.
    def extend(text: Boolean, data: Data, fin: Boolean): Stream[Message] =
      if text && !validUtf8(data, fin)
      then abort(WebsocketError(WebsocketError.Reason.InvalidText))

      if fin then emit(text, data) #:: recur(Unset) else recur((text, data))

    // A data frame arriving mid-message (a fragmented message is still open) is a
    // protocol violation, as is a continuation with nothing to continue.
    def started(fin: Boolean, text: Boolean, data: Data, partial: Optional[(Boolean, Data)])
    :   Stream[Message] =

      if partial.present then abort(WebsocketError(WebsocketError.Reason.BadFragmentation))
      else extend(text, data, fin)

    def recur(partial: Optional[(Boolean, Data)]): Stream[Message] =
      (Frame.parse(cursor): @unchecked) match
        case Unset =>
          Stream()

        case Frame.Ping(data) =>
          channel.enqueue(Frame.Pong(data).encode)
          recur(partial)

        case Frame.Pong(_) =>
          recur(partial)

        case Frame.Close(code, reason) =>
          if !validUtf8(reason, true) then abort(WebsocketError(WebsocketError.Reason.InvalidText))
          channel.enqueue(Frame.Close(if code == 1005 then 1000 else code, Data()).encode)
          channel.stop()
          Stream()

        case Frame.Text(fin, data)   => started(fin, true, data, partial)
        case Frame.Binary(fin, data) => started(fin, false, data, partial)

        case Frame.Continuation(fin, data) =>
          partial.lay(abort(WebsocketError(WebsocketError.Reason.BadFragmentation))):
            (text, accumulated) =>
              extend(text, accumulated ++ data, fin)

    recur(Unset)

object Websocket:
  val magic: Text = t"258EAFA5-E914-47DA-95CA-C5AB0DC85B11"

  given servable: [message, state] => Websocket[message, state] is Servable:
    def serve(websocket: Websocket[message, state]): Http.Response =
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
class Websocket[message, state]
  ( request: Http.Request,
    initial: state,
    decode:  Message => message,
    frame:   Data => Data,
    handle:  (state: state) ?=> message => Control[state] )
  ( using Monitor, Probate ):

  given key0: ("secWebsocketKey" is Directive of Text) = identity(_)

  val key: Text =
    request.headers.secWebsocketKey.prim.or(panic(m"the Sec-WebSocket-Key was missing"))

  val channel: Channel = Channel()

  private def loop(messages: Stream[Message], state: state): state = messages.flow(close(state)):
    handle(using state)(decode(next)) match
      case Continue(state2) =>
        loop(more, state2.or(state))

      case Terminate =>
        channel.stop()
        state

      case Reply(bytes, state2) =>
        channel.enqueue(frame(bytes))
        loop(more, state2.or(state))

      case Conclude(bytes, state2) =>
        channel.enqueue(frame(bytes))
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
