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
import proscenium.compat.*

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
  // A formal `Message is Ingressive`, so the raw `Message` type satisfies the
  // `Ingressive` requirement `webSocket` places on its message type. A `Message`
  // channel is decoded by identity — the reader already produced the `Message` — so
  // this is never actually applied; it could not recover the Text/Binary distinction
  // from raw bytes anyway. In the companion, so it resolves with no import.
  given ingressive: Message is Ingressive = Message.Binary(_)

  // A `Message` serialises to a complete (unmasked) WebSocket frame, so it can
  // flow through Coaxial's `Control.Reply`/`Conclude` and be written verbatim.
  given transmissible: Message is Transmissible =
    case Text(text)   => zephyrine.Stream(Frame.Text(true, text.in[Data]).encode)
    case Binary(data) => zephyrine.Stream(Frame.Binary(true, data).encode)

// A complete WebSocket message: text frames are reassembled and UTF-8-decoded;
// binary frames are reassembled as raw bytes. Control frames (Ping/Pong/Close)
// and fragmentation are handled by the library and never surface here.
enum Message:
  case Text(text: anticipation.Text)
  case Binary(data: Data)

  // The raw payload of a message: a Text message's UTF-8 bytes, or a Binary
  // message's bytes verbatim. Used to decode an incoming message to a typed value.
  private[perihelion] def bytes: Data = this match
    case Text(text)   => text.in[Data]
    case Binary(data) => data

// The outgoing side of a connection: a bounded, thread-safe conduit of encoded
// frames that the server pumps to the socket as the `101` response body
// (mirroring Coaxial's `Duplex.send`). The reader and the handler both enqueue
// here, so a full queue backpressures every producer.
class Channel()(using masking: Masking, buffering: Buffering):
  // The endpoints are held at an AnyRef rim: capture sets do not ride fields of
  // a shared front-end object. Each has one owner by construction: producers
  // serialize through this object's lock, and the server or client pump is the
  // single consumer of the reader endpoint.
  // Cast whole: a tuple binding of the two exclusive endpoints would place
  // both in one inferred type, which separation checking rejects.
  private val endpoints: (AnyRef, AnyRef) = Conduit[Data]().asInstanceOf[(AnyRef, AnyRef)]

  private def intake: (Intake[Data] over Credit)^ =
    endpoints(0).asInstanceOf[(Intake[Data] over Credit)^]

  // The single reader endpoint: consumed exactly once, by the pump that writes
  // the upgraded connection's outgoing bytes.
  private[perihelion] def stream: (Stream[Data] over Credit)^ =
    endpoints(1).asInstanceOf[(Stream[Data] over Credit)^]

  // Mask each frame once, here, on its way out: every frame reaching the conduit
  // is already complete and unmasked (a Reader auto-reply, a handler `Reply`, a
  // `send`, or a `close`), so a client masks it and a server passes it through.
  private[perihelion] def enqueue(frame: Data): Unit = synchronized:
    intake.put(masking.outbound(frame))
    // Flushed per frame: the conduit otherwise buffers a full block before
    // publishing, but an interactive protocol must deliver each frame promptly.
    intake.flush()

  def send(message: Message): Unit logs Websocket.Event =
    Log.fine(Websocket.Event.Sent(message.bytes.length))
    // One message serializes to one complete frame; see `Transmissible`.
    enqueue(Message.transmissible.serialize(message).memoize)

  def stop(): Unit = synchronized(intake.finish())

  def close(code: Int = 1000): Unit logs Websocket.Event =
    Log.info(Websocket.Event.Closed(code))
    enqueue(Frame.Close(code, Data()).encode)
    stop()

// Reads client frames off the connection, reassembles fragmented messages,
// answers Ping with Pong, and ends (stopping the outgoing side) when the peer
// sends Close. Protocol violations raise `Websocket.Error`.
class Reader(body: Spring[Data]^, channel: Channel)(using Tactic[Websocket.Error], Masking):
  def messages: Chain[Message] =
    // Deferred: constructing a stream-backed `Cursor` performs its first
    // refill, which on a live connection blocks until bytes arrive. The
    // cursor is created only when the first message is forced, so a server
    // can send its `101` response (and a client its first frame) first.
    Chain.defer:
      // A neutral reference with an inline accessor: the parsing defs below
      // close over the cursor, which a capability-typed binding would hide
      // from them under the statement rule.
      val cursorRef: AnyRef = Cursor[Data](body()).asInstanceOf[AnyRef]
      inline def cursor: Cursor[Data, {}]^ = cursorRef.asInstanceOf[Cursor[Data, {}]^]

      // Validate `data` as UTF-8. `whole` marks a complete message, where a
      // trailing partial multi-byte sequence is an error; for a fragment prefix it
      // is tolerated, since a code point may straddle a fragment boundary. Used
      // incrementally so invalid bytes fail the connection at once (RFC 6455 §8.1),
      // not only once the whole message is in.
      def validUtf8(data: Data, whole: Boolean): Boolean =
        val decoder = java.nio.charset.StandardCharsets.UTF_8.nn.newDecoder().nn
        // `wrap` yields a writable buffer, but the only operation on it is `decode`, which
        // reads.
        val in = java.nio.ByteBuffer.wrap(Array.unsafeJvm(data)).nn
        val out = java.nio.CharBuffer.allocate(data.length + 1).nn
        !decoder.decode(in, out, whole).nn.isError

      def emit(text: Boolean, data: Data): Message =
        if text then Message.Text(data.utf8) else Message.Binary(data)

      // Extend a (new or in-progress) message by `data`, validating a text message
      // incrementally and emitting it once `fin` is seen.
      def extend(text: Boolean, data: Data, fin: Boolean): Chain[Message] =
        if text && !validUtf8(data, fin)
        then abort(Websocket.Error(Websocket.Error.Reason.InvalidText))

        if fin then emit(text, data) #:: recur(Unset) else recur((text, data))

      // A data frame arriving mid-message (a fragmented message is still open) is a
      // protocol violation, as is a continuation with nothing to continue.
      def started(fin: Boolean, text: Boolean, data: Data, partial: Optional[(Boolean, Data)])
      :   Chain[Message] =

        if partial.present then abort(Websocket.Error(Websocket.Error.Reason.BadFragmentation))
        else extend(text, data, fin)

      def recur(partial: Optional[(Boolean, Data)]): Chain[Message] =
        (Frame.parse(cursor): @unchecked) match
          case Unset =>
            Chain()

          case Frame.Ping(data) =>
            channel.enqueue(Frame.Pong(data).encode)
            recur(partial)

          case Frame.Pong(_) =>
            recur(partial)

          case Frame.Close(code, reason) =>
            if !validUtf8(reason, true) then abort(Websocket.Error(Websocket.Error.Reason.InvalidText))
            channel.enqueue(Frame.Close(if code == 1005 then 1000 else code, Data()).encode)
            channel.stop()
            Chain()

          case Frame.Text(fin, data)   => started(fin, true, data, partial)
          case Frame.Binary(fin, data) => started(fin, false, data, partial)

          case Frame.Continuation(fin, data) =>
            partial.lay(abort(Websocket.Error(Websocket.Error.Reason.BadFragmentation))):
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
        // The channel's reader endpoint is a singleton: the upgrade body is
        // materialized exactly once, by the server's response writer.
        ( Http.Body.Flowing(() => websocket.channel.stream) )

  // WsUrl → Websocket.Url
  // A `ws://` or `wss://` URL. `Url` decoding is scheme-generic, so a `Websocket.Url` parses
  // with no bespoke scheme machinery; the port defaults to 80 (`ws`) or 443 (`wss`) when the
  // authority omits it. A `wss://` connection is opened over TLS (via Coaxial's
  // `SecureEndpoint`), configured by the `Tls` capability in scope (system trust store and
  // hostname verification by default).
  type Url = urticose.Url["ws" | "wss"]

  // WebsocketError → Websocket.Error
  object Error:
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

  case class Error(reason: Websocket.Error.Reason)(using Diagnostics)
  extends fulminate.Error(368, reason.number)(m"the WebSocket protocol was violated because $reason")

  // WebsocketEvent → Websocket.Event
  object Event:
    given communicable: Websocket.Event is Communicable =
      case Sent(bytes)     => m"sent a $bytes-byte websocket message"
      case Received(bytes) => m"received a $bytes-byte websocket message"
      case Closed(code)    => m"closed the websocket connection with code $code"

  enum Event:
    case Sent(bytes: Int) extends Websocket.Event, Log.Network, Log.Protocol
    case Received(bytes: Int) extends Websocket.Event, Log.Network, Log.Protocol
    case Closed(code: Int) extends Websocket.Event, Log.Network, Log.Protocol

  // WsConnection → Websocket.Connection
  // A live WebSocket client connection: the underlying byte `Duplex`, the outgoing frame
  // `Channel` (masking each frame with `Masking.Client`), the reassembled inbound frame
  // stream left after the `101` handshake, and the background pump copying spooled frames
  // onto the socket. It is the `Connection` type of the `Websocket.Url is Duplexable` instance
  // (`wsClient`), so a client is driven by Coaxial's `react`/`exchange` — or lent directly
  // by a session scope (`url.session`), whose free-form send-and-read style these methods
  // serve.
  class Connection
    ( private[perihelion] val duplex:  Duplex,
      private[perihelion] val channel: Channel,
      private[perihelion] val masking: Masking,
      // The connection's pull endpoint (a neutral `AnyRef` carrier for the exclusive
      // `Stream[Data] over Credit`), already advanced past the `101` handshake.
      private[perihelion] val inbound: AnyRef,
      private[perihelion] val pump:    Daemon ):

    // The reassembled inbound messages, one element per complete message: Ping/Pong and
    // Close are handled by the shared `Reader`, and chunk boundaries frame messages.
    def messages(using Tactic[Websocket.Error]): (Stream[Data] over Credit)^{this, caps.any} =
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

// The `Servable` carrier for a WebSocket handler. On serve it produces the `101`
// handshake response whose body is the outgoing frame stream; the handler runs
// concurrently on `task`, consuming reassembled messages and replying via
// Coaxial's `Control` until the peer closes, the handler concludes, or it errors.
class Websocket[message, state]
  ( request: Http.Request,
    initial: state,
    decode:  Message => message,
    handle:  (state: state) ?=> message => Control[state] )
  ( using Monitor, Probate ):

  // A server sends unmasked frames and requires the peer's to be masked.
  given Masking = Masking.Server

  given key0: ("secWebsocketKey" is Directive of Text) = identity(_)

  val key: Text =
    request.headers.secWebsocketKey.prim.or(panic(m"the Sec-WebSocket-Key was missing"))

  val channel: Channel = Channel()

  val task: Task[state] =
    // Bound before the fiber spawns: the async body must not capture the
    // instance under construction, so everything it needs becomes a local.
    val channel0 = channel
    val bodyRef: AnyRef = request.body.asInstanceOf[AnyRef]
    val initial0 = initial
    val decodeRef: AnyRef = decode.asInstanceOf[AnyRef]
    // A neutral reference: a capability-typed binding of the handler would hide
    // it from the recursive loop under the statement rule. Repackaged as a plain
    // curried function since a context function cannot be bound unapplied.
    val handleRef: AnyRef =
      { (s: state) => (m: message) => handle(using s)(m) }.asInstanceOf[AnyRef]

    async:
      recover:
        case error: Websocket.Error =>
          safely(channel0.close(error.reason.closeCode))
          initial0

      . protect:
          // Resolved locally: the class-level `Masking` given would re-capture
          // the instance under construction.
          given Masking = Masking.Server

          def loop(messages: Chain[Message], state: state): state =
            messages.flow(channel0.stop() yet state):
              Log.fine(Websocket.Event.Received(next.bytes.length))

              handleRef.asInstanceOf[state => message => Control[state]]
                (state)(decodeRef.asInstanceOf[Message => message](next)) match
                case Continue(state2) =>
                  loop(more, state2.or(state))

                case Terminate =>
                  channel0.stop()
                  state

                case Reply(bytes, state2) =>
                  channel0.enqueue(bytes)
                  loop(more, state2.or(state))

                case Conclude(bytes, state2) =>
                  channel0.enqueue(bytes)
                  channel0.stop()
                  state2.or(state)

          loop(Reader(bodyRef.asInstanceOf[Spring[Data]^], channel0).messages, initial0)
