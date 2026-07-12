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

  def send(message: Message): Unit logs WebsocketEvent =
    Log.fine(WebsocketEvent.Sent(message.bytes.length))
    // One message serializes to one complete frame; see `Transmissible`.
    enqueue(Message.transmissible.serialize(message).memoize)

  def stop(): Unit = synchronized(intake.finish())

  def close(code: Int = 1000): Unit logs WebsocketEvent =
    Log.info(WebsocketEvent.Closed(code))
    enqueue(Frame.Close(code, Data()).encode)
    stop()

// Reads client frames off the connection, reassembles fragmented messages,
// answers Ping with Pong, and ends (stopping the outgoing side) when the peer
// sends Close. Protocol violations raise `WebsocketError`.
class Reader(body: Spring[Data]^, channel: Channel)(using Tactic[WebsocketError], Masking):
  def messages: LazyList[Message] =
    // Deferred: constructing a stream-backed `Cursor` performs its first
    // refill, which on a live connection blocks until bytes arrive. The
    // cursor is created only when the first message is forced, so a server
    // can send its `101` response (and a client its first frame) first.
    LazyList.defer:
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
        val in = java.nio.ByteBuffer.wrap(data.mutable(using Unsafe)).nn
        val out = java.nio.CharBuffer.allocate(data.length + 1).nn
        !decoder.decode(in, out, whole).nn.isError

      def emit(text: Boolean, data: Data): Message =
        if text then Message.Text(data.utf8) else Message.Binary(data)

      // Extend a (new or in-progress) message by `data`, validating a text message
      // incrementally and emitting it once `fin` is seen.
      def extend(text: Boolean, data: Data, fin: Boolean): LazyList[Message] =
        if text && !validUtf8(data, fin)
        then abort(WebsocketError(WebsocketError.Reason.InvalidText))

        if fin then emit(text, data) #:: recur(Unset) else recur((text, data))

      // A data frame arriving mid-message (a fragmented message is still open) is a
      // protocol violation, as is a continuation with nothing to continue.
      def started(fin: Boolean, text: Boolean, data: Data, partial: Optional[(Boolean, Data)])
      :   LazyList[Message] =

        if partial.present then abort(WebsocketError(WebsocketError.Reason.BadFragmentation))
        else extend(text, data, fin)

      def recur(partial: Optional[(Boolean, Data)]): LazyList[Message] =
        (Frame.parse(cursor): @unchecked) match
          case Unset =>
            LazyList()

          case Frame.Ping(data) =>
            channel.enqueue(Frame.Pong(data).encode)
            recur(partial)

          case Frame.Pong(_) =>
            recur(partial)

          case Frame.Close(code, reason) =>
            if !validUtf8(reason, true) then abort(WebsocketError(WebsocketError.Reason.InvalidText))
            channel.enqueue(Frame.Close(if code == 1005 then 1000 else code, Data()).encode)
            channel.stop()
            LazyList()

          case Frame.Text(fin, data)   => started(fin, true, data, partial)
          case Frame.Binary(fin, data) => started(fin, false, data, partial)

          case Frame.Continuation(fin, data) =>
            partial.lay(abort(WebsocketError(WebsocketError.Reason.BadFragmentation))):
              (text, accumulated) =>
                extend(text, caps.unsafe.unsafeAssumePure(accumulated ++ data), fin)

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
        case error: WebsocketError =>
          safely(channel0.close(error.reason.closeCode))
          initial0

      . protect:
          // Resolved locally: the class-level `Masking` given would re-capture
          // the instance under construction.
          given Masking = Masking.Server

          def loop(messages: LazyList[Message], state: state): state =
            messages.flow(channel0.stop() yet state):
              Log.fine(WebsocketEvent.Received(next.bytes.length))

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
