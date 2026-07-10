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

import java.security.SecureRandom

import anticipation.*
import coaxial.*
import contingency.*
import distillate.*
import gastronomy.*
import gigantism.*
import gossamer.*
import hieroglyph.*
import monotonous.*
import parasite.*
import prepositional.*
import rudiments.*
import spectacular.*
import telekinesis.*
import urticose.*
import vacuous.*

import alphabets.base64Standard
import crypto.permitDeprecatedCrypto
import providers.javaStdlibProvider

// A formal `Message is Ingressive`, so the raw `Message` type satisfies the
// `Ingressive` requirement `webSocket` places on its message type. A `Message`
// channel is decoded by identity — the reader already produced the `Message` — so
// this is never actually applied; it could not recover the Text/Binary distinction
// from raw bytes anyway.
given messageIngressive: Message is Ingressive = Message.Binary(_)

// `true` exactly when the message type is the raw `Message`. A match type reduces
// by subtyping — `Message` matches the first case, any other (e.g. `Ping over Json`)
// falls through to `false` — and `constValue` reads it back as a literal at the
// inline expansion of `webSocket`, where `message` is concrete.
private type RawMessage[message] <: Boolean = message match
  case Message => true
  case _       => false

// Handle an upgraded connection as a stateful WebSocket message loop, mirroring
// Coaxial's `react`. Each reassembled message is decoded to the handler's
// `message` type and passed to `handle` with the current `state`, returning a
// `Control`. The message type is inferred from the handler's parameter, so it is
// usually annotated there: the raw `Message` (text or binary) with `(message:
// Message) => …`, or any `Ingressive` type — a `Text`, a `Json`/`Tel` value, or a
// typed payload `(value: MyAdt over Json) => …`. Every reply is serialised (by its
// `Transmissible`) to a complete frame before reaching the loop, so the loop just
// spools it. The result is `Servable` (the `101` handshake response).
inline def webSocket[state](initial: state = ())[message]
  ( handle: (state: state) ?=> message => Control[state] )
  ( using ingressive: message is Ingressive )
  ( using request: Http.Request )
  ( using Monitor, Probate )
:   Websocket[message, state] =

  val decode: Message => message =
    inline if compiletime.constValue[RawMessage[message]]
    then ((incoming: Message) => incoming).asInstanceOf[Message => message]
    else (incoming: Message) => ingressive.deserialize(incoming.bytes)

  Websocket(request, initial, decode, handle)

// A reply value `MyAdt over Json`/`over Tel`: there is no automatic
// `Encodable in Json` ⇒ `Encodable in Text` bridge, so compose the value↔transport
// codec with the transport's own text codec, and wrap the text in a single (unmasked)
// Text frame. Every `Transmissible` in perihelion yields a complete frame, so the
// send path is uniform: the server spools it verbatim; a client masks it once at the
// `Channel` boundary.
given transmissible: [transport, value]
=>  ( format: transport is Encodable in Text, codec: value is Encodable in transport )
=>  CharEncoder
=>  (value over transport) is Transmissible =
  payload => LazyList(Frame.Text(true, format.encoded(codec.encoded(payload)).data).encode)

// The decode direction. The `Decodable in Text`/`in transport` instances are
// `Tactic`-conditional and don't resolve as nested given constraints, so we
// require a `Tactic[Exception]` directly (satisfied by `strategies.throwUnsafely`;
// `Tactic` is contravariant) and summon them in the body, where it is in scope.
given ingressive: [transport, value]
=>  ( format: transport is Decodable in Text, codec: value is Decodable in transport )
=>  ( CharDecoder, Tactic[Exception] )
=>  (value over transport) is Ingressive =
  bytes => codec.decoded(format.decoded(bytes.text)).over[transport]

// Tag a value with the transport format it should ride, so a reply resolves the
// `over`-composed `Transmissible`. `over` is a phantom type member, so this is a
// no-op cast: `Reply(response.over[Json], state)`.
extension [self](value: self)
  def over[transport]: self over transport = value.asInstanceOf[self over transport]

// A `ws://` or `wss://` URL. `Url` decoding is scheme-generic, so a `WsUrl` parses with
// no bespoke scheme machinery; the port defaults to 80 (`ws`) or 443 (`wss`) when the
// authority omits it. A `wss://` connection is opened over TLS (via Coaxial's
// `SecureEndpoint`), configured by the `Tls` capability in scope (system trust store and
// hostname verification by default).
type WsUrl = Url["ws" | "wss"]

// Split a byte stream at the first CRLFCRLF: returns the header block (up to and
// including the terminator) and the remaining stream (the trailing bytes of the chunk
// that completed it, then the untouched rest). Consumes only as many chunks as needed
// to see the terminator, so it never reads past the `101` into a not-yet-sent frame.
private def readHandshake(chunks: LazyList[Data], acc: Data): (Data, LazyList[Data]) =
  def crlfCrlf(data: Data): Int =
    def matches(i: Int): Boolean =
      data(i) == 13 && data(i + 1) == 10 && data(i + 2) == 13 && data(i + 3) == 10

    def recur(index: Int): Int =
      if index + 3 >= data.length then -1 else if matches(index) then index else recur(index + 1)

    recur(0)

  val marker = crlfCrlf(acc)

  if marker >= 0 then
    val leftover = acc.drop(marker + 4)
    (acc.take(marker + 4), if leftover.length > 0 then leftover #:: chunks else chunks)
  else if chunks.isEmpty then
    (acc, LazyList())
  else
    readHandshake(chunks.tail, acc ++ chunks.head)

// Makes a `WsUrl` a Coaxial client transport, so a WebSocket client is just Coaxial's
// own client loop: `url.react(initialState) { message => … }`, symmetric with the
// server's `webSocket(initial) { … }`. It is a `Duplexable` (not merely a `Serviceable`)
// because its `transmit` spools through a thread-safe `Channel`, so Coaxial's `exchange`
// (the `Sender`-carrying, full-duplex counterpart of `react`) also works — a client can
// send proactively (`url.exchange(state) { reply => … } { sender => sender.send(request) }`),
// not only in reply. `connect` opens the TCP connection and performs the RFC 6455 handshake;
// `receive` runs the shared `Reader` (Ping/Pong and Close handled there) and yields one
// reassembled message per element; `transmit` masks and spools at the `Channel` boundary.
// Client→server frames are masked with a fresh key (RFC 6455 §5.3); a masked server frame
// is rejected.
given wsClient: ( online:            Online,
                  monitor:           Monitor,
                  probate:           Probate,
                  options:           Every[SocketOption.Tcp],
                  tls:               Tls,
                  websocketError:    Tactic[WebsocketError],
                  httpResponseError: Tactic[HttpResponseError],
                  portError:         Tactic[PortError] )
=>  (((WsUrl is Duplexable) { type Output = Data; type Connection = WsConnection })
      ^{monitor, websocketError, httpResponseError, portError}) =
  // The client retains its `Monitor` (the frame pump daemon) and tactics, so the instance
  // is a capability — a given constructed from capabilities produces a capability (see
  // rep/DECISIONS.md).
  new Duplexable:
    type Self = WsUrl

    type Output = Data
    type Connection = WsConnection

    def connect(url: WsUrl, interface: Optional[MacAddress]): WsConnection =
      val secure: Boolean = url.scheme.name == t"wss"

      val host: Host = url.host.or:
        abort(WebsocketError(WebsocketError.Reason.Handshake(t"the URL had no host")))

      val defaultPort: Int = if secure then 443 else 80
      val portNumber: Int = url.authority.lay(defaultPort)(_.port.or(defaultPort))

      // `wss` connects over TLS (`SecureEndpoint`), `ws` over plain TCP; everything after is
      // transport-agnostic, over the `Duplex`.
      val duplex: Duplex =
        if secure then
          val endpoint = SecureEndpoint(host.show, portNumber)
          summon[SecureEndpoint is Connectable].connect(endpoint, interface)
        else
          val endpoint = Endpoint(host.show, Port[Tcp](portNumber))
          summon[Endpoint[TcpPort] is Connectable].connect(endpoint, interface)

      // RFC 6455 §4.1: a fresh 16-byte nonce, Base64-encoded, is the `Sec-WebSocket-Key`;
      // the server's `Sec-WebSocket-Accept` must echo `base64(sha1(key ++ magic))`.
      val nonce: Data =
        val bytes = new Array[Byte](16)
        SecureRandom().nextBytes(bytes)
        bytes.immutable(using Unsafe)

      val key: Text = nonce.serialize[Base64]

      val request: Http.Request =
        Http.Request
          ( Http.Get,
            1.1,
            host,
            url.requestTarget,
            List
              ( Http.Header(t"Connection", t"Upgrade"),
                Http.Header(t"Upgrade", t"websocket"),
                Http.Header(t"Sec-WebSocket-Key", key),
                Http.Header(t"Sec-WebSocket-Version", t"13") ),
            () => Http.emptyBody() )

      duplex.send(Http.Request.serialize(request))

      // Read the response headers up to the CRLFCRLF terminator *without* over-reading:
      // `Http.Response.parse` on a live socket eagerly refills one chunk past the headers,
      // which would block here, since a server that only sends the `101` has no frame to
      // send until we do. So split the header block off the stream and parse just that
      // finite slice; anything after the terminator is the first inbound frame bytes.
      val (headerBytes, inbound) = readHandshake(duplex.stream, Data())
      val response: Http.Response = Http.Response.parse(LazyList(headerBytes))

      if response.status != Http.SwitchingProtocols then
        abort(WebsocketError(WebsocketError.Reason.Handshake(t"the server did not upgrade")))

      given accept: ("secWebsocketAccept" is Directive of Text) = identity(_)
      val expected: Text = t"$key${Websocket.magic}".digest[Sha1].serialize[Base64].keep(28)

      if response.headers.secWebsocketAccept.prim != expected then
        abort(WebsocketError(WebsocketError.Reason.Handshake(t"the Sec-WebSocket-Accept was wrong")))

      val masking: Masking = Masking.Client()
      given Masking = masking
      val channel: Channel = Channel()

      // Once the handshake is read, one writer (this pump, draining the spool) and one
      // reader (`receive`, over `inbound`) share the connection.
      val pump: Daemon = daemon(duplex.send(channel.stream))

      WsConnection(duplex, channel, masking, inbound, pump)

    def receive(connection: WsConnection): LazyList[Data] =
      given Masking = connection.masking
      Reader(() => connection.inbound, connection.channel).messages.map(_.bytes)

    def transmit(connection: WsConnection, input: LazyList[Data]): Unit =
      input.each(connection.channel.enqueue(_))

    def close(connection: WsConnection): Unit =
      given Masking = connection.masking
      safely(connection.channel.enqueue(Frame.Close(1000, Data()).encode))
      connection.channel.stop()
      safely(connection.pump.attend())
      connection.duplex.close()
