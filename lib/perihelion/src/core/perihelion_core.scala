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

import scala.{caps, compiletime}
import proscenium.compat.*

import java.security.SecureRandom

import anticipation.*
import coaxial.*
import coaxial.socketBackends.virtualMachine
import contingency.*
import fulminate.Hazard
import distillate.*
import gastronomy.*
import gigantism.*
import gossamer.*
import hieroglyph.*
import monotonous.*
import parasite.*
import prepositional.*
import denominative.capped
import zephyrine.memoize
import rudiments.*
import spectacular.*
import telekinesis.*
import urticose.*
import vacuous.*

import alphabets.base64Standard
import crypto.permitDeprecatedCrypto
import providers.javaStdlibProvider

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
given overTransmissible: [transport, value]
=>  ( format: transport is Encodable in Text, codec: value is Encodable in transport )
=>  CharEncoder
=>  (value over transport) is Transmissible =
  payload => zephyrine.Stream(Frame.Text(true, format.encoded(codec.encoded(payload)).in[Data]).encode)

// The decode direction. The `Decodable in Text`/`in transport` instances are
// `Tactic`-conditional and don't resolve as nested given constraints, so we
// require a `Tactic[Hazard]` directly (satisfied by `strategies.throwUnsafely`;
// `Tactic` is contravariant) and summon them in the body, where it is in scope.
given overIngressive: [transport, value]
=>  ( format: transport is Decodable in Text, codec: value is Decodable in transport )
=>  ( CharDecoder, Tactic[Hazard] )
=>  (value over transport) is Ingressive =
  bytes => codec.decoded(format.decoded(summon[CharDecoder].decoded(bytes))).over[transport]

// Tag a value with the transport format it should ride, so a reply resolves the
// `over`-composed `Transmissible`. `over` is a phantom type member, so this is a
// no-op cast: `Reply(response.over[Json], state)`.
extension [self](value: self)
  def over[transport]: self over transport = value.asInstanceOf[self over transport]

// Reads the handshake header block — up to and including the CRLFCRLF terminator —
// from the connection's pull endpoint, consuming *exactly* the header bytes: anything
// after the terminator stays unread in the endpoint's window, so this never reads past
// the `101` into a not-yet-sent frame, and the endpoint continues with the first frame
// bytes. On EOF before the terminator, returns whatever arrived.
private def readHandshake(input: (zephyrine.Stream[Data] over zephyrine.Credit)^)
  ( using buffering: zephyrine.Buffering )
:   Data =

  def crlfCrlf(data: Data): Int =
    def matches(i: Int): Boolean =
      data.readUnchecked(i) == 13 && data.readUnchecked(i + 1) == 10 && data.readUnchecked(i + 2) == 13 && data.readUnchecked(i + 3) == 10

    def recur(index: Int): Int =
      if index + 3 >= data.length then -1 else if matches(index) then index else recur(index + 1)

    recur(0)

  val demand = zephyrine.Credit(buffering.capacity(zephyrine.Substrate.Bytes))

  def recur(acc: Data): Data = input.refill(demand) match
    case count: Int =>
      if count > 0 then
        val window = input.lend { region => range => region.materialize(range.capped(count)) }
        val acc2: Data = acc ++ window
        val marker = crlfCrlf(acc2)

        if marker >= 0 then
          input.skip(marker + 4 - acc.length)
          acc2.take(marker + 4)
        else
          input.skip(count)
          recur(acc2)
      else recur(acc)

    case _ =>
      acc

  recur(Data())

// Makes a `Websocket.Url` a Coaxial client transport, so a WebSocket client is just Coaxial's
// own client loop: `url.react(initialState) { message => … }`, symmetric with the
// server's `webSocket(initial) { … }`. It is a `Duplexable` (not merely a `Serviceable`)
// because its `transmit` spools through a thread-safe `Channel`, so Coaxial's `exchange`
// (the `Transmitter`-carrying, full-duplex counterpart of `react`) also works — a client can
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
                  websocketError:    Tactic[Websocket.Error],
                  httpResponseError: Tactic[HttpResponseError],
                  portError:         Tactic[PortError] )
=>  (((Websocket.Url is Duplexable) { type Output = Data; type Connection = Websocket.Connection })
      ^{online, monitor, websocketError, httpResponseError, portError}) =
  // The client retains its `Monitor` (the frame pump daemon) and tactics, so the instance
  // is a capability — a given constructed from capabilities produces a capability (see
  // rep/DECISIONS.md).
  new Duplexable:
    type Self = Websocket.Url

    type Output = Data
    type Connection = Websocket.Connection

    def connect(url: Websocket.Url, interface: Optional[MacAddress]): Websocket.Connection =
      val secure: Boolean = url.scheme.name == t"wss"

      val host: Host = url.host.or:
        abort(Websocket.Error(Websocket.Error.Reason.Handshake(t"the URL had no host")))

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
        val bytes = Array[Byte](16)
        SecureRandom().nextBytes(bytes.raw)
        Array.freeze(bytes)

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
      // A neutral reference: the endpoint is read here (for the handshake) and then
      // stored on the connection for the frame reader; a capability-typed binding
      // would be hidden from the later use by the statement rule.
      val inboundRef: AnyRef = duplex.source.asInstanceOf[AnyRef]

      val headerBytes =
        readHandshake(inboundRef.asInstanceOf[(zephyrine.Stream[Data] over zephyrine.Credit)^])

      val response: Http.Response = Http.Response.parse(Chain(headerBytes))

      if response.status != Http.SwitchingProtocols then
        abort(Websocket.Error(Websocket.Error.Reason.Handshake(t"the server did not upgrade")))

      given accept: ("secWebsocketAccept" is Directive of Text) = identity(_)
      val expected: Text = t"$key${Websocket.magic}".digest[Sha1].serialize[Base64].keep(28)

      if response.headers.secWebsocketAccept.prim != expected then
        abort(Websocket.Error(Websocket.Error.Reason.Handshake(t"the Sec-WebSocket-Accept was wrong")))

      val masking: Masking = Masking.Client()
      given Masking = masking
      val channel: Channel = Channel()

      // Once the handshake is read, one writer (this pump, draining the spool) and one
      // reader (`receive`, over `inbound`) share the connection.
      val pump: Daemon = daemon(duplex.send(channel.stream))

      Websocket.Connection(duplex, channel, masking, inboundRef, pump)

    def receive(connection: Websocket.Connection)
    :   (zephyrine.Stream[Data] over zephyrine.Credit)^{this, caps.any} =

      connection.messages

    def transmit
      ( connection: Websocket.Connection,
        consume input: (zephyrine.Stream[Data] over zephyrine.Credit)^ )
    :   Unit =

      // One `transmit` carries one message = one complete frame, masked at the
      // `Channel` boundary; see `Transmissible`.
      connection.send(input)

    def close(connection: Websocket.Connection): Unit = connection.close()

// A scoped WebSocket session: the connection — handshake completed, frame pump running —
// is lent to the lambda for free-form `send`/`messages` use, and closed (with a `1000`
// Close frame) when the scope ends. For state-machine-style consumption, `url.react` and
// `url.exchange` remain the natural forms; the session serves interactions that do not
// fit a per-message handler. A named instance class rather than an anonymous given: an
// anonymous subclass would freshen the capability types in its inferred `Result` member.
class WsSessional
  ( using duplexable: ((Websocket.Url is Duplexable) { type Output = Data
                                               type Connection = Websocket.Connection })^,
          monitor:    Monitor )
extends Sessional:
  type Self = Websocket.Url

  // A fresh capability (`^`, not `^{caps.any}`): each `session` call's handle is its own
  // existential, so returning it (or anything capturing it) from the block is a level
  // violation the capture checker rejects.
  type Result = Websocket.Connection^

  def session[result](target: Websocket.Url)(lambda: (session: Result) ?=> result): result =
    val connection = duplexable.connect(target, Unset)
    try lambda(using connection) finally connection.close()

given wsSessional: ( duplexable: ((Websocket.Url is Duplexable) { type Output = Data
                                                          type Connection = Websocket.Connection })^,
                     monitor:    Monitor )
=>  (WsSessional^{duplexable, monitor, caps.any}) =

  WsSessional()
