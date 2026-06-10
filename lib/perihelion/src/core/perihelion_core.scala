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
import distillate.*
import gossamer.*
import hieroglyph.*
import parasite.*
import prepositional.*
import telekinesis.*

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
// Coaxial's `exchange`. Each reassembled message is decoded to the handler's
// `message` type and passed to `handle` with the current `state`, returning a
// `Control`. The message type is inferred from the handler's parameter, so it is
// usually annotated there: the raw `Message` (text or binary) with `(message:
// Message) => …`, or any `Ingressive` type — a `Text`, a `Json`/`Tel` value, or a
// typed payload `(value: MyAdt over Json) => …`. A raw `Message` reply is written
// verbatim (already framed); any other reply is written as one Text frame. The
// result is `Servable` (the `101` handshake response).
inline def webSocket[state](initial: state = ())[message]
  ( handle: (state: state) ?=> message => Control[state] )
  ( using ingressive: message is Ingressive )
  ( using request: Http.Request )
  ( using Monitor, Codicil )
:   Websocket[message, state] =

  val decode: Message => message =
    inline if compiletime.constValue[RawMessage[message]]
    then ((incoming: Message) => incoming).asInstanceOf[Message => message]
    else (incoming: Message) => ingressive.deserialize(incoming.bytes)

  val frame: Data => Data =
    inline if compiletime.constValue[RawMessage[message]]
    then (data: Data) => data
    else (data: Data) => Frame.Text(true, data).encode

  Websocket(request, initial, decode, frame, handle)

// A reply value `MyAdt over Json`/`over Tel`: there is no automatic
// `Encodable in Json` ⇒ `Encodable in Text` bridge, so compose the value↔transport
// codec with the transport's own text codec. The result is UNFRAMED (raw text
// bytes); `webSocket`'s loop wraps it in a single Text frame.
given transmissible: [transport, value]
=>  ( format: transport is Encodable in Text, codec: value is Encodable in transport )
=>  CharEncoder
=>  (value over transport) is Transmissible =
  payload => Stream(format.encoded(codec.encoded(payload)).data)

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
