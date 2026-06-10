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
import distillate.*
import gossamer.*
import hieroglyph.*
import parasite.*
import prepositional.*
import telekinesis.*

// Handle an upgraded connection as a stateful WebSocket message loop, mirroring
// Coaxial's `exchange`: each reassembled message is passed to `handle` with the
// current `state`, returning a `Control` that continues, replies, concludes, or
// terminates the connection. The result is `Servable`, so it doubles as the
// handler's `Http.Response` (the `101` handshake).
def webSocket[state](initial: state)(handle: (state: state) ?=> Message => Control[state])
  ( using request: Http.Request )
  ( using Monitor, Codicil )
:   Websocket[Message, state] =

  Websocket(request, initial, message => message, data => data, handle)

// A typed WebSocket loop over a serialisation `transport` (e.g. `Json`, `Tel`):
// each incoming message is decoded `Text → transport → message`, and every reply
// is written as a single Text frame. Tag the reply with the transport —
// `Reply(response.over[Json], state)` — so it resolves the composed `Transmissible`
// below; the loop frames the unframed text bytes that produces.
def typedWebSocket[transport, message, state](initial: state)
  ( handle: (state: state) ?=> message => Control[state] )
  ( using fromText:      transport is Decodable in Text )
  ( using fromTransport: message is Decodable in transport )
  ( using CharDecoder )
  ( using request: Http.Request )
  ( using Monitor, Codicil )
:   Websocket[message, state] =

  Websocket
    ( request,
      initial,
      incoming => fromTransport.decoded(fromText.decoded(incoming.bytes.text)),
      bytes => Frame.Text(true, bytes).encode,
      handle )

// A reply value `MyAdt over Json`/`over Tel`: there is no automatic
// `Encodable in Json` ⇒ `Encodable in Text` bridge, so compose the value↔transport
// codec with the transport's own text codec. The result is UNFRAMED (raw text
// bytes); `typedWebSocket`'s loop wraps it in a single Text frame.
given transmissible: [transport, value]
=>  ( format: transport is Encodable in Text, codec: value is Encodable in transport )
=>  CharEncoder
=>  (value over transport) is Transmissible =
  payload => Stream(format.encoded(codec.encoded(payload)).data)

// Tag a value with the transport format it should ride, so a reply resolves the
// `over`-composed `Transmissible`. `over` is a phantom type member, so this is a
// no-op cast: `Reply(response.over[Json], state)`.
extension [self](value: self)
  def over[transport]: self over transport = value.asInstanceOf[self over transport]
