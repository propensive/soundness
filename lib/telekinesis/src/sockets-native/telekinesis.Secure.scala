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
package telekinesis

import java.io as ji
import java.net as jn

import anticipation.*
import coaxial.*
import contingency.*
import fulminate.*
import gigantism.*
import gossamer.*
import rudiments.*
import spectacular.*
import urticose.*
import vacuous.*
import zephyrine.*

// Scala Native's `https` exchange for `httpBackends.soundnessHttp`: one HTTP/1.1 exchange over the
// OpenSSL connection coaxial's `SecureEndpoint` opens. There is no ALPN here and no HTTP/2 driver
// (the `Http2` stack is JVM-only), so `Connection: close` is sent and the connection closed after
// the response, exactly as the JVM's HTTP/1.1 fallback does.
private[telekinesis] object Secure:
  def exchange
    ( host:    Host,
      port:    Int,
      target:  Text,
      method:  Http.Method,
      headers: List[Http.Header],
      body:    Spring[Data] )
    ( using online: Online, options: Every[Socket.Option.Tcp], buffering: Buffering, tls: Tls )
    ( using Tactic[Connect.Error] )
  :   Http.Response =

    import Connect.Error.Reason.*

    val duplex: Duplex =
      mitigate:
        case error: Socket.Error => socketConnectError(error)

      . protect:
        SecureEndpoint.connectable(using online)(using options, tls)
        . connect(SecureEndpoint(host.show, port), Unset)

    val headers2: List[Http.Header] =
      if headers.exists(_.key.lower == t"connection") then headers
      else Http.Header(t"connection", t"close") :: headers

    val httpRequest = Http.Request(method, 1.1, host, target, headers2, body)

    try sequentialFetch(duplex, httpRequest) finally duplex.close()

// The `Connect.Error` for a connection the socket backend could not make, by its reason. The
// JVM build takes this from the `http2` module, which has no native build; this is its twin
// for the native sources, which the shared `telekinesis_sockets.scala` resolves to here.
private[telekinesis] def socketConnectError(error: Socket.Error): Connect.Error =
  import Connect.Error.Reason.*
  given Diagnostics = error.diagnostics

  error.reason match
    case Socket.Error.Reason.Dns       => Connect.Error(Dns)
    case Socket.Error.Reason.Refused   => Connect.Error(Refused)
    case Socket.Error.Reason.Timeout   => Connect.Error(Timeout)
    case Socket.Error.Reason.Handshake => Connect.Error(Ssl(Ssl.Reason.Handshake))
    case _                             => Connect.Error(Unknown)
