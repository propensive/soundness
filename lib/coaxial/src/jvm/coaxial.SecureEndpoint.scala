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
package coaxial

import scala.caps

import java.net as jn
import java.util as ju
import javax.net.ssl as jns

import anticipation.*
import gigantism.*
import urticose.*
import vacuous.*

// A secure (TLS) TCP endpoint — a `host:port` reached over TLS, the counterpart of the
// plaintext `Endpoint[TcpPort]`. Its `Connectable` opens an `SSLSocket` (trust and keys
// from `Tls.context`, or the system default), sets SNI to `host`, verifies the peer
// hostname unless `Tls.verify` is off, then presents the socket's streams as a `Duplex`.
object SecureEndpoint:
  // Honestly tracked, like `Connectable.tcpEndpoint`: the instance is resolvable only with
  // `Online` permission, so it is a capability carrying that evidence in its capture set.
  given connectable: (online: Online)
  =>  (options: Every[SocketOption.Tcp], tls: Tls)
  =>  ((SecureEndpoint is Connectable)^{online, caps.any}) =

   new Connectable:
    type Self = SecureEndpoint

    def connect(endpoint: SecureEndpoint, interface: Optional[MacAddress]): Duplex =
      val context = tls.context.or(jns.SSLContext.getDefault.nn)
      val socket = context.getSocketFactory.nn.createSocket().nn.asInstanceOf[jns.SSLSocket]
      configure(socket, List.of(options.values))

      interface.let(interfaceFor(_)).let(bindAddress(_)).let: local =>
        socket.bind(jn.InetSocketAddress(local, 0))

      // SNI lets the server select its certificate; endpoint identification then checks
      // that certificate against the hostname (skipped only when verification is off).
      val params = socket.getSSLParameters.nn
      params.setServerNames(ju.List.of(jns.SNIHostName(endpoint.host.s)))
      if tls.verify then params.setEndpointIdentificationAlgorithm("HTTPS")

      // Offer the ALPN protocols (in preference order) so the peer can select the
      // application protocol during the handshake; the choice is read back below.
      if !tls.protocols.stdlib.isEmpty
      then params.setApplicationProtocols(tls.protocols.stdlib.map(_.s).toArray)

      // Restrict the TLS protocol versions when the configuration asks for it.
      if !tls.versions.stdlib.isEmpty then params.setProtocols(tls.versions.stdlib.map(_.s).toArray)

      socket.setSSLParameters(params)

      socket.connect(jn.InetSocketAddress(endpoint.host.s, endpoint.port))
      socket.startHandshake()

      // `getApplicationProtocol` is `""` when nothing was negotiated (and, defensively,
      // could be `null`); either way the transport carries no ALPN protocol.
      val negotiated: Optional[Text] = socket.getApplicationProtocol match
        case null | "" => Unset
        case protocol  => protocol.tt

      streamsDuplex(socket.getInputStream.nn, socket.getOutputStream.nn, negotiated): () =>
        socket.close()

case class SecureEndpoint(host: Text, port: Int)
