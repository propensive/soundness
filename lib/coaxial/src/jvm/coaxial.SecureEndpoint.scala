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
  given connectable: (Online, Every[SocketOption.Tcp], Tls) => SecureEndpoint is Connectable:
    def connect(endpoint: SecureEndpoint, interface: Optional[MacAddress]): Duplex =
      val tls = summon[Tls]
      val context = tls.context.or(jns.SSLContext.getDefault.nn)
      val socket = context.getSocketFactory.nn.createSocket().nn.asInstanceOf[jns.SSLSocket]
      configure(socket, summon[Every[SocketOption.Tcp]].values)

      interface.let(interfaceFor(_)).let(bindAddress(_)).let: local =>
        socket.bind(jn.InetSocketAddress(local, 0))

      // SNI lets the server select its certificate; endpoint identification then checks
      // that certificate against the hostname (skipped only when verification is off).
      val params = socket.getSSLParameters.nn
      params.setServerNames(ju.List.of(jns.SNIHostName(endpoint.host.s)))
      if tls.verify then params.setEndpointIdentificationAlgorithm("HTTPS")
      socket.setSSLParameters(params)

      socket.connect(jn.InetSocketAddress(endpoint.host.s, endpoint.port))
      socket.startHandshake()

      streamsDuplex(socket.getInputStream.nn, socket.getOutputStream.nn): () =>
        socket.close()

case class SecureEndpoint(host: Text, port: Int)
