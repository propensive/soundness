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
package coaxial

import java.net as jn
import javax.net.ssl as jns

import anticipation.*
import contingency.*
import gigantism.*
import prepositional.*
import spectacular.*
import urticose.*
import vacuous.*
import zephyrine.*

import denominative.nil

// A TLS-secured listening TCP port — the server-side counterpart of `SecureEndpoint`, as
// `Tcp.Port` is of `Endpoint[Tcp.Port]`. Its `Bindable` opens an `SSLServerSocket` from the
// contextual `Tls`, whose `context` MUST carry the server's key material (see `Tls.keyed`): the
// JVM's default context has no private key, and a server bound with it would fail every
// handshake. Each accepted connection completes its handshake before it is lent to the
// handler, so a client that fails authentication or negotiation is reported as a
// `Socket.Error` with the `Handshake` reason — ending only that connection's task, as any
// other per-connection failure does — rather than surfacing as a truncated read in the handler.
// `Tls.protocols` (ALPN) and `Tls.versions` are applied as on the client side.
object SecurePort:
  given bindable: (options: Every[Socket.Option.Tcp], tls: Tls) => SecurePort is Bindable:
    type Binding = jns.SSLServerSocket
    type Input = Duplex
    type Output = Data

    def bind(port: SecurePort, interface: Optional[MacAddress]): Binding =
      val context = tls.context.or(jns.SSLContext.getDefault.nn)
      val factory = context.getServerSocketFactory.nn

      val address: Optional[jn.InetAddress] =
        interface.let(interfaceFor(_)).let(bindAddress(_))

      val socket: jns.SSLServerSocket =
        address.let(factory.createServerSocket(port.port.number, 50, _))
        . or(factory.createServerSocket(port.port.number))
        . nn
        . asInstanceOf[jns.SSLServerSocket]

      configure(socket, options.values.to(List))

      val parameters = socket.getSSLParameters.nn

      // stdlib bridge: the SSL parameter setters take Java arrays of *nullable* `String`, and no
      // `ClassTag` witnesses a union element, so the native `to[Array]` cannot build them.
      if !tls.protocols.nil
      then parameters.setApplicationProtocols(tls.protocols.stdlib.map(_.s).toArray)

      if !tls.versions.nil then parameters.setProtocols(tls.versions.stdlib.map(_.s).toArray)

      socket.setSSLParameters(parameters)
      socket

    def connect(binding: Binding): Duplex raises Socket.Error =
      val client: jns.SSLSocket =
        try binding.accept().nn.asInstanceOf[jns.SSLSocket]
        catch case _: java.io.IOException => abort(Socket.Error(Socket.Error.Reason.Accept))

      // The handshake is forced here so that its failure is this connection's, reported once,
      // and never mistaken for an empty request by the handler.
      try client.startHandshake()
      catch case _: java.io.IOException =>
        try client.close() catch case _: java.io.IOException => ()
        abort(Socket.Error(Socket.Error.Reason.Handshake))

      val negotiated: Optional[Text] = client.getApplicationProtocol match
        case null | "" => Unset
        case protocol  => protocol.tt

      streamsDuplex(client.getInputStream.nn, client.getOutputStream.nn, negotiated): () =>
        client.close()

    def transmit(binding: Binding, input: Duplex, bytes: Data): Unit raises Socket.Error =
      input.send(Stream(bytes))

    def close(connection: Duplex): Unit raises Socket.Error = connection.close()
    def stop(binding: Binding): Unit = binding.close()

  given showable: SecurePort is Showable = port => Text(s"tls:${port.port.number}")

case class SecurePort(port: Tcp.Port)
