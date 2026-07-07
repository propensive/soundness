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
import java.nio.channels as jnc
import java.nio.file as jnf

import anticipation.*
import gigantism.*
import prepositional.*
import urticose.*
import vacuous.*

// Opens a persistent, bidirectional `Duplex` connection to an endpoint. The
// counterpart to `Serviceable` for protocols that keep a connection open and read
// and write concurrently (e.g. HTTP/2), rather than the single request/response
// exchange `Serviceable` models. Channels are opened in blocking mode so a read
// loop parks in `read` instead of busy-polling. The channel is opened unconnected
// so socket options (and, for TCP, a local interface bind) take effect before connect.
object Connectable:
  given domainSocket: Every[SocketOption.Domain] => DomainSocket is Connectable:
    // A Unix-domain socket has no network interface, so `interface` is not applicable here.
    def connect(domainSocket: DomainSocket, interface: Optional[MacAddress]): Duplex =
      val address = jn.UnixDomainSocketAddress.of(jnf.Path.of(domainSocket.address.s))
      val channel = jnc.SocketChannel.open(jn.StandardProtocolFamily.UNIX).nn
      channel.configureBlocking(true)
      configure(channel, summon[Every[SocketOption.Domain]].values)
      channel.connect(address)
      Duplex.channel(channel)

  given tcpEndpoint: (Online, Every[SocketOption.Tcp]) => Endpoint[TcpPort] is Connectable:
    def connect(endpoint: Endpoint[TcpPort], interface: Optional[MacAddress]): Duplex =
      val address = jn.InetSocketAddress(endpoint.remote.s, endpoint.port.number)
      val channel = jnc.SocketChannel.open().nn
      channel.configureBlocking(true)
      configure(channel, summon[Every[SocketOption.Tcp]].values)

      interface.let(interfaceFor(_)).let(bindAddress(_)).let: local =>
        channel.bind(jn.InetSocketAddress(local, 0))

      channel.connect(address)
      Duplex.channel(channel)

trait Connectable extends Typeclass:
  def connect(endpoint: Self, interface: Optional[MacAddress]): Duplex
