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

import anticipation.*
import gigantism.*
import prepositional.*
import rudiments.*
import urticose.*
import vacuous.*

object Routable:
  given udpEndpoint: Every[SocketOption.Udp] => Endpoint[UdpPort] is Routable:
    case class Connection(address: jn.InetAddress, port: Int, socket: jn.DatagramSocket)

    def connect(endpoint: Endpoint[UdpPort], interface: Optional[MacAddress]): Connection =
      val address = jn.InetAddress.getByName(endpoint.remote.s).nn
      val socket = jn.DatagramSocket()
      configure(socket, summon[Every[SocketOption.Udp]].values)

      interface.let(interfaceFor(_)).let: nic =>
        socket.setOption(jn.StandardSocketOptions.IP_MULTICAST_IF, nic)

      Connection(address, endpoint.port.number, socket)

    def transmit(connection: Connection, input: LazyList[Data]): Unit =
      input.each: bytes =>
        val packet =
          jn.DatagramPacket
            ( bytes.mutable(using Unsafe), bytes.length, connection.address, connection.port )

        connection.socket.send(packet)

  given udpPort: Every[SocketOption.Udp] => UdpPort is Routable:
    case class Connection(port: Int, socket: jn.DatagramSocket)

    def connect(port: UdpPort, interface: Optional[MacAddress]): Connection =
      val socket = jn.DatagramSocket()
      configure(socket, summon[Every[SocketOption.Udp]].values)

      interface.let(interfaceFor(_)).let: nic =>
        socket.setOption(jn.StandardSocketOptions.IP_MULTICAST_IF, nic)

      Connection(port.number, socket)

    def transmit(connection: Connection, input: LazyList[Data]): Unit =
      input.each: bytes =>
        val packet =
          jn.DatagramPacket
            ( bytes.mutable(using Unsafe),
              bytes.length,
              jn.InetAddress.getLocalHost.nn,
              connection.port )

        connection.socket.send(packet)

trait Routable extends Typeclass:
  type Connection

  def connect(endpoint: Self, interface: Optional[MacAddress]): Connection
  def transmit(connection: Connection, input: LazyList[Data]): Unit
