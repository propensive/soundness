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

import anticipation.*
import contingency.*
import gigantism.*
import hypotenuse.*
import prepositional.*
import rudiments.*
import urticose.*
import vacuous.*

object Bindable:
  given domainSocket: Every[SocketOption.Domain] => DomainSocket is Bindable:
    type Binding = jnc.ServerSocketChannel
    type Output = Data
    type Input = Connection

    // A Unix-domain socket has no network interface, so `interface` is not applicable here.
    def bind(domainSocket: DomainSocket, interface: Optional[MacAddress]): jnc.ServerSocketChannel =
      val address = jn.UnixDomainSocketAddress.of(domainSocket.address.s)

      jnc.ServerSocketChannel.open(jn.StandardProtocolFamily.UNIX).nn.tap: channel =>
        channel.configureBlocking(true)
        configure(channel, summon[Every[SocketOption.Domain]].values)
        channel.bind(address)

    def connect(channel: jnc.ServerSocketChannel): Connection raises ConnectionError =
      try
        val clientChannel: jnc.SocketChannel = channel.accept().nn
        val in = jnc.Channels.newInputStream(clientChannel).nn
        val out = jnc.Channels.newOutputStream(clientChannel).nn

        Connection(in, out)
      catch case _: java.io.IOException => abort(ConnectionError(ConnectionError.Reason.Accept))

    def transmit(channel: jnc.ServerSocketChannel, connection: Connection, bytes: Data)
    :   Unit raises ConnectionError =

      try
        connection.out.write(bytes.mutable(using Unsafe))
        connection.out.flush()
      catch case _: java.io.IOException => abort(ConnectionError(ConnectionError.Reason.Transmit))

    def stop(channel: jnc.ServerSocketChannel): Unit =
      channel.close()

    def close(connection: Connection): Unit raises ConnectionError =
      try
        connection.in.close()
        connection.out.close()
      catch case _: java.io.IOException => abort(ConnectionError(ConnectionError.Reason.Close))

  given tcpPort: Every[SocketOption.Tcp] => TcpPort is Bindable:
    type Binding = jn.ServerSocket
    type Output = Data
    type Input = jn.Socket

    def bind(port: TcpPort, interface: Optional[MacAddress]): Binding =
      val address: Optional[jn.InetAddress] = interface.let(interfaceFor(_)).let(bindAddress(_))

      val socket =
        address.let(jn.ServerSocket(port.number, 50, _)).or(jn.ServerSocket(port.number))

      configure(socket, summon[Every[SocketOption.Tcp]].values)
      socket

    def connect(binding: Binding): jn.Socket raises ConnectionError =
      try binding.accept().nn
      catch case _: java.io.IOException => abort(ConnectionError(ConnectionError.Reason.Accept))

    def transmit(socket: jn.ServerSocket, input: Input, bytes: Data): Unit raises ConnectionError =
      try
        input.getOutputStream.nn.write(bytes.mutable(using Unsafe))
        input.getOutputStream.nn.flush()
      catch case _: java.io.IOException => abort(ConnectionError(ConnectionError.Reason.Transmit))

    def close(socket: jn.Socket): Unit raises ConnectionError =
      try socket.close()
      catch case _: java.io.IOException => abort(ConnectionError(ConnectionError.Reason.Close))

    def stop(socket: jn.ServerSocket): Unit = socket.close()

  given udpPort: Every[SocketOption.Udp] => UdpPort is Bindable:
    type Binding = jn.DatagramSocket
    type Output = UdpResponse
    type Input = Packet

    def bind(port: UdpPort, interface: Optional[MacAddress]): Binding =
      val socket = jn.DatagramSocket(port.number)
      configure(socket, summon[Every[SocketOption.Udp]].values)

      interface.let(interfaceFor(_)).let: nic =>
        socket.setOption(jn.StandardSocketOptions.IP_MULTICAST_IF, nic)

      socket

    def connect(binding: Binding): Packet raises ConnectionError =
      val array = new Array[Byte](1472)
      val packet = jn.DatagramPacket(array, 1472)

      try binding.receive(packet)
      catch case _: java.io.IOException => abort(ConnectionError(ConnectionError.Reason.Accept))

      val address = packet.getSocketAddress.nn.asInstanceOf[jn.InetSocketAddress]

      val ip = address.getAddress.nn.absolve match
        case ip: jn.Inet4Address =>
          val bytes: Array[Byte] = ip.getAddress.nn
          Ipv4(bytes(0), bytes(1), bytes(2), bytes(3))

        case ip: jn.Inet6Address =>
          val bytes: Array[Byte] = ip.getAddress.nn

          Ipv6
            ( Long(bytes.take(8).immutable(using Unsafe)),
              Long(bytes.drop(8).immutable(using Unsafe)) )

      Packet
        ( array.take(packet.getLength).immutable(using Unsafe),
          ip,
          Port.unsafe[Udp](address.getPort) )

    def transmit(socket: jn.DatagramSocket, input: Packet, response: UdpResponse)
    :   Unit raises ConnectionError =

      response match
        case UdpResponse.Ignore => ()

        case UdpResponse.Reply(data) =>
          val sender = input.sender

          val ip: jn.InetAddress = input.sender.absolve match
            case ip: (Ipv4 @unchecked) =>
              val array =
                Array[Byte](ip.byte0.toByte, ip.byte1.toByte, ip.byte2.toByte, ip.byte3.toByte)

              jn.InetAddress.getByAddress(array).nn

            case ip: Ipv6 =>
              val array =
                IArray.from(ip.highBits.bits.bytes ++ ip.lowBits.bits.bytes).mutable(using Unsafe)

              jn.InetAddress.getByAddress(array).nn

          val packet =
            jn.DatagramPacket(data.mutable(using Unsafe), data.length, ip, input.port.number)

          try socket.send(packet)
          catch
            case _: java.io.IOException => abort(ConnectionError(ConnectionError.Reason.Transmit))

    def stop(binding: Binding): Unit = binding.close()
    def close(input: Packet): Unit raises ConnectionError = ()

trait Bindable extends Typeclass:
  type Binding
  type Input
  type Output

  def bind(socket: Self, interface: Optional[MacAddress]): Binding
  def connect(binding: Binding): Input raises ConnectionError
  def transmit(binding: Binding, input: Input, output: Output): Unit raises ConnectionError
  def close(connection: Input): Unit raises ConnectionError
  def stop(binding: Binding): Unit
