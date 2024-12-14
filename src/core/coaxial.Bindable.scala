/*
    Coaxial, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package coaxial

import anticipation.*
import contingency.*
import hypotenuse.*
import nettlesome.*
import rudiments.*
import turbulence.*
import vacuous.*

import java.net as jn
import java.nio.channels as jnc

import Control.*

trait Bindable[SocketType]:
  type Binding
  type Input
  type Output

  def bind(socket: SocketType): Binding
  def connect(binding: Binding): Input
  def transmit(binding: Binding, input: Input, output: Output): Unit
  def close(connection: Input): Unit
  def stop(binding: Binding): Unit

object Bindable:
  given domainSocket(using Tactic[StreamError]): Bindable[DomainSocket] with
    type Binding = jnc.ServerSocketChannel
    type Output = Bytes
    type Input = Connection

    def bind(domainSocket: DomainSocket): jnc.ServerSocketChannel =
      val address = jn.UnixDomainSocketAddress.of(domainSocket.address.s)
      jnc.ServerSocketChannel.open(jn.StandardProtocolFamily.UNIX).nn.tap: channel =>
        channel.configureBlocking(true)
        channel.bind(address)

    def connect(channel: jnc.ServerSocketChannel): Connection =
      val clientChannel: jnc.SocketChannel = channel.accept().nn
      val in = jnc.Channels.newInputStream(clientChannel).nn
      val out = jnc.Channels.newOutputStream(clientChannel).nn

      Connection(in, out)

    def transmit(channel: jnc.ServerSocketChannel, connection: Connection, bytes: Bytes): Unit =
      connection.out.write(bytes.mutable(using Unsafe))
      connection.out.flush()

    def stop(channel: jnc.ServerSocketChannel): Unit =
      channel.close()

    def close(connection: Connection): Unit =
      connection.in.close()
      connection.out.close()

  given tcpPort(using Tactic[StreamError]): Bindable[TcpPort] with
    type Binding = jn.ServerSocket
    type Output = Bytes
    type Input = jn.Socket

    def bind(port: TcpPort): Binding = jn.ServerSocket(port.number)

    def connect(binding: Binding): jn.Socket = binding.accept().nn

    def transmit(socket: jn.ServerSocket, input: Input, bytes: Bytes): Unit =
      input.getOutputStream.nn.write(bytes.mutable(using Unsafe))
      input.getOutputStream.nn.flush()

    def close(socket: jn.Socket): Unit = socket.close()
    def stop(socket: jn.ServerSocket): Unit = socket.close()

  given udpPort: Bindable[UdpPort] with
    type Binding = jn.DatagramSocket
    type Output = UdpResponse
    type Input = UdpPacket

    def bind(port: UdpPort): Binding = jn.DatagramSocket(port.number)

    def connect(binding: Binding): UdpPacket =
      val array = new Array[Byte](1472)
      val packet = jn.DatagramPacket(array, 1472)
      val socket = binding.receive(packet)
      val address = packet.getSocketAddress.nn.asInstanceOf[jn.InetSocketAddress]

      val ip = (address.getAddress.nn: @unchecked) match
        case ip: jn.Inet4Address =>
          val bytes: Array[Byte] = ip.getAddress.nn
          Ipv4(bytes(0), bytes(1), bytes(2), bytes(3))

        case ip: jn.Inet6Address =>
          val bytes: Array[Byte] = ip.getAddress.nn
          Ipv6(Long(bytes.take(8).immutable(using Unsafe)), Long(bytes.drop(8).immutable(using Unsafe)))

      UdpPacket(array.take(packet.getLength).immutable(using Unsafe), ip, UdpPort.unsafe(address.getPort))

    def transmit(socket: jn.DatagramSocket, input: UdpPacket, response: UdpResponse): Unit = response match
      case UdpResponse.Ignore => ()

      case UdpResponse.Reply(data) =>
        val sender = input.sender

        val ip: jn.InetAddress = (input.sender: @unchecked) match
          case ip: Ipv4 =>
            val array = Array[Byte](ip.byte0.toByte, ip.byte1.toByte, ip.byte2.toByte, ip.byte3.toByte)
            jn.InetAddress.getByAddress(array).nn

          case ip: Ipv6 =>
            val array = IArray.from(ip.highBits.bits.bytes ++ ip.lowBits.bits.bytes).mutable(using Unsafe)
            jn.InetAddress.getByAddress(array).nn

        val packet = jn.DatagramPacket(data.mutable(using Unsafe), data.length, ip, input.port.number)
        socket.send(packet)

    def stop(binding: Binding): Unit = binding.close()
    def close(input: UdpPacket): Unit = ()
