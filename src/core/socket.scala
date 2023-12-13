/*
    Coaxial, version [unreleased]. Copyright 2023 Jon Pretty, Propensive OÜ.

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

import nettlesome.*
import parasite.*
import fulminate.*
import turbulence.*
import rudiments.*
import anticipation.*
import perforate.*

import java.net as jn
import java.io as ji

object UnixSocket

class SocketConnection()

case class BindError() extends Error(msg"the port was not available for binding")

object Connectable:
  given tcpPort(using Raises[StreamCutError]): Connectable[TcpPort] with
    type Input = LazyList[Bytes]
    type Output = LazyList[Bytes]
    type Connection = jn.Socket
    
    def connect(remote: Text, port: TcpPort): jn.Socket =
      jn.Socket(jn.InetAddress.getByName(remote.s), port.number)
    
    def send(socket: jn.Socket, hold: Promise[Unit], input: LazyList[Bytes]): Unit =
      val out = socket.getOutputStream.nn

      // We append this to the end of the stream to ensure that the connection stays open
      val end = LazyList.defer:
        safely(hold.await())
        LazyList(Bytes())

      (input #::: end).writeTo(socket.getOutputStream.nn)
    
    def receive(socket: jn.Socket): LazyList[Bytes] = socket.getInputStream.nn.stream[Bytes]

  given udpPort: Connectable[UdpPort] with
    type Input = Bytes
    type Output = Unit
    case class Connection(address: jn.InetAddress, port: Int, socket: jn.DatagramSocket)

    def connect(remote: Text, port: UdpPort): Connection =
      val address = jn.InetAddress.getByName(remote.s).nn
      Connection(address, port.number, jn.DatagramSocket())
    
    def send(connection: Connection, hold: Promise[Unit], input: Bytes): Unit =
      val packet = jn.DatagramPacket(input.mutable(using Unsafe), input.length, connection.address,
          connection.port)
      
      connection.socket.send(packet)
    
    def receive(connection: Connection): Unit = ()

trait Connectable[SocketType]:
  type Input
  type Output
  type Connection

  def connect(remote: Text, socket: SocketType): Connection
  def send(connection: Connection, hold: Promise[Unit], input: Input): Unit
  def receive(connection: Connection): Output

trait Bindable[SocketType]:
  private[coaxial] type Binding
  type Input
  type Output
  def bind(socket: SocketType): Binding
  def connect(binding: Binding): Input
  def process(binding: Binding, input: Input, output: Output): Unit
  def stop(binding: Binding): Unit

case class UdpPacket(data: Bytes, sender: Ipv4 | Ipv6, port: UdpPort)

case class TcpConnection(private[coaxial] val socket: jn.Socket):
  def stream: LazyList[Bytes] raises StreamCutError = Readable.inputStream.read(socket.getInputStream.nn)

enum UdpResponse:
  case Ignore
  case Reply(data: Bytes)

object Bindable:
  given tcpPort(using Raises[StreamCutError]): Bindable[TcpPort] with
    private[coaxial] type Binding = jn.ServerSocket
    type Output = LazyList[Bytes]
    type Input = TcpConnection
    
    def bind(port: TcpPort): Binding = jn.ServerSocket(port.number)
    def connect(binding: Binding): TcpConnection = TcpConnection(binding.accept().nn)
    
    def process(socket: jn.ServerSocket, connection: TcpConnection, response: LazyList[Bytes]): Unit =
      response.writeTo(connection.socket.getOutputStream.nn)

    def stop(binding: Binding): Unit = binding.close()

  given udpPort: Bindable[UdpPort] with
    private[coaxial] type Binding = jn.DatagramSocket
    type Output = UdpResponse
    type Input = UdpPacket
    
    def bind(port: UdpPort): Binding = jn.DatagramSocket(port.number)

    def connect(binding: Binding): UdpPacket =
      val array = new Array[Byte](1472)
      val packet = jn.DatagramPacket(array, 1472)
      val socket = binding.receive(packet)
      val address = packet.getSocketAddress.nn.asInstanceOf[jn.InetSocketAddress]
      val ip = address.getAddress.nn match
        case ip: jn.Inet4Address =>
          val bytes: Array[Byte] = ip.getAddress.nn
          Ipv4(bytes(0), bytes(1), bytes(2), bytes(3))
        case _                           => ??? // FIXME

      UdpPacket(array.slice(0, packet.getLength).immutable(using Unsafe), ip, UdpPort.unsafe(address.getPort))
    
    def process(socket: jn.DatagramSocket, input: UdpPacket, response: UdpResponse): Unit = response match
      case UdpResponse.Ignore => ()

      case UdpResponse.Reply(data) =>
        val sender = input.sender
        
        val ip: jn.InetAddress = input.sender match
          case ip: Ipv4 => jn.InetAddress.getByAddress(Array[Byte](ip.byte0.toByte, ip.byte1.toByte, ip.byte2.toByte, ip.byte3.toByte)).nn
          case _        => ??? // FIXME
        
        val packet = jn.DatagramPacket(data.mutable(using Unsafe), data.length, ip, input.port.number)
        socket.send(packet)
    
    def stop(binding: Binding): Unit = binding.close()

trait SocketService:
  def stop(): Unit

object Socket:
  def listen
      [SocketType]
      (socket: SocketType)
      [ResultType]
      (using bindable: Bindable[SocketType], monitor: Monitor)
      (fn: bindable.Input => bindable.Output)
      : SocketService raises BindError =

    val binding = bindable.bind(socket)
    var continue: Boolean = true
    
    val async = Async:
      while continue do
        val connection = bindable.connect(binding)
        Async(bindable.process(binding, connection, fn(connection)))

    new SocketService:
      def stop(): Unit =
        continue = false
        bindable.stop(binding)
        safely(async.await())

  def connect
      [RemoteType, SocketType]
      (remote: RemoteType, socket: SocketType)
      (using connectable: Connectable[SocketType])
      (using Remote[RemoteType])
      (input: connectable.Input)
      [ResultType]
      (handle: connectable.Output => ResultType)
      (using Monitor)
      : ResultType raises StreamCutError =
    val connection = connectable.connect(summon[Remote[RemoteType]].remoteName(remote), socket)
    val hold: Promise[Unit] = Promise()
    val sender = Async(connectable.send(connection, hold, input))
    
    handle(connectable.receive(connection)).also:
      hold.offer(())
      safely(sender.await())
