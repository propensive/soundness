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

import nettlesome.*
import parasite.*
import hypotenuse.*
import fulminate.*
import hieroglyph.*
import spectacular.*
import turbulence.*
import gossamer.*
import rudiments.*
import vacuous.*
import anticipation.*
import contingency.*

import java.net as jn
import java.io as ji
import java.nio.channels as jnc
import java.nio.file as jnf

object UnixSocket

case class BindError() extends Error(msg"the port was not available for binding")

object DomainSocket:
  def apply[PathType: GenericPath](path: PathType): DomainSocket = DomainSocket(path.pathText)

case class DomainSocket(private[coaxial] val address: Text):
  def path[PathType: SpecificPath] = SpecificPath(address)

sealed trait Control[+StateType]

object Control:
  sealed trait Interactive

  case class Conclude[+StateType](message: Bytes, state: Optional[StateType]) extends Control[StateType]
  case object Terminate extends Control[Nothing]
  case class Continue[+StateType](state: Optional[StateType] = Unset) extends Control[StateType], Interactive
  
  case class Reply[+StateType](message: Bytes, state: Optional[StateType])
  extends Control[StateType], Interactive
  

  object Conclude:
    def apply
        [MessageType, StateType]
        (message: MessageType, state: Optional[StateType] = Unset)
        (using transmissible: Transmissible[MessageType])
        : Conclude[StateType] =
      Conclude(transmissible.serialize(message), state)

  object Reply:
    def apply
        [MessageType, StateType]
        (message: MessageType, state: Optional[StateType] = Unset)
        (using transmissible: Transmissible[MessageType])
        : Reply[StateType] =
      Reply(transmissible.serialize(message), state)

import Control.*

object Connectable:
  given domainSocket(using Raises[StreamError]): Connectable[DomainSocket] with
    type Output = Bytes
    case class Connection(channel: jnc.SocketChannel, in: ji.InputStream, out: ji.OutputStream)

    def connect(domainSocket: DomainSocket): Connection =
      val path = jnf.Path.of(domainSocket.address.s)
      val address = jn.UnixDomainSocketAddress.of(path)
      val channel = jnc.SocketChannel.open(jn.StandardProtocolFamily.UNIX).nn
      channel.connect(address)
      channel.finishConnect()
      val out = jnc.Channels.newOutputStream(channel).nn
      val in = jnc.Channels.newInputStream(channel).nn
      
      Connection(channel, in, out)

    def transmit(connection: Connection, input: Bytes): Unit =
      connection.out.write(input.mutable(using Unsafe))
      connection.out.flush()
      
    def receive(connection: Connection): LazyList[Bytes] =
      connection.in.stream[Bytes]
    
    def close(connection: Connection): Unit = connection.channel.close()

  given tcpEndpoint(using Online, Raises[StreamError]): Connectable[Endpoint[TcpPort]] with
    type Output = Bytes
    type Connection = jn.Socket
      
    def connect(endpoint: Endpoint[TcpPort]): jn.Socket =
      jn.Socket(jn.InetAddress.getByName(endpoint.remote.s), endpoint.port.number)
    
    def transmit(socket: jn.Socket, input: Bytes): Unit =
      val out = socket.getOutputStream.nn
      out.write(input.mutable(using Unsafe))
      out.flush()
    
    def close(socket: jn.Socket): Unit = socket.close()
    
    def receive(socket: jn.Socket): LazyList[Bytes] = socket.getInputStream.nn.stream[Bytes]
  
  given tcpPort(using Raises[StreamError]): Connectable[TcpPort] with
    type Output = Bytes
    type Connection = jn.Socket
    
    def connect(port: TcpPort): jn.Socket = jn.Socket(jn.InetAddress.getLocalHost.nn, port.number)
    def close(socket: jn.Socket): Unit = socket.close()
    def receive(socket: jn.Socket): LazyList[Bytes] = socket.getInputStream.nn.stream[Bytes]
    
    def transmit(socket: jn.Socket, input: Bytes): Unit =
      val out = socket.getOutputStream.nn
      out.write(input.mutable(using Unsafe))
      out.flush()

object Addressable:
  given udpEndpoint: Addressable[Endpoint[UdpPort]] with
    case class Connection(address: jn.InetAddress, port: Int, socket: jn.DatagramSocket)

    def connect(endpoint: Endpoint[UdpPort]): Connection =
      val address = jn.InetAddress.getByName(endpoint.remote.s).nn
      Connection(address, endpoint.port.number, jn.DatagramSocket())
    
    def transmit(connection: Connection, input: Bytes): Unit =
      val packet = jn.DatagramPacket(input.mutable(using Unsafe), input.length, connection.address,
          connection.port)
      
      connection.socket.send(packet)
  
  given udpPort: Addressable[UdpPort] with
    case class Connection(port: Int, socket: jn.DatagramSocket)

    def connect(port: UdpPort): Connection =
      Connection(port.number, jn.DatagramSocket())
    
    def transmit(connection: Connection, input: Bytes): Unit =
      val packet = jn.DatagramPacket(input.mutable(using Unsafe), input.length, jn.InetAddress.getLocalHost.nn,
          connection.port)
      
      connection.socket.send(packet)

trait Addressable[EndpointType]:
  type Connection

  def connect(endpoint: EndpointType): Connection
  def transmit(connection: Connection, input: Bytes): Unit

trait Connectable[EndpointType] extends Addressable[EndpointType]:
  def receive(connection: Connection): LazyList[Bytes]
  def close(connection: Connection): Unit

trait Bindable[SocketType]:
  type Binding
  type Input
  type Output
  
  def bind(socket: SocketType): Binding
  def connect(binding: Binding): Input
  def transmit(binding: Binding, input: Input, output: Output): Unit
  def close(connection: Input): Unit
  def stop(binding: Binding): Unit

case class UdpPacket(data: Bytes, sender: Ipv4 | Ipv6, port: UdpPort)

case class Connection(private[coaxial] val in: ji.InputStream, private[coaxial] val out: ji.OutputStream):
  def stream(): LazyList[Bytes] raises StreamError = in.stream[Bytes]

enum UdpResponse:
  case Ignore
  case Reply(data: Bytes)

object Bindable:
  given domainSocket(using Raises[StreamError]): Bindable[DomainSocket] with
    type Binding = jnc.ServerSocketChannel
    type Output = Bytes
    type Input = Connection

    def bind(domainSocket: DomainSocket): jnc.ServerSocketChannel =
      val address = jn.UnixDomainSocketAddress.of(domainSocket.address.s)
      jnc.ServerSocketChannel.open(jn.StandardProtocolFamily.UNIX).nn.tap: channel =>
        channel.configureBlocking(true)
        channel.bind(address)
        println("bound")
  
    def connect(channel: jnc.ServerSocketChannel): Connection =
      val clientChannel: jnc.SocketChannel = channel.accept().nn
      val in = jnc.Channels.newInputStream(clientChannel).nn
      val out = jnc.Channels.newOutputStream(clientChannel).nn
      println("connected")
      
      Connection(in, out)
    
    def transmit(channel: jnc.ServerSocketChannel, connection: Connection, bytes: Bytes): Unit =
      connection.out.write(bytes.mutable(using Unsafe))
      connection.out.flush()
      println("sent")
    
    def stop(channel: jnc.ServerSocketChannel): Unit =
      channel.close()
      println("stopped")
    
    def close(connection: Connection): Unit =
      connection.in.close()
      connection.out.close()
      println("closed")

  given tcpPort(using Raises[StreamError]): Bindable[TcpPort] with
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
        
      UdpPacket(array.slice(0, packet.getLength).immutable(using Unsafe), ip, UdpPort.unsafe(address.getPort))
    
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

trait Transmissible[-MessageType]:
  def serialize(message: MessageType): Bytes

object Transmissible:
  given bytes: Transmissible[Bytes] = identity(_)
  given text(using CharEncoder): Transmissible[Text] = _.bytes
  given encoder[MessageType: Encoder](using CharEncoder): Transmissible[MessageType] = _.encode.bytes

trait Receivable[+MessageType]:
  def deserialize(message: Bytes): MessageType

object Receivable:
  given bytes: Receivable[Bytes] = identity(_)
  given text(using CharDecoder): Receivable[Text] = _.text
  given decoder[MessageType: Decoder](using CharDecoder): Receivable[MessageType] = _.text.decodeAs[MessageType]

trait SocketService:
  def stop(): Unit



extension [SocketType](socket: SocketType)
  def listen
      [InputType]
      (using bindable: Bindable[SocketType], monitor: Monitor)
      [ResultType]
      (lambda: bindable.Input => bindable.Output)
      : SocketService raises BindError =

    val binding = bindable.bind(socket)
    var continue: Boolean = true
    
    val async = Async:
      while continue do
        val connection = bindable.connect(binding)
        Async(bindable.transmit(binding, connection, lambda(connection)))

    new SocketService:
      def stop(): Unit =
        continue = false
        bindable.stop(binding)
        safely(async.await())

extension [EndpointType](endpoint: EndpointType)
  def connect
      [StateType]
      (initialState: StateType)
      (initialMessage: Bytes = Bytes())
      (handle: (state: StateType) ?=> Bytes => Control[StateType])
      (using connectable: Connectable[EndpointType])
      : StateType =

    val connection = connectable.connect(endpoint)
    
    def recur(input: LazyList[Bytes], state: StateType): StateType = input match
      case head #:: tail => handle(using state)(head) match
        case Continue(state2) => recur(tail, state2.or(state))
        case Terminate        => state
        
        case Reply(message, state2) =>
          connectable.transmit(connection, message)
          recur(tail, state2.or(state))
        
        case Conclude(message, state2) =>
          connectable.transmit(connection, message)
          state2.or(state)
      
      case _ => state
    
    recur(connectable.receive(connection), initialState).also:
      connectable.close(connection)

  def transmit[MessageType]
      (message: MessageType)
      (using transmissible: Transmissible[MessageType], addressable: Addressable[EndpointType])
      (using Monitor)
      : Unit raises StreamError =
    addressable.transmit(addressable.connect(endpoint), transmissible.serialize(message))