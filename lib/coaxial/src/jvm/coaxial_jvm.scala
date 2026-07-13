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

import java.io as ji
import java.net as jn
import java.nio.ByteBuffer
import java.nio.channels as jnc
import java.nio.file as jnf

import anticipation.*
import contingency.*
import hypotenuse.*
import prepositional.*
import rudiments.*
import turbulence.*
import urticose.*
import vacuous.*
import zephyrine.*

// The `java.net`/`java.nio.channels` implementation of `SocketBackend`, split out of `coaxial.core`
// so the platform-neutral socket API can cross-compile; other platforms (e.g. WASI) supply their
// own backend. Each handle type is a small ADT that preserves the exact Java representation each
// role needs — a TCP server binds a stream `ServerSocket` (which honours `setSoTimeout`), a
// Unix-domain server a `ServerSocketChannel`; a request/response exchange is a blocking `Socket`
// for TCP but a non-blocking `SocketChannel` for Unix-domain — so no per-role behaviour is lost.

// A bound, listening stream socket.
private enum ServerBinding:
  case Tcp(socket: jn.ServerSocket)
  case Domain(channel: jnc.ServerSocketChannel)

// One accepted server-side connection, written to and then closed.
private enum ServerLink:
  case Tcp(socket: jn.Socket)
  case Domain(connection: Connection)

// A request/response client connection.
private enum ClientExchange:
  case Tcp(socket: jn.Socket)
  case Domain(channel: jnc.SocketChannel)

// A fire-and-forget datagram destination.
private case class UdpCourier(address: jn.InetAddress, port: Int, socket: jn.DatagramSocket)

package socketBackends:
  given virtualMachine: SocketBackend = new SocketBackend:
    type ServerSocket = ServerBinding
    type ServerChannel = ServerLink
    type DatagramSocket = jn.DatagramSocket
    type Exchange = ClientExchange
    type Courier = UdpCourier

    // A pull endpoint over a readable channel: refill reads directly into the endpoint's buffer,
    // blocking until data arrives; EOF (`-1`) runs `onEnd` and terminates; an I/O failure aborts
    // with the bytes read so far.
    private def channelSource(channel: jnc.ReadableByteChannel)(onEnd: () -> Unit)
      ( using buffering: Buffering, tactic: Tactic[StreamError] )
    :   (Stream[Data] over Credit)^{tactic, caps.any} =

      new Stream[Data]:
        type Transport = Credit

        private val capacity: Int = buffering.capacity(Substrate.Bytes)
        private val storage: Array[Byte] = new Array[Byte](capacity)
        private val wrapped: ByteBuffer = ByteBuffer.wrap(storage).nn
        private var start0: Int = 0
        private var limit0: Int = 0
        private var total: Long = 0
        private var ended: Boolean = false

        protected def window0: AnyRef = storage.asInstanceOf[AnyRef]
        def start: Int = start0
        def limit: Int = limit0
        update def skip(count: Int): Unit = start0 += count

        update def refill(demand: Credit): Optional[Int] =
          if limit0 > start0 then limit0 - start0
          else if ended then Unset
          else
            val granted = summon[Credit is Regulation].grant(demand)

            if granted == 0 then 0 else
              start0 = 0
              limit0 = 0
              wrapped.clear()
              wrapped.limit(capacity.min(granted))

              try channel.read(wrapped) match
                case -1 =>
                  ended = true
                  onEnd()
                  Unset

                case 0 =>
                  refill(demand)

                case count =>
                  total += count
                  limit0 = count
                  count

              catch case error: ji.IOException =>
                ended = true
                // Pre-read into a local: `abort`'s capture-polymorphic argument may
                // not hide this instance's state.
                val sent: Long = total
                abort(StreamError(sent.b))

    //── Stream server (`Bindable` over TCP / Unix-domain) ──────────────────────────────────────
    def listenTcp(port: TcpPort, interface: Optional[MacAddress], options: List[SocketOption])
    :   ServerBinding =

      val address: Optional[jn.InetAddress] = interface.let(interfaceFor(_)).let(bindAddress(_))

      val socket =
        address.let(jn.ServerSocket(port.number, 50, _)).or(jn.ServerSocket(port.number))

      configure(socket, options)
      ServerBinding.Tcp(socket)

    def listenDomain(address: DomainSocket, options: List[SocketOption]): ServerBinding =
      val socketAddress = jn.UnixDomainSocketAddress.of(address.address.s)

      val channel = jnc.ServerSocketChannel.open(jn.StandardProtocolFamily.UNIX).nn
      channel.configureBlocking(true)
      configure(channel, options)
      channel.bind(socketAddress)
      ServerBinding.Domain(channel)

    def accept(socket: ServerBinding): ServerLink raises ConnectionError = socket match
      case ServerBinding.Tcp(server) =>
        try ServerLink.Tcp(server.accept().nn)
        catch case _: ji.IOException => abort(ConnectionError(ConnectionError.Reason.Accept))

      case ServerBinding.Domain(channel) =>
        try
          val client: jnc.SocketChannel = channel.accept().nn
          val in = jnc.Channels.newInputStream(client).nn
          val out = jnc.Channels.newOutputStream(client).nn
          ServerLink.Domain(Connection(in, out))
        catch case _: ji.IOException => abort(ConnectionError(ConnectionError.Reason.Accept))

    def serve(channel: ServerLink, bytes: Data): Unit raises ConnectionError = channel match
      case ServerLink.Tcp(socket) =>
        try
          socket.getOutputStream.nn.write(bytes.mutable(using Unsafe))
          socket.getOutputStream.nn.flush()
        catch case _: ji.IOException => abort(ConnectionError(ConnectionError.Reason.Transmit))

      case ServerLink.Domain(connection) =>
        try
          connection.out.write(bytes.mutable(using Unsafe))
          connection.out.flush()
        catch case _: ji.IOException => abort(ConnectionError(ConnectionError.Reason.Transmit))

    def hangUp(channel: ServerLink): Unit raises ConnectionError = channel match
      case ServerLink.Tcp(socket) =>
        try socket.close()
        catch case _: ji.IOException => abort(ConnectionError(ConnectionError.Reason.Close))

      case ServerLink.Domain(connection) =>
        try
          connection.in.close()
          connection.out.close()
        catch case _: ji.IOException => abort(ConnectionError(ConnectionError.Reason.Close))

    def shutdown(socket: ServerBinding): Unit = socket match
      case ServerBinding.Tcp(server)     => server.close()
      case ServerBinding.Domain(channel) => channel.close()

    //── Datagram server (`Bindable` over UDP) ──────────────────────────────────────────────────
    def listenUdp(port: UdpPort, interface: Optional[MacAddress], options: List[SocketOption])
    :   jn.DatagramSocket =

      val socket = jn.DatagramSocket(port.number)
      configure(socket, options)

      interface.let(interfaceFor(_)).let: nic =>
        socket.setOption(jn.StandardSocketOptions.IP_MULTICAST_IF, nic)

      socket

    def receive(socket: jn.DatagramSocket): Packet raises ConnectionError =
      val array = new Array[Byte](1472)
      val packet = jn.DatagramPacket(array, 1472)

      try socket.receive(packet)
      catch case _: ji.IOException => abort(ConnectionError(ConnectionError.Reason.Accept))

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

    def reply(socket: jn.DatagramSocket, sender: Ipv4 | Ipv6, port: UdpPort, data: Data)
    :   Unit raises ConnectionError =

      val ip: jn.InetAddress = sender.absolve match
        case ip: (Ipv4 @unchecked) =>
          val array =
            Array[Byte](ip.byte0.toByte, ip.byte1.toByte, ip.byte2.toByte, ip.byte3.toByte)

          jn.InetAddress.getByAddress(array).nn

        case ip: Ipv6 =>
          val array: Array[Byte]^ =
            val high = ip.highBits.bits.bytes
            val bytes = new Array[Byte](16)
            var index = 0

            while index < 8 do
              bytes(index) = high(index)
              index += 1

            val low = ip.lowBits.bits.bytes

            while index < 16 do
              bytes(index) = low(index - 8)
              index += 1

            bytes

          jn.InetAddress.getByAddress(array).nn

      val packet = jn.DatagramPacket(data.mutable(using Unsafe), data.length, ip, port.number)

      try socket.send(packet)
      catch case _: ji.IOException => abort(ConnectionError(ConnectionError.Reason.Transmit))

    def unbind(socket: jn.DatagramSocket): Unit = socket.close()

    //── Request/response exchange (`Serviceable`) ──────────────────────────────────────────────
    def dialTcp
      ( endpoint: Endpoint[TcpPort], interface: Optional[MacAddress], options: List[SocketOption] )
    :   ClientExchange =

      val socket =
        interface.let(interfaceFor(_)).let(bindAddress(_)).let: local =>
          jn.Socket(jn.InetAddress.getByName(endpoint.remote.s), endpoint.port.number, local, 0)

        . or(jn.Socket(jn.InetAddress.getByName(endpoint.remote.s), endpoint.port.number))

      configure(socket, options)
      ClientExchange.Tcp(socket)

    def dialTcpPort(port: TcpPort, interface: Optional[MacAddress], options: List[SocketOption])
    :   ClientExchange =

      val socket =
        interface.let(interfaceFor(_)).let(bindAddress(_)).let: local =>
          jn.Socket(jn.InetAddress.getLocalHost.nn, port.number, local, 0)

        . or(jn.Socket(jn.InetAddress.getLocalHost.nn, port.number))

      configure(socket, options)
      ClientExchange.Tcp(socket)

    def dialDomain(address: DomainSocket, options: List[SocketOption]): ClientExchange =
      val socketAddress = jn.UnixDomainSocketAddress.of(jnf.Path.of(address.address.s))
      val channel = jnc.SocketChannel.open(jn.StandardProtocolFamily.UNIX).nn
      configure(channel, options)
      channel.connect(socketAddress)
      channel.configureBlocking(false)
      ClientExchange.Domain(channel)

    def request(exchange: ClientExchange, input: (Stream[Data] over Credit)^): Unit = exchange match
      case ClientExchange.Tcp(socket) =>
        val out = socket.getOutputStream.nn

        input.sweep: (storage, start, count) =>
          out.write(storage.asInstanceOf[Array[Byte]], start, count)
          out.flush()

      case ClientExchange.Domain(channel) =>
        input.sweep: (storage, start, count) =>
          channel.write(ByteBuffer.wrap(storage.asInstanceOf[Array[Byte]], start, count))

        channel.shutdownOutput()

    def response(exchange: ClientExchange)(using Buffering, Tactic[StreamError])
    :   (Stream[Data] over Credit)^{caps.any} =

      exchange match
        case ClientExchange.Tcp(socket) =>
          channelSource(jnc.Channels.newChannel(socket.getInputStream.nn).nn): () =>
            ()

        case ClientExchange.Domain(channel) =>
          channelSource(channel): () =>
            channel.shutdownInput()

    def hangUp(exchange: ClientExchange): Unit = exchange match
      case ClientExchange.Tcp(socket)     => socket.close()
      case ClientExchange.Domain(channel) => channel.close()

    //── Persistent duplex client (`Connectable`) ───────────────────────────────────────────────
    def duplexTcp
      ( endpoint: Endpoint[TcpPort], interface: Optional[MacAddress], options: List[SocketOption] )
    :   Duplex =

      val address = jn.InetSocketAddress(endpoint.remote.s, endpoint.port.number)
      val channel = jnc.SocketChannel.open().nn
      channel.configureBlocking(true)
      configure(channel, options)

      interface.let(interfaceFor(_)).let(bindAddress(_)).let: local =>
        channel.bind(jn.InetSocketAddress(local, 0))

      channel.connect(address)
      Duplex.channel(channel)

    def duplexDomain(address: DomainSocket, options: List[SocketOption]): Duplex =
      val socketAddress = jn.UnixDomainSocketAddress.of(jnf.Path.of(address.address.s))
      val channel = jnc.SocketChannel.open(jn.StandardProtocolFamily.UNIX).nn
      channel.configureBlocking(true)
      configure(channel, options)
      channel.connect(socketAddress)
      Duplex.channel(channel)

    //── Fire-and-forget datagram courier (`Routable`) ──────────────────────────────────────────
    def routeUdp
      ( endpoint: Endpoint[UdpPort], interface: Optional[MacAddress], options: List[SocketOption] )
    :   UdpCourier =

      val address = jn.InetAddress.getByName(endpoint.remote.s).nn
      val socket = jn.DatagramSocket()
      configure(socket, options)

      interface.let(interfaceFor(_)).let: nic =>
        socket.setOption(jn.StandardSocketOptions.IP_MULTICAST_IF, nic)

      UdpCourier(address, endpoint.port.number, socket)

    def routeUdpPort
      ( port: UdpPort, interface: Optional[MacAddress], options: List[SocketOption] )
    :   UdpCourier =

      val socket = jn.DatagramSocket()
      configure(socket, options)

      interface.let(interfaceFor(_)).let: nic =>
        socket.setOption(jn.StandardSocketOptions.IP_MULTICAST_IF, nic)

      UdpCourier(jn.InetAddress.getLocalHost.nn, port.number, socket)

    def dispatch(courier: UdpCourier, input: (Stream[Data] over Credit)^): Unit =
      val bytes = input.memoize

      val packet =
        jn.DatagramPacket(bytes.mutable(using Unsafe), bytes.length, courier.address, courier.port)

      courier.socket.send(packet)
