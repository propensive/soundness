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
import java.util as ju

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

// A bound, listening stream socket. (Native has no `ServerSocketChannel`, so — unlike the JVM
// backend — there is no Unix-domain variant here; the domain methods are unsupported.)
private enum ServerBinding:
  case Tcp(socket: jn.ServerSocket)

// A request/response client connection. (No Unix-domain variant; see `ServerBinding`.)
private enum ClientExchange:
  case Tcp(socket: jn.Socket)

// A fire-and-forget datagram destination.
private case class UdpCourier(address: jn.InetAddress, port: Int, socket: jn.DatagramSocket)

package socketBackends:
  given native: SocketBackend = new SocketBackend:
    type ServerSocket = ServerBinding
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

    // Unix-domain sockets need `UnixDomainSocketAddress` and `ServerSocketChannel`, neither of
    // which Scala Native's javalib provides; unsupported for now (TCP and UDP are the native cut).
    def listenDomain(address: DomainSocket, options: List[SocketOption]): ServerBinding =
      throw UnsupportedOperationException("Unix-domain sockets are unsupported on Scala Native")

    def accept(socket: ServerBinding): Duplex raises ConnectionError = socket match
      case ServerBinding.Tcp(server) =>
        try
          val client = server.accept().nn

          streamsDuplex(client.getInputStream.nn, client.getOutputStream.nn): () =>
            client.close()

        catch case _: ji.IOException => abort(ConnectionError(ConnectionError.Reason.Accept))

    def shutdown(socket: ServerBinding): Unit = socket match
      case ServerBinding.Tcp(server) => server.close()

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
      throw UnsupportedOperationException("Unix-domain sockets are unsupported on Scala Native")

    def request(exchange: ClientExchange, input: (Stream[Data] over Credit)^): Unit = exchange match
      case ClientExchange.Tcp(socket) =>
        val out = socket.getOutputStream.nn

        caps.unsafe.unsafeAssumePure(input).sweep: (storage, start, count) =>
          out.write(storage.asInstanceOf[Array[Byte]], start, count)
          out.flush()

    def response(exchange: ClientExchange)(using Buffering, Tactic[StreamError])
    :   (Stream[Data] over Credit)^{caps.any} =

      exchange match
        case ClientExchange.Tcp(socket) =>
          val source = channelSource(jnc.Channels.newChannel(socket.getInputStream.nn).nn): () =>
            ()

          caps.unsafe.unsafeAssumePure(source)

    def hangUp(exchange: ClientExchange): Unit = exchange match
      case ClientExchange.Tcp(socket) => socket.close()

    //── Persistent duplex client (`Connectable`) ───────────────────────────────────────────────
    def duplexTcp
      ( endpoint: Endpoint[TcpPort], interface: Optional[MacAddress], options: List[SocketOption] )
    :   Duplex =

      // The JVM backend uses a `SocketChannel`; native has none, so a plain blocking `Socket` and
      // its input/output streams back the duplex through `streamsDuplex` (as `Serviceable`'s TCP
      // path already does).
      val socket =
        interface.let(interfaceFor(_)).let(bindAddress(_)).let: local =>
          jn.Socket(jn.InetAddress.getByName(endpoint.remote.s), endpoint.port.number, local, 0)

        . or(jn.Socket(jn.InetAddress.getByName(endpoint.remote.s), endpoint.port.number))

      configure(socket, options)

      streamsDuplex(socket.getInputStream.nn, socket.getOutputStream.nn): () =>
        socket.close()

    def duplexDomain(address: DomainSocket, options: List[SocketOption]): Duplex =
      throw UnsupportedOperationException("Unix-domain sockets are unsupported on Scala Native")

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
      val bytes = caps.unsafe.unsafeAssumePure(input).memoize

      val packet =
        jn.DatagramPacket(bytes.mutable(using Unsafe), bytes.length, courier.address, courier.port)

      courier.socket.send(packet)


// Applies `SocketOption`s to freshly-constructed sockets and resolves a `MacAddress` to a network
// interface. The Java socket kinds share no common Scala interface for `setOption`/`setSoTimeout`,
// so each is adapted to a small `Configurable` and the option-mapping is written once. Options a
// particular socket does not support are silently skipped (guarded by `supportedOptions`), which
// is the runtime backstop for socket-kind nuances within a transport (e.g. a server socket has no
// `TCP_NODELAY`).
private[coaxial] trait Configurable:
  def supported: ju.Set[jn.SocketOption[?]]
  def soTimeout(milliseconds: Int): Unit
  def option[value](socketOption: jn.SocketOption[value], value: value): Unit

  def set[value](socketOption: jn.SocketOption[value], value: value): Unit =
    if supported.contains(socketOption) then option(socketOption, value)

private[coaxial] def applyOptions(options: List[SocketOption])(target: Configurable): Unit =
  import jn.StandardSocketOptions.*
  val yes: java.lang.Boolean = Boolean.box(true)

  options.each:
    case SocketOption.ReuseAddress          => target.set(SO_REUSEADDR.nn, yes)
    case SocketOption.ReusePort             => target.set(SO_REUSEPORT.nn, yes)
    case SocketOption.NoDelay               => target.set(TCP_NODELAY.nn, yes)
    case SocketOption.KeepAlive             => target.set(SO_KEEPALIVE.nn, yes)
    case SocketOption.Broadcast             => target.set(SO_BROADCAST.nn, yes)
    case SocketOption.ReceiveBuffer(n)      => target.set(SO_RCVBUF.nn, Int.box(n))
    case SocketOption.SendBuffer(n)         => target.set(SO_SNDBUF.nn, Int.box(n))
    case SocketOption.TrafficClass(n)       => target.set(IP_TOS.nn, Int.box(n))
    case SocketOption.Linger(seconds)       => target.set(SO_LINGER.nn, Int.box(seconds.or(-1)))
    case SocketOption.Timeout(milliseconds) => target.soTimeout(milliseconds)

private[coaxial] def configure(socket: jn.Socket, options: List[SocketOption]): Unit =
  applyOptions(options):
    new Configurable:
      def supported: ju.Set[jn.SocketOption[?]] = socket.supportedOptions.nn
      def soTimeout(milliseconds: Int): Unit = socket.setSoTimeout(milliseconds)

      def option[value](socketOption: jn.SocketOption[value], value: value): Unit =
        socket.setOption(socketOption, value)

private[coaxial] def configure(socket: jn.ServerSocket, options: List[SocketOption]): Unit =
  applyOptions(options):
    new Configurable:
      def supported: ju.Set[jn.SocketOption[?]] = socket.supportedOptions.nn
      def soTimeout(milliseconds: Int): Unit = socket.setSoTimeout(milliseconds)

      def option[value](socketOption: jn.SocketOption[value], value: value): Unit =
        socket.setOption(socketOption, value)

private[coaxial] def configure(socket: jn.DatagramSocket, options: List[SocketOption]): Unit =
  applyOptions(options):
    new Configurable:
      def supported: ju.Set[jn.SocketOption[?]] = socket.supportedOptions.nn
      def soTimeout(milliseconds: Int): Unit = socket.setSoTimeout(milliseconds)

      def option[value](socketOption: jn.SocketOption[value], value: value): Unit =
        socket.setOption(socketOption, value)

// Resolves a `MacAddress` to the local network interface whose hardware address matches, if any.
private[coaxial] def interfaceFor(mac: MacAddress): Optional[jn.NetworkInterface] =
  val target: Array[Byte] =
    Array(mac.byte0, mac.byte1, mac.byte2, mac.byte3, mac.byte4, mac.byte5).map(_.toByte)

  def recur(interfaces: ju.Enumeration[jn.NetworkInterface]): Optional[jn.NetworkInterface] =
    if !interfaces.hasMoreElements then Unset else
      val nic = interfaces.nextElement.nn
      val hardware = nic.getHardwareAddress

      if hardware != null && ju.Arrays.equals(hardware, target) then nic else recur(interfaces)

  recur(jn.NetworkInterface.getNetworkInterfaces.nn)

// Picks a bind address from a resolved interface, preferring an IPv4 address.
private[coaxial] def bindAddress(nic: jn.NetworkInterface): Optional[jn.InetAddress] =
  def recur(addresses: ju.Enumeration[jn.InetAddress], fallback: Optional[jn.InetAddress])
  :   Optional[jn.InetAddress] =

    if !addresses.hasMoreElements then fallback else
      val address = addresses.nextElement.nn

      if address.isInstanceOf[jn.Inet4Address] then address
      else recur(addresses, fallback.or(address))

  recur(nic.getInetAddresses.nn, Unset)

// Wraps a blocking `InputStream`/`OutputStream` pair — the shape a socket that has no
// `SocketChannel` exposes (e.g. an `SSLSocket`). `shutdown` closes the underlying
// resource. The read side blocks in `read` and copies each fill; EOF (`-1`) ends the
// stream. The stream/write shape mirrors `channelDuplex` and `Serviceable`'s socket path.
// `shutdown` is a pure function: its closures hold only untracked OS resources (like
// `channelDuplex`'s socket), so the `Duplex` remains capture-free under capture checking.
private[coaxial] def streamsDuplex(in: ji.InputStream, out: ji.OutputStream)(shutdown: () -> Unit)
:   Duplex =

  new Duplex:
    // Reads directly into the endpoint's own buffer; EOF (`-1`) ends the stream.
    def source(using buffering: Buffering): (Stream[Data] over Credit)^ =
      new Stream[Data]:
        type Transport = Credit

        private val capacity: Int = buffering.capacity(Substrate.Bytes)
        private val storage: Array[Byte] = new Array[Byte](capacity)
        private var start0: Int = 0
        private var limit0: Int = 0
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

              in.read(storage, 0, capacity.min(granted)) match
                case -1 =>
                  ended = true
                  Unset

                case 0 =>
                  refill(demand)

                case count =>
                  limit0 = count
                  count

    def send(consume data: (Stream[Data] over Credit)^): Unit =
      data.sweep: (storage, start, count) =>
        out.write(storage.asInstanceOf[Array[Byte]], start, count)
        out.flush()

    def close(): Unit = shutdown()
