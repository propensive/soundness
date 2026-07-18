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

import scala.scalanative.unsafe.*

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

      // (Selecting the multicast interface needs `DatagramSocket.setOption`, absent from Scala
      // Native's javalib; unsupported for now — `interface` is ignored for UDP.)

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

      // (Selecting the multicast interface needs `DatagramSocket.setOption`, absent from Scala
      // Native's javalib; unsupported for now — `interface` is ignored for UDP.)

      UdpCourier(address, endpoint.port.number, socket)

    def routeUdpPort
      ( port: UdpPort, interface: Optional[MacAddress], options: List[SocketOption] )
    :   UdpCourier =

      val socket = jn.DatagramSocket()
      configure(socket, options)

      // (Selecting the multicast interface needs `DatagramSocket.setOption`, absent from Scala
      // Native's javalib; unsupported for now — `interface` is ignored for UDP.)

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
// Each socket kind supports a different subset of options through its own type-specific setters
// (Scala Native's javalib has no generic `setOption`/`supportedOptions`); a `Configurable` maps the
// common options to those setters, and an option a given socket kind does not support defaults to a
// no-op (`ReusePort` has no portable setter and is skipped everywhere).
private[coaxial] trait Configurable:
  def reuseAddress(): Unit = ()
  def noDelay(): Unit = ()
  def keepAlive(): Unit = ()
  def broadcast(): Unit = ()
  def receiveBuffer(bytes: Int): Unit = ()
  def sendBuffer(bytes: Int): Unit = ()
  def trafficClass(value: Int): Unit = ()
  def linger(seconds: Int): Unit = ()
  def soTimeout(milliseconds: Int): Unit = ()

private[coaxial] def applyOptions(options: List[SocketOption])(target: Configurable): Unit =
  options.each:
    case SocketOption.ReuseAddress          => target.reuseAddress()
    case SocketOption.ReusePort             => ()
    case SocketOption.NoDelay               => target.noDelay()
    case SocketOption.KeepAlive             => target.keepAlive()
    case SocketOption.Broadcast             => target.broadcast()
    case SocketOption.ReceiveBuffer(n)      => target.receiveBuffer(n)
    case SocketOption.SendBuffer(n)         => target.sendBuffer(n)
    case SocketOption.TrafficClass(n)       => target.trafficClass(n)
    case SocketOption.Linger(seconds)       => target.linger(seconds.or(-1))
    case SocketOption.Timeout(milliseconds) => target.soTimeout(milliseconds)

private[coaxial] def configure(socket: jn.Socket, options: List[SocketOption]): Unit =
  applyOptions(options):
    new Configurable:
      override def reuseAddress(): Unit = socket.setReuseAddress(true)
      override def noDelay(): Unit = socket.setTcpNoDelay(true)
      override def keepAlive(): Unit = socket.setKeepAlive(true)
      override def receiveBuffer(bytes: Int): Unit = socket.setReceiveBufferSize(bytes)
      override def sendBuffer(bytes: Int): Unit = socket.setSendBufferSize(bytes)
      override def trafficClass(value: Int): Unit = socket.setTrafficClass(value)
      override def linger(seconds: Int): Unit = socket.setSoLinger(seconds >= 0, seconds.max(0))
      override def soTimeout(milliseconds: Int): Unit = socket.setSoTimeout(milliseconds)

private[coaxial] def configure(socket: jn.ServerSocket, options: List[SocketOption]): Unit =
  applyOptions(options):
    new Configurable:
      override def reuseAddress(): Unit = socket.setReuseAddress(true)
      override def receiveBuffer(bytes: Int): Unit = socket.setReceiveBufferSize(bytes)
      override def soTimeout(milliseconds: Int): Unit = socket.setSoTimeout(milliseconds)

private[coaxial] def configure(socket: jn.DatagramSocket, options: List[SocketOption]): Unit =
  applyOptions(options):
    new Configurable:
      override def reuseAddress(): Unit = socket.setReuseAddress(true)
      override def broadcast(): Unit = socket.setBroadcast(true)
      override def receiveBuffer(bytes: Int): Unit = socket.setReceiveBufferSize(bytes)
      override def sendBuffer(bytes: Int): Unit = socket.setSendBufferSize(bytes)
      override def trafficClass(value: Int): Unit = socket.setTrafficClass(value)
      override def soTimeout(milliseconds: Int): Unit = socket.setSoTimeout(milliseconds)

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


// The command constants behind the OpenSSL macros re-expressed as `ctrl` calls above; their
// values are ABI (fixed in OpenSSL's public headers).
private[coaxial] val SSL_VERIFY_NONE: Int = 0
private[coaxial] val SSL_VERIFY_PEER: Int = 1
private[coaxial] val SSL_CTRL_SET_TLSEXT_HOSTNAME: Int = 55
private[coaxial] val TLSEXT_NAMETYPE_host_name: Long = 0L
private[coaxial] val BIO_C_SET_CONNECT: Int = 100
private[coaxial] val BIO_C_DO_STATE_MACHINE: Int = 101
private[coaxial] val BIO_C_GET_SSL: Int = 110

// The libssl surface: context and connection setup. All OpenSSL object types (`SSL_CTX`,
// `SSL`, `SSL_METHOD`, `BIO`) are opaque, so each is an untyped `Ptr[Byte]`; C `long` (and
// `unsigned long`) is declared as the ABI-identical `Long` — Scala Native's `CLong` is the
// word-sized `Size`, and every supported target is LP64. OpenSSL 1.1+ initializes itself on
// first use, so no explicit `OPENSSL_init_ssl` call is needed.
@extern @link("ssl")
private[coaxial] object libssl:
  def TLS_client_method(): Ptr[Byte] = extern
  def SSL_CTX_new(method: Ptr[Byte]): Ptr[Byte] = extern
  def SSL_CTX_free(context: Ptr[Byte]): Unit = extern
  def SSL_CTX_set_default_verify_paths(context: Ptr[Byte]): CInt = extern
  def SSL_CTX_set_verify(context: Ptr[Byte], mode: CInt, callback: Ptr[Byte]): Unit = extern
  def SSL_ctrl(ssl: Ptr[Byte], command: CInt, larg: Long, parg: Ptr[Byte]): Long = extern
  def SSL_set1_host(ssl: Ptr[Byte], hostname: CString): CInt = extern
  def BIO_new_ssl_connect(context: Ptr[Byte]): Ptr[Byte] = extern

// The libcrypto surface: the BIO I/O calls and the error queue.
@extern @link("crypto")
private[coaxial] object libcrypto:
  def BIO_ctrl(bio: Ptr[Byte], command: CInt, larg: Long, parg: Ptr[Byte]): Long = extern
  def BIO_read(bio: Ptr[Byte], buffer: Ptr[Byte], length: CInt): CInt = extern
  def BIO_write(bio: Ptr[Byte], buffer: Ptr[Byte], length: CInt): CInt = extern
  def BIO_free_all(bio: Ptr[Byte]): Unit = extern
  def ERR_get_error(): Long = extern
  def ERR_error_string(error: Long, buffer: Ptr[Byte]): CString = extern

// Renders OpenSSL's queued error (if any) for exception messages; `ERR_error_string` with a
// null buffer returns a static buffer.
private[coaxial] def opensslError(): String =
  val error = libcrypto.ERR_get_error()

  if error == 0L then "unknown error"
  else fromCString(libcrypto.ERR_error_string(error, null.asInstanceOf[Ptr[Byte]]))

// Presents a connected SSL BIO chain as a `Duplex`, mirroring `streamsDuplex`: the read side
// blocks in `BIO_read` directly into the endpoint's buffer, and a non-positive result (a
// close_notify or a transport failure) ends the stream. `close` frees the whole BIO chain —
// SSL, socket and all — and releases the connection's `SSL_CTX` reference.
private[coaxial] def bioDuplex(bio: Ptr[Byte], context: Ptr[Byte]): Duplex =
  new Duplex:
    def source(using buffering: Buffering): (Stream[Data] over Credit)^ =
      new Stream[Data]:
        type Transport = Credit

        private[coaxial] val capacity: Int = buffering.capacity(Substrate.Bytes)
        private[coaxial] val storage: Array[Byte] = new Array[Byte](capacity)
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

              // (`asInstanceOf` launders the field read's capture, as `window0` does, and
              // `atUnsafe` is Scala Native's array-to-pointer view — its bounds-checked `at`
              // is shadowed by rudiments' `at` extension; the indices here are in range by
              // construction.)
              val pure = storage.asInstanceOf[Array[Byte]]
              val count = libcrypto.BIO_read(bio, pure.atUnsafe(0), capacity.min(granted))

              if count <= 0 then
                ended = true
                Unset
              else
                limit0 = count
                count

    def send(consume data: (Stream[Data] over Credit)^): Unit =
      // A write failure surfaces as an `IOException`, matching what the JVM backend's
      // stream `write` throws at runtime; `Duplex.send` offers no typed error channel.
      import unsafeExceptions.canThrowAny

      data.sweep: (storage, start, count) =>
        val array = storage.asInstanceOf[Array[Byte]]
        var written = 0

        while written < count do
          val result = libcrypto.BIO_write(bio, array.atUnsafe(start + written), count - written)
          if result <= 0 then throw ji.IOException("TLS: BIO_write failed: "+opensslError())
          written += result

    def close(): Unit =
      libcrypto.BIO_free_all(bio)
      libssl.SSL_CTX_free(context)
