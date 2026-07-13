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

import scala.annotation.nowarn

import anticipation.*
import contingency.*
import distillate.*
import gigantism.*
import gossamer.*
import hellenism.*
import hypotenuse.*
import prepositional.*
import rudiments.*
import spectacular.*
import turbulence.*
import urticose.*
import vacuous.*
import zephyrine.*
import soundness.{invoke, dispose}
import xenophile.*

// The WIT definitions the navigation below is typechecked against, and which the `invoke`
// materializer consults (at its downstream expansion site) for module ids, resource methods and
// parameter types.
type WasiSocketsApi = Interface in Wit at "/coaxial/sockets.wit"
given wasiSocketsApi: WasiSocketsApi = Interface[Wit](cp"/coaxial/sockets.wit")

// The handles a connected TCP socket threads back to the backend: the readable and writable halves
// of the connection, and the socket itself (kept so it can be shut down and dropped).
private case class WasiExchange
  ( input:  WitHandle of "input-stream",
    output: WitHandle of "output-stream",
    socket: WitHandle of "tcp-socket" )

package socketBackends:
  // A `SocketBackend` over `wasi:sockets`. WASI sockets are capability-based (the host grants
  // network access with `wasmtime run -S inherit-network`) and asynchronous: `start-connect` /
  // `accept` are driven to completion by blocking on the socket's `pollable`. Only TCP is
  // supported — WASI has no Unix-domain sockets (those operations raise), and UDP is not yet
  // wired. `inline`, so the `invoke`s expand at the downstream summoning site, where the WASI
  // facades are on the classpath.
  @nowarn("msg=New anonymous class definition will be duplicated at each inline site")
  inline given wasi: SocketBackend = new SocketBackend:
    type ServerSocket = WitHandle of "tcp-socket"
    type DatagramSocket = Unit
    type Exchange = WasiExchange
    type Courier = Unit

    // The shared `wasi:sockets` default network, opened once per operation and dropped after use.
    private def network(): WitHandle of "network" =
      Foreign["instance-network", Wit].`instance-network`.invoke[WitHandle of "network"]

    // Builds an `ip-socket-address` from a dotted-quad host and port. Only IPv4 literals are
    // handled; a host that is not a literal address (needing DNS) raises.
    private def address(host: Text, port: Int) =
      val ip: Ipv4 = unsafely(host.as[Ipv4])

      val octets =
        ( U8(ip.byte0.toByte.bits), U8(ip.byte1.toByte.bits),
          U8(ip.byte2.toByte.bits), U8(ip.byte3.toByte.bits) )

      WitVariant["ip-socket-address", "ipv4"]((U16(port.toShort.bits), octets))

    // Creates a fresh IPv4 TCP socket. The interface root is bound to a `val` (with the refinement
    // `Foreign.apply`'s deferred inline expansion would otherwise leave implicit) so the
    // `ip-address-family` argument conversion can resolve its ecosystem.
    private def createSocket(): WitHandle of "tcp-socket" =
      val factory =
        Foreign["tcp-create-socket", Wit].asInstanceOf[Foreign of "tcp-create-socket" from Wit]

      factory.`create-tcp-socket`(WitCase["ip-address-family"]("ipv4"))
      . invoke[WitHandle of "tcp-socket"]

    // Opens a TCP connection to `host:port` and drives `start-connect` to completion, blocking on
    // the socket's pollable. Returns the connection's stream halves and the socket handle.
    private def connect(host: Text, port: Int)(using Tactic[ConnectionError]): WasiExchange =
      val net = network()
      val socketHandle = createSocket()
      val socket: Foreign of "tcp-socket" from Wit = socketHandle

      try
        socket.`start-connect`(net, address(host, port)).invoke[Unit]
        await(socketHandle)

        val (input, output) =
          socket.`finish-connect`.invoke[(WitHandle of "input-stream", WitHandle of "output-stream")]

        WasiExchange(input, output, socketHandle)
      catch case error: WitError =>
        socketHandle.dispose()
        abort(ConnectionError(ConnectionError.Reason.Accept))

    // Blocks until the socket is ready for the next step of a `start-`/`finish-` operation.
    private def await(socketHandle: WitHandle of "tcp-socket"): Unit =
      val socket: Foreign of "tcp-socket" from Wit = socketHandle
      val pollableHandle = socket.subscribe.invoke[WitHandle of "pollable"]
      val pollable: Foreign of "pollable" from Wit = pollableHandle
      pollable.block.invoke[Unit]
      pollableHandle.dispose()

    // A pull endpoint over a WASI `input-stream`: each refill is one `blocking-read`, whose result
    // becomes the window; the peer closing the stream surfaces as a `WitError`, which ends it.
    private def readStream(inputHandle: WitHandle of "input-stream")(using buffering: Buffering)
    :   (Stream[Data] over Credit)^{caps.any} =

      new Stream[Data]:
        type Transport = Credit

        private val capacity: Int = buffering.capacity(Substrate.Bytes)
        private var chunk: Array[Byte] = new Array[Byte](0)
        private var start0: Int = 0
        private var limit0: Int = 0
        private var ended: Boolean = false

        protected def window0: AnyRef = chunk.asInstanceOf[AnyRef]
        def start: Int = start0
        def limit: Int = limit0
        update def skip(count: Int): Unit = start0 += count

        update def refill(demand: Credit): Optional[Int] =
          if limit0 > start0 then limit0 - start0
          else if ended then Unset
          else
            val granted = summon[Credit is Regulation].grant(demand)

            if granted == 0 then 0 else
              val stream: Foreign of "input-stream" from Wit = inputHandle

              try
                val want = if capacity < granted then capacity else granted
                val data = stream.`blocking-read`(U64(want.toLong.bits)).invoke[Data]
                chunk = data.mutable(using Unsafe)
                start0 = 0
                limit0 = chunk.length
                if limit0 == 0 then refill(demand) else limit0
              catch case error: WitError =>
                ended = true
                Unset

    // Writes a whole pull-stream to a WASI `output-stream`, one `blocking-write-and-flush` per
    // window.
    private def drain(outputHandle: WitHandle of "output-stream", input: (Stream[Data] over Credit)^)
    :   Unit =

      val stream: Foreign of "output-stream" from Wit = outputHandle

      input.sweep: (storage, start, count) =>
        val slice = storage.asInstanceOf[Array[Byte]].slice(start, start + count).nn
        stream.`blocking-write-and-flush`(slice.immutable(using Unsafe)).invoke[Unit]

    // Presents a connected socket's stream halves as a `Duplex`: `source` reads, `send` writes,
    // `close` drops both streams and the socket.
    private def duplexOf(exchange: WasiExchange): Duplex = new Duplex:
      def source(using Buffering): (Stream[Data] over Credit)^ =
        readStream(exchange.input)

      def send(consume data: (Stream[Data] over Credit)^): Unit = drain(exchange.output, data)

      def close(): Unit =
        exchange.input.dispose()
        exchange.output.dispose()
        exchange.socket.dispose()

    //── Stream server (TCP; Unix-domain unsupported) ─────────────────────────────────────────────
    def listenTcp(port: TcpPort, interface: Optional[MacAddress], options: List[SocketOption])
    :   WitHandle of "tcp-socket" =

      unsafely:
        val net = network()
        val socketHandle = createSocket()
        val socket: Foreign of "tcp-socket" from Wit = socketHandle
        socket.`start-bind`(net, address(t"0.0.0.0", port.number)).invoke[Unit]
        socket.`finish-bind`.invoke[Unit]
        socket.`start-listen`.invoke[Unit]
        socket.`finish-listen`.invoke[Unit]
        socketHandle

    def listenDomain(address: DomainSocket, options: List[SocketOption]): WitHandle of "tcp-socket" =
      unsafely(abort(ConnectionError(ConnectionError.Reason.Accept)))

    def accept(socketHandle: WitHandle of "tcp-socket"): Duplex raises ConnectionError =
      val socket: Foreign of "tcp-socket" from Wit = socketHandle

      try
        await(socketHandle)

        val (client, input, output) =
          socket.accept.invoke
            [(WitHandle of "tcp-socket", WitHandle of "input-stream", WitHandle of "output-stream")]

        duplexOf(WasiExchange(input, output, client))
      catch case error: WitError => abort(ConnectionError(ConnectionError.Reason.Accept))

    def shutdown(socketHandle: WitHandle of "tcp-socket"): Unit = socketHandle.dispose()

    //── Datagram server (unsupported on WASI for now) ────────────────────────────────────────────
    def listenUdp(port: UdpPort, interface: Optional[MacAddress], options: List[SocketOption]): Unit =
      ()

    def receive(socket: Unit): Packet raises ConnectionError =
      abort(ConnectionError(ConnectionError.Reason.Accept))

    def reply(socket: Unit, sender: Ipv4 | Ipv6, port: UdpPort, data: Data)
    :   Unit raises ConnectionError =
      abort(ConnectionError(ConnectionError.Reason.Transmit))

    def unbind(socket: Unit): Unit = ()

    //── Request/response exchange (TCP; Unix-domain unsupported) ──────────────────────────────────
    def dialTcp
      ( endpoint: Endpoint[TcpPort], interface: Optional[MacAddress], options: List[SocketOption] )
    :   WasiExchange =
      unsafely(connect(endpoint.remote, endpoint.port.number))

    def dialTcpPort(port: TcpPort, interface: Optional[MacAddress], options: List[SocketOption])
    :   WasiExchange =
      unsafely(connect(t"127.0.0.1", port.number))

    def dialDomain(address: DomainSocket, options: List[SocketOption]): WasiExchange =
      unsafely(abort(ConnectionError(ConnectionError.Reason.Accept)))

    def request(exchange: WasiExchange, input: (Stream[Data] over Credit)^): Unit =
      drain(exchange.output, input)
      exchange.output.dispose()

    def response(exchange: WasiExchange)(using Buffering, Tactic[StreamError])
    :   (Stream[Data] over Credit)^{caps.any} =
      readStream(exchange.input)

    def hangUp(exchange: WasiExchange): Unit =
      exchange.input.dispose()
      exchange.socket.dispose()

    //── Persistent duplex client (TCP; Unix-domain unsupported) ───────────────────────────────────
    def duplexTcp
      ( endpoint: Endpoint[TcpPort], interface: Optional[MacAddress], options: List[SocketOption] )
    :   Duplex =
      duplexOf(unsafely(connect(endpoint.remote, endpoint.port.number)))

    def duplexDomain(address: DomainSocket, options: List[SocketOption]): Duplex =
      duplexOf(unsafely(abort(ConnectionError(ConnectionError.Reason.Accept))))

    //── Fire-and-forget datagram courier (unsupported on WASI for now) ────────────────────────────
    def routeUdp
      ( endpoint: Endpoint[UdpPort], interface: Optional[MacAddress], options: List[SocketOption] )
    :   Unit =
      ()

    def routeUdpPort(port: UdpPort, interface: Optional[MacAddress], options: List[SocketOption]): Unit =
      ()

    def dispatch(courier: Unit, input: (Stream[Data] over Credit)^): Unit = ()
