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
┃    Soundness, version 0.64.0.                                                                    ┃
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

import scala.caps

import anticipation.*
import contingency.*
import fulminate.*
import prepositional.*
import turbulence.*
import urticose.*
import vacuous.*
import zephyrine.*

// The socket vocabulary: the per-platform implementation, the options a socket is opened
// with, the events it logs and the handle on a running server. There is no `Socket` type —
// coaxial speaks in terms of `Bindable`, `Connectable` and `Duplex` — so this is a namespace
// for the vocabulary rather than a companion.
object Socket:
  // SocketBackend → Socket.Backend
  // The pluggable low-level socket backend: the complete set of platform-specific operations that
  // coaxial's `Bindable`/`Connectable`/`Serviceable`/`Routable` typeclasses are defined in terms
  // of, expressed without reference to any platform API. `coaxial.jvm` provides the
  // `java.nio.channels`/`java.net` implementation (`socketBackends.virtualMachineSockets`); other
  // platforms (e.g. WASI's `wasi:sockets`) supply their own.
  //
  // The seam is structured by the four *roles* coaxial's typeclasses play, because each has its
  // own byte-transfer discipline that a flat set of primitives could not preserve without lossy
  // reconstruction:
  //
  //   - a stream *server* (`Bindable` over TCP or Unix-domain) binds, accepts, writes a whole
  //     response and closes each connection;
  //   - a datagram *server* (`Bindable` over UDP) binds, receives a `Packet`, and replies to its
  //     sender;
  //   - a request/response *exchange* (`Serviceable`) connects, streams a request while
  //     half-closing its output, then reads the response to peer half-close;
  //   - a persistent *duplex* (`Connectable`) connects and hands back a `Duplex`, whose reads and
  //     writes are independent and never half-close;
  //   - a fire-and-forget datagram *courier* (`Routable`) connects and dispatches one datagram.
  //
  // The higher-level loops (`listen`/`react`/`exchange`/`duplex`) compose these in coaxial's
  // user-facing API and stay platform-neutral. Each opaque handle type is threaded back to the
  // backend and never inspected by the API. An operation a backend cannot support (e.g. Unix-domain
  // sockets or TLS on WASI) raises the appropriate error rather than approximating; each backend
  // maps its native failures onto coaxial's `Socket.Error`/`Truncation.Error` vocabulary. `options`
  // are coaxial's abstract `Option`s, applied by the backend in whatever terms its platform
  // understands (unsupported options are silently skipped).
  trait Backend:
    //── Stream server (`Bindable` over TCP / Unix-domain) ────────────────────────────────────────
    type ServerSocket

    def listenTcp(port: Tcp.Port, interface: Optional[MacAddress], options: List[Option])
    :   ServerSocket

    def listenDomain(address: DomainSocket, options: List[Option]): ServerSocket

    // Accept the next incoming connection, blocking until one arrives, as a `Duplex`: the handler
    // reads the request from its `source` and the accept loop writes the response with `send`.
    def accept(socket: ServerSocket): Duplex raises Socket.Error
    def shutdown(socket: ServerSocket): Unit

    //── Datagram server (`Bindable` over UDP) ────────────────────────────────────────────────────
    type DatagramSocket

    def listenUdp(port: Udp.Port, interface: Optional[MacAddress], options: List[Option])
    :   DatagramSocket

    // Block for the next datagram; `reply` sends `data` back to a received packet's `sender`.
    def receive(socket: DatagramSocket): Packet raises Socket.Error
    def reply(socket: DatagramSocket, sender: Ipv4 | Ipv6, port: Udp.Port, data: Data)
    :   Unit raises Socket.Error

    def unbind(socket: DatagramSocket): Unit

    //── Request/response exchange (`Serviceable`) ────────────────────────────────────────────────
    type Exchange

    def dialTcp
      ( endpoint: Endpoint[Tcp.Port], interface: Optional[MacAddress], options: List[Option] )
    :   Exchange

    def dialTcpPort(port: Tcp.Port, interface: Optional[MacAddress], options: List[Option])
    :   Exchange

    def dialDomain(address: DomainSocket, options: List[Option]): Exchange

    // Stream the request out, half-closing the output side; then read the response as a fresh
    // single-use pull endpoint whose refill blocks until data arrives or the peer half-closes.
    def request(exchange: Exchange, consume input: (Stream[Data] over Credit)^): Unit

    def response(exchange: Exchange)(using buffering: Buffering, tactic: Tactic[Truncation.Error])
    :   (Stream[Data] over Credit)^{tactic, caps.any}

    def hangUp(exchange: Exchange): Unit

    //── Persistent duplex client (`Connectable`) ─────────────────────────────────────────────────
    // Connect and hand back a `Duplex`: independent reads and writes over one open connection, with
    // no half-close.
    def duplexTcp
      ( endpoint: Endpoint[Tcp.Port], interface: Optional[MacAddress], options: List[Option] )
    :   Duplex

    def duplexDomain(address: DomainSocket, options: List[Option]): Duplex

    //── Fire-and-forget datagram courier (`Routable`) ────────────────────────────────────────────
    type Courier

    def routeUdp
      ( endpoint: Endpoint[Udp.Port], interface: Optional[MacAddress], options: List[Option] )
    :   Courier

    def routeUdpPort
      ( port: Udp.Port, interface: Optional[MacAddress], options: List[Option] )
    :   Courier

    // Dispatch the stream as a single datagram to the courier's destination.
    def dispatch(courier: Courier, consume input: (Stream[Data] over Credit)^): Unit

  // SocketEvent → Socket.Event
  object Event:
    given communicable: Event is Communicable =
      case Listening(endpoint) => m"listening on $endpoint"
      case Connected(endpoint) => m"connected to $endpoint"
      case Closed(endpoint)    => m"closed the connection to $endpoint"

  enum Event:
    case Listening(endpoint: Text) extends Event, Log.Network
    case Connected(endpoint: Text) extends Event, Log.Network
    case Closed(endpoint: Text) extends Event, Log.Network

  // SocketOption → Socket.Option
  // A socket option, applied to a freshly-constructed socket before it is bound or connected. Each
  // option is typed by the connection types it is valid for: the nested marker traits `Tcp`, `Udp`
  // and `Domain` (Unix-domain) each extend `Option`, and every concrete option extends every
  // connection trait it applies to. A bind/connect site for a particular connection then collects
  // `Every[Option.Tcp]` (or `.Udp`/`.Domain`), so only options valid for that connection are
  // ever gathered. Flag options that simply enable a feature are parameterless markers — their
  // presence is a deviation from the default-off baseline; only options carrying a real value (a
  // buffer size, a duration, a linger interval, a traffic class) take a parameter.
  object Option:
    sealed trait Tcp    extends Option
    sealed trait Udp    extends Option
    sealed trait Domain extends Option

    case object ReuseAddress                  extends Tcp, Udp, Domain  // SO_REUSEADDR
    case object ReusePort                      extends Tcp, Udp         // SO_REUSEPORT (OS-dependent)
    case class  ReceiveBuffer(bytes: Int)      extends Tcp, Udp, Domain // SO_RCVBUF
    case class  SendBuffer(bytes: Int)         extends Tcp, Udp, Domain // SO_SNDBUF
    case class  Timeout(milliseconds: Int)     extends Tcp, Udp, Domain // SO_TIMEOUT (blocking)

    case object NoDelay                        extends Tcp              // TCP_NODELAY
    case object KeepAlive                      extends Tcp              // SO_KEEPALIVE
    case class  Linger(seconds: Optional[Int]) extends Tcp, Domain      // SO_LINGER
    case class  TrafficClass(value: Int)       extends Tcp, Udp         // IP_TOS

    case object Broadcast                      extends Udp              // SO_BROADCAST

  sealed trait Option

  // SocketService → Socket.Service
  // A running socket server is a capability: it owns the accept loop and the supervised
  // connection tasks (captured by `stopServer`) until `stop`ped. A named class rather than an
  // anonymous one so the `transparent inline` `listen` does not duplicate it at each call site.
  class Service(stopServer: () => Unit) extends caps.ExclusiveCapability:
    def stop(): Unit = stopServer()

  // ConnectionError → Socket.Error
  object Error:
    enum Reason(val number: Int) extends Clarification:
      case Accept   extends Reason(1)
      case Transmit extends Reason(2)
      case Close    extends Reason(3)

    given communicable: Reason is Communicable =
      case Reason.Accept   => m"a new connection could not be accepted"
      case Reason.Transmit => m"data could not be transmitted to the connection"
      case Reason.Close    => m"the connection could not be closed cleanly"

  case class Error(reason: Socket.Error.Reason)(using Diagnostics)
  extends fulminate.Error(266, reason.number)(m"the connection failed because $reason")
