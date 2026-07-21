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

import anticipation.*
import contingency.*
import prepositional.*
import turbulence.*
import urticose.*
import vacuous.*
import zephyrine.*

// The pluggable low-level socket backend: the complete set of platform-specific operations that
// coaxial's `Bindable`/`Connectable`/`Serviceable`/`Routable` typeclasses are defined in terms
// of, expressed without reference to any platform API. `coaxial.jvm` provides the
// `java.nio.channels`/`java.net` implementation (`socketBackends.virtualMachine`); other
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
// maps its native failures onto coaxial's `ConnectionError`/`StreamError` vocabulary. `options`
// are coaxial's abstract `SocketOption`s, applied by the backend in whatever terms its platform
// understands (unsupported options are silently skipped).
trait SocketBackend:
  //── Stream server (`Bindable` over TCP / Unix-domain) ────────────────────────────────────────
  type ServerSocket

  def listenTcp(port: TcpPort, interface: Optional[MacAddress], options: List[SocketOption])
  :   ServerSocket

  def listenDomain(address: DomainSocket, options: List[SocketOption]): ServerSocket

  // Accept the next incoming connection, blocking until one arrives, as a `Duplex`: the handler
  // reads the request from its `source` and the accept loop writes the response with `send`.
  def accept(socket: ServerSocket): Duplex raises ConnectionError
  def shutdown(socket: ServerSocket): Unit

  //── Datagram server (`Bindable` over UDP) ────────────────────────────────────────────────────
  type DatagramSocket

  def listenUdp(port: UdpPort, interface: Optional[MacAddress], options: List[SocketOption])
  :   DatagramSocket

  // Block for the next datagram; `reply` sends `data` back to a received packet's `sender`.
  def receive(socket: DatagramSocket): Packet raises ConnectionError
  def reply(socket: DatagramSocket, sender: Ipv4 | Ipv6, port: UdpPort, data: Data)
  :   Unit raises ConnectionError

  def unbind(socket: DatagramSocket): Unit

  //── Request/response exchange (`Serviceable`) ────────────────────────────────────────────────
  type Exchange

  def dialTcp
    ( endpoint: Endpoint[TcpPort], interface: Optional[MacAddress], options: List[SocketOption] )
  :   Exchange

  def dialTcpPort(port: TcpPort, interface: Optional[MacAddress], options: List[SocketOption])
  :   Exchange

  def dialDomain(address: DomainSocket, options: List[SocketOption]): Exchange

  // Stream the request out, half-closing the output side; then read the response as a fresh
  // single-use pull endpoint whose refill blocks until data arrives or the peer half-closes.
  def request(exchange: Exchange, input: (Stream[Data] over Credit)^): Unit

  def response(exchange: Exchange)(using Buffering, Tactic[StreamError])
  :   (Stream[Data] over Credit)^{caps.any}

  def hangUp(exchange: Exchange): Unit

  //── Persistent duplex client (`Connectable`) ─────────────────────────────────────────────────
  // Connect and hand back a `Duplex`: independent reads and writes over one open connection, with
  // no half-close.
  def duplexTcp
    ( endpoint: Endpoint[TcpPort], interface: Optional[MacAddress], options: List[SocketOption] )
  :   Duplex

  def duplexDomain(address: DomainSocket, options: List[SocketOption]): Duplex

  //── Fire-and-forget datagram courier (`Routable`) ────────────────────────────────────────────
  type Courier

  def routeUdp
    ( endpoint: Endpoint[UdpPort], interface: Optional[MacAddress], options: List[SocketOption] )
  :   Courier

  def routeUdpPort
    ( port: UdpPort, interface: Optional[MacAddress], options: List[SocketOption] )
  :   Courier

  // Dispatch the stream as a single datagram to the courier's destination.
  def dispatch(courier: Courier, input: (Stream[Data] over Credit)^): Unit
