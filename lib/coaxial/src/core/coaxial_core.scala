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
import java.util as ju

import anticipation.*
import contingency.*
import parasite.*
import prepositional.*
import rudiments.*
import spectacular.*
import turbulence.*
import urticose.MacAddress
import zephyrine.{Stream, Credit, Buffering, Substrate}
import vacuous.*

import Control.*

extension [bindable: {Bindable, Showable}](socket: bindable)
  // A default argument cannot be combined with the path-dependent `bindable.Input`/`.Output`
  // types here (default-getter synthesis fails), so the interface-less form is a separate
  // overload that delegates with `Unset`.
  transparent inline def listen[input](using Monitor, Probate)[result]
    ( lambda: bindable.Input => bindable.Output )
  :   (SocketService^) raises BindError logs SocketEvent =

    socket.listen(lambda)(Unset)

  transparent inline def listen[input](using Monitor, Probate)[result]
    ( lambda: bindable.Input => bindable.Output )
    ( interface: Optional[MacAddress] )
  :   (SocketService^) raises BindError logs SocketEvent =

    val binding = bindable.bind(socket, interface)
    Log.info(SocketEvent.Listening(socket.show))

    // Each accepted connection is served by its own supervised `async` task, so connections are
    // handled concurrently. The task body is impure (it captures the connection and the handler),
    // which is why it is an `async` task rather than a pure `daemon`; the `ConnectionError` from a
    // handler error, a dropped client, or a failed reply is raised through the `AsyncTactic` the
    // task provides, and `safely` absorbs it so one bad connection only ends its own task — the
    // connection is always closed — while the accept loop keeps running. A failure to *accept* a
    // connection runs on the loop thread, so its `safely` skips that iteration; on `stop()` the
    // loop is already `Stopping`, so the interrupted `accept()` simply unwinds.
    val bindLoop = loop:
      safely(bindable.connect(binding)).let: connection =>
        async:
          safely:
            try bindable.transmit(binding, connection, lambda(connection))
            finally bindable.close(connection)

    val task = async(bindLoop.run())

    SocketService: () =>
      bindLoop.stop()
      bindable.stop(binding)
      safely(task.await())
      Log.fine(SocketEvent.Closed(socket.show))


// `Serviceable` instances are capabilities (their givens retain tactics and socket options), so
// the evidence is an explicit capturing using-parameter rather than a context bound, which would
// demand a pure instance.
extension [endpoint: Showable](endpoint: endpoint)(using serviceable: (endpoint is Serviceable)^)
  def transmit[message: Transmissible](input: message)(using SocketEvent is Loggable)
  :   (Stream[Data] over Credit)^{serviceable, caps.any} =
    val connection = serviceable.connect(endpoint, Unset)
    Log.fine(SocketEvent.Connected(endpoint.show))

    serviceable.transmit(connection, message.serialize(input))
    serviceable.receive(connection)


  // React to each inbound message with a `Control`: `Reply`/`Conclude` send a response,
  // `Continue` waits for more, `Terminate` stops. A `Serviceable` can only respond, never
  // initiate; a `Duplexable` additionally offers `exchange`, which can also send proactively.
  def react[state](initialState: state)[message: Ingressive]
    ( handle: (state: state) ?=> message => Control[state] )
    ( using SocketEvent is Loggable )(using buffering: Buffering)
  :   state =

    val connection = serviceable.connect(endpoint, Unset)
    Log.fine(SocketEvent.Connected(endpoint.show))

    // One refill window is one inbound message: chunk boundaries frame messages,
    // exactly as each lazy-list element did. A while-loop rather than a
    // self-recursive local def, which may not capture the exclusive endpoint.
    try
      val input = serviceable.receive(connection)
      val demand = Credit(buffering.capacity(Substrate.Bytes))
      var state = initialState
      var done = false

      while !done do input.refill(demand) match
        case count: Int =>
          if count > 0 then
            val data = input.addressable.materialize(input.window(using Unsafe), input.start, count)
            input.skip(count)

            handle(using state)(message.deserialize(data)) match
              case Continue(state2) => state = state2.or(state)
              case Terminate        => done = true

              case Reply(message, state2) =>
                serviceable.transmit(connection, Stream(message))
                state = state2.or(state)

              case Conclude(message, state2) =>
                serviceable.transmit(connection, Stream(message))
                state = state2.or(state)
                done = true

        case _ =>
          done = true

      state
    finally serviceable.close(connection)


// As for `Serviceable` above: `Duplexable` instances may be capabilities (a WebSocket client
// retains a `Monitor` and its tactics), so the evidence is a capturing using-parameter.
extension [endpoint: Showable](endpoint: endpoint)(using duplexable: (endpoint is Duplexable)^)
  // A full-duplex exchange: as soon as the connection opens — before the `handle` loop
  // begins — `interact` is handed a `Sender`, so a client can send *proactively* (send
  // first, or push from a task it spawns), concurrently with the reactive `handle` loop
  // (which still replies via `Control`). `handle` precedes `interact` so the message type
  // is fixed by the (annotated) handler. Available only for a `Duplexable` transport,
  // whose `transmit` is safe to call concurrently; a request/response `Serviceable` has
  // only the reactive `react`.
  def exchange[state](initialState: state)[message: {Ingressive, Transmissible}]
    ( handle: (state: state) ?=> message => Control[state] )
    ( interact: Sender[message]^ => Unit )
    ( using SocketEvent is Loggable )(using buffering: Buffering)
  :   state =

    val connection = duplexable.connect(endpoint, Unset)
    Log.fine(SocketEvent.Connected(endpoint.show))

    // A named `Post` rather than a lambda: forwarding to the consuming `transmit`
    // requires a `consume` parameter, which only a method can declare.
    interact:
      Sender[message]:
        new Sender.Post:
          def apply(consume stream: (Stream[Data] over Credit)^): Unit =
            duplexable.transmit(connection, stream)

    // As `react`: one refill window is one inbound message.
    try
      val input = duplexable.receive(connection)
      val demand = Credit(buffering.capacity(Substrate.Bytes))
      var state = initialState
      var done = false

      while !done do input.refill(demand) match
        case count: Int =>
          if count > 0 then
            val data = input.addressable.materialize(input.window(using Unsafe), input.start, count)
            input.skip(count)

            handle(using state)(message.deserialize(data)) match
              case Continue(state2) => state = state2.or(state)
              case Terminate        => done = true

              case Reply(message, state2) =>
                duplexable.transmit(connection, Stream(message))
                state = state2.or(state)

              case Conclude(message, state2) =>
                duplexable.transmit(connection, Stream(message))
                state = state2.or(state)
                done = true

        case _ =>
          done = true

      state
    finally duplexable.close(connection)


extension [endpoint: {Routable as routable, Showable}](endpoint: endpoint)
  def transmit[transmissible: Transmissible](message: transmissible)
    ( using Monitor, Tactic[StreamError], SocketEvent is Loggable )
  :   Unit =

    Log.fine(SocketEvent.Connected(endpoint.show))
    routable.transmit(routable.connect(endpoint, Unset), transmissible.serialize(message))


extension [endpoint: {Connectable as connectable, Showable}](endpoint: endpoint)
  // Open a persistent, bidirectional connection for the duration of `lambda` and
  // always close it afterwards — whether `lambda` returns or throws. This is the
  // shape a multiplexing protocol such as HTTP/2 needs (concurrent reads and writes
  // over one open connection), but unlike the request/response `exchange` the
  // connection is never half-closed. A long-lived connection that must outlive any
  // single block keeps `lambda` running (e.g. parked on its supervisor) until the
  // enclosing scope ends, at which point the loan closes the connection.
  def duplex[result](lambda: Duplex => result)(using SocketEvent is Loggable): result =
    duplex(lambda)(Unset)

  def duplex[result](lambda: Duplex => result)(interface: Optional[MacAddress])
    ( using SocketEvent is Loggable )
  :   result =

    val connection = connectable.connect(endpoint, interface)
    Log.info(SocketEvent.Connected(endpoint.show))

    try lambda(connection) finally
      connection.close()
      Log.fine(SocketEvent.Closed(endpoint.show))


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

private[coaxial] def configure(channel: jnc.NetworkChannel, options: List[SocketOption]): Unit =
  applyOptions(options):
    new Configurable:
      def supported: ju.Set[jn.SocketOption[?]] = channel.supportedOptions.nn
      def soTimeout(milliseconds: Int): Unit = ()  // not meaningful for a (selectable) channel

      def option[value](socketOption: jn.SocketOption[value], value: value): Unit =
        channel.setOption(socketOption, value)

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


// Importable socket-option contributions. Each flag is a named `given` declared at its concrete
// option type (not `SocketOption`), so the per-connection `Every[SocketOption.Tcp]` / `.Udp` /
// `.Domain` searches collect it. Bring one into scope with, e.g.,
// `import socketOptions.noDelaySocketOption`. Options that carry a value are factory methods whose
// result type is the concrete option, so a
// `given SocketOption.ReceiveBuffer = socketOptions.receiveBuffer(65536)` is likewise collected.
package socketOptions:
  given reuseAddressSocketOption: SocketOption.ReuseAddress.type = SocketOption.ReuseAddress
  given reusePortSocketOption:    SocketOption.ReusePort.type    = SocketOption.ReusePort
  given noDelaySocketOption:      SocketOption.NoDelay.type      = SocketOption.NoDelay
  given keepAliveSocketOption:    SocketOption.KeepAlive.type    = SocketOption.KeepAlive
  given broadcastSocketOption:    SocketOption.Broadcast.type    = SocketOption.Broadcast

  def receiveBuffer(bytes: Int): SocketOption.ReceiveBuffer = SocketOption.ReceiveBuffer(bytes)
  def sendBuffer(bytes: Int): SocketOption.SendBuffer = SocketOption.SendBuffer(bytes)
  def linger(seconds: Optional[Int] = Unset): SocketOption.Linger = SocketOption.Linger(seconds)
  def trafficClass(value: Int): SocketOption.TrafficClass = SocketOption.TrafficClass(value)
  def timeout(milliseconds: Int): SocketOption.Timeout = SocketOption.Timeout(milliseconds)
