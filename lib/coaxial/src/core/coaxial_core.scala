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
┃    Soundness, version 0.54.0.                                                                    ┃
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
import rudiments.*
import spectacular.*
import turbulence.*
import urticose.MacAddress
import vacuous.*

import Control.*

extension [bindable: {Bindable, Showable}](socket: bindable)
  // A default argument cannot be combined with the path-dependent `bindable.Input`/`.Output`
  // types here (default-getter synthesis fails), so the interface-less form is a separate
  // overload that delegates with `Unset`.
  def listen[input](using Monitor, Probate)[result](lambda: bindable.Input => bindable.Output)
  :   SocketService logs SocketEvent raises BindError =

    socket.listen(lambda)(Unset)

  def listen[input](using Monitor, Probate)[result](lambda: bindable.Input => bindable.Output)
    ( interface: Optional[MacAddress] )
  :   SocketService logs SocketEvent raises BindError =

    val binding = bindable.bind(socket, interface)
    Log.info(SocketEvent.Listening(socket.show))

    // Each accepted connection is handled on its own daemon, so connections are served
    // concurrently and a per-connection failure — a handler error, a dropped client, or a
    // failure to write the reply — surfaces as a raised `ConnectionError` that the trap below
    // turns into `Remedy.Accept`, ending only that connection's daemon while the accept loop
    // keeps running. The connection is always closed afterwards. A failure to *accept* a
    // connection runs on the loop thread (not a daemon), so `safely` skips that iteration; on
    // `stop()` the loop is already `Stopping`, so the interrupted `accept()` simply unwinds.
    contain:
      case _ => Remedy.Accept

    . within:
        val bindLoop = loop:
          safely(bindable.connect(binding)).let: connection =>
            daemon:
              // A connection failure aborts just this handler; throw it to the enclosing
              // `contain`, which closes the connection and carries on accepting others.
              given Tactic[ConnectionError] = AsyncTactic()

              try bindable.transmit(binding, connection, lambda(connection))
              finally bindable.close(connection)

        val task = async(bindLoop.run())

        new SocketService:
          def stop(): Unit =
            bindLoop.stop()
            bindable.stop(binding)
            safely(task.await())
            Log.fine(SocketEvent.Closed(socket.show))


extension [endpoint: {Serviceable as serviceable, Showable}](endpoint: endpoint)
  def transmit[message: Transmissible](input: message): Stream[Data] logs SocketEvent =
    val connection = serviceable.connect(endpoint, Unset)
    Log.fine(SocketEvent.Connected(endpoint.show))

    serviceable.transmit(connection, message.serialize(input))
    serviceable.receive(connection)


  def exchange[state](initialState: state)[message: Ingressive](initialMessage: message = Data())
    ( handle: (state: state) ?=> message => Control[state] )
  :   state logs SocketEvent =

    val connection = serviceable.connect(endpoint, Unset)
    Log.fine(SocketEvent.Connected(endpoint.show))

    def recur(input: Stream[Data], state: state): state = input.flow(state):
      handle(using state)(message.deserialize(next)) match
        case Continue(state2) => recur(more, state2.or(state))
        case Terminate        => state

        case Reply(message, state2) =>
          serviceable.transmit(connection, Stream(message))
          recur(more, state2.or(state))

        case Conclude(message, state2) =>
          serviceable.transmit(connection, Stream(message))
          state2.or(state)

    recur(serviceable.receive(connection), initialState).also(serviceable.close(connection))


extension [endpoint: {Routable as routable, Showable}](endpoint: endpoint)
  def transmit[transmissible: Transmissible](message: transmissible)(using Monitor)
  :   Unit logs SocketEvent raises StreamError =

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
  def duplex[result](lambda: Duplex => result): result logs SocketEvent = duplex(lambda)(Unset)

  def duplex[result](lambda: Duplex => result)(interface: Optional[MacAddress])
  :   result logs SocketEvent =

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
