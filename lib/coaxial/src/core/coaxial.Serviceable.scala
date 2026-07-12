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
import gigantism.*
import prepositional.*
import rudiments.*
import turbulence.*
import urticose.*
import vacuous.*
import zephyrine.*

object Serviceable:
  // A pull endpoint over a readable channel: refill reads directly into the
  // endpoint's buffer, blocking until data arrives; EOF (`-1`) runs `onEnd` and
  // terminates; an I/O failure aborts with the bytes read so far.
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

  given domainSocket: (tactic: Tactic[StreamError], options: Every[SocketOption.Domain])
  =>  ( (DomainSocket is Serviceable)^{tactic, caps.any} ) = new Serviceable:
    type Self = DomainSocket
    type Output = Data

    case class Connection(channel: jnc.SocketChannel)

    // A Unix-domain socket has no network interface, so `interface` is not applicable here.
    def connect(domainSocket: DomainSocket, interface: Optional[MacAddress]): Connection =
      val address = jn.UnixDomainSocketAddress.of(jnf.Path.of(domainSocket.address.s))
      val channel = jnc.SocketChannel.open(jn.StandardProtocolFamily.UNIX).nn
      configure(channel, summon[Every[SocketOption.Domain]].values)
      channel.connect(address)
      channel.configureBlocking(false)

      Connection(channel)

    def transmit(connection: Connection, consume input: (Stream[Data] over Credit)^): Unit =
      input.foreachWindow: (storage, start, count) =>
        connection.channel.write(ByteBuffer.wrap(storage.asInstanceOf[Array[Byte]], start, count))

      connection.channel.shutdownOutput()

    def receive(connection: Connection): (Stream[Data] over Credit)^{this, caps.any} =
      channelSource(connection.channel)(() => connection.channel.shutdownInput())

    def close(connection: Connection): Unit = connection.channel.close()

  given tcpEndpoint: Online => (tactic: Tactic[StreamError]) => (options: Every[SocketOption.Tcp])
  =>  ( (Endpoint[TcpPort] is Serviceable)^{tactic} ) = new Serviceable:
    type Self = Endpoint[TcpPort]
    type Output = Data
    type Connection = jn.Socket

    def connect(endpoint: Endpoint[TcpPort], interface: Optional[MacAddress]): jn.Socket =
      val socket =
        interface.let(interfaceFor(_)).let(bindAddress(_)).let: local =>
          jn.Socket(jn.InetAddress.getByName(endpoint.remote.s), endpoint.port.number, local, 0)

        . or(jn.Socket(jn.InetAddress.getByName(endpoint.remote.s), endpoint.port.number))

      configure(socket, summon[Every[SocketOption.Tcp]].values)
      socket

    def transmit(socket: jn.Socket, consume input: (Stream[Data] over Credit)^): Unit =
      val out = socket.getOutputStream.nn

      input.foreachWindow: (storage, start, count) =>
        out.write(storage.asInstanceOf[Array[Byte]], start, count)
        out.flush()

    def close(socket: jn.Socket): Unit = socket.close()
    def receive(socket: jn.Socket): (Stream[Data] over Credit)^{this, caps.any} =
      channelSource(jnc.Channels.newChannel(socket.getInputStream.nn).nn)(() => ())

  given tcpPort: (tactic: Tactic[StreamError]) => (options: Every[SocketOption.Tcp])
  =>  ( (TcpPort is Serviceable)^{tactic} ) = new Serviceable:
    type Self = TcpPort
    type Output = Data
    type Connection = jn.Socket

    def connect(port: TcpPort, interface: Optional[MacAddress]): jn.Socket =
      val socket =
        interface.let(interfaceFor(_)).let(bindAddress(_)).let: local =>
          jn.Socket(jn.InetAddress.getLocalHost.nn, port.number, local, 0)

        . or(jn.Socket(jn.InetAddress.getLocalHost.nn, port.number))

      configure(socket, summon[Every[SocketOption.Tcp]].values)
      socket

    def close(socket: jn.Socket): Unit = socket.close()
    def receive(socket: jn.Socket): (Stream[Data] over Credit)^{this, caps.any} =
      channelSource(jnc.Channels.newChannel(socket.getInputStream.nn).nn)(() => ())

    def transmit(socket: jn.Socket, consume input: (Stream[Data] over Credit)^): Unit =
      val out = socket.getOutputStream.nn

      input.foreachWindow: (storage, start, count) =>
        out.write(storage.asInstanceOf[Array[Byte]], start, count)
        out.flush()

trait Serviceable extends Routable:
  // A fresh pull endpoint over the connection's read side; single-use, like the
  // connection itself. The endpoint may retain this instance's capabilities (a
  // `Tactic[StreamError]`), so the result is instance-relative rather than fresh.
  def receive(connection: Connection): (Stream[Data] over Credit)^{this, caps.any}
  def close(connection: Connection): Unit
