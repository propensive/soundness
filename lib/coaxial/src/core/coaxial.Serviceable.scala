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
import java.nio.ByteBuffer
import java.nio.channels as jnc
import java.nio.file as jnf

import anticipation.*
import contingency.*
import rudiments.*
import turbulence.*
import urticose.*
import vacuous.*

object Serviceable:
  given domainSocket: Tactic[StreamError] => DomainSocket is Serviceable:
    type Output = Data

    case class Connection(channel: jnc.SocketChannel)

    def connect(domainSocket: DomainSocket): Connection =
      val path = jnf.Path.of(domainSocket.address.s)
      val address = jn.UnixDomainSocketAddress.of(path)
      val channel = jnc.SocketChannel.open(address).nn
      channel.configureBlocking(false)

      Connection(channel)

    def transmit(connection: Connection, input: Stream[Data]): Unit =
      List.from(input).each: bytes =>
        connection.channel.write(ByteBuffer.wrap(bytes.mutable(using Unsafe)))

      connection.channel.shutdownOutput()

    def receive(connection: Connection): Stream[Data] =
      val buffer = ByteBuffer.allocate(512).nn

      def recur(): Stream[Data] =
        connection.channel.read(buffer) match
          case -1 =>
            connection.channel.shutdownInput()
            Stream()

          case n =>
            buffer.flip()
            val array = new Array[Byte](buffer.remaining)
            buffer.get(array)
            buffer.clear()
            array.immutable(using Unsafe) #:: recur()

      recur()

    def close(connection: Connection): Unit = connection.channel.close()

  given tcpEndpoint: (Online, Tactic[StreamError]) => Endpoint[TcpPort] is Serviceable:
    type Output = Data
    type Connection = jn.Socket

    def connect(endpoint: Endpoint[TcpPort]): jn.Socket =
      jn.Socket(jn.InetAddress.getByName(endpoint.remote.s), endpoint.port.number)

    def transmit(socket: jn.Socket, input: Stream[Data]): Unit =
      val out = socket.getOutputStream.nn

      List.from(input).each: bytes =>
        out.write(bytes.mutable(using Unsafe))
        out.flush()

    def close(socket: jn.Socket): Unit = socket.close()
    def receive(socket: jn.Socket): Stream[Data] = socket.getInputStream.nn.stream[Data]

  given tcpPort: Tactic[StreamError] => TcpPort is Serviceable:
    type Output = Data
    type Connection = jn.Socket

    def connect(port: TcpPort): jn.Socket = jn.Socket(jn.InetAddress.getLocalHost.nn, port.number)
    def close(socket: jn.Socket): Unit = socket.close()
    def receive(socket: jn.Socket): Stream[Data] = socket.getInputStream.nn.stream[Data]

    def transmit(socket: jn.Socket, input: Stream[Data]): Unit =
      val out = socket.getOutputStream.nn

      List.from(input).each: bytes =>
        out.write(bytes.mutable(using Unsafe))
        out.flush()

trait Serviceable extends Routable:
  def receive(connection: Connection): Stream[Data]
  def close(connection: Connection): Unit
