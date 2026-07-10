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
import java.nio.ByteBuffer
import java.nio.channels as jnc

import anticipation.*
import rudiments.*
import vacuous.*

object Duplex:
  // Wraps a blocking `InputStream`/`OutputStream` pair — the shape a socket that has no
  // `SocketChannel` exposes (e.g. an `SSLSocket`). `shutdown` closes the underlying
  // resource. The read side blocks in `read` and copies each fill; EOF (`-1`) ends the
  // stream. The stream/write shape mirrors `channel` and `Serviceable`'s socket path.
  def streams(in: ji.InputStream, out: ji.OutputStream)(shutdown: () => Unit): Duplex = new Duplex:
    private val buffer = new Array[Byte](65536)

    def stream: LazyList[Data] =
      def recur(): LazyList[Data] = in.read(buffer) match
        case -1    => LazyList()
        case count => buffer.take(count).immutable(using Unsafe) #:: recur()

      recur()

    def send(data: LazyList[Data]): Unit =
      data.each: bytes =>
        out.write(bytes.mutable(using Unsafe))
        out.flush()

    def close(): Unit = shutdown()

  // Wraps a blocking `SocketChannel` (TCP or Unix-domain). The read side fills a
  // reusable buffer and blocks in `read`; EOF (`-1`) terminates the stream.
  def channel(socketChannel: jnc.SocketChannel): Duplex = new Duplex:
    private val buffer = ByteBuffer.allocate(65536).nn

    def stream: LazyList[Data] =
      def recur(): LazyList[Data] =
        buffer.clear()

        socketChannel.read(buffer) match
          case -1 => LazyList()

          case _ =>
            buffer.flip()
            val array = new Array[Byte](buffer.remaining)
            buffer.get(array)
            array.immutable(using Unsafe) #:: recur()

      recur()

    def send(data: LazyList[Data]): Unit =
      data.each: bytes =>
        val out = ByteBuffer.wrap(bytes.mutable(using Unsafe)).nn
        while out.hasRemaining do socketChannel.write(out)

    def close(): Unit = socketChannel.close()

// A persistent, bidirectional connection: unlike `Serviceable`'s request/response
// `transmit`/`receive` (which shuts down the output side after sending), a `Duplex`
// stays open for repeated, independent reads and writes — the shape a multiplexing
// protocol such as HTTP/2 needs, where one side both streams requests and receives
// server-initiated frames concurrently. Reads block until data arrives or the peer
// closes; `send` may be called many times and never half-closes the connection.
trait Duplex:
  def stream: LazyList[Data]
  def send(data: LazyList[Data]): Unit
  def close(): Unit
