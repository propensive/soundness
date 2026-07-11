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
import prepositional.*
import rudiments.*
import turbulence.Spool
import vacuous.*
import zephyrine.*

object Duplex:
  // An in-process connected pair: whatever one side sends, the other receives.
  // Chunk boundaries are preserved (each send is memoized to one element), which
  // in-memory protocol tests rely on. Ends when a side's `close` stops its
  // outbound spool.
  def pair(): (Duplex, Duplex) =
    val leftToRight: Spool[Data] = Spool()
    val rightToLeft: Spool[Data] = Spool()

    def side(inbound: Spool[Data], outbound: Spool[Data]): Duplex = new Duplex:
      def stream: LazyList[Data] = inbound.stream

      def send(consume data: (Stream[Data] over Credit)^): Unit =
        outbound.put(data.memoize)

      def close(): Unit = outbound.stop()

    (side(leftToRight, rightToLeft), side(rightToLeft, leftToRight))

  // Wraps a blocking `InputStream`/`OutputStream` pair — the shape a socket that has no
  // `SocketChannel` exposes (e.g. an `SSLSocket`). `shutdown` closes the underlying
  // resource. The read side blocks in `read` and copies each fill; EOF (`-1`) ends the
  // stream. The stream/write shape mirrors `channel` and `Serviceable`'s socket path.
  // `shutdown` is a pure function: its closures hold only untracked OS resources (like
  // `channel`'s socket), so the `Duplex` remains capture-free under capture checking.
  def streams(in: ji.InputStream, out: ji.OutputStream)(shutdown: () -> Unit): Duplex = new Duplex:
    // Untracked: `stream` is called by a single reader (the Duplex contract), and
    // each delivered chunk is copied out of the buffer by `take` before reuse.
    @caps.unsafe.untrackedCaptures
    private val buffer: Array[Byte] = new Array[Byte](65536)

    def stream: LazyList[Data] =
      def recur(): LazyList[Data] = in.read(buffer) match
        case -1    => LazyList()
        case count => buffer.take(count).immutable(using Unsafe) #:: recur()

      recur()

    def send(consume data: (Stream[Data] over Credit)^): Unit =
      data.foreachWindow: (storage, start, count) =>
        out.write(storage.asInstanceOf[Array[Byte]], start, count)
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

    def send(consume data: (Stream[Data] over Credit)^): Unit =
      data.foreachWindow: (storage, start, count) =>
        val out = ByteBuffer.wrap(storage.asInstanceOf[Array[Byte]], start, count).nn
        while out.hasRemaining do socketChannel.write(out)

    def close(): Unit = socketChannel.close()

    override def source(using buffering: Buffering): Stream[Data] over Credit =
      new Stream[Data]:
        type Transport = Credit

        private val capacity: Int = buffering.capacity(Substrate.Bytes)
        private val storage: Array[Byte] = new Array[Byte](capacity)
        private val wrapped: ByteBuffer = ByteBuffer.wrap(storage).nn
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
              wrapped.clear()
              wrapped.limit(capacity.min(granted))

              socketChannel.read(wrapped) match
                case -1 =>
                  ended = true
                  Unset

                case 0 =>
                  refill(demand)

                case count =>
                  limit0 = count
                  count

// A persistent, bidirectional connection: unlike `Serviceable`'s request/response
// `transmit`/`receive` (which shuts down the output side after sending), a `Duplex`
// stays open for repeated, independent reads and writes — the shape a multiplexing
// protocol such as HTTP/2 needs, where one side both streams requests and receives
// server-initiated frames concurrently. Reads block until data arrives or the peer
// closes; `send` may be called many times and never half-closes the connection.
trait Duplex:
  def stream: LazyList[Data]
  def send(consume data: (Stream[Data] over Credit)^): Unit
  def close(): Unit

  // Pull endpoint over the read side. The default adapts the legacy
  // `stream`; `Duplex.channel` overrides it to read directly into the
  // endpoint's buffer, allocation-free.
  def source(using Buffering): Stream[Data] over Credit = Stream(stream.iterator)

  // Push endpoint over the write side: repeatable, like `send`, and
  // `finish()` flushes without half-closing the connection.
  def intake(using buffering: Buffering): (Intake[Data] over Credit)^{this} =
    new Intake[Data]:
      type Transport = Credit

      private val block: Int = buffering.capacity(Substrate.Bytes)
      private val storage: Array[Byte] = new Array[Byte](block)
      private var mark0: Int = 0

      def demand: Credit = Credit(Long.MaxValue)
      protected def buffer0: AnyRef = storage.asInstanceOf[AnyRef]
      def mark: Int = mark0

      update def reserve(min: Int): Int =
        val free = block - mark0

        if free >= min then free else
          drain()
          block

      update def commit(count: Int): Unit =
        mark0 += count
        if mark0 == block then drain()

      override update def flush(): Unit = drain()
      update def finish(): Unit = drain()

      private update def drain(): Unit =
        if mark0 > 0 then
          send(Stream(storage.slice(0, mark0).nn.immutable(using Unsafe)))
          mark0 = 0
