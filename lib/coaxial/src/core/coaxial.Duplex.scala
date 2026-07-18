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

import anticipation.*
import prepositional.*
import rudiments.*
import turbulence.Relay
import vacuous.*
import zephyrine.*

object Duplex:
  // An in-process connected pair: whatever one side sends, the other receives.
  // Chunk boundaries are preserved (each send is memoized to one element), which
  // in-memory protocol tests rely on. Ends when a side's `close` stops its
  // outbound spool.
  def pair(): (Duplex, Duplex) =
    val leftToRight: Relay[Data] = Relay()
    val rightToLeft: Relay[Data] = Relay()

    def side(inbound: Relay[Data], outbound: Relay[Data]): Duplex = new Duplex:
      // Each memoized send is exposed as one refill window, preserving the
      // chunk boundaries the in-memory protocol tests rely on.
      def source(using Buffering): (Stream[Data] over Credit)^ = inbound.stream.records.stream

      def send(consume data: (Stream[Data] over Credit)^): Unit =
        outbound.put(data.memoize)

      def close(): Unit = outbound.stop()

    (side(leftToRight, rightToLeft), side(rightToLeft, leftToRight))

// A persistent, bidirectional connection: unlike `Serviceable`'s request/response
// `transmit`/`receive` (which shuts down the output side after sending), a `Duplex`
// stays open for repeated, independent reads and writes — the shape a multiplexing
// protocol such as HTTP/2 needs, where one side both streams requests and receives
// server-initiated frames concurrently. Reads block until data arrives or the peer
// closes; `send` may be called many times and never half-closes the connection.
trait Duplex:
  def send(consume data: (Stream[Data] over Credit)^): Unit
  def close(): Unit

  // Pull endpoint over the read side: a fresh single-use endpoint whose refill
  // blocks until data arrives or the peer closes. The `Duplex` contract is a
  // single reader, so `source` should be opened once. The `^` is load-bearing:
  // a bare stateful result reads as `.rd` at separation-checked use sites.
  def source(using Buffering): (Stream[Data] over Credit)^

  // The ALPN-negotiated application protocol (e.g. `h2` or `http/1.1`) for a TLS
  // transport, or `Unset` for a plaintext connection or when the peer selected none.
  // A unified HTTP client reads this once, straight after connecting, to pick an
  // HTTP/2 or HTTP/1.1 driver over this very socket.
  def alpnProtocol: Optional[Text] = Unset

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
