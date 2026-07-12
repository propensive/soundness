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
package turbulence

import java.util.concurrent as juc

import anticipation.*
import fulminate.*
import parasite.*
import prepositional.*
import rudiments.*
import vacuous.*
import zephyrine.*

// Fan-out: one pump task pulls the source and delivers every chunk to each
// of `count` subscriber endpoints through bounded queues. Chunks are
// materialized once and shared immutably between subscribers; a full
// subscriber queue parks the pump, so the slowest subscriber gates the
// source — the correct backpressure semantics for replication. A source
// failure is rethrown from each subscriber's next refill.
object Manifold:
  private class Block(val storage: AnyRef, val start: Int, val size: Int)

  def apply[medium](consume source: (Stream[medium] over Credit)^, count: Int)
    ( using addressable0: medium is Addressable,
            buffering:    Buffering,
            monitor:      Monitor,
            probate:      Probate )
  :   IndexedSeq[(Stream[medium] over Credit)^] =

    // The pump pulls boundary-transfer-sized credits, not staging-block-sized
    // ones: every chunk is delivered to each subscriber through a synchronized
    // hand-off, so the transfer size bounds the hand-off count.
    val block: Int = buffering.transfer(addressable0.substrate)

    // One spin-then-park SPSC ring per subscriber: the pump is the single
    // producer of each, and each subscriber the single consumer of its own.
    // Sealed: a ring's guarantees come from volatile publication order, not
    // aliasing analysis (as `Conduit`'s core), so the capture is erased to let
    // the rings ride the collection.
    val queues: IndexedSeq[Handoff] =
      IndexedSeq.fill(count)(caps.unsafe.unsafeAssumePure(Handoff(buffering.window)))

    @volatile var error: Throwable | Null = null

    val stable: Boolean = source.windowStable

    // A shared (stable) block costs no memory per element, so there is no reason
    // to bound its size: pull the whole window at once, collapsing the hand-off
    // count. A copied (transient) block stays transfer-bounded.
    val pull: Int = if stable then Int.MaxValue else block

    async:
      def loop(): Unit = source.refill(Credit(pull)) match
        case size: Int =>
          // A stable source (a fixed in-memory buffer) is shared by reference,
          // exactly as ZIO/FS2 pass immutable chunks: every subscriber reads
          // the same window range, and it is never overwritten. A transient
          // source is snapshotted once into fresh storage — still shared
          // read-only between all subscribers, but copied out of the window
          // before the next refill reuses it.
          val start = source.start

          val storage =
            if stable then source.window(using Unsafe)
            else
              val fresh = addressable0.allocate(size)

              addressable0.transfer
                ( source.window(using Unsafe).asInstanceOf[addressable0.Storage],
                  source.start, fresh, 0, size )

              fresh

          source.skip(size)
          val handoff = Block(storage.asInstanceOf[AnyRef], if stable then start else 0, size)

          // While-loops rather than `each`: a closure over the rings would
          // capture their reach capability.
          var index = 0

          while index < queues.length do
            queues(index).offer(handoff)
            index += 1

          loop()

        case _ =>
          var index = 0

          while index < queues.length do
            queues(index).finish()
            index += 1

      try loop() catch case exception: Exception =>
        error = exception
        var index = 0

        while index < queues.length do
          queues(index).finish()
          index += 1

    // The subscribers are typed exclusive element-wise at the collection rim: capture
    // sets do not ride standard-collection elements, and each queue feeds exactly one
    // subscriber by construction.
    locally:
      val subscribers = queues.map: queue =>
        sealSubscriber(new Stream[medium](using addressable0):
          type Transport = Credit

          // The shared storage is written only by the pump, before the
          // hand-off; subscribers only read the window, so exposing it
          // directly is safe and copy-free.
          private var storage: AnyRef = addressable0.allocate(0).asInstanceOf[AnyRef]
          private var start0: Int = 0
          private var limit0: Int = 0
          private var end0: Int = 0
          private var ended: Boolean = false

          protected def window0: AnyRef = storage
          def start: Int = start0
          def limit: Int = limit0
          update def skip(count: Int): Unit = start0 += count

          update def refill(demand: Credit): Optional[Int] =
            if limit0 > start0 then limit0 - start0
            else if ended then Unset
            else
              val granted = summon[Credit is Regulation].grant(demand)

              if granted == 0 then 0
              else if limit0 < end0 then
                limit0 += (end0 - limit0).min(granted)
                limit0 - start0
              else
                queue.take() match
                  case null =>
                    ended = true
                    val error0 = error
                    if error0 == null then Unset else throw error0

                  case received: Block =>
                    // A shared block may reference a sub-range of the source's
                    // buffer, so it carries its own start; subscribers only ever
                    // read it.
                    storage = received.storage
                    start0 = received.start
                    end0 = received.start + received.size
                    limit0 = start0 + received.size.min(granted)
                    limit0 - start0

                  case _ =>
                    panic(m"unexpected value in manifold hand-off")
        )

      subscribers.asInstanceOf[IndexedSeq[(Stream[medium] over Credit)^]]

  // Bridges a fresh subscriber into a collection element: capture sets do not ride
  // standard-collection elements, so the exclusivity is re-asserted element-wise in the
  // declared result type.
  private def sealSubscriber[medium]
    ( stream: (Stream[medium] over Credit)^ )
  :   AnyRef =
    stream.asInstanceOf[AnyRef]
