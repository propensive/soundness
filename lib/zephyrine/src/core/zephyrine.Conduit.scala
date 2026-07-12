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
package zephyrine

import language.experimental.separationChecking

import java.util.concurrent as juc

import fulminate.*
import prepositional.*
import rudiments.*
import vacuous.*

// The asynchronous boundary of a streaming pipeline: a factory yielding an `Intake` for
// the writing thread and a `Stream` for the reading thread, with exactly one thread ever
// touching each. Data crosses in blocks through a bounded queue, so a writer that
// outpaces its reader parks in `reserve` — cross-thread backpressure — and the free
// capacity is the credit the intake reports as its `demand`. This is the only
// synchronized component of a pipeline; every other stage is single-owner mutable state
// on one side of a conduit or the other.
//
// The two endpoints are separate exclusive capabilities: a single object owning both
// sides could not give two threads separated exclusive access. They share a
// `SharedCapability`-classified core — correctly exempt from separation checking, since
// the core's guarantees come from the queue's happens-before and its volatile flags, not from
// aliasing analysis. Capture sets are erased crossing the queue (it carries `AnyRef`);
// single ownership of a published block is discharged on the writer's side before the
// hand-off, and visibility is the queue's publication guarantee.
object Conduit:
  private object End

  private class Block(val storage: AnyRef, val size: Int)

  // The synchronized substrate both endpoints capture.
  private final class Core(val window: Int) extends caps.SharedCapability:
    val queue: juc.ArrayBlockingQueue[AnyRef] = juc.ArrayBlockingQueue(window)
    // JMM-managed flags: their safety is the volatile publication guarantee, not
    // aliasing analysis, so their captures are untracked.
    @caps.unsafe.untrackedCaptures @volatile var error: Throwable | Null = null
    @caps.unsafe.untrackedCaptures @volatile var closed: Boolean = false

  def apply[medium]()
    ( using addressable0: medium is Addressable )(using buffering: Buffering)
  :   ((Intake[medium] over Credit)^, (Stream[medium] over Credit)^) =

    val block: Int = buffering.capacity(addressable0.substrate)
    val ceiling: Int = buffering.transfer(addressable0.substrate).max(block)
    val core = new Core(buffering.window)

    // The write side; single writer. Blocks are minted at the size `reserve` is
    // asked for, between `block` and `ceiling`: a bulk `put` crosses the
    // boundary in ceiling-sized hand-offs rather than being re-chunked to the
    // staging block size, since each hand-off costs a synchronized queue
    // transfer. The memory bound is `window + 1` blocks of at most `ceiling`.
    val intake: (Intake[medium] over Credit)^ = new Intake[medium](using addressable0):
      type Transport = Credit

      private var current: addressable0.Storage = addressable0.allocate(block)
      private var capacity: Int = block
      private var mark0: Int = 0

      // Advisory free space: block-sized credit for each free queue slot, plus
      // the room left in the block being written. Zero exactly when `reserve`
      // would park on a full queue.
      def demand: Credit =
        Credit((core.window - core.queue.size).toLong*block + (capacity - mark0))

      protected def buffer0: AnyRef = current.asInstanceOf[AnyRef]
      def mark: Int = mark0

      update def reserve(min: Int): Int =
        val free = capacity - mark0

        if free >= min.max(1) then free else
          publish()
          capacity = min.min(ceiling).max(block)
          current = addressable0.allocate(capacity)
          capacity

      update def commit(count: Int): Unit =
        mark0 += count
        if mark0 == capacity then publish()

      override update def flush(): Unit = publish()

      update def finish(): Unit =
        flush()
        core.queue.put(End)

      override update def fail(error: Throwable): Unit =
        core.error = error
        core.queue.clear()
        core.queue.put(End)

      // Hand the written block over (or discard it after close). The storage
      // moves to the queue, so the capacity drops to zero and the next
      // `reserve` mints a fresh block, sized to its request.
      private update def publish(): Unit =
        if mark0 > 0 then
          if !core.closed then core.queue.put(Block(current.asInstanceOf[AnyRef], mark0))
          mark0 = 0
          capacity = 0

    // The read side; single reader. `refill` parks (in `queue.take`) until the
    // writer publishes a block or finishes.
    val stream: (Stream[medium] over Credit)^ = new Stream[medium](using addressable0):
      type Transport = Credit

      private var storage: addressable0.Storage = addressable0.allocate(0)
      private var start0: Int = 0
      private var limit0: Int = 0
      private var size: Int = 0
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

          if granted == 0 then 0
          else if limit0 < size then
            limit0 += (size - limit0).min(granted)
            limit0 - start0
          else
            (core.queue.take().nn: @unchecked) match
              case End =>
                ended = true
                val error0 = core.error
                if error0 == null then Unset else throw error0

              case received: Block =>
                // Single-ownership transfer: the writer never touches a published
                // block again (proven on its side by the fresh re-mint in `publish`).
                storage = received.storage.asInstanceOf[addressable0.Storage]
                size = received.size
                start0 = 0
                limit0 = size.min(granted)
                limit0

              case _ =>
                panic(m"unexpected value in conduit queue")

      override update def close(): Unit =
        core.closed = true
        core.queue.clear()

    (intake, stream)
