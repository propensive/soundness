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
import java.util.concurrent.atomic as juca

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
// the core's guarantees come from the queue's happens-before and the atomics, not from
// aliasing analysis. Capture sets are erased crossing the queue (it carries `AnyRef`);
// single ownership of a published block is discharged on the writer's side before the
// hand-off, and visibility is the queue's publication guarantee.
object Conduit:
  private object End

  private class Block(val storage: AnyRef, val size: Int)

  // The synchronized substrate both endpoints capture.
  private final class Core(window: Int) extends caps.SharedCapability:
    val queue: juc.ArrayBlockingQueue[AnyRef] = juc.ArrayBlockingQueue(window)
    val written: juca.AtomicLong = juca.AtomicLong(0)
    val consumed: juca.AtomicLong = juca.AtomicLong(0)
    // JMM-managed flags: their safety is the volatile publication guarantee, not
    // aliasing analysis, so their captures are untracked.
    @caps.unsafe.untrackedCaptures @volatile var error: Throwable | Null = null
    @caps.unsafe.untrackedCaptures @volatile var closed: Boolean = false

  def apply[medium]()
    ( using addressable0: medium is Addressable )(using buffering: Buffering)
  :   ((Intake[medium] over Credit)^, (Stream[medium] over Credit)^) =

    val block: Int = buffering.capacity(addressable0.substrate)
    val core = new Core(buffering.window)

    // Everything this conduit can hold: the queued blocks plus the block being
    // written. `demand` is this allowance less what is currently buffered, so
    // it reaches zero exactly when `reserve` would block.
    val allowance: Long = block.toLong * (buffering.window + 1)

    // The write side; single writer.
    val intake: (Intake[medium] over Credit)^ = new Intake[medium](using addressable0):
      type Transport = Credit

      private var current: addressable0.Storage = addressable0.allocate(block)
      private var mark0: Int = 0

      def demand: Credit = Credit(allowance - (core.written.get - core.consumed.get))

      protected def buffer0: AnyRef = current.asInstanceOf[AnyRef]
      def mark: Int = mark0

      update def reserve(min: Int): Int =
        val free = block - mark0

        if free >= min then free else
          publish()
          block

      update def commit(count: Int): Unit =
        mark0 += count
        core.written.addAndGet(count)
        if mark0 == block then publish()

      override update def flush(): Unit = if mark0 > 0 then publish()

      update def finish(): Unit =
        flush()
        core.queue.put(End)

      override update def fail(error: Throwable): Unit =
        core.error = error
        core.queue.clear()
        core.queue.put(End)

      private update def publish(): Unit =
        if mark0 > 0 then
          if core.closed then
            core.written.addAndGet(-mark0)
            mark0 = 0
          else
            core.queue.put(Block(current.asInstanceOf[AnyRef], mark0))
            current = addressable0.allocate(block)
            mark0 = 0

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

      update def skip(count: Int): Unit =
        start0 += count
        core.consumed.addAndGet(count)

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
