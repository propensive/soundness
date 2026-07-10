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

import java.util.concurrent as juc
import java.util.concurrent.atomic as juca

import fulminate.*
import prepositional.*
import rudiments.*
import vacuous.*

// The asynchronous boundary of a streaming pipeline: an `Intake` on its write
// side and a `Stream` on its read side, with exactly one thread ever touching
// each. Data crosses in blocks through a bounded queue, so a writer that
// outpaces its reader parks in `reserve` — cross-thread backpressure — and
// the free capacity is the credit this conduit reports as its `demand`. This
// is the only synchronized component of a pipeline; every other stage is
// single-owner mutable state on one side of a conduit or the other.
//
// A conduit is the generalization of `Producer.Channel`: block-granular
// hand-off, but with element-granular credit, storage handed to the reader
// without materializing an immutable chunk, and failure propagation — a
// producer-side `fail` is rethrown from the reader's next `refill`.
object Conduit:
  private object End

  private class Block(val storage: AnyRef, val size: Int)

final class Conduit[medium]
  (using val addressable0: medium is Addressable)(using buffering: Buffering)
extends Intake[medium](using addressable0):
  type Transport = Credit

  private val block: Int = buffering.capacity(addressable0.substrate)
  private val queue: juc.ArrayBlockingQueue[AnyRef] = juc.ArrayBlockingQueue(buffering.window)

  private val written: juca.AtomicLong = juca.AtomicLong(0)
  private val consumed: juca.AtomicLong = juca.AtomicLong(0)
  @volatile private var error: Throwable | Null = null
  @volatile private var closed: Boolean = false

  private var current: addressable0.Storage = addressable0.allocate(block)
  private var mark0: Int = 0

  // Everything this conduit can hold: the queued blocks plus the block being
  // written. `demand` is this allowance less what is currently buffered, so
  // it reaches zero exactly when `reserve` would block.
  private val allowance: Long = block.toLong * (buffering.window + 1)

  def demand: Credit = Credit(allowance - (written.get - consumed.get))

  protected def buffer0: AnyRef = current.asInstanceOf[AnyRef]
  def mark: Int = mark0

  def reserve(min: Int): Int =
    val free = block - mark0

    if free >= min then free else
      publish()
      block

  def commit(count: Int): Unit =
    mark0 += count
    written.addAndGet(count)
    if mark0 == block then publish()

  override def flush(): Unit = if mark0 > 0 then publish()

  def finish(): Unit =
    flush()
    queue.put(Conduit.End)

  override def fail(error: Throwable): Unit =
    this.error = error
    queue.clear()
    queue.put(Conduit.End)

  private def publish(): Unit =
    if mark0 > 0 then
      if closed then
        written.addAndGet(-mark0)
        mark0 = 0
      else
        queue.put(Conduit.Block(current.asInstanceOf[AnyRef], mark0))
        current = addressable0.allocate(block)
        mark0 = 0

  // The read side; single reader. `refill` parks (in `queue.take`) until the
  // writer publishes a block or finishes.
  val stream: Stream[medium] over Credit = new Stream[medium](using addressable0):
    type Transport = Credit

    private var storage: addressable0.Storage = addressable0.allocate(0)
    private var start0: Int = 0
    private var limit0: Int = 0
    private var size: Int = 0
    private var ended: Boolean = false

    protected def window0: AnyRef = storage.asInstanceOf[AnyRef]
    def start: Int = start0
    def limit: Int = limit0

    def skip(count: Int): Unit =
      start0 += count
      consumed.addAndGet(count)

    def refill(demand: Credit): Optional[Int] =
      if limit0 > start0 then limit0 - start0
      else if ended then Unset
      else
        val granted = summon[Credit is Regulation].grant(demand)

        if granted == 0 then 0
        else if limit0 < size then
          limit0 += (size - limit0).min(granted)
          limit0 - start0
        else
          (queue.take().nn: @unchecked) match
            case Conduit.End =>
              ended = true
              val error0 = error
              if error0 == null then Unset else throw error0

            case received: Conduit.Block =>
              storage = received.storage.asInstanceOf[addressable0.Storage]
              size = received.size
              start0 = 0
              limit0 = size.min(granted)
              limit0

            case _ =>
              panic(m"unexpected value in conduit queue")

    override def close(): Unit =
      closed = true
      queue.clear()
