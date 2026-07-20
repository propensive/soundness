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
┃    Soundness, version 0.64.0.                                                                    ┃
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

import java.util.concurrent.atomic as juca
import java.util.concurrent.locks.LockSupport

// A bounded single-producer/single-consumer hand-off ring: the exchange
// mechanism behind `Conduit` and the fan-out subscriber queues. Exactly one
// thread may call the producer operations (`offer`, `finish`) and exactly one
// the consumer operations (`take`, `close`) — the discipline the owning
// endpoints already enforce.
//
// Unlike a lock-based queue, both sides spin briefly (`Thread.onSpinWait`)
// before parking, so a producer and consumer in lockstep exchange items
// without kernel transitions — the dominant cost of a blocking hand-off. The
// unpark handshake is the standard Dekker pattern: each side publishes its
// index (a volatile write) before reading the other's waiting flag, and sets
// its waiting flag before re-checking the index, so a wakeup can never be
// missed.
//
// A `SharedCapability`: like `Conduit`'s core, its guarantees come from
// volatile publication order, not aliasing analysis.
final class Handoff(window: Int) extends caps.SharedCapability:
  private val capacity: Int = Integer.highestOneBit((window.max(2)*2) - 1)
  private val mask: Int = capacity - 1
  private val slots: juca.AtomicReferenceArray[AnyRef] = juca.AtomicReferenceArray(capacity)
  private val head: juca.AtomicLong = juca.AtomicLong(0)
  private val tail: juca.AtomicLong = juca.AtomicLong(0)

  @caps.unsafe.untrackedCaptures @volatile private var producer: Thread | Null = null
  @caps.unsafe.untrackedCaptures @volatile private var consumer: Thread | Null = null
  @caps.unsafe.untrackedCaptures @volatile private var done: Boolean = false
  @caps.unsafe.untrackedCaptures @volatile private var closed: Boolean = false

  // Spin budget before parking: sized to bridge the counterpart's wakeup
  // latency (a virtual-thread reschedule or kernel unpark, ~1-2 µs), so two
  // sides in lockstep keep exchanging without kernel transitions.
  private inline val spins = 1024

  // Approximate occupancy, for advisory demand calculations; never used for
  // synchronization.
  def size: Int = (tail.get - head.get).toInt.max(0)
  def free: Int = capacity - size

  // Producer side: block (spin, then park) until a slot frees, unless the
  // consumer has closed, in which case the item is discarded.
  def offer(item: AnyRef): Unit =
    // Interruption-as-cancellation surfaces exactly as a blocking queue's
    // would; the exception is unchecked for callers, as from Java.
    import unsafeExceptions.canThrowAny
    val position = tail.get
    var spun: Int = 0

    while !closed do
      if position - head.get < capacity then
        slots.lazySet((position & mask).toInt, item)
        tail.set(position + 1)
        val waiting = consumer
        if waiting != null then LockSupport.unpark(waiting)
        return
      else if spun < spins then
        spun += 1
        Thread.onSpinWait()
      else
        producer = Thread.currentThread.nn
        if position - head.get == capacity && !closed then LockSupport.park()
        producer = null

        // Interruption is cancellation: `park` returns immediately on an
        // interrupted thread, so without this check an interrupted producer
        // would spin rather than aborting, as a blocking queue's `put` would.
        if Thread.interrupted() then throw InterruptedException()

  // Producer side: no more items will be offered. Idempotent.
  def finish(): Unit =
    done = true
    val waiting = consumer
    if waiting != null then LockSupport.unpark(waiting)

  // Consumer side: the next item, or `null` once the producer has finished
  // and the ring is drained.
  def take(): AnyRef | Null =
    // See `offer` for the interruption contract.
    import unsafeExceptions.canThrowAny
    val position = head.get
    var spun: Int = 0

    while true do
      if position < tail.get then
        val index = (position & mask).toInt
        val item = slots.get(index)
        slots.lazySet(index, null)
        head.set(position + 1)
        val waiting = producer
        if waiting != null then LockSupport.unpark(waiting)
        return item
      else if done then return null
      else if spun < spins then
        spun += 1
        Thread.onSpinWait()
      else
        consumer = Thread.currentThread.nn
        if position == tail.get && !done then LockSupport.park()
        consumer = null

        // Interruption is cancellation: see `offer`.
        if Thread.interrupted() then throw InterruptedException()

    null // unreachable

  // Consumer side: stop accepting; drain buffered items (releasing a parked
  // producer) and discard everything offered subsequently.
  def close(): Unit =
    closed = true
    while take0() != null do ()
    val waiting = producer
    if waiting != null then LockSupport.unpark(waiting)

  // A non-blocking take, for draining on close.
  private def take0(): AnyRef | Null =
    val position = head.get

    if position < tail.get then
      val index = (position & mask).toInt
      val item = slots.get(index)
      slots.lazySet(index, null)
      head.set(position + 1)
      val waiting = producer
      if waiting != null then LockSupport.unpark(waiting)
      item
    else null
