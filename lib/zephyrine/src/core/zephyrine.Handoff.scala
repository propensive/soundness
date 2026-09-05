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

import scala.caps

import java.util.concurrent.atomic as juca
import java.util.concurrent.locks.LockSupport

import rudiments.*

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
final class Handoff(depth: Int) extends caps.SharedCapability:
  private val capacity: Int = Integer.highestOneBit((depth.max(2)*2) - 1)
  private val mask: Int = capacity - 1
  // Raw, where `head` and `tail` are `Atomic[Long]`. `Atomic.Refs` reads a slot as `Optional`,
  // which is right — a fresh slot is genuinely empty — but `drain` hands slots straight into a
  // caller-owned `scala.Array[AnyRef | Null]`, and `Optional` does not assign into that without
  // a cast. Migrating the slot type therefore changes `drain`'s signature and ripples into
  // `Conduit`'s adoption buffer, in the one path this module is benchmarked on. It is a
  // worthwhile change; it is not a mechanical one, and it does not belong in the commit that
  // introduces the wrapper.
  private val slots: juca.AtomicReferenceArray[AnyRef] = juca.AtomicReferenceArray(capacity)
  private val head: Atomic[Long] = Atomic(0L)
  private val tail: Atomic[Long] = Atomic(0L)

  @caps.unsafe.untrackedCaptures @volatile private var producer: Thread | Null = null
  @caps.unsafe.untrackedCaptures @volatile private var consumer: Thread | Null = null
  @caps.unsafe.untrackedCaptures @volatile private var done: Boolean = false
  @caps.unsafe.untrackedCaptures @volatile private var closed: Boolean = false

  // Spin budget before parking: sized to bridge the counterpart's wakeup
  // latency (a virtual-thread reschedule or kernel unpark, ~1-2 µs), so two
  // sides in lockstep keep exchanging without kernel transitions.
  private inline val spins = 1024

  // The ring's slot count: the largest burst a `drain` can move, so the
  // consumer can size its adoption buffer to never truncate one.
  def width: Int = capacity

  // Approximate occupancy, for advisory demand calculations; never used for
  // synchronization.
  def size: Int = (tail() - head()).toInt.max(0)
  def free: Int = capacity - size

  // Producer side: block (spin, then park) until a slot frees, unless the
  // consumer has closed, in which case the item is discarded.
  def offer(item: AnyRef): Unit =
    // Interruption-as-cancellation surfaces exactly as a blocking queue's
    // would; the exception is unchecked for callers, as from Java.
    import unsafeExceptions.canThrowAny
    val position = tail()
    var spun: Int = 0

    while !closed do
      if position - head() < capacity then
        slots.lazySet((position & mask).toInt, item)
        tail() = position + 1

        // Deliberately unparked on EVERY item, not only on the empty→non-empty
        // transition. The narrower wake is sound — a parked consumer must have
        // observed emptiness at its guarded re-check, so only the offer ending
        // the emptiness can have anyone to wake — and it removes the dominant
        // profile frame (the virtual thread's park-permit exchange,
        // `getAndSetBoolean`, 33% of the pipeline). It was measured, and it is
        // 33% SLOWER: re-granting the permit per item keeps the counterpart's
        // next `park` returning immediately, extending the spin budget across
        // the thread boundary, where the sparing wake lets it truly sleep and
        // repays the saved exchanges in wakeup latency at every ring drain.
        // The profile cost is the cheaper side of the trade.
        val waiting = consumer
        if waiting != null then LockSupport.unpark(waiting)
        return
      else if spun < spins then
        spun += 1
        Thread.onSpinWait()
      else
        producer = Thread.currentThread.nn
        if position - head() == capacity && !closed then LockSupport.park()
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

  // Consumer side: adopt every buffered item in one synchronized step —
  // blocking, like `take`, until at least one item is available or the
  // producer has finished. Up to `into.length` items are moved into the
  // caller's (consumer-owned, unsynchronized) buffer, with a single `head`
  // publication and a single producer unpark for the whole burst, where a
  // `take` loop pays one of each per item. Returns the number of items moved:
  // zero only once the producer has finished and the ring is drained.
  def drain(into: scala.Array[AnyRef | Null]): Int =
    // See `offer` for the interruption contract.
    import unsafeExceptions.canThrowAny
    val position = head()
    var spun: Int = 0

    while true do
      val limit = tail()

      if position < limit then
        val count = (limit - position).toInt.min(into.length)
        // The write view of the caller's buffer: single-consumer ownership is
        // the caller's discipline, as with every consumer-side operation.
        val burst = into.asInstanceOf[scala.Array[AnyRef | Null]^]
        var moved = 0

        while moved < count do
          val index = ((position + moved) & mask).toInt
          burst(moved) = slots.get(index)
          slots.lazySet(index, null)
          moved += 1

        head() = position + count
        val waiting = producer
        if waiting != null then LockSupport.unpark(waiting)
        return count
      else if done then return 0
      else if spun < spins then
        spun += 1
        Thread.onSpinWait()
      else
        consumer = Thread.currentThread.nn
        if position == tail() && !done then LockSupport.park()
        consumer = null

        // Interruption is cancellation: see `offer`.
        if Thread.interrupted() then throw InterruptedException()

    0 // unreachable

  // Consumer side: the next item, or `null` once the producer has finished
  // and the ring is drained.
  def take(): AnyRef | Null =
    // See `offer` for the interruption contract.
    import unsafeExceptions.canThrowAny
    val position = head()
    var spun: Int = 0

    while true do
      if position < tail() then
        val index = (position & mask).toInt
        val item = slots.get(index)
        slots.lazySet(index, null)
        head() = position + 1
        val waiting = producer
        if waiting != null then LockSupport.unpark(waiting)
        return item
      else if done then return null
      else if spun < spins then
        spun += 1
        Thread.onSpinWait()
      else
        consumer = Thread.currentThread.nn
        if position == tail() && !done then LockSupport.park()
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
    val position = head()

    if position < tail() then
      val index = (position & mask).toInt
      val item = slots.get(index)
      slots.lazySet(index, null)
      head() = position + 1
      val waiting = producer
      if waiting != null then LockSupport.unpark(waiting)
      item
    else null
