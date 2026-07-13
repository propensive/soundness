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

import denominative.*
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
  // A queued hand-off: `storage` is either a block the writer minted and
  // filled (start 0), or the immutable backing of a chunk passed through by
  // reference, windowed at [start, start + size). `recyclable` is true only for
  // writer-minted blocks, whose storage the reader may return to the freelist
  // for reuse; a passed-through backing is the caller's immutable data and must
  // never be recycled.
  private class Block(val storage: AnyRef, val start: Int, val size: Int, val recyclable: Boolean)

  // The synchronized substrate both endpoints capture: a spin-then-park SPSC
  // ring, the failure flag, and a non-blocking pool of spent blocks the reader
  // returns to the writer for reuse.
  private final class Core(val window: Int) extends caps.SharedCapability:
    val handoff: Handoff = Handoff(window)
    val freelist: Freelist = Freelist(window + 1)
    // A JMM-managed flag: its safety is the volatile publication guarantee, not
    // aliasing analysis, so its captures are untracked.
    @caps.unsafe.untrackedCaptures @volatile var error: Throwable | Null = null

  def apply[medium]()
    ( using addressable0: medium is Addressable )(using buffering: Buffering)
  :   ((Intake[medium] over Credit)^, (Stream[medium] over Credit)^) =

    val block: Int = buffering.capacity(addressable0.substrate)
    val ceiling: Int = buffering.transfer(addressable0.substrate).max(block)
    val core = new Core(buffering.window)

    // Whether to recycle drained transfer blocks. When on, every minted block is
    // physically `ceiling`-sized, so the pool holds a single size and a reused
    // block always fits any reservation — no size matching, no wasted re-zeroing.
    val recycle: Boolean = buffering.recycle

    // The write side; single writer. Without recycling, blocks are minted at the
    // size `reserve` is asked for, between `block` and `ceiling`: a bulk `put`
    // crosses the boundary in ceiling-sized hand-offs rather than being
    // re-chunked to the staging block size, since each hand-off costs a
    // synchronized queue transfer. With recycling, every block is physically
    // `ceiling`-sized (so any drained block fits any future reservation and the
    // pool stays single-size), while `capacity` still bounds the usable prefix,
    // leaving the publish cadence unchanged. The memory bound is `window + 1`
    // blocks of at most `ceiling` either way.
    val intake: (Intake[medium] over Credit)^ = new Intake[medium](using addressable0):
      type Transport = Credit

      // Untracked, with cast-erased assignments and an exclusive write view:
      // the block is reached only through this (single-writer) intake until
      // `publish` hands it to the ring.
      @caps.unsafe.untrackedCaptures
      private var current: addressable0.Storage =
        addressable0.allocate(if recycle then ceiling else block).asInstanceOf[addressable0.Storage]

      private var capacity: Int = block
      private var mark0: Int = 0

      // Advisory free space: block-sized credit for each free ring slot, plus
      // the room left in the block being written. Zero exactly when `reserve`
      // would park on a full ring.
      def demand: Credit =
        Credit(core.handoff.free.toLong*block + (capacity - mark0))

      protected def buffer0: AnyRef = current.asInstanceOf[AnyRef]
      def mark: Int = mark0

      update def reserve(min: Int): Int =
        val free = capacity - mark0

        if free >= min.max(1) then free else
          publish()
          capacity = min.min(ceiling).max(block)

          // With recycling, mint a physically `ceiling`-sized block — reused from
          // the pool of blocks the reader has drained (overwritten in place, so
          // never re-zeroed) or freshly allocated when the pool is empty — and
          // let `capacity` bound the usable prefix. Without recycling, mint at
          // exactly the requested size, as before.
          current =
            if recycle then
              val reused = core.freelist.poll()

              (if reused != null then reused else addressable0.allocate(ceiling))
              . asInstanceOf[addressable0.Storage]
            else
              addressable0.allocate(capacity).asInstanceOf[addressable0.Storage]

          capacity

      update def commit(count: Int): Unit =
        mark0 += count
        if mark0 == capacity then publish()

      // Zero-copy pass-through: a chunk at least a block long, whose medium
      // exposes its immutable backing, crosses the boundary by reference — no
      // copy, one hand-off. Anything buffered is published first, preserving
      // order; small chunks take the default coalescing copy path.
      override update def put(source: medium, offset: Ordinal, size: Int): Unit =
        val backing: Optional[addressable0.Storage]^{caps.any.rd} =
          if size >= block then addressable0.backing(source) else Unset

        if backing == Unset then
          // The default coalescing copy loop, inlined: `super` calls on update
          // methods are not permitted, and the loop reads the minted block
          // directly in any case.
          var done: Int = 0

          while done < size do
            val free = reserve(size - done)
            val count = free.min(size - done)
            addressable0.copyChunk
              (source, offset.n0 + done, current.asInstanceOf[addressable0.Storage^], mark0, count)
            commit(count)
            done += count
        else
          publish()

          core.handoff.offer
            ( Block(backing.asInstanceOf[AnyRef], offset.n0, size, recyclable = false) )

      override update def flush(): Unit = publish()

      update def finish(): Unit =
        flush()
        core.handoff.finish()

      override update def fail(error: Throwable): Unit =
        core.error = error
        core.handoff.finish()

      // Hand the written block over (the ring discards it after close). The
      // storage moves to the ring, so the capacity drops to zero and the next
      // `reserve` mints a fresh block, sized to its request.
      private update def publish(): Unit =
        if mark0 > 0 then
          core.handoff.offer(Block(current.asInstanceOf[AnyRef], 0, mark0, recyclable = true))
          mark0 = 0
          capacity = 0

    // The read side; single reader. `refill` parks (in `queue.take`) until the
    // writer publishes a block or finishes.
    val stream: (Stream[medium] over Credit)^ = new Stream[medium](using addressable0):
      type Transport = Credit

      // Untracked as `current` above: blocks adopted from the ring are owned by
      // this (single-reader) stream.
      @caps.unsafe.untrackedCaptures
      private var storage: addressable0.Storage =
        addressable0.allocate(0).asInstanceOf[addressable0.Storage]
      private var start0: Int = 0
      private var limit0: Int = 0
      private var end0: Int = 0
      private var ended: Boolean = false
      // Whether the block currently in `storage` was writer-minted (so its
      // storage may be returned to the pool once drained) or a passed-through
      // backing (the caller's immutable data, never to be recycled).
      private var recyclable0: Boolean = false

      protected def window0: AnyRef = storage.asInstanceOf[AnyRef]
      def start: Int = start0
      def limit: Int = limit0

      update def skip(count: Int): Unit = start0 += count

      update def refill(demand: Credit): Optional[Int] =
        // Fail-fast: a producer failure pre-empts everything buffered, exactly
        // as the queue-clearing hand-off did.
        val error0 = if ended then null else core.error

        if error0 != null then
          ended = true
          throw error0
        else if limit0 > start0 then limit0 - start0
        else if ended then Unset
        else
          val granted = summon[Credit is Regulation].grant(demand)

          if granted == 0 then 0
          else if limit0 < end0 then
            limit0 += (end0 - limit0).min(granted)
            limit0 - start0
          else
            core.handoff.take() match
              case null =>
                ended = true
                Unset

              case received: Block =>
                // Single-ownership transfer: a minted block is never touched by
                // the writer again (proven on its side by the fresh re-mint in
                // `publish`), and a passed-through chunk backing is immutable.
                //
                // The outgoing block is fully drained (this branch runs only
                // once `limit0` reached `end0`) and its window is relinquished
                // the moment this `refill` returns a new one, so a ceiling-sized
                // minted block is safe to return to the writer for reuse. The
                // freelist's publication order carries the ownership across.
                if recycle && recyclable0 && addressable0.storageSize(storage) == ceiling
                then core.freelist.offer(storage.asInstanceOf[AnyRef])

                storage = received.storage.asInstanceOf[addressable0.Storage]
                recyclable0 = received.recyclable
                start0 = received.start
                end0 = received.start + received.size
                limit0 = start0 + received.size.min(granted)
                limit0 - start0

              case _ =>
                panic(m"unexpected value in conduit hand-off")

      override update def close(): Unit = core.handoff.close()

    (intake, stream)
