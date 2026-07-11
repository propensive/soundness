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

import prepositional.*
import rudiments.*
import vacuous.*

// The pull endpoint of a streaming pipeline: a mutable buffer whose readable
// window is exposed zero-copy to exactly one consumer. Backpressure is
// intrinsic to pulling — a consumer that does not call `refill` demands
// nothing — and the `demand` argument of each `refill` call is the reactive
// message bounding how much the upstream may produce to satisfy it. A `Stream`
// is transformed into a differently-typed `Stream` with `through`, and
// connected to an `Intake` with `flowTo`; a chain of `through`s involves no
// threads and no synchronization, executing as nested calls on the consumer's
// thread.
object Stream:
  // A single-chunk in-memory stream. The chunk is copied into fresh storage
  // once, at construction, so the window can be exposed mutably.
  def apply[medium](value: medium)(using addressable0: medium is Addressable)
  :   (Stream[medium] over Credit)^ =

    new Stream[medium]:
      type Transport = Credit

      private val size: Int = addressable0.length(value)
      private val storage: addressable0.Storage = addressable0.allocate(size.max(1))
      private var start0: Int = 0
      private var limit0: Int = 0
      private var loaded: Boolean = false

      protected def window0: AnyRef = storage.asInstanceOf[AnyRef]
      def start: Int = start0
      def limit: Int = limit0
      update def skip(count: Int): Unit = start0 += count

      update def refill(demand: Credit): Optional[Int] =
        if limit0 > start0 then limit0 - start0 else
          if !loaded then
            addressable0.copyChunk(value, 0, storage, 0, size)
            loaded = true

          val granted = summon[Credit is Regulation].grant(demand)
          val remaining = size - limit0

          if remaining == 0 then Unset
          else if granted == 0 then 0
          else
            limit0 += remaining.min(granted)
            limit0 - start0

  // Adapts a chunk iterator (the legacy interoperation shape) into a stream.
  // Demand bounds only how much of each chunk is exposed per refill, not the
  // iterator's own production, which is outside this stream's control.
  def apply[medium](iterator: Iterator[medium])(using addressable0: medium is Addressable)
  :   (Stream[medium] over Credit)^ =

    new Stream[medium]:
      type Transport = Credit

      private var storage: addressable0.Storage = addressable0.allocate(0)
      private var start0: Int = 0
      private var limit0: Int = 0
      private var size: Int = 0

      protected def window0: AnyRef = storage.asInstanceOf[AnyRef]
      def start: Int = start0
      def limit: Int = limit0
      update def skip(count: Int): Unit = start0 += count

      update def refill(demand: Credit): Optional[Int] =
        if limit0 > start0 then limit0 - start0 else
          val granted = summon[Credit is Regulation].grant(demand)

          if granted == 0 then 0
          else if limit0 < size then
            limit0 += (size - limit0).min(granted)
            limit0 - start0
          else
            def advance(): Optional[Int] =
              if !iterator.hasNext then Unset else
                val chunk = iterator.next()
                size = addressable0.length(chunk)

                if size == 0 then advance() else
                  if addressable0.storageSize(storage) < size then
                    storage = addressable0.allocate(size)

                  addressable0.copyChunk(chunk, 0, storage, 0, size)
                  start0 = 0
                  limit0 = size.min(granted)
                  limit0

            advance()

// A stream is a stateful capability: a bare `Stream` reference is read-only, and the
// exclusive `Stream[...]^` that factories return is required to refill, skip or close.
// `ExclusiveCapability, Stateful` rather than `Mutable`, so implementations may freely
// capture their sources (iterators, queues, sockets), which are not Unscoped.
trait Stream[medium](using val addressable: medium is Addressable)
extends caps.ExclusiveCapability, caps.Stateful:
  type Transport

  // Ensure at least one element is readable (blocking if necessary),
  // producing no more than `demand` permits, and return the number of
  // readable elements, i.e. `limit - start`. Returns `0` only when `demand`
  // grants nothing; returns `Unset` at end-of-stream. If unconsumed elements
  // remain in the window, they are reported without producing more.
  update def refill(demand: Transport): Optional[Int]

  // Zero-copy view of this stream's buffer; elements `start until limit` are
  // readable. Valid only until the next `refill` or `close` — the same
  // single-owner discipline as `Cursor.unsafeBuffer`, and the hook by which
  // separation checking will later enforce it. Implementations provide the
  // untyped `window0`; since `Addressable` instances are unique per medium,
  // the cast in `window` is sound.
  final def window(using Unsafe): addressable.Storage =
    window0.asInstanceOf[addressable.Storage]

  protected def window0: AnyRef
  def start: Int
  def limit: Int

  // Consume `count` elements of the window without materializing them.
  update def skip(count: Int): Unit

  // Release resources, propagating upstream through the whole chain.
  update def close(): Unit = ()
