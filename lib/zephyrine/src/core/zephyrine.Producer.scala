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

import java.util.concurrent as juc

import anticipation.Data
import denominative.*

// A sink into which a value of one medium is written in pieces. The same producing code can be
// driven two ways without duplicating it: `Producer.collect` runs it synchronously and returns the
// whole result, while `Producer.apply` (streaming) writes chunks into a bounded buffer drained
// lazily through `iterator` — the producing code then runs on a separate fiber. Neither path
// allocates per `put` along a contiguous run. `Operand` is the element type — `Char` for
// `Producer[Text]`, `Byte` for `Producer[Data]` — and types the element-at-a-time `push`.
object Producer:
  // A byte producer: `Producer[Data]` with its element type pinned to `Byte`, the shape binary
  // encoders (CBOR, Protobuf, …) write into. `Producer[Data](…)` / `Producer.collect[Data]` already
  // surface this refinement; this alias names it for helper signatures.
  type Bytes = Producer[Data] { type Operand = Byte }

  // Streaming: chunks are queued (with backpressure) and drained through `iterator`. The producing
  // code must run on a separate fiber, since `put` blocks once the queue is full.
  def apply[medium](block: Int = 4096, depth: Int = 2)(using addr: medium is Addressable)
  :   (Channel[medium] { type Operand = addr.Operand })^ =

    Channel(block, depth)(using addr)

  // Synchronous: run `body`, accumulating directly into a builder, and return the whole value. No
  // concurrency, no chunk buffer, and none of the streaming path's single-thread deadlock risk.
  def collect[medium](using addressable: medium is Addressable)(hint: Int = 4096)
    ( body: ((Producer[medium] { type Operand = addressable.Operand })^) => Unit )
  :   medium =

    val target = addressable.blank(hint)

    val producer = new Producer[medium]:
      type Operand = addressable.Operand

      def put(source: medium): Unit =
        val size = addressable.length(source)
        if size > 0 then addressable.clone(source, Prim, (size - 1).z)(target)

      def put(source: medium, offset: Ordinal, size: Int): Unit =
        if size > 0 then addressable.clone(source, offset, (offset.n0 + size - 1).z)(target)

      def push(operand: Operand): Unit = addressable.append(target, operand)

    body(producer)
    addressable.build(target)

  final class Channel[medium](block: Int, depth: Int)(using val addressable: medium is Addressable)
  extends Producer[medium]:

    type Operand = addressable.Operand

    private object Done

    private val queue: juc.ArrayBlockingQueue[medium | Done.type] =
      juc.ArrayBlockingQueue(depth)

    // Untracked, cast-erased: reached only through this producer.
    @caps.unsafe.untrackedCaptures
    private val current: addressable.Storage =
      addressable.allocate(block).asInstanceOf[addressable.Storage]
    private var index: Ordinal = Prim

    private inline def free: Int = block - index.n0

    private inline update def publish(): Unit =
      if index != Prim then
        queue.put(addressable.materialize(current, 0, index.n0))
        index = Prim

    update def put(source: medium): Unit = put(source, Prim, addressable.length(source))

    update def put(source: medium, offset: Ordinal, size: Int): Unit =
      var done = 0

      while size - done > free do
        addressable.copyChunk(source, offset.n0 + done, current, index.n0, free)
        done += free
        index = (index.n0 + free).z
        publish()

      addressable.copyChunk(source, offset.n0 + done, current, index.n0, size - done)
      index = (index.n0 + size - done).z

      if free == 0 then publish()

    update def push(operand: Operand): Unit =
      addressable.storageUpdate(current, index.n0, operand)
      index = (index.n0 + 1).z
      if free == 0 then publish()

    update def finish(): Unit =
      publish()
      queue.put(Done)

    // The reader-side view: like a conduit's stream endpoint, it is owned by the
    // consuming thread and mediated by the queue's happens-before; the seal is that rim.
    lazy val iterator: Iterator[medium] = caps.unsafe.unsafeAssumePure(new Iterator[medium]:
      // The iterator is the reader-side view of the channel (non-Stateful by design:
      // scala.Iterator's methods cannot be update methods); its staging slot is untracked.
      @caps.unsafe.untrackedCaptures
      private var ready: medium | Done.type = Done

      def hasNext: Boolean =
        ready = queue.take().nn
        ready != Done

      def next(): medium = ready.asInstanceOf[medium])

// A producer is a stateful capability: writing requires an exclusive reference, and the
// root classification here lets `Intake` (and every other implementation) mark its
// writes as update methods.
trait Producer[medium] extends caps.ExclusiveCapability, caps.Stateful:
  type Operand
  update def put(source: medium): Unit
  update def put(source: medium, offset: Ordinal, size: Int): Unit

  // Write a single element: a `Char` for `Producer[Text]`, a `Byte` for `Producer[Data]`.
  update def push(operand: Operand): Unit
