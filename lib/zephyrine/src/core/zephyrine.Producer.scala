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
import anticipation.*
import denominative.*
import prepositional.*
import vacuous.*

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
  // code must run on a separate fiber, since `put` blocks once the queue is full. The staging
  // block defaults to the buffering policy's capacity for the medium; `depth` keeps its own
  // shallow default rather than adopting `Buffering.depth`, since this queue holds materialized
  // chunks rather than recycled transfer blocks, so a conduit-deep queue would multiply retention
  // for no measured benefit.
  def apply[medium](block: Optional[Int] = Unset, depth: Int = 2)
    ( using addr: medium is Addressable, buffering: Buffering )
  :   (Channel[medium, addr.Operand])^ =

    Channel[medium, addr.Operand](block.or(buffering.capacity(addr.substrate)), depth)(using addr)

  // Synchronous: run `body`, accumulating directly into a builder, and return the whole value. No
  // concurrency, no chunk buffer, and none of the streaming path's single-thread deadlock risk.
  // The hint sizes the initial builder: small by default, since the builder doubles as it fills
  // (the copying that costs for a large output is the same from any start), whereas a large
  // start is paid in full by every small output — a 4096 default was the biggest single
  // allocation of rendering a one-line JSON response.
  def collect[medium](using addressable: medium is Addressable)(hint: Int = 256)
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

  // Synchronous streaming: each staging block is handed to `consume` on the caller's thread as
  // it fills, so the producing code runs inline — no fiber, no queue — and the consumer still
  // sees the output in pieces before it is complete. This is the model of Jackson's generator
  // writing to an `OutputStream`, and the right shape for a push sink (a file, a socket, an
  // `OutputStream`); a pull consumer (an `Iterator`, an HTTP body) needs `apply`'s channel.
  // `finish()` flushes the partial last block.
  // Like `collect`, the producing `body` runs inline, so the sink never escapes and `deliver`
  // need not be consumed; the partial last block is flushed once `body` returns.
  def sink[medium](deliver: medium => Unit, block: Optional[Int] = Unset)
    ( using addr: medium is Addressable, buffering: Buffering )
    ( body: ((Producer[medium] { type Operand = addr.Operand })^) => Unit )
  :   Unit =

    val producer =
      Sink[medium, addr.Operand](deliver, block.or(buffering.capacity(addr.substrate)))(using addr)

    body(producer)
    producer.finish()

  // The staging shared by the streaming producers: writes fill a block of `block` elements,
  // and each full block (and, at `finish`, the partial last one) is materialized and published
  // — to a queue by `Channel`, to a consumer by `Sink`.
  abstract class Staged[medium, operand](block: Int)
    ( using val addressable: medium is Addressable { type Operand = operand } )
  extends Producer[medium]:

    type Operand = operand

    // Untracked, cast-erased: reached only through this producer.
    @caps.unsafe.untrackedCaptures
    private val current: addressable.Storage =
      addressable.allocate(block).asInstanceOf[addressable.Storage]
    private var index: Ordinal = Prim

    private inline def free: Int = block - index.n0

    protected update def publishChunk(chunk: medium): Unit

    private inline update def publish(): Unit =
      if index != Prim then
        publishChunk(addressable.materialize(current, 0, index.n0))
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

    update def finish(): Unit = publish()

  // Streaming to a queue: full blocks wait (with backpressure) for the consuming thread to drain
  // them through `iterator`, so the producing code must run on its own fiber.
  final class Channel[medium, operand](block: Int, depth: Int)
    ( using medium is Addressable { type Operand = operand } )
  extends Staged[medium, operand](block):

    private object Done

    private val queue: juc.ArrayBlockingQueue[medium | Done.type] =
      juc.ArrayBlockingQueue(depth)

    protected update def publishChunk(chunk: medium): Unit = queue.put(chunk)

    override update def finish(): Unit =
      super.finish()
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

  // Streaming to a consumer: each full block is handed to `consume` inline, on the producing
  // thread, so no fiber or queue is involved; see `Producer.sink`.
  final class Sink[medium, operand](deliver: medium => Unit, block: Int)
    ( using medium is Addressable { type Operand = operand } )
  extends Staged[medium, operand](block):

    protected update def publishChunk(chunk: medium): Unit = deliver(chunk)

  // A consumer lent a filled block: the block's storage as a `Region` with its branded
  // extent, valid only for the duration of the call — the discipline of `Stream.lend` — so
  // nothing is copied, and a consumer that must keep the bytes materializes them itself.
  type Lending[medium] = (region: Region[medium]) => (Interval in region.type) => Unit

  // Streaming text as UTF-8: characters are encoded straight into a byte block, and each full
  // block is lent to `lending` inline, so a text serializer writes bytes to a socket or an
  // `OutputStream` without materializing a `Text` per block and encoding it again, and without
  // copying the block. A lone surrogate encodes as U+FFFD, as `String.getBytes` would.
  def lendUtf8(lending: Lending[Data], block: Optional[Int] = Unset)(using buffering: Buffering)
    ( body: ((Producer[Text] { type Operand = Char })^) => Unit )
  :   Unit =

    val producer = Utf8Sink(lending, block.or(buffering.capacity(Substrate.Bytes)))
    body(producer)
    producer.finish()

  // The owning form of `lendUtf8`: each block is delivered as a fresh `Data` the consumer may
  // keep.
  def utf8(deliver: Data => Unit, block: Optional[Int] = Unset)(using buffering: Buffering)
    ( body: ((Producer[Text] { type Operand = Char })^) => Unit )
  :   Unit =

    lendUtf8(region => interval => deliver(region.materialize(interval)), block)(body)

  final class Utf8Sink(lending: Lending[Data], block: Int) extends Producer[Text]:
    type Operand = Char

    // Encoding goes through the JDK's UTF-8 `CharsetEncoder`, whose ASCII path is vectorized:
    // characters are inflated into a reused scratch `CharBuffer` and encoded into the byte block,
    // which is a heap `ByteBuffer` (a Java object the checker does not track, reached only through
    // this producer), materialized by copying its filled prefix. The encoder keeps a trailing high
    // surrogate between calls, and a malformed pair encodes as U+FFFD, as `String.getBytes` would.
    private val encoder: java.nio.charset.CharsetEncoder =
      java.nio.charset.StandardCharsets.UTF_8.nn.newEncoder().nn
      . onMalformedInput(java.nio.charset.CodingErrorAction.REPLACE).nn
      . onUnmappableCharacter(java.nio.charset.CodingErrorAction.REPLACE).nn

    private val current: java.nio.ByteBuffer = java.nio.ByteBuffer.allocate(block).nn

    // The scratch is a raw array with its own fill count — a `CharBuffer`'s position bookkeeping
    // per `put` measured as most of the sink's cost — wrapped only when a whole block is encoded.
    private val scratch: scala.Array[Char] = new scala.Array[Char](block.max(8))
    private val scratchView: java.nio.CharBuffer = java.nio.CharBuffer.wrap(scratch).nn
    private var filled: Int = 0

    private update def publish(): Unit =
      if current.position() > 0 then
        Region.over[Data, Unit](current.array().nn, 0, current.position())(lending)
        current.clear()

    // Encodes the scratch, publishing the block each time it fills; a trailing high surrogate is
    // moved to the front of the scratch for the next call to complete.
    private update def drain(endOfInput: Boolean): Unit =
      val input = java.nio.CharBuffer.wrap(scratch, 0, filled).nn
      var result = encoder.encode(input, current, endOfInput).nn

      while result.isOverflow do
        publish()
        result = encoder.encode(input, current, endOfInput).nn

      val remaining = input.remaining()
      if remaining > 0 then System.arraycopy(scratch, filled - remaining, scratch, 0, remaining)
      filled = remaining

    update def put(source: Text): Unit = put(source, Prim, source.s.length)

    update def put(source: Text, offset: Ordinal, size: Int): Unit =
      val string = source.s
      val end = offset.n0 + size
      var from = offset.n0

      // Shape 3 (a zephyrine internal): `from` advances through `[offset, offset + size)` of
      // `string` by the number of characters that fit the scratch, which is drained only when
      // full — so the encoder runs over whole blocks — and is never full after a `drain` (at
      // most one pending surrogate remains), so every step makes progress.
      while from < end do
        if filled == scratch.length then drain(false)
        val count = (end - from).min(scratch.length - filled)
        string.getChars(from, from + count, scratch, filled)
        filled += count
        from += count

    update def push(operand: Char): Unit =
      if filled == scratch.length then drain(false)
      scratchView.put(filled, operand)
      filled += 1

    update def finish(): Unit =
      drain(true)
      var result = encoder.flush(current).nn

      while result.isOverflow do
        publish()
        result = encoder.flush(current).nn

      filled = 0
      encoder.reset()
      publish()

  // The media a text serializer's push form can be delivered as: `Text` blocks through `sink`,
  // or UTF-8 `Data` blocks through `utf8`. A format's `emit[medium](value, deliver)` summons
  // one, so the serializer itself is written once, against `Producer[Text]`.
  trait Emission[medium]:
    def run(deliver: medium => Unit)
      ( body: ((Producer[Text] { type Operand = Char })^) => Unit )
      ( using Buffering )
    :   Unit

  object Emission:
    given text: Emission[Text]:
      def run(deliver: Text => Unit)
        ( body: ((Producer[Text] { type Operand = Char })^) => Unit )
        ( using Buffering )
      :   Unit =

        sink[Text](deliver)(body)

    given data: Emission[Data]:
      def run(deliver: Data => Unit)
        ( body: ((Producer[Text] { type Operand = Char })^) => Unit )
        ( using Buffering )
      :   Unit =

        utf8(deliver)(body)

// A producer is a stateful capability: writing requires an exclusive reference, and the
// root classification here lets `Intake` (and every other implementation) mark its
// writes as update methods.
trait Producer[medium] extends caps.ExclusiveCapability, caps.Stateful:
  type Operand
  update def put(source: medium): Unit
  update def put(source: medium, offset: Ordinal, size: Int): Unit

  // Write a single element: a `Char` for `Producer[Text]`, a `Byte` for `Producer[Data]`.
  update def push(operand: Operand): Unit
