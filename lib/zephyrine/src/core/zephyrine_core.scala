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

import anticipation.*
import fulminate.*
import hieroglyph.*
import prepositional.*
import rudiments.*
import vacuous.*

export zephyrine.internal.{Credit, Datum}

// Fluent construction: build an in-memory pull `Stream` from a streamable value
// or from a chunk iterator, in place of `Stream(value)` / `Stream(iterator)`.
extension [medium: Addressable](value: medium)
  def stream: (Stream[medium] over Credit)^ = Stream(value)

extension [medium: Addressable](iterator: Iterator[medium]^)
  def stream: (Stream[medium] over Credit)^{iterator, caps.any} = Stream(iterator)

extension [value](value: value)(using positionable: value is Positionable)
  // Locate the source `Position` of the node addressed by `path` within
  // `value`, or `Unset` if the path doesn't resolve (or the value carries no
  // position tracking). The path type is fixed by the value's `Positionable`
  // instance (e.g. a `Json` is located by a `JsonPointer`).
  def locate(path: positionable.Operand): Optional[positionable.Result] =
    positionable.locate(value, path)

  // As `locate`, but for the position of the object/mapping *key* matching the
  // final path segment (rather than its value).
  def locateKey(path: positionable.Operand): Optional[positionable.Result] =
    positionable.locateKey(value, path)

package lineation:
  inline given linefeedChars: Lineation:
    type Operand = Char

    inline def active: Boolean = true
    inline def track(datum: Char): Boolean = datum == '\n'

  inline given carriageReturnChar: Lineation:
    type Operand = Char

    inline def active: Boolean = true
    inline def track(datum: Char): Boolean = datum == '\r'

  inline given linefeedByte: Lineation:
    type Operand = Byte

    inline def active: Boolean = true
    inline def track(datum: Byte): Boolean = datum == 10

  inline given carriageReturnByte: Lineation:
    type Operand = Byte

    inline def active: Boolean = true
    inline def track(datum: Byte): Boolean = datum == 13

package parsing:
  // Bring into scope with `import parsing.trackPositions` to make the generic
  // `read`/`load` entry points of the position-aware parsers record source
  // positions, overriding the `PositionTracking.Off` companion default.
  given trackPositions: PositionTracking = PositionTracking.On

export Cursor.{Mark, Offset}

// A stream of parsed records: each window is an `IArray` chunk of them (the boxed
// medium), so credit counts records and `Buffering` sizes stage buffers by
// reference count. Records are immutable values, so they cross stage and thread
// boundaries by reference; a record must not itself hold a live endpoint. This
// alias is the conventional shape for record-granularity streaming (rows, events,
// frames, messages) between the windowed media (`Data`, `Text`) and materialized
// collections. The record type is unbounded here — the `Addressable` given that
// admits a record type governs at stream construction — but it must erase to a
// reference type.
type Records[record] = Stream[IArray[record]] over Credit

extension [in, transport](consume stream: (Stream[in] over transport)^)
  // Pull-composition: a differently-typed `Stream` whose refills translate
  // demand through the stage and pull from this stream. Runs entirely on
  // the consumer's thread. The stage may be a raw `Duct` or any descriptor
  // value with a `Ductile` instance.
  def via[stage](consume stage: stage^)
    ( using ductile: (stage is Ductile by in) { type Upstream = transport },
            buffering: Buffering )
  :   (Stream[ductile.Result] over ductile.Transport)^ =

    // Constructed in a helper: a local binding of the fresh duct would hide it from the
    // anonymous class that wraps it (the statement rule); consume parameters carry
    // explicit capture sets and hide nothing.
    throughDuct[in, ductile.Result, transport, ductile.Transport]
      (ductile.duct(stage), stream)

  // The pump loop connecting a pull-chain to a push-chain, on the calling
  // thread: poll the intake's demand, refill with it, transfer the window.
  // This is the only place data crosses from the pull side to the push side
  // of a pipeline.
  def pump(consume intake: (Intake[in] over transport)^): Unit =
    def loop(): Unit =
      stream.refill(intake.demand) match
        case Unset =>
          intake.finish()

        case count: Int =>
          if count > 0 then
            intake.absorb
              ( stream.window(using Unsafe).asInstanceOf[intake.addressable.Storage],
                stream.start,
                count )

            stream.skip(count)
          else
            intake.reserve(1)

          loop()

    try loop() finally stream.close()

extension [out, transport](consume intake: (Intake[out] over transport)^)
  // Push-composition: a differently-typed `Intake` which reports translated
  // demand, and whose commits step synchronously through the stage into
  // this intake's writable window. The same stage value serves `via`
  // and `accepting`; only the attachment differs.
  def accepting[stage](consume stage: stage^)
    ( using ductile: (stage is Ductile to out) { type Transport = transport },
            buffering: Buffering )
  :   (Intake[ductile.Operand] over ductile.Upstream)^ =

    // See `throughDuct` above.
    acceptingDuct[ductile.Operand, out, ductile.Upstream, transport]
      (ductile.duct(stage), intake)


// ─── terminal operations and explicit replay ───────────────────────────────────────────
//
// The safe front-end replacing the LazyList views: pipeline stages compose with the
// consume-typed `through`, and these terminal operations drain an exclusive endpoint
// without exposing its window — each borrow is read, used and skipped before the next
// refill. `memoize` is the explicit replacement for LazyList's implicit caching: it
// drains the stream once into an immutable value, which may then be shared freely.

extension [medium](consume stream: (Stream[medium] over Credit)^)
  // Drain the stream, applying `operation` to each successive window (its raw
  // storage, start index and element count); it must not retain the storage.
  // For a byte stream the `Stream[Data]` overload below types the window as
  // `Array[Byte]`, so the common case needs no cast.
  def sweep(operation: (AnyRef, Int, Int) => Unit)(using buffering: Buffering): Unit =
    // A drain loop wants boundary-transfer-sized credit: a staging-block ask
    // would slice each larger window into many partial refills.
    val block = buffering.transfer(stream.addressable.substrate)

    def loop(): Unit = stream.refill(Credit(block)) match
      case count: Int =>
        operation(stream.window(using Unsafe).asInstanceOf[AnyRef], stream.start, count)
        stream.skip(count)
        loop()

      case _ => ()

    try loop() finally stream.close()

  // Drain the stream into a single immutable value: the explicit, bounded replacement
  // for a LazyList's implicit memoization. The result is frozen and freely shareable.
  def memoize(using buffering: Buffering): medium =
    val addressable = stream.addressable
    val target = addressable.blank(buffering.capacity(addressable.substrate))

    sweep: (storage, start, count) =>
      addressable.cloneStorage(storage.asInstanceOf[addressable.Storage], start, count)(target)

    addressable.build(target)

  // Drain the stream, threading an accumulator through each window: the
  // window-level fold, the terminal counterpart to a pull chain. The operation
  // receives the running state, the raw window storage, its start index and its
  // element count; it must not retain the storage. Unlike an element-wise fold,
  // this exposes the raw window, so a byte reduction runs over the array with no
  // per-element boxing. (The `Stream[Data]` overload below types the window.)
  def fold[state](initial: state)(operation: (state, AnyRef, Int, Int) => state)
    (using buffering: Buffering)
  :   state =

    val block = buffering.transfer(stream.addressable.substrate)

    def loop(state: state): state = stream.refill(Credit(block)) match
      case count: Int =>
        val state2 =
          operation(state, stream.window(using Unsafe).asInstanceOf[AnyRef], stream.start, count)

        stream.skip(count)
        loop(state2)

      case _ => state

    try loop(initial) finally stream.close()

  // The first `count` elements, then end-of-stream. The remainder of the
  // upstream is released unread on `close`. (FS2 `take`, ZIO `ZStream.take`.)
  def take(count: Long): (Stream[medium] over Credit)^ = takeStream(stream, count)

  // Skip the first `count` elements, then pass the rest through unchanged. The
  // skipped elements are pulled and discarded on the first `refill`. (FS2
  // `drop`, ZIO `ZStream.drop`.)
  def drop(count: Long): (Stream[medium] over Credit)^ = dropStream(stream, count)

  // The legacy view: a lazy `LazyList` of one materialized chunk per refill,
  // strictly demand-driven — construction pulls nothing, and each forced cell
  // pulls at most one refill (the deferral `Cursor.remainder` documents). This
  // is the audited bridge for consumers not yet converted to the kernel:
  // `LazyList` is pure, so it cannot carry the endpoint capability its cells
  // close over, and the ownership discipline is suspended beyond this point —
  // implicit memoization retains every forced chunk. Prefer the kernel
  // terminals above; never introduce new bridges elsewhere.
  def toLazyList(using buffering: Buffering): LazyList[medium] =
    val block = buffering.transfer(stream.addressable.substrate)

    def recur(): LazyList[medium] = stream.refill(Credit(block)) match
      case count: Int =>
        val chunk =
          stream.addressable.materialize(stream.window(using Unsafe), stream.start, count)

        stream.skip(count)
        chunk #:: recur()

      case _ =>
        stream.close()
        LazyList.empty

    LazyList.empty.lazyAppendedAll(recur())

extension [record](consume stream: (Stream[IArray[record]] over Credit)^)
  // Element-wise access to a stream of records: a single-consumer iterator over
  // the records of successive windows, in order. The iterator owns the endpoint:
  // it closes the stream when it reports exhaustion, so a consumer must drain it
  // (or close the stream by other means) to release the upstream. Per-record
  // combinators come free from the `Iterator` interface (and rudiments' `each`);
  // window-level access for hot loops is `sweep`/`fold` above.
  def records(using Buffering): Iterator[record]^ = recordIterator(stream)

// A pull endpoint lending a bounded view of a cursor: the next `length` elements
// (or all remaining, if `Unset`), exposed zero-copy — the cursor's own buffer
// backs each window, and skipping the stream advances the cursor. The cursor is
// LENT, not consumed: closing this stream leaves it open, positioned at the
// boundary, and the caller resumes it there. This is the shape of a delimited
// payload inside a longer parse: an HTTP body before the next pipelined request,
// an archive entry before the next header, a multipart part before the next
// boundary.
//
// While the lent stream is live, the caller must not touch the cursor: the
// aliasing is deliberate, and the result's capture of the cursor is what the
// checker has to reason about it — temporal validity is not fully expressible,
// so this factory is an audited point. Bulk skips bypass lineation, so lend only
// cursors whose position tracking is inactive (the default for protocol and
// binary parsing).
def streamOf[data](cursor: Cursor[data, {}]^, length: Optional[Long] = Unset)
:   (Stream[data] over Credit)^{cursor, caps.any} =

    new Stream[data](using cursor.addressable):
      type Transport = Credit

      private var remaining: Long = length.or(Long.MaxValue)

      // A snapshot of the cursor's buffer state, taken by `refill`: only update
      // methods may access the exclusive cursor, so the read-only accessors
      // below serve the snapshot. The buffer reference is cast-erased and never
      // exposed before the first refill (hence the pure placeholder initial).
      private var storage: AnyRef = ""
      private var start0: Int = 0
      private var limit0: Int = 0

      protected def window0: AnyRef = storage
      def start: Int = start0
      def limit: Int = limit0

      update def skip(count: Int): Unit =
        remaining -= count
        start0 += count
        cursor.unsafeAdvanceBy(count)(using Unsafe)

      // Demand does not bound exposure: the cursor refills by its own bounded
      // block, which is what bounds memory (as the iterator factory on
      // `Stream`'s companion notes of its chunks). An unconsumed window is
      // reported, not extended: `cursor.more` short-circuits while buffered
      // elements remain, so re-snapshotting it is free.
      update def refill(demand: Credit): Optional[Int] =
        if remaining <= 0 then Unset
        else if cursor.more then
          storage = cursor.unsafeBuffer(using Unsafe).asInstanceOf[AnyRef]
          start0 = cursor.unsafePos(using Unsafe)
          val available = cursor.unsafeWriteEnd(using Unsafe) - start0
          val readable = if remaining < available then remaining.toInt else available
          limit0 = start0 + readable
          readable
        else Unset

      // Deliberately not overridden: `close()` must leave the lent cursor open
      // for the caller to resume.

private def recordIterator[record]
  ( consume stream: (Stream[IArray[record]] over Credit)^ )
  ( using buffering: Buffering )
:   Iterator[record]^ =

    new Iterator[record]:
      private val block: Int = buffering.transfer(Substrate.Boxes)

      // The current window: records `index until limit` of `storage` are
      // unread; `consumed` is skipped lazily, just before the next refill, per
      // the refill contract (an unskipped window is reported, not extended).
      // A stdlib class cannot extend `Stateful`, so its state is untracked
      // (the `inputStream` adapter's precedent).
      @caps.unsafe.untrackedCaptures
      private var storage: Array[AnyRef] = new Array[AnyRef](0)
      @caps.unsafe.untrackedCaptures
      private var index: Int = 0
      @caps.unsafe.untrackedCaptures
      private var limit: Int = 0
      @caps.unsafe.untrackedCaptures
      private var consumed: Int = 0
      @caps.unsafe.untrackedCaptures
      private var done: Boolean = false

      def hasNext: Boolean = index < limit || (!done && replenish())

      private def replenish(): Boolean =
        if consumed > 0 then
          stream.skip(consumed)
          consumed = 0

        stream.refill(Credit(block)) match
          case count: Int =>
            storage = stream.window(using Unsafe).asInstanceOf[Array[AnyRef]]
            index = stream.start
            limit = stream.start + count
            consumed = count
            index < limit

          case _ =>
            done = true
            stream.close()
            false

      def next(): record =
        if !hasNext then panic(m"the record stream is exhausted")
        val result = storage(index).asInstanceOf[record]
        index += 1
        result

private def throughDuct[in, out, upTransport, downTransport]
  ( consume duct:
      (Duct[in, out] { type Transport = downTransport; type Upstream = upTransport })^,
    consume stream: (Stream[in] over upTransport)^ )
  ( using buffering: Buffering )
:   (Stream[out] over downTransport)^ =

    new Stream[out](using duct.output):
      type Transport = downTransport

      private val capacity: Int =
        buffering.capacity(duct.output.substrate).max(duct.quantum)

      // Untracked, cast-erased: reached only through this endpoint.
      @caps.unsafe.untrackedCaptures
      private val storage: duct.output.Storage =
        duct.output.allocate(capacity).asInstanceOf[duct.output.Storage]
      private var start0: Int = 0
      private var limit0: Int = 0
      private var ended: Boolean = false
      private var flushed: Boolean = false

      protected def window0: AnyRef = storage.asInstanceOf[AnyRef]
      def start: Int = start0
      def limit: Int = limit0
      update def skip(count: Int): Unit = start0 += count

      update def refill(demand: downTransport): Optional[Int] =
        if limit0 > start0 then limit0 - start0
        else if flushed then Unset
        else
          start0 = 0
          limit0 = 0
          val granted = duct.regulation.grant(demand)

          if granted == 0 then 0 else
            val space = capacity.min(granted.max(duct.quantum))

            while limit0 == 0 && !flushed do
              if ended then
                val produced = duct.flush(storage, limit0, space - limit0)
                if produced == 0 then flushed = true else limit0 += produced
              else
                stream.refill(duct.translate(demand)) match
                  case count: Int =>
                    // The downstream demand granted at least `quantum`, so a
                    // starved upstream means the duct's `translate` violated
                    // its contract; spinning here would livelock silently.
                    if count == 0
                    then panic(m"a duct translated a productive demand into one granting nothing")
                    else
                      // `Addressable` instances are unique per medium, so the
                      // stream's storage and the duct's input storage
                      // coincide, even though their paths differ.
                      val progress =
                        duct.step
                          ( stream.window(using Unsafe).asInstanceOf[duct.input.Storage],
                            stream.start,
                            count,
                            storage,
                            limit0,
                            space - limit0 )

                      stream.skip(progress.consumed)
                      limit0 += progress.produced

                  case _ =>
                    ended = true

            if flushed && limit0 == start0 then Unset else limit0 - start0

      override update def close(): Unit =
        duct.close()
        stream.close()

// `take`/`drop` wrappers, in helpers rather than inline in the extension for
// the same reason as `throughDuct`: a local binding of the upstream would hide
// it from the anonymous class, whereas the consumed parameter carries its
// capture explicitly. Each delegates window access to the upstream (zero copy)
// and only adjusts the element budget.

private def takeStream[medium](consume stream: (Stream[medium] over Credit)^, count: Long)
:   (Stream[medium] over Credit)^ =

    new Stream[medium](using stream.addressable):
      type Transport = Credit

      private var remaining: Long = count.max(0)

      protected def window0: AnyRef = stream.window(using Unsafe).asInstanceOf[AnyRef]
      def start: Int = stream.start

      def limit: Int =
        val available = stream.limit - stream.start
        stream.start + (if remaining < available then remaining.toInt else available)

      update def skip(elements: Int): Unit =
        remaining -= elements
        stream.skip(elements)

      update def refill(demand: Credit): Optional[Int] =
        if remaining <= 0 then Unset else stream.refill(demand) match
          case available: Int =>
            if remaining < available then remaining.toInt else available

          case _ => Unset

      override update def close(): Unit = stream.close()

private def dropStream[medium](consume stream: (Stream[medium] over Credit)^, count: Long)
:   (Stream[medium] over Credit)^ =

    new Stream[medium](using stream.addressable):
      type Transport = Credit

      private var pending: Long = count.max(0)

      protected def window0: AnyRef = stream.window(using Unsafe).asInstanceOf[AnyRef]
      def start: Int = stream.start
      def limit: Int = stream.limit
      update def skip(elements: Int): Unit = stream.skip(elements)

      update def refill(demand: Credit): Optional[Int] =
        var ended: Boolean = false

        while pending > 0 && !ended do
          stream.refill(Credit(pending.min(Int.MaxValue.toLong))) match
            case available: Int =>
              val discard = available.toLong.min(pending).toInt
              stream.skip(discard)
              pending -= discard

            case _ => ended = true

        if ended then Unset else stream.refill(demand)

      override update def close(): Unit = stream.close()

private def acceptingDuct[in, out, upTransport, downTransport]
  ( consume duct:
      (Duct[in, out] { type Transport = downTransport; type Upstream = upTransport })^,
    consume intake: (Intake[out] over downTransport)^ )
  ( using buffering: Buffering )
:   (Intake[in] over upTransport)^ =

    new Intake[in](using duct.input):
      type Transport = upTransport

      private val capacity: Int = buffering.capacity(duct.input.substrate)
      // Untracked, cast-erased: reached only through this endpoint.
      @caps.unsafe.untrackedCaptures
      private val storage: duct.input.Storage =
        duct.input.allocate(capacity).asInstanceOf[duct.input.Storage]
      private var mark0: Int = 0

      def demand: upTransport = duct.translate(intake.demand)
      protected def buffer0: AnyRef = storage.asInstanceOf[AnyRef]
      def mark: Int = mark0

      // `commit` always drains the whole buffer through the duct (partial
      // atoms are carried in duct state, not here), so all space is free.
      update def reserve(min: Int): Int = capacity - mark0

      update def commit(count: Int): Unit =
        mark0 += count
        var offset: Int = 0

        while offset < mark0 do
          val free = intake.reserve(duct.quantum)

          val progress =
            duct.step
              ( storage,
                offset,
                mark0 - offset,
                intake.buffer(using Unsafe).asInstanceOf[duct.output.Storage],
                intake.mark,
                free )

          intake.commit(progress.produced)
          offset += progress.consumed

        mark0 = 0

      override update def flush(): Unit = intake.flush()

      update def finish(): Unit =
        var produced: Int = -1

        while produced != 0 do
          val free = intake.reserve(duct.quantum)

          produced =
            duct.flush
              ( intake.buffer(using Unsafe).asInstanceOf[duct.output.Storage],
                intake.mark,
                free )

          if produced > 0 then intake.commit(produced)

        duct.close()
        intake.finish()

      override update def fail(error: Throwable): Unit =
        duct.close()
        intake.fail(error)
