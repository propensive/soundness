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

import fulminate.*
import prepositional.*
import rudiments.*
import vacuous.*

export zephyrine.internal.{Credit, Datum}

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

extension [in, transport](consume stream: (Stream[in] over transport)^)
  // Pull-composition: a differently-typed `Stream` whose refills translate
  // demand through the stage and pull from this stream. Runs entirely on
  // the consumer's thread. The stage may be a raw `Duct` or any descriptor
  // value with a `Ductile` instance.
  def through[stage](consume stage: stage^)
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
  def flowTo(consume intake: (Intake[in] over transport)^): Unit =
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
  // this intake's writable window. The same stage value serves `through`
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
  // Drain the stream, applying `operation` to each successive window. The lambda
  // receives the storage, start index and element count; it must not retain the storage.
  def foreachWindow(operation: (AnyRef, Int, Int) => Unit)(using buffering: Buffering): Unit =
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

    foreachWindow: (storage, start, count) =>
      addressable.cloneStorage(storage.asInstanceOf[addressable.Storage], start, count)(target)

    addressable.build(target)

  // Drain the stream, threading an accumulator through each window: the
  // window-level fold, the terminal counterpart to a pull chain. The operation
  // receives the running state, the window storage, its start index and its
  // element count; it must not retain the storage. Unlike an element-wise fold,
  // this exposes the raw window, so a byte reduction runs over the array with
  // no per-element boxing.
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

      private val storage: duct.output.Storage = duct.output.allocate(capacity)
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
      private val storage: duct.input.Storage = duct.input.allocate(capacity)
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
