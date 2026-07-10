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

extension [in, transport](stream: Stream[in] over transport)
  // Pull-composition: a differently-typed `Stream` whose refills translate
  // demand through the stage and pull from this stream. Runs entirely on
  // the consumer's thread. The stage may be a raw `Duct` or any descriptor
  // value with a `Ductile` instance.
  def through[stage](stage: stage)
    ( using ductile: (stage is Ductile by in) { type Upstream = transport },
            buffering: Buffering )
  :   Stream[ductile.Result] over ductile.Transport =

    val duct = ductile.duct(stage)

    new Stream[ductile.Result](using duct.output):
      type Transport = duct.Transport

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
      def skip(count: Int): Unit = start0 += count

      def refill(demand: duct.Transport): Optional[Int] =
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

      override def close(): Unit =
        duct.close()
        stream.close()

  // The pump loop connecting a pull-chain to a push-chain, on the calling
  // thread: poll the intake's demand, refill with it, transfer the window.
  // This is the only place data crosses from the pull side to the push side
  // of a pipeline.
  def flowTo(intake: Intake[in] over transport): Unit =
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

extension [out, transport](intake: Intake[out] over transport)
  // Push-composition: a differently-typed `Intake` which reports translated
  // demand, and whose commits step synchronously through the stage into
  // this intake's writable window. The same stage value serves `through`
  // and `accepting`; only the attachment differs.
  def accepting[stage](stage: stage)
    ( using ductile: (stage is Ductile to out) { type Transport = transport },
            buffering: Buffering )
  :   Intake[ductile.Operand] over ductile.Upstream =

    val duct = ductile.duct(stage)

    new Intake[ductile.Operand](using duct.input):
      type Transport = duct.Upstream

      private val capacity: Int = buffering.capacity(duct.input.substrate)
      private val storage: duct.input.Storage = duct.input.allocate(capacity)
      private var mark0: Int = 0

      def demand: duct.Upstream = duct.translate(intake.demand)
      protected def buffer0: AnyRef = storage.asInstanceOf[AnyRef]
      def mark: Int = mark0

      // `commit` always drains the whole buffer through the duct (partial
      // atoms are carried in duct state, not here), so all space is free.
      def reserve(min: Int): Int = capacity - mark0

      def commit(count: Int): Unit =
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

      override def flush(): Unit = intake.flush()

      def finish(): Unit =
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

      override def fail(error: Throwable): Unit =
        duct.close()
        intake.fail(error)
