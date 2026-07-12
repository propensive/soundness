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
package turbulence

import java.io as ji
import java.nio as jn

import anticipation.*
import contingency.*
import prepositional.*
import rudiments.*
import vacuous.*
import zephyrine.*

// A target which can be opened as a push endpoint: the successor to
// `Writable`, accepting writes incrementally through an `Intake` instead of
// consuming a whole `LazyList`. As with `Writable`, a write failure `raise`s
// a typed `StreamError` through an `Emit` captured by the given — a writer
// only reports a cut, never aborts. `finish` closes the underlying resource,
// matching `Writable`'s end-of-stream behaviour.
object Sink:
  given outputStream: [output <: ji.OutputStream] => (streamCut: Emit[StreamError], buffering: Buffering)
  =>  ((output is Sink by Data over Credit)^{streamCut}) =
    // Laundered for the Scala.js pipeline (see #1520): its pre-capture-checking
    // SAM expansion turns this given into an anonymous class that hides the
    // evidence; the pure thunk empties the capture.
    val cut: () -> AnyRef = caps.unsafe.unsafeAssumePure { () => streamCut.asInstanceOf[AnyRef] }

    value =>
      new Intake[Data]:
        type Transport = Credit

        private val block: Int = summon[Buffering].capacity(Substrate.Bytes)
        private val storage: Array[Byte] = new Array[Byte](block)
        private var mark0: Int = 0
        private var total: Long = 0
        private var broken: Boolean = false

        def demand: Credit = Credit(if broken then 0 else Long.MaxValue)
        protected def buffer0: AnyRef = storage.asInstanceOf[AnyRef]
        def mark: Int = mark0

        update def reserve(min: Int): Int =
          val free = block - mark0

          if free >= min then free else
            drain()
            block

        update def commit(count: Int): Unit =
          mark0 += count
          if mark0 == block then drain()

        override update def flush(): Unit =
          drain()
          try value.flush() catch case _: ji.IOException => ()

        update def finish(): Unit =
          drain()
          // Pre-read into a local: `raise`'s capture-polymorphic argument may not
          // hide this instance's state.
          val written: Long = total
          try value.close() catch case _: ji.IOException => raise(StreamError(written.b))(using cut().asInstanceOf[Emit[StreamError]^])

        private update def drain(): Unit =
          if mark0 > 0 && !broken then
            try
              value.write(storage, 0, mark0)
              value.flush()
              total += mark0
            catch case _: ji.IOException =>
              broken = true
              { val written: Long = total; raise(StreamError(written.b))(using cut().asInstanceOf[Emit[StreamError]^]) }

          mark0 = 0

  given channel: (streamCut: Emit[StreamError], buffering: Buffering)
  =>  ((jn.channels.WritableByteChannel is Sink by Data over Credit)^{streamCut}) =
    // Laundered for the Scala.js pipeline (see #1520): its pre-capture-checking
    // SAM expansion turns this given into an anonymous class that hides the
    // evidence; the pure thunk empties the capture.
    val cut: () -> AnyRef = caps.unsafe.unsafeAssumePure { () => streamCut.asInstanceOf[AnyRef] }

    value =>
      new Intake[Data]:
        type Transport = Credit

        private val block: Int = summon[Buffering].capacity(Substrate.Bytes)
        private val storage: Array[Byte] = new Array[Byte](block)
        private var mark0: Int = 0
        private var total: Long = 0
        private var broken: Boolean = false

        def demand: Credit = Credit(if broken then 0 else Long.MaxValue)
        protected def buffer0: AnyRef = storage.asInstanceOf[AnyRef]
        def mark: Int = mark0

        update def reserve(min: Int): Int =
          val free = block - mark0

          if free >= min then free else
            drain()
            block

        update def commit(count: Int): Unit =
          mark0 += count
          if mark0 == block then drain()

        override update def flush(): Unit = drain()

        update def finish(): Unit =
          drain()
          // Pre-read into a local, as above.
          val written: Long = total
          try value.close() catch case _: Exception => raise(StreamError(written.b))(using cut().asInstanceOf[Emit[StreamError]^])

        private update def drain(): Unit =
          if mark0 > 0 && !broken then
            val buffer = jn.ByteBuffer.wrap(storage, 0, mark0).nn

            try
              while buffer.hasRemaining do
                if value.write(buffer) == -1 then
                  broken = true
                  { val written: Long = total; raise(StreamError(written.b))(using cut().asInstanceOf[Emit[StreamError]^]) }

              total += mark0
            catch case _: Exception =>
              broken = true
              { val written: Long = total; raise(StreamError(written.b))(using cut().asInstanceOf[Emit[StreamError]^]) }

          mark0 = 0

  // Adapts a whole-`LazyList` writing function to the incremental `Intake`
  // protocol by accumulating chunks until `finish` — the basis of the
  // transitional `Writable` bridges below.
  def buffered[target, medium]
    ( target: target, write: (target, LazyList[medium]) => Unit )
    ( using addressable0: medium is Addressable )
  :   (Intake[medium] over Credit)^{write, caps.any} =

    new Intake[medium]:
      type Transport = Credit

      private val block: Int = 2048
      // Untracked, cast-erased: reached only through this endpoint.
      @caps.unsafe.untrackedCaptures
      private val storage: addressable0.Storage =
        addressable0.allocate(block).asInstanceOf[addressable0.Storage]
      private var mark0: Int = 0
      private var chunks: List[medium] = Nil

      def demand: Credit = Credit(Long.MaxValue)
      protected def buffer0: AnyRef = storage.asInstanceOf[AnyRef]
      def mark: Int = mark0

      update def reserve(min: Int): Int =
        val free = block - mark0

        if free >= min then free else
          drain()
          block

      update def commit(count: Int): Unit =
        mark0 += count
        if mark0 == block then drain()

      update def finish(): Unit =
        drain()
        write(target, chunks.reverse.to(LazyList))

      private update def drain(): Unit =
        if mark0 > 0 then
          chunks ::= addressable0.materialize(storage, 0, mark0)
          mark0 = 0


trait Sink extends Typeclass, Operable:
  type Transport
  def intake(target: Self): (Intake[Operand] over Transport)^

  def contramap[self2](lambda: self2 => Self)
  :   (self2 is Sink by Operand over Transport)^{this, lambda} =
    target => intake(lambda(target))
