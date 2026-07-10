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

import java.util.concurrent as juc

import anticipation.*
import fulminate.*
import parasite.*
import prepositional.*
import rudiments.*
import vacuous.*
import zephyrine.*

// Fan-out: one pump task pulls the source and delivers every chunk to each
// of `count` subscriber endpoints through bounded queues. Chunks are
// materialized once and shared immutably between subscribers; a full
// subscriber queue parks the pump, so the slowest subscriber gates the
// source — the correct backpressure semantics for replication. A source
// failure is rethrown from each subscriber's next refill.
object Manifold:
  private object End

  def apply[medium](source: Stream[medium] over Credit, count: Int)
    ( using addressable0: medium is Addressable,
            buffering:    Buffering,
            monitor:      Monitor,
            probate:      Probate )
  :   IndexedSeq[Stream[medium] over Credit] =

    val block: Int = buffering.capacity(addressable0.substrate)

    val queues: IndexedSeq[juc.ArrayBlockingQueue[AnyRef]] =
      IndexedSeq.fill(count)(juc.ArrayBlockingQueue[AnyRef](buffering.window))

    @volatile var error: Throwable | Null = null

    async:
      def loop(): Unit = source.refill(Credit(block)) match
        case size: Int =>
          val chunk =
            addressable0.materialize
              ( source.window(using Unsafe).asInstanceOf[addressable0.Storage],
                source.start,
                size )

          source.skip(size)
          queues.each(_.put(chunk.asInstanceOf[AnyRef]))
          loop()

        case _ =>
          queues.each(_.put(End))

      try loop() catch case exception: Exception =>
        error = exception
        queues.each(_.put(End))

    queues.map: queue =>
      new Stream[medium](using addressable0):
        type Transport = Credit

        // The shared chunk is immutable; subscribers only read the window,
        // so exposing its backing array directly is safe and copy-free.
        private var storage: AnyRef = addressable0.allocate(0).asInstanceOf[AnyRef]
        private var start0: Int = 0
        private var limit0: Int = 0
        private var size: Int = 0
        private var ended: Boolean = false

        protected def window0: AnyRef = storage
        def start: Int = start0
        def limit: Int = limit0
        def skip(count: Int): Unit = start0 += count

        def refill(demand: Credit): Optional[Int] =
          if limit0 > start0 then limit0 - start0
          else if ended then Unset
          else
            val granted = summon[Credit is Regulation].grant(demand)

            if granted == 0 then 0
            else if limit0 < size then
              limit0 += (size - limit0).min(granted)
              limit0 - start0
            else
              (queue.take().nn: @unchecked) match
                case End =>
                  ended = true
                  val error0 = error
                  if error0 == null then Unset else throw error0

                case chunk =>
                  storage = chunk
                  size = addressable0.length(chunk.asInstanceOf[medium])
                  start0 = 0
                  limit0 = size.min(granted)
                  limit0
