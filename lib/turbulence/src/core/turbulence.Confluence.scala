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
import java.util.concurrent.atomic as juca

import anticipation.*
import fulminate.*
import parasite.*
import prepositional.*
import rudiments.*
import vacuous.*
import zephyrine.*

// Fan-in: merges several pull endpoints into one, in arrival order. Pull
// architecture admits no `select` over blocked refills, so this is the
// honest cost of multiplexing: one pump task per input, each pulling
// block-sized credits into a shared bounded queue — a full queue parks the
// pumps, so slow consumption backpressures every input — and a reader
// endpoint that parks while the queue is empty. The merged stream ends when
// every input has ended; an input's failure is rethrown from the reader's
// next refill.
object Confluence:
  private object End

  private class Block(val storage: AnyRef, val size: Int)

  def apply[medium, cap^](sources: (Stream[medium] over Credit)^{cap}*)
    ( using addressable0: medium is Addressable,
            buffering:    Buffering,
            monitor:      Monitor,
            probate:      Probate )
  :   (Stream[medium] over Credit)^ =

    val block: Int = buffering.capacity(addressable0.substrate)

    val queue: juc.ArrayBlockingQueue[AnyRef] =
      juc.ArrayBlockingQueue(buffering.window.max(sources.length))

    val remaining: juca.AtomicInteger = juca.AtomicInteger(sources.length)
    @volatile var error: Throwable | Null = null

    def finish(): Unit = if remaining.decrementAndGet == 0 then queue.put(End)

    // Indexed iteration: capture sets do not ride standard-collection elements, so each
    // source is re-asserted exclusive at its fiber rim — one consuming fiber per source
    // by construction.
    var index = 0

    // A while-loop rather than a for-comprehension: the desugared foreach closure would
    // capture the sources' reach capability. Each element crosses to its fiber as a
    // neutral carrier and is re-asserted exclusive inside: one consuming fiber per
    // source by construction.
    while index < sources.length do
      val handoff: AnyRef = sources(index).asInstanceOf[AnyRef]

      async:
        val source = handoff.asInstanceOf[(Stream[medium] over Credit)^]

        def loop(): Unit = source.refill(Credit(block)) match
          case count: Int =>
            val window = source.window(using Unsafe)
            val storage = addressable0.allocate(count)

            addressable0.transfer
              ( window.asInstanceOf[addressable0.Storage], source.start, storage, 0, count )

            source.skip(count)
            queue.put(Block(storage.asInstanceOf[AnyRef], count))
            loop()

          case _ =>
            finish()

        try loop() catch case exception: Exception =>
          error = exception
          finish()

      index += 1

    new Stream[medium](using addressable0):
      type Transport = Credit

      private var storage: addressable0.Storage = addressable0.allocate(0)
      private var start0: Int = 0
      private var limit0: Int = 0
      private var size: Int = 0
      private var ended: Boolean = false

      protected def window0: AnyRef = storage.asInstanceOf[AnyRef]
      def start: Int = start0
      def limit: Int = limit0
      update def skip(count: Int): Unit = start0 += count

      update def refill(demand: Credit): Optional[Int] =
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

              case received: Block =>
                storage = received.storage.asInstanceOf[addressable0.Storage]
                size = received.size
                start0 = 0
                limit0 = size.min(granted)
                limit0

              case _ =>
                panic(m"unexpected value in confluence queue")
