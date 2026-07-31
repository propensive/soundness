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
package turbulence

import scala.caps

import java.util.concurrent as juc

import proscenium.compat.*
import vacuous.*
import prepositional.*
import zephyrine.*

// The push-side bridge from any number of producer threads to a pull endpoint
// of record chunks: `Spool`'s kernel successor. Producers `put` records (an
// unbounded MPSC queue insertion — producers never block on the reader), and
// one of them eventually calls `stop`; the single reader owns `stream`, whose
// refill blocks for the first record and then drains whatever else has already
// arrived into the same window — records batch across the thread boundary
// instead of paying one hand-off per element. (`Conduit` is the byte/char
// counterpart: strictly SPSC and block-structured. A relay is for
// many-producer, record-granularity traffic: events, notifications, messages.)
object Relay:
  private object Termination extends scala.caps.Pure

class Relay[record]():
  private val queue: juc.LinkedBlockingQueue[record | Relay.Termination.type] =
    juc.LinkedBlockingQueue()

  def put(record: record): Unit = queue.put(record)
  def stop(): Unit = queue.put(Relay.Termination)

  // The element-wise view: a lazy `Chain` draining the shared queue one
  // record at a time. Unlike `stream`, it needs no `Addressable` medium — so
  // it works for any record type, including an abstract one — at the cost of a
  // cons cell per record; use it for low-rate event delivery (logging, message
  // buses) where per-record batching would not pay. This is the direct
  // successor to `Spool.stream`, laundering the endpoint into a pure Chain,
  // and the audited bridge until every consumer takes a windowed `stream`.
  @scala.annotation.nowarn("msg=match may not be exhaustive")
  def lazyList: Chain[record] =
    def pull(): Chain[record] = queue.take().nn match
      case Relay.Termination => Chain()
      case value             => value.asInstanceOf[record] #:: pull()

    Chain().lazyAppendedAll(pull())

  // The pull endpoint over this relay's records: single-owner, drained by one
  // thread; create it once. Records arriving after `stop` are not delivered.
  // The medium evidence resolves at the (concretely-typed) call site — the
  // generic `Addressable.boxed` for ordinary reference-typed records.
  @scala.annotation.nowarn("msg=match may not be exhaustive")
  def stream(using addressable: (Array[record]^{}) is Addressable, buffering: Buffering)
  :   (Stream[Array[record]^{}] over Credit)^ =

    new Stream[Array[record]^{}](using addressable):
      type Transport = Credit

      private val capacity: Int = buffering.capacity(Substrate.Boxes)

      // Untracked, cast-erased: reached only through this endpoint. All
      // `Substrate.Boxes` media store records erased (`boxed`, `texts`), so
      // the window storage is written directly.
      @caps.unsafe.untrackedCaptures
      private val storage: scala.Array[AnyRef] =
        new scala.Array[AnyRef](capacity).asInstanceOf[scala.Array[AnyRef]]

      private var start0: Int = 0
      private var limit0: Int = 0
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

          if granted == 0 then 0 else
            start0 = 0
            limit0 = 0
            val space = capacity.min(granted)

            queue.take().nn match
              case Relay.Termination =>
                ended = true
                Unset

              case first =>
                storage.asInstanceOf[scala.Array[AnyRef]^](0) = first.asInstanceOf[AnyRef]
                limit0 = 1

                // Opportunistic batching: whatever else has already arrived
                // joins the same window, up to its space. A termination found
                // mid-drain still delivers the batch; the next refill ends.
                var draining = true

                while draining && limit0 < space do queue.poll() match
                  case null              => draining = false
                  case Relay.Termination => ended = true
                                            draining = false
                  case record            => storage.asInstanceOf[scala.Array[AnyRef]^](limit0) =
                                              record.asInstanceOf[AnyRef]
                                            limit0 += 1

                limit0
