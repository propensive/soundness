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

// A value which can be opened as a pull endpoint: the successor to
// `Streamable`, producing a demand-aware `Stream` over a mutable buffer
// instead of a `LazyList`. Sources needing buffers or error contexts capture
// them as contextual values of the given (`Buffering`, `Tactic[StreamError]`),
// so the typeclass itself remains a single abstract method.
object Source extends Source2:
  given bytes: Data is Source by Data over Credit = Stream(_)
  given text: [textual <: Text] => textual is Source by Text over Credit = value => Stream(value)

  // Legacy views: a lazy list of chunks is a source, though its production is
  // beyond demand control; demand bounds only the exposure of each chunk.
  given lazyListData: LazyList[Data] is Source by Data over Credit = value =>
    Stream(value.iterator)

  given lazyListText: LazyList[Text] is Source by Text over Credit = value =>
    Stream(value.iterator)

  // The HTTP-body interchange protocol is itself a pull source, so a
  // request or response body can be `read` directly.
  given httpBody: Buffering => HttpStreams.Body is Source by Data over Credit = _.stream

  given inputStream: [input <: ji.InputStream] => (Tactic[StreamError], Buffering)
  =>  input is Source by Data over Credit =

    value => Source.stream(jn.channels.Channels.newChannel(value).nn)

  given channel: (Tactic[StreamError], Buffering)
  =>  jn.channels.ReadableByteChannel is Source by Data over Credit =

    Source.stream(_)

  given reader: [input <: ji.Reader] => (Tactic[StreamError], Buffering)
  =>  input is Source by Text over Credit =

    value =>
      new Stream[Text]:
        type Transport = Credit

        private val capacity: Int = summon[Buffering].capacity(Substrate.Chars)
        private val storage: Array[Char] = new Array[Char](capacity)
        private var start0: Int = 0
        private var limit0: Int = 0
        private var total: Long = 0
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

            if granted == 0 then 0 else
              start0 = 0
              limit0 = 0

              try value.read(storage, 0, capacity.min(granted)) match
                case -1 =>
                  ended = true
                  value.close()
                  Unset

                case count =>
                  total += count
                  limit0 = count
                  count

              catch case error: ji.IOException =>
                ended = true
                try value.close() catch case _: Exception => ()
                abort(StreamError(total.b))

        override def close(): Unit =
          ended = true
          try value.close() catch case _: Exception => ()

  // The byte pump underlying `inputStream` and `channel` sources: reads
  // directly into the stream's own storage, at most one granted block at a
  // time.
  private def stream(input: jn.channels.ReadableByteChannel)
    ( using tactic: Tactic[StreamError], buffering: Buffering )
  :   Stream[Data] over Credit =

    new Stream[Data]:
      type Transport = Credit

      private val capacity: Int = buffering.capacity(Substrate.Bytes)
      private val storage: Array[Byte] = new Array[Byte](capacity)
      private val buffer: jn.ByteBuffer = jn.ByteBuffer.wrap(storage).nn
      private var start0: Int = 0
      private var limit0: Int = 0
      private var total: Long = 0
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

          if granted == 0 then 0 else
            start0 = 0
            limit0 = 0
            buffer.clear()
            buffer.limit(capacity.min(granted))

            try input.read(buffer) match
              case -1 =>
                ended = true
                try input.close() catch case _: Exception => ()
                Unset

              case 0 =>
                0

              case count =>
                total += count
                limit0 = count
                count

            catch case error: Exception =>
              ended = true
              try input.close() catch case _: Exception => ()
              abort(StreamError(total.b))

      override def close(): Unit =
        ended = true
        try input.close() catch case _: Exception => ()

// Transitional: any `Streamable` instance over a buffer-backed medium is a
// `Source`, at lower priority than the native instances above, so downstream
// code migrates without ceremony. Production of the underlying `LazyList` is
// not demand-controlled.
trait Source2:
  given streamableData: [value] => (streamable: value is Streamable by Data)
  =>  value is Source by Data over Credit =

    source => Stream(streamable.stream(source).iterator)

  given streamableText: [value] => (streamable: value is Streamable by Text)
  =>  value is Source by Text over Credit =

    source => Stream(streamable.stream(source).iterator)

trait Source extends Typeclass, Operable:
  type Transport
  def stream(value: Self): Stream[Operand] over Transport

  def contramap[self2](lambda: self2 => Self): self2 is Source by Operand over Transport =
    value => stream(lambda(value))
