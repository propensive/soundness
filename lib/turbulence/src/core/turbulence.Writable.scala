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

import java.io as ji
import java.nio as jn

import anticipation.*
import contingency.*
import hieroglyph.*
import prepositional.*
import rudiments.*
import symbolism.*
import vacuous.*
import zephyrine.{stream as _, *}

object Writable:
  // Drain a byte stream into an `OutputStream`, window by window, straight
  // from the stream's own storage. A write failure `raise`s a typed
  // `StreamError` (an `Emit`, not a `Tactic`: a writer only reports a cut,
  // never aborts) and stops writing; the target closes either way.
  private def drain(outputStream: ji.OutputStream, stream: (Stream[Data] over Credit)^)
    ( using streamCut: Emit[StreamError], buffering: Buffering )
  :   Unit =

    var total: Long = 0L
    var failed: Boolean = false

    // The non-consume `write` signature crosses to the consuming drain as a
    // neutral reference (the `accept` convention).
    stream.asInstanceOf[AnyRef].asInstanceOf[(Stream[Data] over Credit)^].sweep:
      (storage, start, count) =>
        if !failed then
          try
            outputStream.write(storage.asInstanceOf[Array[Byte]], start, count)
            total += count
          catch case error: ji.IOException => failed = true

    if failed then raise(StreamError(total.b))
    else
      try
        outputStream.flush()
        outputStream.close()
      catch case error: ji.IOException => raise(StreamError(total.b))

  given outputStreamData: [output <: ji.OutputStream]
  =>  (streamCut: Emit[StreamError], buffering: Buffering)
  =>  ((output is Writable by Data)^{streamCut}) =

    (outputStream, stream) => drain(outputStream, stream)

  given outputStreamText: (streamCut: Emit[StreamError], encoder: CharEncoder,
      buffering: Buffering)
  =>  ((ji.OutputStream is Writable by Text)^{streamCut}) =

    (outputStream, stream) =>
      drain
        ( outputStream,
          stream.asInstanceOf[AnyRef].asInstanceOf[(Stream[Text] over Credit)^].via(encoder)
          . asInstanceOf[(Stream[Data] over Credit)^] )

  // Operand adapters: a byte writer accepts characters through the encoder
  // duct, and vice versa.
  given decodingAdapter: [writable]
  =>  (writable0: (writable is Writable by Text)^, decoder: CharDecoder, buffering: Buffering)
  =>  ((writable is Writable by Data)^{writable0}) =

    (target, stream) =>
      writable0.write
        ( target,
          stream.asInstanceOf[AnyRef].asInstanceOf[(Stream[Data] over Credit)^].via(decoder)
          . asInstanceOf[(Stream[Text] over Credit)^] )

  given encodingAdapter: [writable]
  =>  (writable0: (writable is Writable by Data)^, encoder: CharEncoder, buffering: Buffering)
  =>  ((writable is Writable by Text)^{writable0}) =

    (target, stream) =>
      writable0.write
        ( target,
          stream.asInstanceOf[AnyRef].asInstanceOf[(Stream[Text] over Credit)^].via(encoder)
          . asInstanceOf[(Stream[Data] over Credit)^] )

  // A medium writer accepts a record stream of its own chunks: each record is
  // one chunk, written in order — the shape of an event log, where every
  // formatted event is one `Text` (or `Data`) chunk.
  given chunked: [medium, writable]
  =>  (writable0: (writable is Writable by medium)^, addressable: medium is Addressable,
      buffering: Buffering)
  =>  ((writable is Writable by IArray[medium])^{writable0}) =

    (target, stream) =>
      writable0.write
        ( target,
          Stream
            (stream.asInstanceOf[AnyRef].asInstanceOf[(Stream[IArray[medium]] over Credit)^]
              . records) )

  given channel: (streamCut: Emit[StreamError], buffering: Buffering)
  =>  ((jn.channels.WritableByteChannel is Writable by Data)^{streamCut}) =

    (channel, stream) =>
      var total: Long = 0L
      var failed: Boolean = false

      stream.asInstanceOf[AnyRef].asInstanceOf[(Stream[Data] over Credit)^].sweep:
        (storage, start, count) =>
          if !failed then
            val buffer = jn.ByteBuffer.wrap(storage.asInstanceOf[Array[Byte]], start, count).nn

            try
              while buffer.hasRemaining do
                if channel.write(buffer) == -1 then
                  failed = true
                  buffer.limit(0)

              total += count
            catch case error: Exception => failed = true

      if failed then raise(StreamError(total.b))

// A target which accepts a whole stream of `Operand` chunks and writes it to
// completion: the stream parameter is a single-owner pull endpoint which the
// writer drains (formerly a `LazyList`). `Sink` is the endpoint-minting
// counterpart, for callers that push.
trait Writable extends Typeclass, Operable:
  def write(target: Self, stream: (Stream[Operand] over Credit)^): Unit

  def contramap[self2](lambda: self2 => Self): (self2 is Writable by Operand)^{this, lambda} =
    (target, stream) => write(lambda(target), stream)
