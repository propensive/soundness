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
┃    Soundness, version 0.54.0.                                                                    ┃
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

object Writable:
  // A write failure `raise`s a typed `StreamError`, so the obligation is discharged by an `Emit`
  // from the scope where the `Writable` is summoned (a `trap`-injected one, or one propagated as
  // `emits StreamError`). `Emit` rather than `Tactic`: a writer only reports a cut, never `abort`s.
  given outputStreamData: [output <: ji.OutputStream] => (streamCut: Emit[StreamError])
  =>  output is Writable by Data =

    (outputStream, stream) =>
      @tailrec
      def recur(total: Bytes, todo: Stream[Data]): Unit =
        todo.flow(close(outputStream, total)):
          val array = next.asInstanceOf[Array[Byte]]
          if write(outputStream, array, total) then recur(total + array.length.b, more)

      recur(0.b, stream)


  given outputStreamText: (streamCut: Emit[StreamError], encoder: CharEncoder)
  =>  ji.OutputStream is Writable by Text =

    (outputStream, stream) =>
      @tailrec
      def recur(total: Bytes, todo: Stream[Text]): Unit =
        todo.flow(close(outputStream, total)):
          val array = encoder.encode(next.asInstanceOf[Text]).asInstanceOf[Array[Byte]]
          if write(outputStream, array, total) then recur(total + array.length.b, more)

      recur(0.b, stream)


  given decodingAdapter: [writable: Writable by Text] => (decoder: CharDecoder)
  =>  writable is Writable by Data =

    (target, stream) => writable.write(target, decoder.decoded(stream))


  given encodingAdapter: [writable: Writable by Data] => (encoder: CharEncoder)
  =>  writable is Writable by Text =

    (target, stream) => writable.write(target, encoder.encoded(stream))


  given channel: Emit[StreamError]
  =>  jn.channels.WritableByteChannel is Writable by Data =

    (channel, stream) =>
      @tailrec
      def recur(total: Bytes, todo: Stream[jn.ByteBuffer]): Unit =
        todo.flow(()):
          val count = try channel.write(next) catch case e: Exception => -1

          if count == -1 then raise(StreamError(total))
          else recur(total + count.b, if next.hasRemaining then todo else more)

      recur(0.b, stream.map { bytes => jn.ByteBuffer.wrap(bytes.mutable(using Unsafe)).nn })

  // Writes one chunk to an `OutputStream`, `raise`-ing a `StreamError` (and returning `false`) on
  // an `IOException` so the caller stops; `true` means continue.
  private def write(outputStream: ji.OutputStream, array: Array[Byte], total: Bytes)
    ( using Emit[StreamError] )
  :   Boolean =

    try
      outputStream.write(array)
      outputStream.flush()
      true
    catch case _: ji.IOException => raise(StreamError(total)) yet false

  private def close(outputStream: ji.OutputStream, total: Bytes)(using Emit[StreamError]): Unit =
    try outputStream.close() catch case _: ji.IOException => raise(StreamError(total))

trait Writable extends Typeclass, Operable:
  def write(target: Self, stream: Stream[Operand]): Unit

  def contramap[self2](lambda: self2 => Self): self2 is Writable by Operand =
    (target, stream) => write(lambda(target), stream)
