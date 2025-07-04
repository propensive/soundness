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
┃    Soundness, version 0.37.0.                                                                    ┃
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
import proscenium.*
import rudiments.*
import symbolism.*
import vacuous.*

object Writable:
  given outputStreamBytes: [output <: ji.OutputStream] => (streamCut: Tactic[StreamError])
        =>  output is Writable by Bytes =
    (outputStream, stream) =>
      stream.each: bytes =>
        outputStream.write(bytes.mutable(using Unsafe))
        outputStream.flush()

      outputStream.close()

  given outputStreamText: (streamCut: Tactic[StreamError], encoder: CharEncoder)
        =>  ji.OutputStream is Writable by Text =

    (outputStream, stream) =>
      stream.each: text =>
        outputStream.write(encoder.encode(text).mutable(using Unsafe))
        outputStream.flush()

      outputStream.close()

  given decodingAdapter: [writable: Writable by Text] => (decoder: CharDecoder)
        =>  writable is Writable by Bytes =
    (target, stream) => writable.write(target, decoder.decoded(stream))

  given encodingAdapter: [writable: Writable by Bytes] => (encoder: CharEncoder)
        =>  writable is Writable by Text =
    (target, stream) => writable.write(target, encoder.encoded(stream))

  given channel: Tactic[StreamError]
        =>  jn.channels.WritableByteChannel is Writable by Bytes = (channel, stream) =>
    @tailrec
    def recur(total: Memory, todo: Stream[jn.ByteBuffer]): Unit =
      todo.flow(()):
        val count = try channel.write(head) catch case e: Exception => -1

        if count == -1 then raise(StreamError(total))
        else recur(total + count.b, if head.hasRemaining then todo else tail)

    recur(0.b, stream.map { bytes => jn.ByteBuffer.wrap(bytes.mutable(using Unsafe)).nn })

trait Writable extends Typeclass, Operable:
  def write(target: Self, stream: Stream[Operand]): Unit

  def contramap[self2](lambda: self2 => Self): self2 is Writable by Operand =
    (target, stream) => write(lambda(target), stream)
