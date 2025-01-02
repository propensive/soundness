/*
    Turbulence, version [unreleased]. Copyright 2025 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package turbulence

import java.io as ji
import java.nio as jn

import anticipation.*
import contingency.*
import hieroglyph.*
import prepositional.*
import rudiments.*
import vacuous.*

object Writable:
  given [OutType <: ji.OutputStream](using streamCut: Tactic[StreamError])
      => OutType is Writable by Bytes as outputStreamBytes =
    (outputStream, stream) =>
      stream.each: bytes =>
        outputStream.write(bytes.mutable(using Unsafe))
        outputStream.flush()

      outputStream.close()

  given (using streamCut: Tactic[StreamError], encoder: CharEncoder)
          => ji.OutputStream is Writable by Text as outputStreamText =

    (outputStream, stream) =>
      stream.each: text =>
        outputStream.write(encoder.encode(text).mutable(using Unsafe))
        outputStream.flush()

      outputStream.close()

  given [TargetType: Writable by Text](using decoder: CharDecoder)
      => TargetType is Writable by Bytes as decodingAdapter =
    (target, stream) => TargetType.write(target, decoder.decode(stream))

  given [TargetType: Writable by Bytes](using encoder: CharEncoder)
      => TargetType is Writable by Text as encodingAdapter =
    (target, stream) => TargetType.write(target, encoder.encode(stream))

  given (using Tactic[StreamError])
      => jn.channels.WritableByteChannel is Writable by Bytes as channel = (channel, stream) =>
    @tailrec
    def recur(total: Memory, todo: LazyList[jn.ByteBuffer]): Unit =
      todo.flow(()):
        val count = try channel.write(head) catch case e: Exception => -1
        
        if count == -1 then raise(StreamError(total))
        else recur(total + count.b, if head.hasRemaining then todo else tail)

    recur(0.b, stream.map { bytes => jn.ByteBuffer.wrap(bytes.mutable(using Unsafe)).nn })

trait Writable:
  type Self
  type Operand
  def write(target: Self, stream: LazyList[Operand]): Unit

  def contramap[SelfType2](lambda: SelfType2 => Self): SelfType2 is Writable by Operand =
    (target, stream) => write(lambda(target), stream)
