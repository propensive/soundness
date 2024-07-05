/*
    Turbulence, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

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

import anticipation.*
import contingency.*
import hieroglyph.*
import rudiments.*
import symbolism.*

object Appendable extends FallbackAppendable:
  given (using Stdio) => SimpleAppendable[Out.type, Bytes] as stdoutBytes = (_, bytes) =>
    Out.write(bytes)

  given (using Stdio) => SimpleAppendable[Out.type, Text] as stdoutText = (_, text) =>
    Out.print(text)

  given (using Stdio) => SimpleAppendable[Out.type, Line] as stdoutLine = (_, line) =>
    Out.print(line.content)
    Out.println()

  given (using Stdio) => SimpleAppendable[Err.type, Bytes] as sterrBytes = (_, bytes) =>
    Err.write(bytes)

  given (using Stdio) => SimpleAppendable[Err.type, Text] as stderrText = (_, text) =>
    Err.print(text)

  given (using Stdio) => SimpleAppendable[Err.type, Line] as stderrLine = (_, line) =>
    Err.print(line.content)
    Err.println()

  given [OutType <: ji.OutputStream](using Tactic[StreamError])
      => OutType is Appendable by Bytes as outputStreamBytes =
    (outputStream, stream) =>
      stream.each: bytes =>
        outputStream.write(unsafely(bytes.mutable))
        outputStream.flush()

      outputStream.close()

sealed trait FallbackAppendable:
  given [TargetType: Appendable by Text](using decoder: CharDecoder)
      => TargetType is Appendable by Bytes as decodingAdapter =
    (target, stream) => TargetType.append(target, decoder.decode(stream))

  given [TargetType: Appendable by Bytes](using encoder: CharEncoder)
      => TargetType is Appendable by Text as encodingAdapter =
    (target, stream) => TargetType.append(target, encoder.encode(stream))

trait Appendable:
  type Self
  type Operand

  def append(target: Self, stream: LazyList[Operand]): Unit
  def asWritable: Self is Writable by Operand = append(_, _)

  def contramap[SelfType2](lambda: SelfType2 => Self): SelfType2 is Appendable by Operand =
    (target, stream) => append(lambda(target), stream)
