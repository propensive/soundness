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
import vacuous.*

object Appendable:
  given stdoutBytes(using stdio: Stdio): SimpleAppendable[Out.type, Bytes] =
    (stderr, bytes) => stdio.write(bytes)

  given stdoutText(using stdio: Stdio): SimpleAppendable[Out.type, Text] =
    (stdout, text) => stdio.print(text)

  given stderrBytes(using stdio: Stdio): SimpleAppendable[Err.type, Bytes] =
    (stderr, bytes) => stdio.writeErr(bytes)

  given stderrText(using stdio: Stdio): SimpleAppendable[Err.type, Text] =
    (stderr, text) => stdio.printErr(text)

  given [OutType <: ji.OutputStream](using streamCut: Errant[StreamError]) => OutType is Appendable by Bytes as outputStreamBytes =
    (outputStream, stream) =>
      stream.each: bytes =>
        outputStream.write(bytes.mutable(using Unsafe))
        outputStream.flush()

      outputStream.close()

  given [TargetType: Appendable by Text](using decoder: CharDecoder)
      => TargetType is Appendable by Bytes as decodingAdapter =
    (target, stream) => TargetType.append(target, decoder.decode(stream))

  given [TargetType: Appendable by Bytes](using encoder: CharEncoder)
      => TargetType is Appendable by Text as encodingAdapter =
    (target, stream) => TargetType.append(target, encoder.encode(stream))

trait Appendable:
  type Self
  type Element
  def append(target: Self, stream: LazyList[Element]): Unit
  def asWritable: Self is Writable by Element = append(_, _)

  def contramap[SelfType2](lambda: SelfType2 => Self): SelfType2 is Appendable by Element =
    (target, stream) => append(lambda(target), stream)
