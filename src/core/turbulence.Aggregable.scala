/*
    Turbulence, version 0.26.0. Copyright 2025 Jon Pretty, Propensive OÜ.

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
import hieroglyph.*
import prepositional.*
import rudiments.*
import vacuous.*

object Aggregable:
  given Bytes is Aggregable by Bytes as bytesBytes = source =>
    def recur(buf: ji.ByteArrayOutputStream, source: LazyList[Bytes]): Bytes =
      source.flow(buf.toByteArray().nn.immutable(using Unsafe)):
        buf.write(head.mutable(using Unsafe)); recur(buf, tail)

    recur(ji.ByteArrayOutputStream(), source)

  given (using decoder: CharDecoder) => Text is Aggregable by Bytes as bytesText =
    bytesBytes.map(decoder.decode)

  given Text is Aggregable by Text as textText = source =>
    val buffer = new StringBuffer()
    source.each { chunk => buffer.append(chunk.s) }
    buffer.toString.tt

  given [ElementType, ElementType2]
     (using aggregable: ElementType2 is Aggregable by ElementType)
      => LazyList[ElementType2] is Aggregable by ElementType as lazyList =
    element => LazyList(aggregable.aggregate(element))

trait Aggregable:
  aggregable =>
  type Self
  type Operand
  def aggregate(source: LazyList[Operand]): Self

  def map[SelfType2](lambda: Self => SelfType2): SelfType2 is Aggregable by Operand = source =>
    lambda(aggregable.aggregate(source))
