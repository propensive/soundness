/*
    Bifurcate, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package bifurcate

import anticipation.*
import hypotenuse.*
import rudiments.*
import wisteria.*

object Debufferable extends ProductDerivable[Debufferable]:
  def apply[DataType](byteWidth: Int)(lambda: (Bytes, Int) => DataType): DataType is Debufferable =
    new:
      def width: Int = byteWidth

      def debuffer(buffer: Buffer): DataType =
        lambda(buffer.bytes, buffer.offset).also(buffer.advance(width))

  given B8 is Debufferable = Debufferable(1)(_(_).bits)
  given B16 is Debufferable = Debufferable(2)(B16(_, _))
  given B32 is Debufferable = Debufferable(4)(B32(_, _))
  given B64 is Debufferable = Debufferable(8)(B64(_, _))

  given S8 is Debufferable = Debufferable(1)(_(_).bits.s8)
  given S16 is Debufferable = Debufferable(2)(B16(_, _).s16)
  given S32 is Debufferable = Debufferable(4)(B32(_, _).s32)
  given S64 is Debufferable = Debufferable(8)(B64(_, _).s64)

  given U8 is Debufferable = Debufferable(1)(_(_).bits.u8)
  given U16 is Debufferable = Debufferable(2)(B16(_, _).u16)
  given U32 is Debufferable = Debufferable(4)(B32(_, _).u32)
  given U64 is Debufferable = Debufferable(8)(B64(_, _).u64)

  given Byte is Debufferable = Debufferable(1)(_(_))
  given Short is Debufferable = Debufferable(2)(B16(_, _).s16.short)
  given Int is Debufferable = Debufferable(4)(B32(_, _).s32.int)
  given Long is Debufferable = Debufferable(8)(B64(_, _).s64.long)

  class Join[DerivationType <: Product: ProductReflection]
     (val width: Int, debuffer0: Buffer => DerivationType)
  extends Debufferable:
    type Self = DerivationType
    def debuffer(buffer: Buffer): DerivationType = debuffer0(buffer)

  inline def join[DerivationType <: Product: ProductReflection]: DerivationType is Debufferable =
    Join[DerivationType]
     (contexts { [FieldType] => _.width }.sum,
      buffer => construct { [FieldType] => context => context.debuffer(buffer) })

trait Debufferable:
  type Self
  def width: Int
  def debuffer(buffer: Buffer): Self
