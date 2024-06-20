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

object Unspoolable extends ProductDerivable[Unspoolable]:
  def apply[DataType](byteWidth: Int)(lambda: (Bytes, Int) => DataType): DataType is Unspoolable =
    new:
      def width: Int = byteWidth

      def unspool(spool: Spool): DataType =
        lambda(spool.bytes, spool.offset).also(spool.advance(width))

  given B8 is Unspoolable = Unspoolable(1)(_(_).bits)
  given B16 is Unspoolable = Unspoolable(2)(B16(_, _))
  given B32 is Unspoolable = Unspoolable(4)(B32(_, _))
  given B64 is Unspoolable = Unspoolable(8)(B64(_, _))

  given S8 is Unspoolable = Unspoolable(1)(_(_).bits.s8)
  given S16 is Unspoolable = Unspoolable(2)(B16(_, _).s16)
  given S32 is Unspoolable = Unspoolable(4)(B32(_, _).s32)
  given S64 is Unspoolable = Unspoolable(8)(B64(_, _).s64)

  given U8 is Unspoolable = Unspoolable(1)(_(_).bits.u8)
  given U16 is Unspoolable = Unspoolable(2)(B16(_, _).u16)
  given U32 is Unspoolable = Unspoolable(4)(B32(_, _).u32)
  given U64 is Unspoolable = Unspoolable(8)(B64(_, _).u64)

  given Byte is Unspoolable = Unspoolable(1)(_(_))
  given Short is Unspoolable = Unspoolable(2)(B16(_, _).s16.short)
  given Int is Unspoolable = Unspoolable(4)(B32(_, _).s32.int)
  given Long is Unspoolable = Unspoolable(8)(B64(_, _).s64.long)

  inline def join[DerivationType <: Product: ProductReflection]: DerivationType is Unspoolable =
    new:
      def unspool(spool: Spool): DerivationType =
        construct { [FieldType] => context => context.unspool(spool) }

      def width = contexts { [FieldType] => _.width }.sum

trait Unspoolable:
  type Self
  def width: Int
  def unspool(spool: Spool): Self
