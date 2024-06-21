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

object Spoolable extends ProductDerivable[Spoolable]:

  // given B8 is Spoolable = Spoolable(1)(_(_).bits)
  // given B16 is Spoolable = Spoolable(2)(B16(_, _))
  // given B32 is Spoolable = Spoolable(4)(B32(_, _))
  //given B64 is Spoolable = Spoolable(8)(B64(_, _))

  // given I8 is Spoolable = Spoolable(1)(_(_).bits.i8)
  // given I16 is Spoolable = Spoolable(2)(B16(_, _).i16)
  // given I32 is Spoolable = Spoolable(4)(B32(_, _).i32)
  // given I64 is Spoolable = Spoolable(8)(B64(_, _).i64)

  // given U8 is Spoolable = Spoolable(1)(_(_).bits.u8)
  // given U16 is Spoolable = Spoolable(2)(B16(_, _).u16)
  // given U32 is Spoolable = Spoolable(4)(B32(_, _).u32)
  // given U64 is Spoolable = Spoolable(8)(B64(_, _).u64)

  // given Byte is Spoolable = Spoolable(1)(_(_))
  // given Short is Spoolable = Spoolable(2)(B16(_, _).i16.short)
  // given Int is Spoolable = Spoolable(4)(B32(_, _).i32.int)
  // given Long is Spoolable = Spoolable(8)(B64(_, _).i64.long)

  inline def join[DerivationType <: Product: ProductReflection]: DerivationType is Spoolable = new:
    def spool(spool: Spool, value: DerivationType): Unit = fields(value):
      [FieldType] => field =>
        context.spool(spool, field)

    def width = contexts { [FieldType] => _.width }.sum

trait Spoolable:
  type Self
  def width: Int
  def spool(spool: Spool, value: Self): Unit
