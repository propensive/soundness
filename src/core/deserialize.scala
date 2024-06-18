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

import wisteria.*
import rudiments.*
import anticipation.*
import hypotenuse.*

object Unpackable extends ProductDerivable[Unpackable]:

  def apply[DataType](byteWidth: Int)(lambda: (Bytes, Int) => DataType): DataType is Unpackable =
    new Unpackable:
      type Self = DataType
      def unpack(data: Bytes, offset: Int): DataType = lambda(data, offset)
      def width: Int = byteWidth

  given B8 is Unpackable = Unpackable(1) { (data, offset) => data(offset).bits }
  given B16 is Unpackable = Unpackable(2)(B16(_, _))
  given B32 is Unpackable = Unpackable(4)(B32(_, _))
  given B64 is Unpackable = Unpackable(8)(B64(_, _))

  given I8 is Unpackable = Unpackable(1) { (data, offset) => data(offset).bits.i8 }
  given I16 is Unpackable = Unpackable(2)(B16(_, _).i16)
  given I32 is Unpackable = Unpackable(4)(B32(_, _).i32)
  given I64 is Unpackable = Unpackable(8)(B64(_, _).i64)

  given U8 is Unpackable = Unpackable(1) { (data, offset) => data(offset).bits.u8 }
  given U16 is Unpackable = Unpackable(2)(B16(_, _).u16)
  given U32 is Unpackable = Unpackable(4)(B32(_, _).u32)
  given U64 is Unpackable = Unpackable(8)(B64(_, _).u64)

  given Byte is Unpackable = Unpackable(1) { (data, offset) => data(offset) }
  given Short is Unpackable = Unpackable(2)(B16(_, _).i16.short)
  given Int is Unpackable = Unpackable(4)(B32(_, _).i32.int)
  given Long is Unpackable = Unpackable(8)(B64(_, _).i64.long)

  inline def join[DerivationType <: Product: ProductReflection]: DerivationType is Unpackable = new:
    def unpack(data: Bytes, offset: Int): DerivationType =
      var position: Int = offset

      construct:
        [FieldType] => context =>
          context.unpack(data, position).also:
            position += context.width

    def width: Int = contexts { [FieldType] => _.width }.sum

trait Unpackable:
  type Self
  def unpack(data: Bytes, offset: Int): Self
  def width: Int

extension (bytes: Bytes)
  def unpack[DataType: Unpackable](offset: Int = 0): DataType =
    DataType.unpack(bytes, offset)

  def unpackIArray[DataType: {ClassTag, Unpackable as unpackable}](size: Int, offset: Int = 0)
          : IArray[DataType] =

    val width = unpackable.width

    IArray.create[DataType](size): array =>
      array.indices.each: index =>
        array(index) = unpackable.unpack(bytes, offset + index*width)

def byteWidth[DataType: Unpackable]: Int = DataType.width
