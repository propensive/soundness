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

object Bifurcate:
  opaque type P8[PackType <: Nat] = Byte
  opaque type P16[PackType <: Nat] = Short
  opaque type P32[PackType <: Nat] = Int
  opaque type P64[PackType <: Nat] = Long

  opaque type I[WidthType <: Nat] = Int
  opaque type U[WidthType <: Nat] = Int

object Deserializer extends ProductDerivation[Deserializer]:

  def apply[DataType](byteWidth: Int)(lambda: (Bytes, Int) => DataType): Deserializer[DataType] =
    new Deserializer:
      def deserialize(data: Bytes, offset: Int): DataType = lambda(data, offset)
      def width: Int = byteWidth

  given b8: Deserializer[B8] = Deserializer(1) { (data, offset) => data(offset).bits }
  given b16: Deserializer[B16] = Deserializer(2)(B16(_, _))
  given b32: Deserializer[B32] = Deserializer(4)(B32(_, _))
  given b64: Deserializer[B64] = Deserializer(8)(B64(_, _))
  
  given i8: Deserializer[I8] = Deserializer(1) { (data, offset) => data(offset).bits.i8 }
  given i16: Deserializer[I16] = Deserializer(2)(B16(_, _).i16)
  given i32: Deserializer[I32] = Deserializer(4)(B32(_, _).i32)
  given i64: Deserializer[I64] = Deserializer(8)(B64(_, _).i64)
  
  given u8: Deserializer[U8] = Deserializer(1) { (data, offset) => data(offset).bits.u8 }
  given u16: Deserializer[U16] = Deserializer(2)(B16(_, _).u16)
  given u32: Deserializer[U32] = Deserializer(4)(B32(_, _).u32)
  given u64: Deserializer[U64] = Deserializer(8)(B64(_, _).u64)
  
  given byte: Deserializer[Byte] = Deserializer(1) { (data, offset) => data(offset) }
  given short: Deserializer[Short] = Deserializer(2)(B16(_, _).i16.short)
  given int: Deserializer[Int] = Deserializer(4)(B32(_, _).i32.int)
  given long: Deserializer[Long] = Deserializer(8)(B64(_, _).i64.long)

  inline def join[DerivationType <: Product: ProductReflection]: Deserializer[DerivationType] =
    new Deserializer[DerivationType]:
      def deserialize(data: Bytes, offset: Int): DerivationType =
        var position: Int = offset
        
        construct:
          [FieldType] => deserializer =>
            deserializer.deserialize(data, position).also:
              position += deserializer.width
      
      def width: Int = contexts { [FieldType] => deserializer => deserializer.width }.sum
  
trait Deserializer[+DataType]:
  def deserialize(data: Bytes, offset: Int): DataType
  def width: Int

extension (bytes: Bytes)
  def deserialize[DataType](offset: Int = 0)(using deserializer: Deserializer[DataType]): DataType =
    deserializer.deserialize(bytes, offset)
  
  def deserializeIArray[DataType: ClassTag](size: Int, offset: Int = 0)
      (using deserializer: Deserializer[DataType])
          : IArray[DataType] =

    val width = deserializer.width
    
    IArray.create[DataType](size): array =>
      array.indices.each: index =>
        array(index) = deserializer.deserialize(bytes, offset + index*width)

def byteWidth[DataType](using deserializer: Deserializer[DataType]): Int = deserializer.width
