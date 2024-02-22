package bifurcate

import wisteria.*
import rudiments.*
import anticipation.*
import hypotenuse.*

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
  def deserialize
      [DataType]
      (offset: Int = 0)
      (using deserializer: Deserializer[DataType])[ResultType]
      (lambda: (width: Int) ?=> DataType => ResultType)
      : ResultType =
    lambda(using deserializer.width)(deserializer.deserialize(bytes, offset))
