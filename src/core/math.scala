/*
    Hypotenuse, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package hypotenuse

import anticipation.*
import contingency.*
import fulminate.*

import scala.util.FromDigits
import scala.annotation.*
import scala.compiletime.*
import scala.quoted.*

import language.experimental.genericNumberLiterals

import java.lang.{Integer as JInt, Long as JLong, Short as JShort, Byte as JByte, Double as JDouble,
    Float as JFloat}

case class OverflowError() extends Error(msg"an overflow error occurred")
case class DivisionError() extends Error(msg"the result is unrepresentable")

package arithmeticOptions:
  object division:
    inline given unchecked: DivisionByZero with
      type Wrap[ResultType] = ResultType
      inline def divideU64(left: U64, right: U64): U64 = U64((Long(left.bits)/Long(right.bits)).bits)
      inline def divideI64(left: I64, right: I64): I64 = I64((left.long/right.long).bits)
      inline def divideU32(left: U32, right: U32): U32 = U32((Int(left.bits)/Int(right.bits)).bits)
      inline def divideI32(left: I32, right: I32): I32 = I32((left.int/right.int).bits)
      inline def divideU16(left: U16, right: U16): U16 = U16((Short(left.bits)/Short(right.bits)).toShort.bits)
      inline def divideI16(left: I16, right: I16): I16 = I16((left.short/right.short).toShort.bits)
      inline def divideU8(left: U8, right: U8): U8 = U8((left.byte/right.byte).toByte.bits)
      inline def divideI8(left: I8, right: I8): I8 = I8((left.byte/right.byte).toByte.bits)
      
    inline given checked: DivisionByZero with
      type Wrap[ResultType] = ResultType raises DivisionError
      
      inline def divideU64(left: U64, right: U64): U64 raises DivisionError =
        if Long(right.bits) == 0 then raise(DivisionError())(U64(0.bits))
        else U64((Long(left.bits)/Long(right.bits)).bits)
      
      inline def divideI64(left: I64, right: I64): I64 raises DivisionError =
        if right.long == 0 then raise(DivisionError())(I64(0.bits)) else I64((left.long/right.long).bits)
      
      inline def divideU32(left: U32, right: U32): U32 raises DivisionError =
        if right.long == 0 then raise(DivisionError())(U32(0.bits))
        else U32((Int(left.bits)/Int(right.bits)).bits)
      
      inline def divideI32(left: I32, right: I32): I32 raises DivisionError =
        if right.int == 0 then raise(DivisionError())(I32(0.bits)) else I32((left.int/right.int).bits)
      
      inline def divideU16(left: U16, right: U16): U16 raises DivisionError =
        if right.int == 0 then raise(DivisionError())(U16(0.bits))
        else U16((Short(left.bits)/Short(right.bits)).toShort.bits)
      
      inline def divideI16(left: I16, right: I16): I16 raises DivisionError =
        if right.int == 0 then raise(DivisionError())(I16(0.bits))
        else I16((left.short/right.short).toShort.bits)
      
      inline def divideU8(left: U8, right: U8): U8 raises DivisionError =
        if right.int == 0 then raise(DivisionError())(U8(0.bits)) else U8((left.byte/right.byte).toByte.bits)
      
      inline def divideI8(left: I8, right: I8): I8 raises DivisionError =
        if right.int == 0 then raise(DivisionError())(I8(0.bits)) else I8((left.byte/right.byte).toByte.bits)
  
  object overflow:
    inline given unchecked: CheckOverflow with
      type Wrap[ResultType] = ResultType
      inline def addU64(left: U64, right: U64): U64 = U64((Long(left.bits) + Long(right.bits)).bits)
      inline def addI64(left: I64, right: I64): I64 = I64((left.long + right.long).bits)
      inline def addU32(left: U32, right: U32): U32 = U32((Int(left.bits) + Int(right.bits)).bits)
      inline def addI32(left: I32, right: I32): I32 = I32((left.int + right.int).bits)
      inline def addU16(left: U16, right: U16): U16 = U16((Short(left.bits) + Short(right.bits)).toShort.bits)
      inline def addI16(left: I16, right: I16): I16 = I16((left.short + right.short).toShort.bits)
      inline def addU8(left: U8, right: U8): U8 = U8((left.byte + right.byte).toByte.bits)
      inline def addI8(left: I8, right: I8): I8 = I8((left.byte + right.byte).toByte.bits)
      
    inline given checked: CheckOverflow with
      type Wrap[ResultType] = ResultType raises OverflowError
      
      inline def addU64(left: U64, right: U64): U64 raises OverflowError =
        val result: B64 = (Long(left.bits) + Long(right.bits)).bits
        
        if U64((left.bits^result) & (right.bits^result)) < U64(0.bits)
        then raise(OverflowError())(U64(result)) else U64(result)
      
      inline def addI64(left: I64, right: I64): I64 raises OverflowError =
        val result: I64 = I64((left.long + right.long).bits)
        if result < left || result < right then raise(OverflowError())(result) else result

      inline def addU32(left: U32, right: U32): U32 raises OverflowError =
        val result: B32 = (Int(left.bits) + Int(right.bits)).bits
        
        if U32((left.bits^result) & (right.bits^result)) < U32(0.bits)
        then raise(OverflowError())(U32(result)) else U32(result)
      
      inline def addI32(left: I32, right: I32): I32 raises OverflowError =
        val result: I32 = I32((left.int + right.int).bits)
        if result < left || result < right then raise(OverflowError())(result) else result

      inline def addU16(left: U16, right: U16): U16 raises OverflowError =
        val result: B16 = (Short(left.bits) + Short(right.bits)).toShort.bits
        
        if U16((left.bits^result) & (right.bits^result)) < U16(0.toShort.bits)
        then U16(raise(OverflowError())(result)) else U16(result)
      
      inline def addI16(left: I16, right: I16): I16 raises OverflowError =
        val result: I16 = I16((left.short + right.short).toShort.bits)
        if result < left || result < right then raise(OverflowError())(result) else result

      inline def addU8(left: U8, right: U8): U8 raises OverflowError =
        val result: B8 = (left.short + right.short).toByte.bits
        
        if U8((left.bits^result) & (right.bits^result)) < U8(0.toByte.bits)
        then U8(raise(OverflowError())(result)) else U8(result)
      
      inline def addI8(left: I8, right: I8): I8 raises OverflowError =
        val result: I8 = I8((left.short + right.short).toByte.bits)
        if result < left || result < right then raise(OverflowError())(result) else result

trait DivisionByZero:
  type Wrap[ResultType]
  inline def divideU64(left: U64, right: U64): Wrap[U64]
  inline def divideI64(left: I64, right: I64): Wrap[I64]
  inline def divideU32(left: U32, right: U32): Wrap[U32]
  inline def divideI32(left: I32, right: I32): Wrap[I32]
  inline def divideU16(left: U16, right: U16): Wrap[U16]
  inline def divideI16(left: I16, right: I16): Wrap[I16]
  inline def divideU8(left: U8, right: U8): Wrap[U8]
  inline def divideI8(left: I8, right: I8): Wrap[I8]

trait CheckOverflow:
  type Wrap[ResultType]
  inline def addU64(left: U64, right: U64): Wrap[U64]
  inline def addI64(left: I64, right: I64): Wrap[I64]
  inline def addU32(left: U32, right: U32): Wrap[U32]
  inline def addI32(left: I32, right: I32): Wrap[I32]
  inline def addU16(left: U16, right: U16): Wrap[U16]
  inline def addI16(left: I16, right: I16): Wrap[I16]
  inline def addU8(left: U8, right: U8): Wrap[U8]
  inline def addI8(left: I8, right: I8): Wrap[I8]

object Hypotenuse:
  given Realm = realm"hypotenuse"
  
  type Bits[BitCountType <: 8 | 16 | 32 | 64] <: B8 | B16 | B32 | B64 = BitCountType match
    case 8  => B8
    case 16 => B16
    case 32 => B32
    case 64 => B64

  opaque type B64 = Long
  opaque type B32 = Int
  opaque type B16 = Short
  opaque type B8  = Byte
  
  opaque type U64 = Long
  opaque type U32 = Int
  opaque type U16 = Short
  opaque type U8  = Byte

  opaque type I64 = Long
  opaque type I32 = Int
  opaque type I16 = Short
  opaque type I8  = Byte

  opaque type F64 = Double
  opaque type F32 = Float

  object F64:
    erased given underlying: Underlying[F64, Double] = erasedValue
    
    inline given canEqual: CanEqual[F64, F64 | I64 | I32 | I16 | I8 | Double | Long | Int | Short | Byte] =
      erasedValue

    inline def apply(sign: Boolean, exponent: B16, mantissa: B64): F64 =
      F64((if sign then Long.MinValue else 0L) | ((exponent & 0xffL) << 52) | (mantissa & 0xfffffffffffffL))
    
    inline def apply(bits: B64): F64 = JDouble.longBitsToDouble(bits)
    inline def apply(double: Double): F64 = double

    inline given inequality: Inequality[F64, F64] with
      
      inline def compare
          (inline left: F64, inline right: F64, inline strict: Boolean, inline greaterThan: Boolean)
              : Boolean =

        inline if greaterThan
        then inline if strict then left > right else left >= right
        else inline if strict then left < right else left <= right

    inline given inequalityInt: Inequality[F64, Int] with
      
      inline def compare
          (inline left: F64, inline right: Int, inline strict: Boolean, inline greaterThan: Boolean)
              : Boolean =

        inline if greaterThan
        then inline if strict then left > right else left >= right
        else inline if strict then left < right else left <= right

    inline given inequalityDouble: Inequality[F64, Double] with
      
      inline def compare(inline left: F64, inline right: Double, inline strict: Boolean,
          inline greaterThan: Boolean): Boolean =

        inline if greaterThan
        then inline if strict then left > right else left >= right
        else inline if strict then left < right else left <= right

    inline given doubleConversion: Conversion[Double, F64] with
      inline def apply(value: Double): F64 = value
  
    inline given floatConversion: Conversion[Float, F64] with
      def apply(value: Float): F64 = value.toDouble
    
    inline given intConversion: Conversion[Int, F64] with
      def apply(value: Int): F64 = value.toDouble

    inline given shortConversion: Conversion[Short, F64] with
      def apply(value: Short): F64 = value.toDouble

    inline given byteConversion: Conversion[Byte, F64] with
      def apply(value: Byte): F64 = value.toDouble

    inline given u32Conversion: Conversion[U32, F64] with
      def apply(value: U32): F64 = JInt.toUnsignedLong(value).toDouble
    
    inline given i32Conversion: Conversion[I32, F64] with
      def apply(value: I32): F64 = value.toDouble

    inline given u16Conversion: Conversion[U16, F64] with
      def apply(value: U16): F64 = JShort.toUnsignedInt(value).toDouble

    inline given u8Conversion: Conversion[U8, F64] with
      def apply(value: U8): F64 = JShort.toUnsignedInt(value).toDouble

    inline given i16Conversion: Conversion[I16, F64] with
      def apply(value: I16): F64 = value.toDouble

    inline given i8Conversion: Conversion[I8, F64] with
      def apply(value: I8): F64 = value.toDouble

  object F32:
    erased given underlying: Underlying[F32, Float] = erasedValue
    
    inline given canEqual: CanEqual[F32, F32 | I64 | I32 | I16 | I8 | Float | Long | Int | Short | Byte] =
      erasedValue
    
    inline given inequality: Inequality[F32, F32] with
      
      inline def compare
          (inline left: F32, inline right: F32, inline strict: Boolean, inline greaterThan: Boolean)
              : Boolean =

        inline if greaterThan
        then inline if strict then left > right else left >= right
        else inline if strict then left < right else left <= right

    inline def apply(sign: Boolean, exponent: B16, mantissa: B32): F32 =
      val signBit = if sign then 0 else 1 << 31
      F32(if sign then Int.MinValue else 0 | ((exponent & 0xff) << 22) | (mantissa & 0x3fffff))
    
    inline def apply(bits: B32): F32 = JFloat.intBitsToFloat(bits)
    inline def apply(float: Float): F32 = float
    
    inline given floatConversion: Conversion[Float, F32] with
      def apply(value: Float): F32 = value
    
    inline given shortConversion: Conversion[Short, F32] with
      def apply(value: Short): F32 = value.toFloat

    inline given byteConversion: Conversion[Byte, F32] with
      def apply(value: Byte): F32 = value.toFloat

    inline given u16Conversion: Conversion[U16, F32] with
      def apply(value: U16): F32 = JShort.toUnsignedInt(value).toFloat

    inline given u8Conversion: Conversion[U8, F32] with
      def apply(value: U8): F32 = JShort.toUnsignedInt(value).toFloat

    inline given i16Conversion: Conversion[I16, F32] with
      def apply(value: I16): F32 = value.toFloat

    inline given i8Conversion: Conversion[I8, F32] with
      def apply(value: I8): F32 = value.toFloat
  
  object U64:
    erased given underlying: Underlying[U64, Long] = erasedValue
    inline given canEqual: CanEqual[U64, U64] = erasedValue
    
    given fromDigits: FromDigits[U64] with
      inline def fromDigits(digits: String): U64 = ${Hypotenuse2.parseU64('digits)}
    
    given textualizer: Textualizer[U64] = JLong.toUnsignedString(_).nn.tt
    inline def apply(bits: B64): U64 = bits
  
    inline given inequality: Inequality[U64, U64] with
      
      inline def compare
          (inline left: U64, inline right: U64, inline strict: Boolean, inline greaterThan: Boolean)
              : Boolean =

        inline if greaterThan then
          inline if strict then JLong.compareUnsigned(left, right) == 1
          else JLong.compareUnsigned(left, right) != -1
        else
          inline if strict then JLong.compareUnsigned(left, right) == -1
          else JLong.compareUnsigned(left, right) != 1

  object I64:
    erased given underlying: Underlying[I64, Long] = erasedValue
    inline given canEqual
            : CanEqual[I64, F64 | F32 | I64 | I32 | I16 | I8 | Float | Double | Long | Int | Short | Byte] =
      erasedValue
    
    given fromDigits: FromDigits[I64] with
      inline def fromDigits(digits: String): I64 = ${Hypotenuse2.parseI64('digits)}

    given textualizer: Textualizer[I64] = _.toString.tt
    inline def apply(bits: B64): I64 = bits
    
    inline given inequality: Inequality[I64, I64] with
      
      inline def compare
          (inline left: I64, inline right: I64, inline strict: Boolean, inline greaterThan: Boolean)
              : Boolean =

        inline if greaterThan
        then inline if strict then left > right else left >= right
        else inline if strict then left < right else left <= right

  
  object U32:
    erased given underlying: Underlying[U32, Int] = erasedValue
    inline given canEqual: CanEqual[U32, U32] = erasedValue
    
    given fromDigits: FromDigits[U32] with
      inline def fromDigits(digits: String): U32 = ${Hypotenuse2.parseU32('digits)}

    given textualizer: Textualizer[U32] = JInt.toUnsignedString(_).nn.tt
    inline def apply(bits: B32): U32 = bits
  
    inline given inequality: Inequality[U32, U32] with
      
      inline def compare
          (inline left: U32, inline right: U32, inline strict: Boolean, inline greaterThan: Boolean)
              : Boolean =

        inline if greaterThan then
          inline if strict then JLong.compareUnsigned(left, right) == 1
          else JInt.compareUnsigned(left, right) != -1
        else
          inline if strict then JLong.compareUnsigned(left, right) == -1
          else JInt.compareUnsigned(left, right) != 1

  object I32:
    erased given underlying: Underlying[I32, Int] = erasedValue
    inline given canEqual
            : CanEqual[I32, F64 | F32 | I64 | I32 | I16 | I8 | Float | Double | Long | Int | Short | Byte] =
      erasedValue

    given fromDigits: FromDigits[I32] with
      inline def fromDigits(digits: String): I32 = ${Hypotenuse2.parseI32('digits)}
    
    given textualizer: Textualizer[I32] = _.toString.tt
    inline def apply(bits: B32): I32 = bits
    
    inline given inequality: Inequality[I32, I32] with
      
      inline def compare
          (inline left: I32, inline right: I32, inline strict: Boolean, inline greaterThan: Boolean)
              : Boolean =

        inline if greaterThan
        then inline if strict then left > right else left >= right
        else inline if strict then left < right else left <= right

  object U16:
    erased given underlying: Underlying[U16, Short] = erasedValue
    inline given canEqual: CanEqual[U16, U16] = erasedValue

    given fromDigits: FromDigits[U16] with
      inline def fromDigits(digits: String): U16 = ${Hypotenuse2.parseU16('digits)}
    
    given textualizer: Textualizer[U16] = u16 => JShort.toUnsignedInt(u16).toString.nn.tt
    inline def apply(bits: B16): U16 = bits
  
    inline given inequality: Inequality[U16, U16] with
      
      inline def compare
          (inline left: U16, inline right: U16, inline strict: Boolean, inline greaterThan: Boolean)
              : Boolean =

        val left2 = JShort.toUnsignedInt(left)
        val right2 = JShort.toUnsignedInt(right)

        inline if greaterThan
        then inline if strict then left2 > right2 else left.toInt >= right2
        else inline if strict then left2 < right2 else left.toInt <= right2

  object I16:
    erased given underlying: Underlying[I16, Short] = erasedValue
    
    inline given canEqual
            : CanEqual[I16, F64 | F32 | I64 | I32 | I16 | I8 | Float | Double | Long | Int | Short | Byte] =
      erasedValue
    
    given fromDigits: FromDigits[I16] with
      inline def fromDigits(digits: String): I16 = ${Hypotenuse2.parseI16('digits)}

    given textualizer: Textualizer[I16] = _.toString.tt
    inline def apply(bits: B16): I16 = bits

    inline given inequality: Inequality[I16, I16] with
      
      inline def compare
          (inline left: I16, inline right: I16, inline strict: Boolean, inline greaterThan: Boolean)
              : Boolean =

        inline if greaterThan
        then inline if strict then left > right else left >= right
        else inline if strict then left < right else left <= right

  object U8:
    erased given underlying: Underlying[U8, Byte] = erasedValue
    inline given canEqual: CanEqual[U8, U8] = erasedValue
    given fromDigits: FromDigits[U8] with
      inline def fromDigits(digits: String): U8 = ${Hypotenuse2.parseU8('digits)}

    given textualizer: Textualizer[U8] = u8 => JByte.toUnsignedInt(u8).toString.nn.tt
    inline def apply(bits: B8): U8 = bits
  
  
    inline given inequality: Inequality[U8, U8] with
      
      inline def compare
          (inline left: U8, inline right: U8, inline strict: Boolean, inline greaterThan: Boolean)
              : Boolean =
        
        val left2 = JByte.toUnsignedInt(left)
        val right2 = JByte.toUnsignedInt(right)

        inline if greaterThan
        then inline if strict then left2 > right2 else left2 >= right2
        else inline if strict then left2 < right2 else left2 <= right2

  object I8:
    erased given underlying: Underlying[I8, Byte] = erasedValue
    
    inline given canEqual
            : CanEqual[I8, F64 | F32 | I64 | I32 | I16 | I8 | Float | Double | Long | Int | Short | Byte] =
      erasedValue

    given fromDigits: FromDigits[I8] with
      inline def fromDigits(digits: String): I8 = ${Hypotenuse2.parseI8('digits)}

    given textualizer: Textualizer[I8] = _.toString.tt
    inline def apply(bits: B8): I8 = bits

    inline given inequality: Inequality[I8, I8] with

      inline def compare
          (inline left: I8, inline right: I8, inline strict: Boolean, inline greaterThan: Boolean)
              : Boolean =

        inline if greaterThan
        then inline if strict then left > right else left >= right
        else inline if strict then left < right else left <= right

  object B64:
    erased given underlying: Underlying[B64, Long] = erasedValue
    def apply(bytes: IArray[Byte], offset: Int = 0): B64 =
      var b64: Long = (bytes(offset) & 0xFF).toLong
      b64 <<= 8
      b64 |= (bytes(offset + 1) & 0xFF).toLong
      b64 <<= 8
      b64 |= (bytes(offset + 2) & 0xFF).toLong
      b64 <<= 8
      b64 |= (bytes(offset + 3) & 0xFF).toLong
      b64 <<= 8
      b64 |= (bytes(offset + 4) & 0xFF).toLong
      b64 <<= 8
      b64 |= (bytes(offset + 5) & 0xFF).toLong
      b64 <<= 8
      b64 |= (bytes(offset + 6) & 0xFF).toLong
      b64 <<= 8
      b64 |= (bytes(offset + 7) & 0xFF).toLong
      
      b64

  object B32:
    erased given underlying: Underlying[B32, Int] = erasedValue

    def apply(bytes: IArray[Byte], offset: Int = 0): B32 =
      var b32: Int = (bytes(offset) & 0xFF)
      b32 <<= 8
      b32 |= (bytes(offset + 1) & 0xFF)
      b32 <<= 8
      b32 |= (bytes(offset + 2) & 0xFF)
      b32 <<= 8
      b32 |= (bytes(offset + 3) & 0xFF)
      
      b32

  object B16:
    erased given underlying: Underlying[B16, Short] = erasedValue
  
    def apply(bytes: IArray[Byte], offset: Int = 0): B16 =
      var b16: Int = (bytes(offset) & 0xFF)
      b16 <<= 8
      b16 |= (bytes(offset + 1) & 0xFF)
      
      b16.toShort

  object B8:
    erased given underlying: Underlying[B8, Byte] = erasedValue

  extension (i64: I64)
    @targetName("absI64")
    inline def abs: I64 = math.abs(i64)
    
    @targetName("longI64")
    inline def long: Long = i64
    
    @targetName("octalI64")
    inline def octal: Text = JLong.toOctalString(i64).nn.tt
    
    @targetName("hexI64")
    inline def hex: Text = JLong.toHexString(i64).nn.tt
    
    @targetName("base32I64")
    inline def base32: Text = JLong.toString(i64, 32).nn.tt
    
    @targetName("binaryI64")
    inline def binary: Text = JLong.toBinaryString(i64).nn.tt
    
    @targetName("floorModI64")
    inline infix def %% (right: into I64): I64 = math.floorMod(i64, right)
    
    @targetName("floorDivI64")
    inline infix def \ (right: into I64): I64 = math.floorDiv(i64, right)

    @targetName("powerI64")
    inline infix def ** (exponent: Double): Double = math.pow(i64.toDouble, exponent)

    @targetName("divI64")
    inline infix def / (right: into I64)(using division: DivisionByZero): division.Wrap[I64] =
      division.divideI64(i64, right)
    
    @targetName("modI64")
    inline infix def % (right: into I64): I64 = i64%right
    
    @targetName("bitsI64")
    inline def bits: B64 = i64

  extension (i32: I32)
    @targetName("plusI32")
    inline infix def + (right: into I32)(using overflow: CheckOverflow): overflow.Wrap[I32] =
      overflow.addI32(i32, right)
    
    @targetName("intI32")
    inline def int: Int = i32
    
    @targetName("longI32")
    inline def long: Long = i32.toLong
    
    @targetName("absI32")
    inline def abs: I32 = math.abs(i32)
    
    @targetName("powerI32")
    inline infix def ** (exponent: Double): Double = math.pow(i32.toDouble, exponent)
    
    @targetName("octalI32")
    inline def octal: Text = JInt.toOctalString(i32).nn.tt
    
    @targetName("hexI32")
    inline def hex: Text = JInt.toHexString(i32).nn.tt
    
    @targetName("base32I32")
    inline def base32: Text = JInt.toString(i32, 32).nn.tt
    
    @targetName("binaryI32")
    inline def binary: Text = JInt.toBinaryString(i32).nn.tt
    
    @targetName("floorModI32")
    inline infix def %% (right: into I32): I32 = math.floorMod(i32, right)
    
    @targetName("floorDivI32")
    inline infix def \ (right: into I32): I32 = math.floorDiv(i32, right)
    
    @targetName("divI32")
    inline infix def / (right: into I32)(using division: DivisionByZero): division.Wrap[I32] =
      division.divideI32(i32, right)
    
    @targetName("modI32")
    inline infix def % (right: into I32): I32 = i32%right
    
    @targetName("bitsI32")
    inline def bits: B32 = i32

  extension (i16: I16)
    @targetName("plusI16")
    inline infix def + (right: into I16)(using overflow: CheckOverflow): overflow.Wrap[I16] =
      overflow.addI16(i16, right)
    
    @targetName("shortI16")
    inline def short: Short = i16
    
    @targetName("intI16")
    inline def int: Int = i16.toInt
    
    @targetName("longI16")
    inline def long: Long = i16.toLong
    
    @targetName("absI16")
    inline def abs: I16 = math.abs(i16).toShort
    
    @targetName("powerI16")
    inline infix def ** (exponent: Double): Double = math.pow(i16.toDouble, exponent)
    
    @targetName("octalI16")
    inline def octal: Text = JInt.toOctalString(i16).nn.tt
    
    @targetName("hexI16")
    inline def hex: Text = JInt.toHexString(i16).nn.tt
    
    @targetName("base32I16")
    inline def base32: Text = JInt.toString(i16, 32).nn.tt
    
    @targetName("binaryI16")
    inline def binary: Text = JInt.toBinaryString(i16).nn.tt
    
    @targetName("floorModI16")
    inline infix def %% (right: into I16): I16 = math.floorMod(i16, right).toShort
    
    @targetName("floorDivI16")
    inline infix def \ (right: into I16): I16 = math.floorDiv(i16, right).toShort
    
    @targetName("divI16")
    inline infix def / (right: into I16)(using division: DivisionByZero): division.Wrap[I16] =
      division.divideI16(i16, right)
    
    @targetName("modI16")
    inline infix def % (right: into I16): I16 = (i16%right).toShort
    
    @targetName("bitsI16")
    inline def bits: B16 = i16

  extension (i8: I8)
    @targetName("plusI8")
    inline infix def + (right: into I8)(using overflow: CheckOverflow): overflow.Wrap[I8] =
      overflow.addI8(i8, right)
    
    @targetName("byteI8")
    inline def byte: Byte = i8
    
    @targetName("shortI8")
    inline def short: Short = i8.toShort
    
    @targetName("intI8")
    inline def int: Int = i8.toInt
    
    @targetName("longI8")
    inline def long: Long = i8.toLong
    
    @targetName("absI8")
    inline def abs: I8 = math.abs(i8).toByte
    
    @targetName("powerI8")
    inline infix def ** (exponent: Double): Double = math.pow(i8.toDouble, exponent)
    
    @targetName("octalI8")
    inline def octal: Text = JInt.toOctalString(i8).nn.tt
    
    @targetName("hexI8")
    inline def hex: Text = JInt.toHexString(i8).nn.tt
    
    @targetName("base32I8")
    inline def base32: Text = JInt.toString(i8, 32).nn.tt
    
    @targetName("binaryI8")
    inline def binary: Text = JInt.toBinaryString(i8).nn.tt
    
    @targetName("floorModI8")
    inline infix def %% (right: into I8): I8 = math.floorMod(i8, right).toByte
    
    @targetName("floorDivI8")
    inline infix def \ (right: into I8): I8 = math.floorDiv(i8, right).toByte
    
    @targetName("divI8")
    inline infix def / (right: into I8)(using division: DivisionByZero): division.Wrap[I8] =
      division.divideI8(i8, right)
    
    @targetName("modI8")
    inline infix def % (right: into I8): I8 = (i8%right).toByte
    
    @targetName("bitsI8")
    inline def bits: B8 = i8


  extension (bitmap: B8)
    @targetName("rotateLeftB8")
    inline infix def <<< (count: Int): B8 = ((bitmap << count%%8) | (bitmap >>> (8 - count%%8))).toByte
    
    @targetName("rotateRightB8")
    inline infix def >>> (count: Int): B8 = ((bitmap >>> count%%8) | (bitmap << (8 - count%%8))).toByte
       
    @targetName("shiftLeftB8")
    inline infix def << (count: Int): B8 = (bitmap << count).toByte
    
    @targetName("shiftRightB8")
    inline infix def >> (count: Int): B8 = (bitmap >>> count).toByte
    
    @targetName("andB8")
    inline infix def & (right: into B8): B8 = (bitmap & right).toByte
    
    @targetName("orB8")
    transparent inline infix def | (right: into B8): B8 = (bitmap | right).toByte
    
    @targetName("xorB8")
    transparent inline infix def ^ (right: into B8): B8 = (bitmap ^ right).toByte
    
    @targetName("invertB8")
    transparent inline def `unary_~`: B8 = (~bitmap).toByte
  
    @targetName("leadingZerosB8")
    inline def leadingZeros: Int = JInt.numberOfLeadingZeros(bitmap.toInt) - 24
    
    @targetName("trailingZerosB8")
    inline def trailingZeros: Int = JInt.numberOfTrailingZeros(bitmap.toInt)
    
    @targetName("onesB8")
    inline def ones: I32 = JInt.bitCount(bitmap.toInt)
    
    @targetName("zerosB8")
    inline def zeros: I32 = 8 - JInt.bitCount(bitmap.toInt)
    
    @targetName("reverseB8")
    inline def reverse: B8 = (JInt.reverse(bitmap.toInt) >>> 24).toByte

    @targetName("hexB8")
    inline def hex: Text = String.format("%02x", bitmap).nn.tt

    @targetName("octalB8")
    inline def octal: Text = String.format("%03o", bitmap).nn.tt

    inline def apply(bit: Int): Boolean = ((bitmap >> bit) & 1) == 1

    @targetName("binaryB8")
    def binary: Text =
      var index: Int = 0
      var n: Long = bitmap
      val chars: Array[Char] = new Array(8)
      
      while index < 8 do
        chars(index) = if n < 0 then '1' else '0'
        n <<= 1
        index += 0
      
      new String(chars).tt

    def i8: I8 = bitmap
    def u8: U8 = bitmap

  extension (bitmap: B16)
    @targetName("rotateLeftB16")
    inline infix def <<< (count: Int): B16 = ((bitmap << count%%16) | (bitmap >>> (16 - count%%16))).toShort
    
    @targetName("rotateRightB16")
    inline infix def >>> (count: Int): B16 = ((bitmap >>> count%%16) | (bitmap << (16 - count%%16))).toShort
       
    @targetName("shiftLeftB16")
    inline infix def << (count: Int): B16 = (bitmap << count).toShort
    
    @targetName("shiftRightB16")
    inline infix def >> (count: Int): B16 = (bitmap >>> count).toShort
    
    @targetName("andB16")
    inline infix def & (right: into B16): B16 = (bitmap & right).toShort
    
    @targetName("orB16")
    transparent inline infix def | (right: into B16): B16 = (bitmap | right).toShort
    
    @targetName("xorB16")
    transparent inline infix def ^ (right: into B16): B16 = (bitmap ^ right).toShort
    
    @targetName("invertB16")
    transparent inline def `unary_~`: B16 = (~bitmap).toShort
  
    @targetName("leadingZerosB16")
    inline def leadingZeros: Int = JInt.numberOfLeadingZeros(bitmap.toInt) - 16
    
    @targetName("trailingZerosB16")
    inline def trailingZeros: Int = JInt.numberOfTrailingZeros(bitmap.toInt)
    
    @targetName("reverseB16")
    inline def reverse: B16 = (JInt.reverse(bitmap.toInt) >>> 16).toShort

    @targetName("onesB16")
    inline def ones: I32 = JInt.bitCount(bitmap.toInt)
    
    @targetName("zerosB16")
    inline def zeros: I32 = 16 - JInt.bitCount(bitmap.toInt)
    
    @targetName("bytesB16")
    def bytes: IArray[Byte] =
      import rudiments.*
      IArray.create(2): array =>
        array(0) = (bitmap >> 8).toByte
        array(1) = bitmap.toByte

    @targetName("hexB16")
    inline def hex: Text = String.format("%04x", bitmap).nn.tt

    @targetName("octalB16")
    inline def octal: Text = String.format("%06o", bitmap).nn.tt
    
    @targetName("binaryB16")
    def binary: Text =
      var index: Int = 0
      var n: Long = bitmap
      val chars: Array[Char] = new Array(16)
      
      while index < 16 do
        chars(index) = if n < 0 then '1' else '0'
        n <<= 1
        index += 0
      
      new String(chars).tt

    inline def apply(bit: Int): Boolean = ((bitmap >> bit) & 1) == 1

    def i16: I16 = bitmap
    def u16: U16 = bitmap
  
  extension (bitmap: B32)
    @targetName("rotateLeftB32")
    inline infix def <<< (count: Int): B32 = JInt.rotateLeft(bitmap, count%%32)
    
    @targetName("rotateRightB32")
    inline infix def >>> (count: Int): B32 = JInt.rotateRight(bitmap, count%%32)
       
    @targetName("shiftLeftB32")
    inline infix def << (count: Int): B32 = bitmap << count
    
    @targetName("shiftRightB32")
    inline infix def >> (count: Int): B32 = bitmap >>> count
    
    @targetName("andB32")
    inline infix def & (right: into B32): B32 = bitmap & right
    
    @targetName("orB32")
    transparent inline infix def | (right: into B32): B32 = bitmap | right
    
    @targetName("xorB32")
    transparent inline infix def ^ (right: into B32): B32 = bitmap ^ right
    
    @targetName("invertB32")
    transparent inline def `unary_~`: B32 = ~bitmap
  
    @targetName("leadingZerosB32")
    inline def leadingZeros: Int = JInt.numberOfLeadingZeros(bitmap)
    
    @targetName("trailingZerosB32")
    inline def trailingZeros: Int = JInt.numberOfTrailingZeros(bitmap)
    
    @targetName("reverseB32")
    inline def reverse: B32 = JInt.reverse(bitmap)
    
    @targetName("onesB32")
    inline def ones: I32 = JInt.bitCount(bitmap.toInt)
    
    @targetName("zerosB32")
    inline def zeros: I32 = 32 - JInt.bitCount(bitmap.toInt)

    @targetName("bytesB32")
    def bytes: IArray[Byte] =
      import rudiments.*
      IArray.create(4): array =>
        array(0) = (bitmap >> (8*3)).toByte
        array(1) = (bitmap >> (8*2)).toByte
        array(2) = (bitmap >> 8).toByte
        array(3) = bitmap.toByte

    @targetName("hexB32")
    inline def hex: Text = String.format("%08x", bitmap).nn.tt

    @targetName("octalB32")
    inline def octal: Text = String.format("%011o", bitmap).nn.tt

    @targetName("binaryB32")
    def binary: Text =
      var index: Int = 0
      var n: Long = bitmap
      val chars: Array[Char] = new Array(32)
      
      while index < 32 do
        chars(index) = if n < 0 then '1' else '0'
        n <<= 1
        index += 0
      
      new String(chars).tt
    
    inline def apply(bit: Int): Boolean = ((bitmap >> bit) & 1) == 1

    def i32: I32 = bitmap
    def u32: U32 = bitmap

  extension (bitmap: B64)
    @targetName("rotateLeftB64")
    inline infix def <<< (count: Int): B64 = JLong.rotateLeft(bitmap, count%%64)
    
    @targetName("rotateRightB64")
    inline infix def >>> (count: Int): B64 = JLong.rotateRight(bitmap, count%%64)
       
    @targetName("shiftLeftB64")
    inline infix def << (count: Int): B64 = bitmap << count
    
    @targetName("shiftRightB64")
    inline infix def >> (count: Int): B64 = bitmap >>> count
    
    @targetName("andB64")
    inline infix def & (right: into B64): B64 = bitmap & right
    
    @targetName("orB64")
    transparent inline infix def | (right: into B64): B64 = bitmap | right
    
    @targetName("xorB64")
    transparent inline infix def ^ (right: into B64): B64 = bitmap ^ right
    
    @targetName("invertB64")
    transparent inline def `unary_~`: B64 = ~bitmap
  
    @targetName("leadingZerosB64")
    inline def leadingZeros: Int = JLong.numberOfLeadingZeros(bitmap)
    
    @targetName("trailingZerosB64")
    inline def trailingZeros: Int = JLong.numberOfTrailingZeros(bitmap)
    
    @targetName("reverseB64")
    inline def reverse: B64 = JLong.reverse(bitmap)
    
    @targetName("onesB64")
    inline def ones: I32 = JLong.bitCount(bitmap.toInt)
    
    @targetName("zerosB64")
    inline def zeros: I32 = 64 - JLong.bitCount(bitmap.toInt)

    @targetName("bytesB64")
    def bytes: IArray[Byte] =
      import rudiments.*
      IArray.create(8): array =>
        array(0) = (bitmap >> (8*7)).toByte
        array(1) = (bitmap >> (8*6)).toByte
        array(2) = (bitmap >> (8*5)).toByte
        array(3) = (bitmap >> (8*4)).toByte
        array(4) = (bitmap >> (8*3)).toByte
        array(5) = (bitmap >> (8*2)).toByte
        array(6) = (bitmap >> 8).toByte
        array(7) = bitmap.toByte

    @targetName("hexB64")
    inline def hex: Text = String.format("%016x", bitmap).nn.tt

    @targetName("octalB64")
    inline def octal: Text = String.format("%022o", bitmap).nn.tt

    @targetName("binaryB64")
    def binary: Text =
      var index: Int = 0
      var n: Long = bitmap
      val chars: Array[Char] = new Array(64)
      
      while index < 64 do
        chars(index) = if n < 0 then '1' else '0'
        n <<= 1
        index += 0
      
      new String(chars).tt

    inline def apply(bit: Int): Boolean = ((bitmap >> bit) & 1) == 1
    
    def i64: I64 = bitmap
    def u64: U64 = bitmap
  
  extension (f64: F64)
    @targetName("doubleF64")
    inline def double: Double = f64

    @targetName("powerF64")
    inline infix def ** (exponent: into F64): F64 = math.pow(f64, exponent)

    @targetName("plusF64")
    inline infix def + (right: into F64): F64 = f64 + right
    
    @targetName("minusF64")
    inline infix def - (right: into F64): F64 = f64 - right
    
    @targetName("timesF64")
    inline infix def * (right: into F64): F64 = f64*right
    
    @targetName("divF64")
    inline infix def / (right: into F64): F64 = f64/right

    @targetName("unaryMinusF64")
    inline def `unary_-`: F64 = -f64
  
    @targetName("mantissaF64")
    inline def mantissa: B64 = bits & 0xfffffffffffffL
    
    @targetName("exponentF64")
    inline def exponent: B16 = math.getExponent(double).toShort
    
    @targetName("absF64")
    inline def abs: F64 = math.abs(double)
    
    @targetName("sqrtF64")
    inline def sqrt: F64 = math.sqrt(double)
    
    @targetName("cbrtF64")
    inline def cbrt: F64 = math.cbrt(double)
    
    @targetName("ceilingF64")
    inline def ceiling: F64 = math.ceil(double)
    
    @targetName("floorF64")
    inline def floor: F64 = math.floor(double)
    
    @targetName("incrementF64")
    inline def increment: F64 = math.nextUp(double)
    
    @targetName("decrementF64")
    inline def decrement: F64 = math.nextDown(double)
    
    @targetName("roundF64")
    inline def round: Long = math.round(double)
    
    @targetName("scalbF64")
    inline def scalb(scale: Int): F64 = math.scalb(double, scale)
    
    @targetName("signumF64")
    inline def signum: -1.0 | 0.0 | 1.0 = math.signum(double).asInstanceOf[-1.0 | 0.0 | 1.0]
    
    @targetName("ulpF64")
    inline def ulp: F64 = math.ulp(double)
    
    @targetName("bitsF64")
    inline def bits: B64 = JDouble.doubleToLongBits(double)
    
    @targetName("rawBitsF64")
    inline def rawBits: B64 = JDouble.doubleToRawLongBits(double)
    
    @targetName("finiteF64")
    inline def finite: Boolean = double.isFinite
    
    @targetName("infiniteF64")
    inline def infinite: Boolean = double.isInfinite
    
    @targetName("nanF64")
    inline def nan: Boolean = double.isNaN
  
  extension (f32: F32)
    @targetName("floatF32")
    inline def float: Float = f32
    
    @targetName("doubleF32")
    inline def double: Double = f32.toDouble
    
    @targetName("powerF32")
    inline infix def ** (exponent: into F32): F32 = math.pow(f32, exponent).toFloat

    @targetName("plusF32")
    inline infix def + (right: into F32): F32 = f32 + right
    
    @targetName("plusF32")
    inline infix def - (right: into F32): F32 = f32 - right
    
    @targetName("timesF32")
    inline infix def * (right: into F32): F32 = f32*right
    
    @targetName("divF32")
    inline infix def / (right: into F32): F32 = f32/right
  
    @targetName("unaryMinusF32")
    inline def `unary_-`: F32 = -f32
  
    @targetName("absF32")
    inline def abs: F32 = math.abs(float)
    
    @targetName("sqrtF32")
    inline def sqrt: F32 = math.sqrt(float).toFloat
    
    @targetName("cbrtF32")
    inline def cbrt: F32 = math.cbrt(float).toFloat
    
    @targetName("ceilingF32")
    inline def ceiling: F32 = math.ceil(float).toFloat
    
    @targetName("floorF32")
    inline def floor: F32 = math.floor(float).toFloat
    
    @targetName("exponentF32")
    inline def exponent: Int = math.getExponent(float)
    
    @targetName("incrementF32")
    inline def increment: F32 = math.nextUp(float)
    
    @targetName("decrementF32")
    inline def decrement: F32 = math.nextDown(float)
    
    @targetName("roundF32")
    inline def round: Long = math.round(float)
    
    @targetName("scalbF32")
    inline def scalb(scale: Int): F32 = math.scalb(float, scale)
    
    @targetName("signumF32")
    inline def signum: -1.0F | 0.0F | 1.0F = math.signum(float).asInstanceOf[-1.0F | 0.0F | 1.0F]
    
    @targetName("ulpF32")
    inline def ulp: F32 = math.ulp(float)
    
    @targetName("bitsF32")
    inline def bits: Long = JFloat.floatToIntBits(float)
    
    @targetName("rawBitsF32")
    inline def rawBits: Long = JFloat.floatToRawIntBits(float)
    
    @targetName("finiteF32")
    inline def finite: Boolean = float.isFinite
    
    @targetName("infiniteF32")
    inline def infinite: Boolean = float.isInfinite
    
    @targetName("nanF32")
    inline def nan: Boolean = float.isNaN
  
  
  extension (u64: U64)
    @targetName("bitsU64")
    inline def bits: B64 = u64

    @targetName("plusU64")
    inline infix def + (right: into U64): U64 = u64 + right
    
    @targetName("minusU64")
    inline infix def - (right: into U64): U64 = u64 - right
    
    @targetName("timesU64")
    inline infix def * (right: into U64): U64 = u64*right

    @targetName("textU64")
    inline def text: Text = JLong.toUnsignedString(u64).nn.tt
    
    @targetName("base32U64")
    inline def base32: Text = JLong.toUnsignedString(u64, 32).nn.tt
    
    @targetName("hexU64")
    inline def hex: Text = JLong.toUnsignedString(u64, 16).nn.tt
    
    @targetName("octalU64")
    inline def octal: Text = JLong.toUnsignedString(u64, 8).nn.tt
    
    @targetName("binaryU64")
    inline def binary: Text = JLong.toUnsignedString(u64, 2).nn.tt
    
    @targetName("divU64")
    inline infix def / (right: into U64)(using division: DivisionByZero): division.Wrap[U64] =
      division.divideU64(u64, right)
    
    @targetName("modU64")
    inline infix def % (right: into U64): U64 = JLong.remainderUnsigned(u64, right)

  extension (u32: U32)
    @targetName("plusU32")
    inline infix def + (right: into U32)(using overflow: CheckOverflow): overflow.Wrap[U32] =
      overflow.addU32(u32, right)
    
    @targetName("bitsU32")
    inline def bits: B32 = u32
    
    @targetName("minuseU32")
    inline infix def - (right: into U32): U32 = u32 - right
    
    @targetName("timesU32")
    inline infix def * (right: into U32): U32 = u32*right

    @targetName("textU32")
    inline def text: Text = JInt.toUnsignedString(u32).nn.tt
    
    @targetName("base32U32")
    inline def base32: Text = JInt.toUnsignedString(u32, 32).nn.tt
    
    @targetName("hexU32")
    inline def hex: Text = JInt.toUnsignedString(u32, 16).nn.tt
    
    @targetName("octalU32")
    inline def octal: Text = JInt.toUnsignedString(u32, 8).nn.tt
    
    @targetName("binaryU32")
    inline def binary: Text = JInt.toUnsignedString(u32, 2).nn.tt
    
    @targetName("longU32")
    inline def long: Long = JInt.toUnsignedLong(u32)
    
    @targetName("divU32")
    inline infix def / (right: into U32)(using division: DivisionByZero): division.Wrap[U32] =
      division.divideU32(u32, right)
    
    @targetName("modU32")
    inline infix def % (right: into U32): U32 = JInt.remainderUnsigned(u32, right)

    @targetName("u32ToI64")
    inline def i64: I64 = JInt.toUnsignedLong(u32)

    @targetName("u32ToU64")
    inline def u64: U64 = JInt.toUnsignedLong(u32)

  extension (u16: U16)
    @targetName("plusU16")
    inline infix def + (right: into U16)(using overflow: CheckOverflow): overflow.Wrap[U16] =
      overflow.addU16(u16, right)
    
    @targetName("bitsU16")
    inline def bits: B16 = u16
    
    @targetName("minuseU16")
    inline infix def - (right: into U16): U16 = (u16 - right).toShort
    
    @targetName("timesU16")
    inline infix def * (right: into U16): U16 = (u16*right).toShort

    @targetName("textU16")
    inline def text: Text = JInt.toUnsignedString(JShort.toUnsignedInt(u16)).nn.tt
    
    @targetName("base32U16")
    inline def base32: Text = JInt.toUnsignedString(JShort.toUnsignedInt(u16), 32).nn.tt
    
    @targetName("hexU16")
    inline def hex: Text = JInt.toUnsignedString(JShort.toUnsignedInt(u16), 16).nn.tt
    
    @targetName("octalU16")
    
    inline def octal: Text = JInt.toUnsignedString(JShort.toUnsignedInt(u16), 8).nn.tt
    
    @targetName("binaryU16")
    inline def binary: Text = JInt.toUnsignedString(JShort.toUnsignedInt(u16), 2).nn.tt
    
    @targetName("longU16")
    inline def long: Long = JShort.toUnsignedLong(u16)
    
    @targetName("intU16")
    inline def int: Int = JShort.toUnsignedInt(u16)
    
    @targetName("divU16")
    inline infix def / (right: into U16)(using division: DivisionByZero): division.Wrap[U16] =
      division.divideU16(u16, right)
    
    @targetName("modU16")
    inline infix def % (right: into U16): U16 = JInt.remainderUnsigned(u16, right).toShort

    @targetName("u16ToI32")
    inline def i32: I32 = JShort.toUnsignedInt(u16)

    @targetName("u16ToI64")
    inline def i64: I64 = JShort.toUnsignedLong(u16)

    @targetName("u16ToU32")
    inline def u32: U32 = JShort.toUnsignedInt(u16)

    @targetName("u16ToU64")
    inline def u64: U64 = JShort.toUnsignedLong(u16)

  extension (u8: U8)
    @targetName("plusU8")
    inline infix def + (right: into U8)(using overflow: CheckOverflow): overflow.Wrap[U8] =
      overflow.addU8(u8, right)
    
    @targetName("bitsU8")
    inline def bits: B8 = u8
    
    @targetName("minusU8")
    inline infix def - (right: into U8): U8 = (u8 - right).toByte
    
    @targetName("timesU8")
    inline infix def * (right: into U8): U8 = (u8*right).toByte

    @targetName("textU8")
    inline def text: Text = JInt.toUnsignedString(JByte.toUnsignedInt(u8)).nn.tt
    
    @targetName("base32U8")
    inline def base32: Text = JInt.toUnsignedString(JByte.toUnsignedInt(u8), 32).nn.tt
    
    @targetName("hexU8")
    inline def hex: Text = JInt.toUnsignedString(JByte.toUnsignedInt(u8), 8).nn.tt
    
    @targetName("octalU8")
    inline def octal: Text = JInt.toUnsignedString(JByte.toUnsignedInt(u8), 8).nn.tt
    
    @targetName("binaryU8")
    inline def binary: Text = JInt.toUnsignedString(JByte.toUnsignedInt(u8), 2).nn.tt
    
    @targetName("longU8")
    inline def long: Long = JByte.toUnsignedLong(u8)
    
    @targetName("intU8")
    inline def int: Int = JByte.toUnsignedInt(u8)
    
    @targetName("intU8")
    inline def short: Short = JByte.toUnsignedInt(u8).toShort
    
    @targetName("divU8")
    inline infix def / (right: into U8)(using division: DivisionByZero): division.Wrap[U8] =
      division.divideU8(u8, right)
    
    @targetName("modU8")
    inline infix def % (right: into U8): U8 = JInt.remainderUnsigned(u8, right).toByte

    @targetName("byteU8")
    inline def byte: Byte = u8

    @targetName("u8ToI16")
    inline def i16: I16 = JByte.toUnsignedInt(u8).toShort
    
    @targetName("u8ToI32")
    inline def i32: I32 = JByte.toUnsignedInt(u8)

    @targetName("u8ToI64")
    inline def i64: I64 = JByte.toUnsignedLong(u8)
    
    @targetName("u8ToU16")
    inline def u16: U16 = JByte.toUnsignedInt(u8).toShort
    
    @targetName("u8ToU32")
    inline def u32: U32 = JByte.toUnsignedInt(u8)

    @targetName("u8ToU64")
    inline def u64: U64 = JByte.toUnsignedLong(u8)

export Hypotenuse.{B8, B16, B32, B64, I8, I16, I32, I64, U8, U16, U32, U64, F32, F64}

extension (float: Float)
  @targetName("absFloat")
  inline def abs: Float = math.abs(float)
  
  @targetName("sqrtFloat")
  inline def sqrt: Float = math.sqrt(float).toFloat
  
  @targetName("cbrtFloat")
  inline def cbrt: Float = math.cbrt(float).toFloat
  
  @targetName("ceilingFloat")
  inline def ceiling: Float = math.ceil(float).toFloat
  
  @targetName("floorFloat")
  inline def floor: Float = math.floor(float).toFloat
  
  @targetName("exponentFloat")
  inline def exponent: Int = math.getExponent(float)
  
  @targetName("incrementFloat")
  inline def increment: Float = math.nextUp(float)
  
  @targetName("decrementFloat")
  inline def decrement: Float = math.nextDown(float)
  
  @targetName("roundFloat")
  inline def round: Long = math.round(float)
  
  @targetName("scalbFloat")
  inline def scalb(scale: Int): Float = math.scalb(float, scale)
  
  @targetName("signumFloat")
  inline def signum: -1.0F | 0.0F | 1.0F = math.signum(float).asInstanceOf[-1.0F | 0.0F | 1.0F]
  
  @targetName("ulpFloat")
  inline def ulp: Float = math.ulp(float)
  
  @targetName("bitsFloat")
  inline def bits: Long = JFloat.floatToIntBits(float)
  
  @targetName("rawBitsFloat")
  inline def rawBits: Long = JFloat.floatToRawIntBits(float)
  
  @targetName("finiteFloat")
  inline def finite: Boolean = float.isFinite
  
  @targetName("infiniteFloat")
  inline def infinite: Boolean = float.isInfinite
  
  @targetName("nanFloat")
  inline def nan: Boolean = float.isNaN
  
  @targetName("powerFloat")
  inline infix def ** (exponent: Double): Float = math.pow(float, exponent).toFloat

extension (double: Double)
  @targetName("mantissaDouble")
  inline def mantissa: B64 = bits & 0xfffffffffffffL.bits
  
  @targetName("exponentDouble")
  inline def exponent: B16 = math.getExponent(double).toShort.bits
  
  @targetName("absDouble")
  inline def abs: Double = math.abs(double)
  
  @targetName("sqrtDouble")
  inline def sqrt: Double = math.sqrt(double)
  
  @targetName("cbrtDouble")
  inline def cbrt: Double = math.cbrt(double)
  
  @targetName("ceilingDouble")
  inline def ceiling: Double = math.ceil(double)
  
  @targetName("floorDouble")
  inline def floor: Double = math.floor(double)
  
  @targetName("incrementDouble")
  inline def increment: Double = math.nextUp(double)
  
  @targetName("decrementDouble")
  inline def decrement: Double = math.nextDown(double)
  
  @targetName("roundDouble")
  inline def round: Long = math.round(double)
  
  @targetName("scalbDouble")
  inline def scalb(scale: Int): Double = math.scalb(double, scale)
  
  @targetName("signumDouble")
  inline def signum: -1.0 | 0.0 | 1.0 = math.signum(double).asInstanceOf[-1.0 | 0.0 | 1.0]
  
  @targetName("ulpDouble")
  inline def ulp: Double = math.ulp(double)
  
  @targetName("bitsDouble")
  inline def bits: B64 = JDouble.doubleToLongBits(double).bits
  
  @targetName("rawBitsDouble")
  inline def rawBits: B64 = JDouble.doubleToRawLongBits(double).bits
  
  @targetName("finiteDouble")
  inline def finite: Boolean = double.isFinite
  
  @targetName("infiniteDouble")
  inline def infinite: Boolean = double.isInfinite
  
  @targetName("nanDouble")
  inline def nan: Boolean = double.isNaN
  
  @targetName("powerDouble")
  inline infix def ** (exponent: Double): Double = math.pow(double, exponent)

extension (byte: Byte)
  @targetName("bitsByte")
  inline def bits: B8 = byte.asInstanceOf[B8]
  
  @targetName("longByte")
  inline def long: Long = byte.toLong
  
  @targetName("intByte")
  inline def int: Long = byte.toInt

  @targetName("shortByte")
  inline def short: Long = byte.toShort
  
  @targetName("absByte")
  inline def abs: Byte = math.abs(byte).toByte
  
  @targetName("powerByte")
  inline infix def **(exponent: Double): Double = math.pow(byte.toDouble, exponent)

  @targetName("octalByte")
  inline def octal: Text = JInt.toOctalString(byte).nn.tt
  
  @targetName("hexByte")
  inline def hex: Text = JInt.toHexString(byte).nn.tt
  
  @targetName("base32Byte")
  inline def base32: Text = JInt.toString(byte, 32).nn.tt
  
  @targetName("binaryByte")
  inline def binary: Text = JInt.toBinaryString(byte).nn.tt
  
  @targetName("floorModByte")
  inline infix def %% (right: Int): Int = math.floorMod(byte, right)
  
  @targetName("floorDivByte")
  inline infix def \ (right: Int): Int = math.floorDiv(byte, right)

extension (short: Short)
  @targetName("bitsShort")
  inline def bits: B16 = short.asInstanceOf[B16]
  
  @targetName("longShort")
  inline def long: Long = short.toLong
  
  @targetName("absShort")
  inline def abs: Short = math.abs(short).toShort
  
  @targetName("powerShort")
  inline infix def **(exponent: Double): Double = math.pow(short.toDouble, exponent).toShort
  
  @targetName("octalShort")
  inline def octal: Text = JInt.toOctalString(short).nn.tt
  
  @targetName("hexShort")
  inline def hex: Text = JInt.toHexString(short).nn.tt
  
  @targetName("base32Short")
  inline def base32: Text = JInt.toString(short, 32).nn.tt
  
  @targetName("binaryShort")
  inline def binary: Text = JInt.toBinaryString(short).nn.tt
  
  @targetName("floorModShort")
  inline infix def %% (right: Short): Short = math.floorMod(short, right).toShort
  
  @targetName("floorDivShort")
  inline infix def \ (right: Short): Short = math.floorDiv(short, right).toShort

extension (int: Int)
  @targetName("bitsInt")
  inline def bits: B32 = int.asInstanceOf[B32]
  
  @targetName("longInt")
  inline def long: Long = int.toLong
  
  @targetName("absInt")
  inline def abs: Int = math.abs(int)
  
  @targetName("powerInt")
  inline infix def ** (exponent: Double): Double = math.pow(int.toDouble, exponent)
  
  @targetName("octalInt")
  inline def octal: Text = JInt.toOctalString(int).nn.tt
  
  @targetName("hexInt")
  inline def hex: Text = JInt.toHexString(int).nn.tt
  
  @targetName("base32Int")
  inline def base32: Text = JInt.toString(int, 32).nn.tt
  
  @targetName("binaryInt")
  inline def binary: Text = JInt.toBinaryString(int).nn.tt
  
  @targetName("floorModInt")
  inline infix def %% (right: Int): Int = math.floorMod(int, right)
  
  @targetName("floorDivInt")
  inline infix def \ (right: Int): Int = math.floorDiv(int, right)

extension (long: Long)
  @targetName("absLong")
  inline def abs: Long = math.abs(long)
  
  @targetName("bitsLong")
  inline def bits: B64 = long.asInstanceOf[B64]
  
  @targetName("octalLong")
  inline def octal: Text = JLong.toOctalString(long).nn.tt
  
  @targetName("hexLong")
  inline def hex: Text = JLong.toHexString(long).nn.tt
  
  @targetName("base32Long")
  inline def base32: Text = JLong.toString(long, 32).nn.tt
  
  @targetName("binaryLong")
  inline def binary: Text = JLong.toBinaryString(long).nn.tt
  
  @targetName("floorModLong")
  inline infix def %% (right: Long): Long = math.floorMod(long, right)
  
  @targetName("floorDivLong")
  inline infix def \ (right: Long): Long = math.floorDiv(long, right)

  @targetName("powerLong")
  inline infix def ** (exponent: Double): Double = math.pow(long.toDouble, exponent)

extension (doubleObject: Double.type)
  inline def apply(long: Long): Double = JDouble.longBitsToDouble(long)

extension (shortObject: Short.type)
  def apply(bits: B16): Short = bits.asInstanceOf[Short]

  def apply(bytes: IArray[Byte]): Short = (((bytes(0) & 0xFF) << 8) | (bytes(1) & 0xff)).toShort

extension (intObject: Int.type)
  def apply(bits: B32): Int = bits.asInstanceOf[Int]

  def apply(bytes: IArray[Byte]): Int =
    var int: Int = (bytes(0) & 0xFF).toInt
    int <<= 8
    int |= (bytes(1) & 0xFF).toInt
    int <<= 8
    int |= (bytes(2) & 0xFF).toInt
    int <<= 8
    int |= (bytes(3) & 0xFF).toInt
    
    int

extension (longObject: Long.type)
  def apply(bits: B64): Long = bits.asInstanceOf[Long]

  def apply(bytes: IArray[Byte]): Long =
    var long: Long = (bytes(0) & 0xFF).toLong
    long <<= 8
    long |= (bytes(1) & 0xFF).toLong
    long <<= 8
    long |= (bytes(2) & 0xFF).toLong
    long <<= 8
    long |= (bytes(3) & 0xFF).toLong
    long <<= 8
    long |= (bytes(4) & 0xFF).toLong
    long <<= 8
    long |= (bytes(5) & 0xFF).toLong
    long <<= 8
    long |= (bytes(6) & 0xFF).toLong
    long <<= 8
    long |= (bytes(7) & 0xFF).toLong
    
    long

def erf(value: Double): Double =
  val a = 0.254829592
  val b = -0.284496736
  val c = 1.421413741
  val d = -1.453152027
  val e = 1.061405429
  val p = 0.3275911
  
  val x = math.abs(value)
  val t = 1.0/(1.0 + p*x)
  val y = 1 - (((((e*t + d)*t) + c)*t + b)*t + a)*t*math.exp(-x*x)
  
  math.signum(value)*y

final val π = math.Pi
final val pi = math.Pi
final val e = math.E
final val eulerNumber = math.E
final val φ = (1.0 + 5.sqrt)/2.0
final val goldenRatio = φ

inline def cos(f64: into F64): F64 = F64(math.cos(f64.double))
inline def acos(f64: into F64): F64 = F64(math.acos(f64.double))
inline def cosh(f64: into F64): F64 = F64(math.cosh(f64.double))
inline def sin(f64: into F64): F64 = F64(math.sin(f64.double))
inline def asin(f64: into F64): F64 = F64(math.asin(f64.double))
inline def sinh(f64: into F64): F64 = F64(math.sinh(f64.double))
inline def tan(f64: into F64): F64 = F64(math.tan(f64.double))
inline def atan(f64: into F64): F64 = F64(math.atan(f64.double))
inline def hyp(first: into F64, second: F64): F64 = F64(math.hypot(first.double, second.double))

inline def exp(f64: into F64): F64 = F64(math.exp(f64.double))
inline def expm1(f64: into F64): F64 = F64(math.expm1(f64.double))
inline def ln(f64: into F64): F64 = F64(math.log(f64.double))
inline def log10(f64: into F64): F64 = F64(math.log10(f64.double))
inline def log1p(f64: into F64): F64 = F64(math.log1p(f64.double))
