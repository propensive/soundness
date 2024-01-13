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
import perforate.*
import fulminate.*

import scala.util.FromDigits
import scala.annotation.*
import scala.compiletime.*
import scala.quoted.*

import language.experimental.genericNumberLiterals

import java.lang.{Integer as JInt, Long as JLong, Short as JShort, Byte as JByte, Double as JDouble,
    Float as JFloat}

case class OverflowError() extends Error(msg"an overflow error occurred")

package arithmeticOptions:
  object division
  
  object overflow:
    inline given unchecked: CheckOverflow with
      type Wrap[ResultType] = ResultType
      inline def addU64(left: U64, right: U64): U64 = U64((left.long + right.long).bits)
      inline def addI64(left: I64, right: I64): I64 = I64((left.long + right.long).bits)
      inline def addU32(left: U32, right: U32): U32 = U32((left.int + right.int).bits)
      inline def addI32(left: I32, right: I32): I32 = I32((left.int + right.int).bits)
      inline def addU16(left: U16, right: U16): U16 = U16((left.short + right.short).toShort.bits)
      inline def addI16(left: I16, right: I16): I16 = I16((left.short + right.short).toShort.bits)
      inline def addU8(left: U8, right: U8): U8 = U8((left.byte + right.byte).bits)
      inline def addI8(left: I8, right: I8): I8 = I8((left.byte + right.byte).bits)

    inline given checked: CheckOverflow with
      type Wrap[ResultType] = ResultType raises OverflowError
      
      inline def addU64(left: U64, right: U64): U64 raises OverflowError =
        val result: B64 = (left.long + right.long).bits
        
        if U64((left.bits^result) & (right.bits^result)) < U64(0.bits)
        then raise(OverflowError())(U64(result)) else U64(result)
      
      inline def addI64(left: I64, right: I64): I64 raises OverflowError =
        val result: I64 = I64((left.long + right.long).bits)
        if result < left || result < right then raise(OverflowError())(result) else result

      inline def addU32(left: U32, right: U32): U32 raises OverflowError =
        val result: B32 = (left.int + right.int).bits
        
        if U32((left.bits^result) & (right.bits^result)) < U32(0.bits)
        then raise(OverflowError())(U32(result)) else U32(result)
      
      inline def addI32(left: I32, right: I32): I32 raises OverflowError =
        val result: I32 = I32((left.int + right.int).bits)
        if result < left || result < right then raise(OverflowError())(result) else result

      inline def addU16(left: U16, right: U16): U16 raises OverflowError =
        val result: B16 = (left.short + right.short).toShort.bits
        
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
        val result: I8 = I8((left.short + right.short).bits)
        if result < left || result < right then raise(OverflowError())(result) else result

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
    inline def apply(sign: Boolean, exponent: B16, mantissa: B64): F64 =
      F64((if sign then Long.MinValue else 0L) | ((exponent & 0xffL) << 52) | (mantissa & 0xfffffffffffffL))
    
    inline def apply(bits: B64): F64 = JDouble.longBitsToDouble(bits)
    inline def apply(double: Double): F64 = double
  
  object F32:
    inline def apply(sign: Boolean, exponent: B16, mantissa: B32): F32 =
      val signBit = if sign then 0 else 1 << 31
      F32(if sign then Int.MinValue else 0 | ((exponent & 0xff) << 22) | (mantissa & 0x3fffff))
    
    inline def apply(bits: B32): F32 = JFloat.intBitsToFloat(bits)
    inline def apply(float: Float): F32 = float
  
  object U64:
    given fromDigits: FromDigits[U64] with
      inline def fromDigits(digits: String): U64 = ${Hypotenuse2.parseU64('digits)}
    
    given textualizer: Textualizer[U64] = JLong.toUnsignedString(_).nn.tt
    inline def apply(bits: B64): U64 = bits
  
  object I64:
    given fromDigits: FromDigits[I64] with
      inline def fromDigits(digits: String): I64 = ${Hypotenuse2.parseI64('digits)}

    given textualizer: Textualizer[I64] = _.toString.tt
    inline def apply(bits: B64): I64 = bits
  
  object U32:
    given fromDigits: FromDigits[U32] with
      inline def fromDigits(digits: String): U32 = ${Hypotenuse2.parseU32('digits)}

    given textualizer: Textualizer[U32] = JInt.toUnsignedString(_).nn.tt
    inline def apply(bits: B32): U32 = bits
  
  object I32:
    given fromDigits: FromDigits[I32] with
      inline def fromDigits(digits: String): I32 = ${Hypotenuse2.parseI32('digits)}
    
    given textualizer: Textualizer[I32] = _.toString.tt
    inline def apply(bits: B32): I32 = bits

  object U16:
    given fromDigits: FromDigits[U16] with
      inline def fromDigits(digits: String): U16 = ${Hypotenuse2.parseU16('digits)}
    
    given textualizer: Textualizer[U16] = u16 => JShort.toUnsignedInt(u16).toString.nn.tt
    inline def apply(bits: B16): U16 = bits
  
  object I16:
    given fromDigits: FromDigits[I16] with
      inline def fromDigits(digits: String): I16 = ${Hypotenuse2.parseI16('digits)}

    given textualizer: Textualizer[I16] = _.toString.tt
    inline def apply(bits: B16): I16 = bits

  object U8:
    given fromDigits: FromDigits[U8] with
      inline def fromDigits(digits: String): U8 = ${Hypotenuse2.parseU8('digits)}

    given textualizer: Textualizer[U8] = u8 => JByte.toUnsignedInt(u8).toString.nn.tt
    inline def apply(bits: B8): U8 = bits
  
  object I8:
    given fromDigits: FromDigits[I8] with
      inline def fromDigits(digits: String): I8 = ${Hypotenuse2.parseI8('digits)}

    given textualizer: Textualizer[I8] = _.toString.tt
    inline def apply(bits: B8): I8 = bits

  extension (i64: I64)
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
    inline infix def %% (right: I64): I64 = math.floorMod(i64, right)
    
    @targetName("floorDivI64")
    inline infix def \ (right: I64): I64 = math.floorDiv(i64, right)

    @targetName("powerI64")
    infix inline def ** (exponent: Double): Double = math.pow(i64.toDouble, exponent)

    @targetName("bytesI64")
    def bytes: IArray[Byte] =
      var array: Array[Byte] = new Array[Byte](8)
      var index = 0
      
      while index < 8 do
        array(index) = (i64 >> (8*(7 - index))).toByte
        index += 1

      array.asInstanceOf[IArray[Byte]]

    @targetName("ltI64")
    infix inline def < (right: I64): Boolean = i64 < right
    
    @targetName("gtI64")
    infix inline def > (right: I64): Boolean = i64 > right
    
    @targetName("gteI64")
    inline def >= (right: I64): Boolean = i64 >= right
    
    @targetName("lteI64")
    inline def <= (right: I64): Boolean = i64 <= right
    
    @targetName("divI64")
    infix inline def / (right: I64): I64 = i64/right
    
    @targetName("modI64")
    infix inline def % (right: I64): I64 = i64%right

  extension (i32: I32)
    @targetName("plusI32")
    inline infix def + (right: I32)(using overflow: CheckOverflow): overflow.Wrap[I32] =
      overflow.addI32(i32, right)
    
    @targetName("intI32")
    inline def int: Int = i32
    
    @targetName("longI32")
    inline def long: Long = i32.toLong
    
    @targetName("absI32")
    inline def abs: I32 = math.abs(i32)
    
    @targetName("powerI32")
    infix inline def ** (exponent: Double): Double = math.pow(i32.toDouble, exponent)
    
    @targetName("octalI32")
    inline def octal: Text = JInt.toOctalString(i32).nn.tt
    
    @targetName("hexI32")
    inline def hex: Text = JInt.toHexString(i32).nn.tt
    
    @targetName("base32I32")
    inline def base32: Text = JInt.toString(i32, 32).nn.tt
    
    @targetName("binaryI32")
    inline def binary: Text = JInt.toBinaryString(i32).nn.tt
    
    @targetName("floorModI32")
    inline infix def %% (right: I32): I32 = math.floorMod(i32, right)
    
    @targetName("floorDivI32")
    inline def \ (right: I32): I32 = math.floorDiv(i32, right)
    
    @targetName("ltI32")
    infix inline def < (right: I32): Boolean = i32 < right
    
    @targetName("gtI32")
    infix inline def > (right: I32): Boolean = i32 > right
    
    @targetName("gteI32")
    inline def >= (right: I32): Boolean = i32 >= right
    
    @targetName("lteI32")
    inline def <= (right: I32): Boolean = i32 <= right
    
    @targetName("divI32")
    infix inline def / (right: I32): I32 = i32/right
    
    @targetName("modI32")
    infix inline def % (right: I32): I32 = i32%right

  extension (i16: I16)
    @targetName("plusI16")
    inline infix def + (right: I16)(using overflow: CheckOverflow): overflow.Wrap[I16] =
      overflow.addI16(i16, right)
    
    @targetName("shortI16")
    inline def short: Int = i16
    
    @targetName("intI16")
    inline def int: Int = i16.toInt
    
    @targetName("longI16")
    inline def long: Long = i16.toLong
    
    @targetName("absI16")
    inline def abs: I16 = math.abs(i16).toShort
    
    @targetName("powerI16")
    infix inline def ** (exponent: Double): Double = math.pow(i16.toDouble, exponent)
    
    @targetName("octalI16")
    inline def octal: Text = JInt.toOctalString(i16).nn.tt
    
    @targetName("hexI16")
    inline def hex: Text = JInt.toHexString(i16).nn.tt
    
    @targetName("base32I16")
    inline def base32: Text = JInt.toString(i16, 32).nn.tt
    
    @targetName("binaryI16")
    inline def binary: Text = JInt.toBinaryString(i16).nn.tt
    
    @targetName("floorModI16")
    inline infix def %% (right: I16): I16 = math.floorMod(i16, right).toShort
    
    @targetName("floorDivI16")
    inline def \ (right: I16): I16 = math.floorDiv(i16, right).toShort
    
    @targetName("ltI16")
    infix inline def < (right: I16): Boolean = i16 < right
    
    @targetName("gtI16")
    infix inline def > (right: I16): Boolean = i16 > right
    
    @targetName("gteI16")
    inline def >= (right: I16): Boolean = i16 >= right
    
    @targetName("lteI16")
    inline def <= (right: I16): Boolean = i16 <= right
    
    @targetName("divI16")
    infix inline def / (right: I16): I16 = (i16/right).toShort
    
    @targetName("modI16")
    infix inline def % (right: I16): I16 = (i16%right).toShort

  extension (i8: I8)
    @targetName("plusI8")
    inline infix def + (right: I8)(using overflow: CheckOverflow): overflow.Wrap[I8] =
      overflow.addI8(i8, right)
    
    @targetName("byteI8")
    inline def byte: Int = i8
    
    @targetName("shortI8")
    inline def short: Int = i8.toShort
    
    @targetName("intI8")
    inline def int: Int = i8.toInt
    
    @targetName("longI8")
    inline def long: Long = i8.toLong
    
    @targetName("absI8")
    inline def abs: I8 = math.abs(i8).toByte
    
    @targetName("powerI8")
    infix inline def ** (exponent: Double): Double = math.pow(i8.toDouble, exponent)
    
    @targetName("octalI8")
    inline def octal: Text = JInt.toOctalString(i8).nn.tt
    
    @targetName("hexI8")
    inline def hex: Text = JInt.toHexString(i8).nn.tt
    
    @targetName("base32I8")
    inline def base32: Text = JInt.toString(i8, 32).nn.tt
    
    @targetName("binaryI8")
    inline def binary: Text = JInt.toBinaryString(i8).nn.tt
    
    @targetName("floorModI8")
    inline infix def %% (right: I8): I8 = math.floorMod(i8, right).toByte
    
    @targetName("floorDivI8")
    inline def \ (right: I8): I8 = math.floorDiv(i8, right).toByte
    
    @targetName("ltI8")
    infix inline def < (right: I8): Boolean = i8 < right
    
    @targetName("gtI8")
    infix inline def > (right: I8): Boolean = i8 > right
    
    @targetName("gteI8")
    inline def >= (right: I8): Boolean = i8 >= right
    
    @targetName("lteI8")
    inline def <= (right: I8): Boolean = i8 <= right
    
    @targetName("divI8")
    infix inline def / (right: I8): I8 = (i8/right).toByte
    
    @targetName("modI8")
    infix inline def % (right: I8): I8 = (i8%right).toByte


  extension (bitmap: B8)
    @targetName("rotateLeftB8")
    inline def <<<(count: Int): B8 = ((bitmap << count%%8) | (bitmap >>> (8 - count%%8))).toByte
    
    @targetName("rotateRightB8")
    inline def >>>(count: Int): B8 = ((bitmap >>> count%%8) | (bitmap << (8 - count%%8))).toByte
       
    @targetName("shiftLeftB8")
    inline def <<(count: Int): B8 = (bitmap << count).toByte
    
    @targetName("shiftRightB8")
    inline def >>(count: Int): B8 = (bitmap >>> count).toByte
    
    @targetName("andB8")
    infix inline def & (right: B8): B8 = (bitmap & right).toByte
    
    @targetName("orB8")
    infix transparent inline def | (right: B8): B8 = (bitmap | right).toByte
    
    @targetName("xorB8")
    infix transparent inline def ^ (right: B8): B8 = (bitmap ^ right).toByte
    
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

  extension (bitmap: B16)
    @targetName("rotateLeftB16")
    inline def <<<(count: Int): B16 = ((bitmap << count%%16) | (bitmap >>> (16 - count%%16))).toShort
    
    @targetName("rotateRightB16")
    inline def >>>(count: Int): B16 = ((bitmap >>> count%%16) | (bitmap << (16 - count%%16))).toShort
       
    @targetName("shiftLeftB16")
    inline def <<(count: Int): B16 = (bitmap << count).toShort
    
    @targetName("shiftRightB16")
    inline def >>(count: Int): B16 = (bitmap >>> count).toShort
    
    @targetName("andB16")
    infix inline def & (right: B16): B16 = (bitmap & right).toShort
    
    @targetName("orB16")
    infix transparent inline def | (right: B16): B16 = (bitmap | right).toShort
    
    @targetName("xorB16")
    infix transparent inline def ^ (right: B16): B16 = (bitmap ^ right).toShort
    
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

  extension (bitmap: B32)
    @targetName("rotateLeftB32")
    inline def <<<(count: Int): B32 = JInt.rotateLeft(bitmap, count%%32)
    
    @targetName("rotateRightB32")
    inline def >>>(count: Int): B32 = JInt.rotateRight(bitmap, count%%32)
       
    @targetName("shiftLeftB32")
    inline def <<(count: Int): B32 = bitmap << count
    
    @targetName("shiftRightB32")
    inline def >>(count: Int): B32 = bitmap >>> count
    
    @targetName("andB32")
    infix inline def & (right: B32): B32 = bitmap & right
    
    @targetName("orB32")
    infix transparent inline def | (right: B32): B32 = bitmap | right
    
    @targetName("xorB32")
    infix transparent inline def ^ (right: B32): B32 = bitmap ^ right
    
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


  extension (bitmap: B64)
    @targetName("rotateLeftB64")
    inline def <<<(count: Int): B64 = JLong.rotateLeft(bitmap, count%%64)
    
    @targetName("rotateRightB64")
    inline def >>>(count: Int): B64 = JLong.rotateRight(bitmap, count%%64)
       
    @targetName("shiftLeftB64")
    inline def <<(count: Int): B64 = bitmap << count
    
    @targetName("shiftRightB64")
    inline def >>(count: Int): B64 = bitmap >>> count
    
    @targetName("andB64")
    infix inline def & (right: B64): B64 = bitmap & right
    
    @targetName("orB64")
    infix transparent inline def | (right: B64): B64 = bitmap | right
    
    @targetName("xorB64")
    infix transparent inline def ^ (right: B64): B64 = bitmap ^ right
    
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

  extension (u64: U64)

    @targetName("bitsU64")
    inline def bits: B64 = u64

    @targetName("plusU64")
    infix inline def + (right: U64): U64 = u64 + right
    
    @targetName("minusU64")
    infix inline def - (right: U64): U64 = u64 - right
    
    @targetName("timesU64")
    infix inline def * (right: U64): U64 = u64*right

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
    
    @targetName("ltU64")
    infix inline def < (right: U64): Boolean = JLong.compareUnsigned(u64, right) == -1
    
    @targetName("gtU64")
    infix inline def > (right: U64): Boolean = JLong.compareUnsigned(u64, right) == 1
    
    @targetName("gteU64")
    inline def >= (right: U64): Boolean = JLong.compareUnsigned(u64, right) != -1
    
    @targetName("lteU64")
    inline def <= (right: U64): Boolean = JLong.compareUnsigned(u64, right) != 1
    
    @targetName("divU64")
    infix inline def / (right: U64): U64 = JLong.divideUnsigned(u64, right)
    
    @targetName("modU64")
    infix inline def % (right: U64): U64 = JLong.remainderUnsigned(u64, right)

    @targetName("longU64")
    inline def long: Long = u64

  extension (u32: U32)
    @targetName("plusU32")
    infix inline def + (right: U32)(using overflow: CheckOverflow): overflow.Wrap[U32] = overflow.addU32(u32, right)
    
    @targetName("bitsU32")
    inline def bits: B32 = u32
    
    @targetName("minuseU32")
    infix inline def - (right: U32): U32 = u32 - right
    
    @targetName("timesU32")
    infix inline def * (right: U32): U32 = u32*right

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
    
    @targetName("intU32")
    inline def int: Int = u32
    
    @targetName("ltU32")
    infix inline def < (right: U32): Boolean = JInt.compareUnsigned(u32, right) == -1
    
    @targetName("gtU32")
    infix inline def > (right: U32): Boolean = JInt.compareUnsigned(u32, right) == 1
    
    @targetName("gteU32")
    inline def >= (right: U32): Boolean = JInt.compareUnsigned(u32, right) != -1
    
    @targetName("lteU32")
    inline def <= (right: U32): Boolean = JInt.compareUnsigned(u32, right) != 1
    
    @targetName("divU32")
    infix inline def / (right: U32): U32 = JInt.divideUnsigned(u32, right)
    
    @targetName("modU32")
    infix inline def % (right: U32): U32 = JInt.remainderUnsigned(u32, right)

  extension (u16: U16)
    @targetName("plusU16")
    infix inline def + (right: U16)(using overflow: CheckOverflow): overflow.Wrap[U16] = overflow.addU16(u16, right)
    
    @targetName("bitsU16")
    inline def bits: B16 = u16
    
    @targetName("minuseU16")
    infix inline def - (right: U16): U16 = (u16 - right).toShort
    
    @targetName("timesU16")
    infix inline def * (right: U16): U16 = (u16*right).toShort

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
    
    @targetName("ltU16")
    infix inline def < (right: U16): Boolean = JShort.compareUnsigned(u16, right) == -1
    
    @targetName("gtU16")
    infix inline def > (right: U16): Boolean = JShort.compareUnsigned(u16, right) == 1
    
    @targetName("gteU16")
    inline def >= (right: U16): Boolean = JShort.compareUnsigned(u16, right) != -1
    
    @targetName("lteU16")
    inline def <= (right: U16): Boolean = JShort.compareUnsigned(u16, right) != 1
    
    @targetName("divU16")
    infix inline def / (right: U16): U16 = JInt.divideUnsigned(u16, right).toShort
    
    @targetName("modU16")
    infix inline def % (right: U16): U16 = JInt.remainderUnsigned(u16, right).toShort

    @targetName("shortU16")
    inline def short: Short = u16

  extension (u8: U8)
    @targetName("plusU8")
    infix inline def + (right: U8)(using overflow: CheckOverflow): overflow.Wrap[U8] = overflow.addU8(u8, right)
    
    @targetName("bitsU8")
    inline def bits: B8 = u8
    
    @targetName("minusU8")
    infix inline def - (right: U8): U8 = (u8 - right).toByte
    
    @targetName("timesU8")
    infix inline def * (right: U8): U8 = (u8*right).toByte

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
    
    @targetName("ltU8")
    infix inline def < (right: U8): Boolean = JShort.compareUnsigned(u8, right) == -1
    
    @targetName("gtU8")
    infix inline def > (right: U8): Boolean = JShort.compareUnsigned(u8, right) == 1
    
    @targetName("gteU8")
    inline def >= (right: U8): Boolean = JByte.compareUnsigned(u8, right) != -1
    
    @targetName("lteU8")
    inline def <= (right: U8): Boolean = JByte.compareUnsigned(u8, right) != 1
    
    @targetName("divU8")
    infix inline def / (right: U8): U8 = JInt.divideUnsigned(u8, right).toByte
    
    @targetName("modU8")
    infix inline def % (right: U8): U8 = JInt.remainderUnsigned(u8, right).toByte

    @targetName("byteU8")
    inline def byte: Byte = u8

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
  infix inline def ** (exponent: Double): Float = math.pow(float, exponent).toFloat

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
  infix inline def ** (exponent: Double): Double = math.pow(double, exponent)

extension (byte: Int)
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
  infix inline def %% (right: Int): Int = math.floorMod(byte, right)
  
  @targetName("floorDivByte")
  infix inline def \ (right: Int): Int = math.floorDiv(byte, right)

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
  infix inline def %% (right: Short): Short = math.floorMod(short, right).toShort
  
  @targetName("floorDivShort")
  infix inline def \ (right: Short): Short = math.floorDiv(short, right).toShort

extension (int: Int)
  @targetName("bitsInt")
  inline def bits: B32 = int.asInstanceOf[B32]
  
  @targetName("longInt")
  inline def long: Long = int.toLong
  
  @targetName("absInt")
  inline def abs: Int = math.abs(int)
  
  @targetName("powerInt")
  inline infix def **(exponent: Double): Double = math.pow(int.toDouble, exponent)
  
  @targetName("octalInt")
  inline def octal: Text = JInt.toOctalString(int).nn.tt
  
  @targetName("hexInt")
  inline def hex: Text = JInt.toHexString(int).nn.tt
  
  @targetName("base32Int")
  inline def base32: Text = JInt.toString(int, 32).nn.tt
  
  @targetName("binaryInt")
  inline def binary: Text = JInt.toBinaryString(int).nn.tt
  
  @targetName("floorModInt")
  infix inline def %% (right: Int): Int = math.floorMod(int, right)
  
  @targetName("floorDivInt")
  infix inline def \ (right: Int): Int = math.floorDiv(int, right)

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
  infix inline def %% (right: Long): Long = math.floorMod(long, right)
  
  @targetName("floorDivLong")
  infix inline def \ (right: Long): Long = math.floorDiv(long, right)

  @targetName("powerLong")
  infix inline def ** (exponent: Double): Double = math.pow(long.toDouble, exponent)

  @targetName("bytesLong")
  def bytes: IArray[Byte] =
    var array: Array[Byte] = new Array[Byte](8)
    var index = 0
    
    while index < 8 do
      array(index) = (long >> (8*(7 - index))).toByte
      index += 1

    array.asInstanceOf[IArray[Byte]]

extension (doubleObject: Double.type)
  inline def apply(long: Long): Double = JDouble.longBitsToDouble(long)


extension (intObject: Int.type)
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

inline def cos(double: Double): Double = math.cos(double)
inline def acos(double: Double): Double = math.acos(double)
inline def cosh(double: Double): Double = math.cosh(double)
inline def sin(double: Double): Double = math.sin(double)
inline def asin(double: Double): Double = math.asin(double)
inline def sinh(double: Double): Double = math.sinh(double)
inline def tan(double: Double): Double = math.tan(double)
inline def atan(double: Double): Double = math.atan(double)
inline def hypot(first: Double, second: Double): Double = math.hypot(first, second)

inline def exp(double: Double): Double = math.exp(double)
inline def expm1(double: Double): Double = math.expm1(double)
inline def log(double: Double): Double = math.log(double)
inline def log10(double: Double): Double = math.log10(double)
inline def log1p(double: Double): Double = math.log1p(double)
