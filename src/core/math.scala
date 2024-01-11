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

import scala.annotation.*
import scala.compiletime.*

case class OverflowError() extends Error(msg"an overflow error occurred")

package arithmeticOptions:
  object division
  
  object overflow:
    inline given unchecked: CheckOverflow with
      type Wrap[ResultType] = ResultType
      inline def addU32(left: U32, right: U32): U32 = U32((left.int + right.int).bits)
      inline def addI32(left: I32, right: I32): I32 = I32((left.int + right.int).bits)

    inline given checked: CheckOverflow with
      type Wrap[ResultType] = ResultType raises OverflowError
      inline def addU32(left: U32, right: U32): U32 raises OverflowError =
        val result: U32 = U32((left.int + right.int).bits)
        
        if U32((left.bits^result.bits) & (right.bits^result.bits)) < U32(0.bits)
        then raise(OverflowError())(result) else result
      
      inline def addI32(left: I32, right: I32): I32 raises OverflowError =
        val result: I32 = I32((left.int + right.int).bits)
        if result < left || result < right then raise(OverflowError())(result) else result

trait CheckOverflow:
  type Wrap[ResultType]
  inline def addU32(left: U32, right: U32): Wrap[U32]
  inline def addI32(left: I32, right: I32): Wrap[I32]

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

  object Bits:
    def apply(): B64 = apply(0L)
    def apply(byte: Byte): B8 = byte
    def apply(short: Short): B16 = short
    def apply(int: Int): B32 = int
    def apply(long: Long): B64 = long
  
  extension (i32: I32)
    @targetName("plusI32")
    inline def +(right: I32)(using overflow: CheckOverflow): overflow.Wrap[I32] = overflow.addI32(i32, right)
    
    @targetName("ltI32")
    inline def <(right: I32): Boolean = i32 < right
    
    @targetName("intI32")
    inline def int: Int = i32
    
    @targetName("longI32")
    inline def long: Long = i32.toLong
    
    @targetName("absI32")
    inline def abs: I32 = math.abs(i32)
    
    @targetName("power")
    inline infix def **(exponent: Double): Double = math.pow(i32.toDouble, exponent)
    
    @targetName("octalI32")
    inline def octal: Text = java.lang.Integer.toOctalString(i32).nn.tt
    
    @targetName("hexI32")
    inline def hex: Text = java.lang.Integer.toHexString(i32).nn.tt
    
    @targetName("base32I32")
    inline def base32: Text = java.lang.Integer.toString(i32, 32).nn.tt
    
    @targetName("binaryI32")
    inline def binary: Text = java.lang.Integer.toBinaryString(i32).nn.tt
    
    @targetName("floorMod")
    inline def %%(right: I32): I32 = math.floorMod(i32, right)
    
    @targetName("floorDiv")
    inline def \(right: I32): I32 = math.floorDiv(i32, right)

  extension (i64: I64)
    inline def abs: I64 = math.abs(i64)
    inline def decrement: I64 = math.decrementExact(i64)
    inline def increment: I64 = math.incrementExact(i64)
    
    @targetName("octalI64")
    inline def octal: Text = java.lang.Long.toOctalString(i64).nn.tt
    
    @targetName("hexI64")
    inline def hex: Text = java.lang.Long.toHexString(i64).nn.tt
    
    @targetName("base32I64")
    inline def base32: Text = java.lang.Long.toString(i64, 32).nn.tt
    
    @targetName("binaryI64")
    inline def binary: Text = java.lang.Long.toBinaryString(i64).nn.tt
    
    @targetName("floorMod")
    inline def %%(right: I64): I64 = math.floorMod(i64, right)
    
    @targetName("floorDiv")
    inline def \(right: I64): I64 = math.floorDiv(i64, right)

    @targetName("power")
    inline infix def **(exponent: Double): Double = math.pow(i64.toDouble, exponent)

    def bytes: IArray[Byte] =
      var array: Array[Byte] = new Array[Byte](8)
      var index = 0
      
      while index < 8 do
        array(index) = (i64 >> (8*(7 - index))).toByte
        index += 1

      array.asInstanceOf[IArray[Byte]]

  object U64:
    inline def apply(bits: B64): U64 = bits
  
  object U32:
    inline def apply(bits: B32): U32 = bits
  
  object I32:
    inline def apply(bits: B32): I32 = bits

  extension (bitmap: B8)
    @targetName("rotateLeft")
    inline def <<<(count: Int): B8 = ((bitmap << count%%8) | (bitmap >>> (8 - count%%8))).toByte
    
    @targetName("rotateRight")
    inline def >>>(count: Int): B8 = ((bitmap >>> count%%8) | (bitmap << (8 - count%%8))).toByte
       
    @targetName("shiftLeft")
    inline def <<(count: Int): B8 = (bitmap << count).toByte
    
    @targetName("shiftRight")
    inline def >>(count: Int): B8 = (bitmap >>> count).toByte
    
    @targetName("and")
    inline def &(right: B8): B8 = (bitmap & right).toByte
    
    @targetName("or")
    transparent inline def |(right: B8): B8 = (bitmap | right).toByte
    
    @targetName("xor")
    transparent inline def ^(right: B8): B8 = (bitmap ^ right).toByte
    
    @targetName("invert")
    transparent inline def `unary_~`: B8 = (~bitmap).toByte
  
    inline def leadingZeros: Int = java.lang.Integer.numberOfLeadingZeros(bitmap.toInt) - 24
    inline def trailingZeros: Int = java.lang.Integer.numberOfTrailingZeros(bitmap.toInt)
    inline def reverse: B8 = (java.lang.Integer.reverse(bitmap.toInt) >>> 24).toByte

  extension (bitmap: B16)
    @targetName("rotateLeft")
    inline def <<<(count: Int): B16 = ((bitmap << count%%16) | (bitmap >>> (16 - count%%16))).toShort
    
    @targetName("rotateRight")
    inline def >>>(count: Int): B16 = ((bitmap >>> count%%16) | (bitmap << (16 - count%%16))).toShort
       
    @targetName("shiftLeft")
    inline def <<(count: Int): B16 = (bitmap << count).toShort
    
    @targetName("shiftRight")
    inline def >>(count: Int): B16 = (bitmap >>> count).toShort
    
    @targetName("and")
    inline def &(right: B16): B16 = (bitmap & right).toShort
    
    @targetName("or")
    transparent inline def |(right: B16): B16 = (bitmap | right).toShort
    
    @targetName("xor")
    transparent inline def ^(right: B16): B16 = (bitmap ^ right).toShort
    
    @targetName("invert")
    transparent inline def `unary_~`: B16 = (~bitmap).toShort
  
    inline def leadingZeros: Int = java.lang.Integer.numberOfLeadingZeros(bitmap.toInt) - 16
    inline def trailingZeros: Int = java.lang.Integer.numberOfTrailingZeros(bitmap.toInt)
    inline def reverse: B16 = (java.lang.Integer.reverse(bitmap.toInt) >>> 16).toShort

  extension (bitmap: B32)
    @targetName("rotateLeft")
    inline def <<<(count: Int): B32 = bitmap.rotateLeft(count%%32)
    
    @targetName("rotateRight")
    inline def >>>(count: Int): B32 = bitmap.rotateRight(count%%32)
       
    @targetName("shiftLeft")
    inline def <<(count: Int): B32 = bitmap << count
    
    @targetName("shiftRight")
    inline def >>(count: Int): B32 = bitmap >>> count
    
    @targetName("and")
    inline def &(right: B32): B32 = bitmap & right
    
    @targetName("or")
    transparent inline def |(right: B32): B32 = bitmap | right
    
    @targetName("xor")
    transparent inline def ^(right: B32): B32 = bitmap ^ right
    
    @targetName("invert")
    transparent inline def `unary_~`: B32 = ~bitmap
  
    inline def leadingZeros: Int = java.lang.Integer.numberOfLeadingZeros(bitmap)
    inline def trailingZeros: Int = java.lang.Integer.numberOfTrailingZeros(bitmap)
    inline def reverse: B32 = java.lang.Integer.reverse(bitmap)

  extension (bitmap: B64)
    @targetName("rotateLeft")
    inline def <<<(count: Int): B64 = bitmap.rotateLeft(count%%64)
    
    @targetName("rotateRight")
    inline def >>>(count: Int): B64 = bitmap.rotateRight(count%%64)
       
    @targetName("shiftLeft")
    inline def <<(count: Int): B64 = bitmap << count
    
    @targetName("shiftRight")
    inline def >>(count: Int): B64 = bitmap >>> count
    
    @targetName("and")
    inline def &(right: B64): B64 = bitmap & right
    
    @targetName("or")
    transparent inline def |(right: B64): B64 = bitmap | right
    
    @targetName("xor")
    transparent inline def ^(right: B64): B64 = bitmap ^ right
    
    @targetName("invert")
    transparent inline def `unary_~`: B64 = ~bitmap
  
    inline def leadingZeros: Int = java.lang.Long.numberOfLeadingZeros(bitmap)
    inline def trailingZeros: Int = java.lang.Long.numberOfTrailingZeros(bitmap)
    inline def reverse: B64 = java.lang.Long.reverse(bitmap)

  extension (u64: U64)

    inline def bits: B64 = u64

    @targetName("plus")
    inline def +(right: U64): U64 = u64 + right
    
    @targetName("minus")
    inline def -(right: U64): U64 = u64 - right
    
    @targetName("times")
    inline def *(right: U64): U64 = u64*right

    inline def text: Text = java.lang.Long.toUnsignedString(u64).nn.tt
    inline def base32: Text = java.lang.Long.toUnsignedString(u64, 32).nn.tt
    inline def hex: Text = java.lang.Long.toUnsignedString(u64, 16).nn.tt
    
    @targetName("octalU64")
    inline def octal: Text = java.lang.Long.toUnsignedString(u64, 8).nn.tt
    inline def binary: Text = java.lang.Long.toUnsignedString(u64, 2).nn.tt
    
    @targetName("lt")
    inline def <(right: U64): Boolean = java.lang.Long.compareUnsigned(u64, right) == -1
    
    @targetName("gt")
    inline def >(right: U64): Boolean = java.lang.Long.compareUnsigned(u64, right) == 1
    
    @targetName("gte")
    inline def >=(right: U64): Boolean = java.lang.Long.compareUnsigned(u64, right) != -1
    
    @targetName("lte")
    inline def <=(right: U64): Boolean = java.lang.Long.compareUnsigned(u64, right) != 1
    
    @targetName("div")
    inline def /(right: U64): U64 = java.lang.Long.divideUnsigned(u64, right)
    
    @targetName("mod")
    inline def %(right: U64): U64 = java.lang.Long.remainderUnsigned(u64, right)

  extension (u32: U32)
    inline def +(right: U32)(using overflow: CheckOverflow): overflow.Wrap[U32] = overflow.addU32(u32, right)
    
    inline def bits: B32 = u32
    
    @targetName("minus")
    inline def -(right: U32): U32 = u32 - right
    
    @targetName("times")
    inline def *(right: U32): U32 = u32*right

    inline def text: Text = java.lang.Integer.toUnsignedString(u32).nn.tt
    inline def base32: Text = java.lang.Integer.toUnsignedString(u32, 32).nn.tt
    inline def hex: Text = java.lang.Integer.toUnsignedString(u32, 16).nn.tt
    
    @targetName("octalU32")
    inline def octal: Text = java.lang.Integer.toUnsignedString(u32, 8).nn.tt
    inline def binary: Text = java.lang.Integer.toUnsignedString(u32, 2).nn.tt
    inline def long: Long = java.lang.Integer.toUnsignedLong(u32)
    inline def int: Int = u32
    
    @targetName("lt")
    inline def <(right: U32): Boolean = java.lang.Integer.compareUnsigned(u32, right) == -1
    
    @targetName("gt")
    inline def >(right: U32): Boolean = java.lang.Integer.compareUnsigned(u32, right) == 1
    
    @targetName("gte")
    inline def >=(right: U32): Boolean = java.lang.Integer.compareUnsigned(u32, right) != -1
    
    @targetName("lte")
    inline def <=(right: U32): Boolean = java.lang.Integer.compareUnsigned(u32, right) != 1
    
    @targetName("div")
    inline def /(right: U32): U32 = java.lang.Integer.divideUnsigned(u32, right)
    
    @targetName("mod")
    inline def %(right: U32): U32 = java.lang.Integer.remainderUnsigned(u32, right)

export Hypotenuse.{B8, B16, B32, B64, I8, I16, I32, I64, U8, U16, U32, U64}

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

extension (float: Float)
  inline def abs: Float = math.abs(float)
  inline def sqrt: Float = math.sqrt(float).toFloat
  inline def cbrt: Float = math.cbrt(float).toFloat
  inline def ceiling: Float = math.ceil(float).toFloat
  inline def floor: Float = math.floor(float).toFloat
  inline def exponent: Int = math.getExponent(float)
  inline def increment: Float = math.nextUp(float)
  inline def decrement: Float = math.nextDown(float)
  inline def round: Long = math.round(float)
  inline def scalb(scale: Int): Float = math.scalb(float, scale)
  inline def signum: -1.0F | 0.0F | 1.0F = math.signum(float).asInstanceOf[-1.0F | 0.0F | 1.0F]
  inline def ulp: Float = math.ulp(float)
  inline def bits: Long = java.lang.Float.floatToIntBits(float)
  inline def rawBits: Long = java.lang.Float.floatToRawIntBits(float)
  inline def finite: Boolean = float.isFinite
  inline def infinite: Boolean = float.isInfinite
  inline def nan: Boolean = float.isNaN
  
  @targetName("power")
  inline infix def **(exponent: Double): Float = math.pow(float, exponent).toFloat

extension (double: Double)
  inline def abs: Double = math.abs(double)
  inline def sqrt: Double = math.sqrt(double)
  inline def cbrt: Double = math.cbrt(double)
  inline def ceiling: Double = math.ceil(double)
  inline def floor: Double = math.floor(double)
  inline def exponent: Int = math.getExponent(double)
  inline def increment: Double = math.nextUp(double)
  inline def decrement: Double = math.nextDown(double)
  inline def round: Long = math.round(double)
  inline def scalb(scale: Int): Double = math.scalb(double, scale)
  inline def signum: -1.0 | 0.0 | 1.0 = math.signum(double).asInstanceOf[-1.0 | 0.0 | 1.0]
  inline def ulp: Double = math.ulp(double)
  inline def bits: Long = java.lang.Double.doubleToLongBits(double)
  inline def rawBits: Long = java.lang.Double.doubleToRawLongBits(double)
  inline def finite: Boolean = double.isFinite
  inline def infinite: Boolean = double.isInfinite
  inline def nan: Boolean = double.isNaN
  
  @targetName("power")
  inline infix def **(exponent: Double): Double = math.pow(double, exponent)

extension (int: Int)
  inline def bits: B32 = int.asInstanceOf[B32]
  inline def long: Long = int.toLong
  inline def abs: Int = math.abs(int)
  inline def decrement: Int = math.decrementExact(int)
  inline def increment: Int = math.incrementExact(int)
  
  @targetName("power")
  inline infix def **(exponent: Double): Double = math.pow(int.toDouble, exponent)

  inline def countBits: Int = java.lang.Long.bitCount(int)
  inline def highestBit: Int = java.lang.Integer.highestOneBit(int)
  inline def lowestBit: Int = java.lang.Integer.lowestOneBit(int)
  inline def leadingZeros: Int = java.lang.Integer.numberOfLeadingZeros(int)
  inline def trailingZeros: Int = java.lang.Integer.numberOfTrailingZeros(int)
  inline def reverseBits: Int = java.lang.Integer.reverse(int)
  inline def reverseBytes: Int = java.lang.Integer.reverseBytes(int)
  inline def rotateLeft(bits: Int): Int = java.lang.Integer.rotateLeft(int, bits)
  inline def rotateRight(bits: Int): Int = java.lang.Integer.rotateRight(int, bits)
  inline def octal: Text = java.lang.Integer.toOctalString(int).nn.tt
  inline def hex: Text = java.lang.Integer.toHexString(int).nn.tt
  inline def base32: Text = java.lang.Integer.toString(int, 32).nn.tt
  inline def binary: Text = java.lang.Integer.toBinaryString(int).nn.tt
  
  @targetName("floorMod")
  inline def %%(right: Int): Int = math.floorMod(int, right)
  
  @targetName("floorDiv")
  inline def \(right: Int): Int = math.floorDiv(int, right)

extension (long: Long)
  inline def abs: Long = math.abs(long)
  inline def decrement: Long = math.decrementExact(long)
  inline def increment: Long = math.incrementExact(long)
  inline def countBits: Int = java.lang.Long.bitCount(long)
  inline def highestBit: Long = java.lang.Long.highestOneBit(long)
  inline def lowestBit: Long = java.lang.Long.lowestOneBit(long)
  inline def leadingZeros: Int = java.lang.Long.numberOfLeadingZeros(long)
  inline def trailingZeros: Int = java.lang.Long.numberOfTrailingZeros(long)
  inline def reverseBits: Long = java.lang.Long.reverse(long)
  inline def reverseBytes: Long = java.lang.Long.reverseBytes(long)
  inline def rotateLeft(bits: Int): Long = java.lang.Long.rotateLeft(long, bits)
  inline def rotateRight(bits: Int): Long = java.lang.Long.rotateRight(long, bits)
  inline def octal: Text = java.lang.Long.toOctalString(long).nn.tt
  inline def hex: Text = java.lang.Long.toHexString(long).nn.tt
  inline def base32: Text = java.lang.Long.toString(long, 32).nn.tt
  inline def binary: Text = java.lang.Long.toBinaryString(long).nn.tt
  
  @targetName("floorMod")
  inline def %%(right: Long): Long = math.floorMod(long, right)
  
  @targetName("floorDiv")
  inline def \(right: Long): Long = math.floorDiv(long, right)

  @targetName("power")
  inline infix def **(exponent: Double): Double = math.pow(long.toDouble, exponent)

  def bytes: IArray[Byte] =
    var array: Array[Byte] = new Array[Byte](8)
    var index = 0
    
    while index < 8 do
      array(index) = (long >> (8*(7 - index))).toByte
      index += 1

    array.asInstanceOf[IArray[Byte]]

extension (doubleObject: Double.type)
  inline def apply(long: Long): Double = java.lang.Double.longBitsToDouble(long)


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