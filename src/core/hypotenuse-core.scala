/*
    Hypotenuse, version 0.24.0. Copyright 2025 Jon Pretty, Propensive OÜ.

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

import language.experimental.genericNumberLiterals
import language.experimental.into

import java.lang.{Integer as JInt, Long as JLong, Double as JDouble, Float as JFloat}

import scala.annotation.*

import anticipation.*
import contingency.*
import prepositional.*

export Hypotenuse.{B8, B16, B32, B64, S8, S16, S32, S64, U8, U16, U32, U64, F32, F64}

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
  inline infix def ** (exponent: Double): Double = math.pow(byte.toDouble, exponent)

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

  @tailrec @targetName("gcdByte")
  def gcd(right: Byte): Byte = if right == 0 then byte else right.gcd((byte%right).toByte)

  @targetName("lcmByte")
  def lcm(right: Byte): Byte = (byte*right/byte.gcd(right)).toByte

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

  @tailrec @targetName("gcdShort")
  def gcd(right: Short): Short = if right == 0 then short else right.gcd((short%right).toShort)

  @targetName("lcmShort")
  def lcm(right: Short): Short = (short*right/short.gcd(right)).toShort

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

  @tailrec @targetName("gcdInt")
  def gcd(right: Int): Int = if right == 0 then int else right.gcd(int%right)

  @targetName("lcmInt")
  def lcm(right: Int): Int = int*right/int.gcd(right)

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

  @tailrec @targetName("gcdLong")
  def gcd(right: Long): Long = if right == 0 then long else right.gcd(long%right)

  @targetName("lcmLong")
  def lcm(right: Long): Long = long*right/long.gcd(right)

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
final val euler = math.E
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

extension [LeftType](inline left: LeftType)
  @targetName("lt")
  inline infix def < [RightType](inline right: RightType)
     (using inline commensurable: LeftType is Commensurable by RightType)
          : Boolean =

    commensurable.compare(left, right, true, false)

  @targetName("lte")
  inline infix def <= [RightType](inline right: RightType)
     (using inline commensurable: LeftType is Commensurable by RightType)
          : Boolean =

    commensurable.compare(left, right, false, false)

  @targetName("gt")
  inline infix def > [RightType](inline right: RightType)
     (using inline commensurable: LeftType is Commensurable by RightType)
          : Boolean =

    commensurable.compare(left, right, true, true)

  @targetName("gte")
  inline infix def >= [RightType](inline right: RightType)
    (using inline commensurable: LeftType is Commensurable by RightType)
          : Boolean =

    commensurable.compare(left, right, false, true)

  inline infix def min (inline right: LeftType)(using LeftType is Orderable): LeftType =
    if left < right then left else right

  inline infix def max (inline right: LeftType)(using LeftType is Orderable): LeftType =
    if left >= right then left else right

package arithmeticOptions:
  object division:
    inline given DivisionByZero as unchecked:
      type Wrap[ResultType] = ResultType
      inline def divideU64(left: U64, right: U64): U64 = U64((Long(left.bits)/Long(right.bits)).bits)
      inline def divideS64(left: S64, right: S64): S64 = S64((left.long/right.long).bits)
      inline def divideU32(left: U32, right: U32): U32 = U32((Int(left.bits)/Int(right.bits)).bits)
      inline def divideS32(left: S32, right: S32): S32 = S32((left.int/right.int).bits)
      inline def divideU16(left: U16, right: U16): U16 = U16((Short(left.bits)/Short(right.bits)).toShort.bits)
      inline def divideS16(left: S16, right: S16): S16 = S16((left.short/right.short).toShort.bits)
      inline def divideU8(left: U8, right: U8): U8 = U8((left.byte/right.byte).toByte.bits)
      inline def divideS8(left: S8, right: S8): S8 = S8((left.byte/right.byte).toByte.bits)

    inline given DivisionByZero as checked:
      type Wrap[ResultType] = ResultType raises DivisionError

      inline def divideU64(left: U64, right: U64): U64 raises DivisionError =
        if Long(right.bits) == 0 then raise(DivisionError(), U64(0.bits))
        else U64((Long(left.bits)/Long(right.bits)).bits)

      inline def divideS64(left: S64, right: S64): S64 raises DivisionError =
        if right.long == 0 then raise(DivisionError(), S64(0.bits)) else S64((left.long/right.long).bits)

      inline def divideU32(left: U32, right: U32): U32 raises DivisionError =
        if right.long == 0 then raise(DivisionError(), U32(0.bits))
        else U32((Int(left.bits)/Int(right.bits)).bits)

      inline def divideS32(left: S32, right: S32): S32 raises DivisionError =
        if right.int == 0 then raise(DivisionError(), S32(0.bits)) else S32((left.int/right.int).bits)

      inline def divideU16(left: U16, right: U16): U16 raises DivisionError =
        if right.int == 0 then raise(DivisionError(), U16(0.bits))
        else U16((Short(left.bits)/Short(right.bits)).toShort.bits)

      inline def divideS16(left: S16, right: S16): S16 raises DivisionError =
        if right.int == 0 then raise(DivisionError(), S16(0.bits))
        else S16((left.short/right.short).toShort.bits)

      inline def divideU8(left: U8, right: U8): U8 raises DivisionError =
        if right.int == 0 then raise(DivisionError(), U8(0.bits)) else U8((left.byte/right.byte).toByte.bits)

      inline def divideS8(left: S8, right: S8): S8 raises DivisionError =
        if right.int == 0 then raise(DivisionError(), S8(0.bits)) else S8((left.byte/right.byte).toByte.bits)

  object overflow:
    inline given CheckOverflow as unchecked:
      type Wrap[ResultType] = ResultType
      inline def addU64(left: U64, right: U64): U64 = U64((Long(left.bits) + Long(right.bits)).bits)
      inline def addS64(left: S64, right: S64): S64 = S64((left.long + right.long).bits)
      inline def addU32(left: U32, right: U32): U32 = U32((Int(left.bits) + Int(right.bits)).bits)
      inline def addS32(left: S32, right: S32): S32 = S32((left.int + right.int).bits)
      inline def addU16(left: U16, right: U16): U16 = U16((Short(left.bits) + Short(right.bits)).toShort.bits)
      inline def addS16(left: S16, right: S16): S16 = S16((left.short + right.short).toShort.bits)
      inline def addU8(left: U8, right: U8): U8 = U8((left.byte + right.byte).toByte.bits)
      inline def addS8(left: S8, right: S8): S8 = S8((left.byte + right.byte).toByte.bits)

    inline given CheckOverflow as checked:
      type Wrap[ResultType] = ResultType raises OverflowError

      inline def addU64(left: U64, right: U64): U64 raises OverflowError =
        val result: B64 = (Long(left.bits) + Long(right.bits)).bits

        if U64((left.bits^result) & (right.bits^result)) < U64(0.bits)
        then raise(OverflowError(), U64(result)) else U64(result)

      inline def addS64(left: S64, right: S64): S64 raises OverflowError =
        val result: S64 = S64((left.long + right.long).bits)
        if result < left || result < right then raise(OverflowError(), result) else result

      inline def addU32(left: U32, right: U32): U32 raises OverflowError =
        val result: B32 = (Int(left.bits) + Int(right.bits)).bits

        if U32((left.bits^result) & (right.bits^result)) < U32(0.bits)
        then raise(OverflowError(), U32(result)) else U32(result)

      inline def addS32(left: S32, right: S32): S32 raises OverflowError =
        val result: S32 = S32((left.int + right.int).bits)
        if result < left || result < right then raise(OverflowError(), result) else result

      inline def addU16(left: U16, right: U16): U16 raises OverflowError =
        val result: B16 = (Short(left.bits) + Short(right.bits)).toShort.bits

        if U16((left.bits^result) & (right.bits^result)) < U16(0.toShort.bits)
        then U16(raise(OverflowError(), result)) else U16(result)

      inline def addS16(left: S16, right: S16): S16 raises OverflowError =
        val result: S16 = S16((left.short + right.short).toShort.bits)
        if result < left || result < right then raise(OverflowError(), result) else result

      inline def addU8(left: U8, right: U8): U8 raises OverflowError =
        val result: B8 = (left.short + right.short).toByte.bits

        if U8((left.bits^result) & (right.bits^result)) < U8(0.toByte.bits)
        then U8(raise(OverflowError(), result)) else U8(result)

      inline def addS8(left: S8, right: S8): S8 raises OverflowError =
        val result: S8 = S8((left.short + right.short).toByte.bits)
        if result < left || result < right then raise(OverflowError(), result) else result
