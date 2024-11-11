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

import language.experimental.genericNumberLiterals
import language.experimental.into

import java.lang.{Integer as JInt, Long as JLong, Short as JShort, Byte as JByte, Double as JDouble,
    Float as JFloat}

import scala.util.FromDigits
import scala.annotation.*
import scala.compiletime.*

import anticipation.*
import fulminate.*
import denominative.*

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

  opaque type S64 = Long
  opaque type S32 = Int
  opaque type S16 = Short
  opaque type S8  = Byte

  opaque type F64 = Double
  opaque type F32 = Float

  object F64:
    erased given Underlying[F64, Double] as underlying = erasedValue

    inline given CanEqual[F64, F64 | S64 | S32 | S16 | S8 | Double | Long | Int | Short | Byte] as canEqual =
      erasedValue

    inline def apply(inline sign: Boolean, inline exponent: B16, inline mantissa: B64): F64 =
      F64((if sign then Long.MinValue else 0L) | ((exponent & 0xffL) << 52) | (mantissa & 0xfffffffffffffL))

    inline def apply(inline bits: B64): F64 = JDouble.longBitsToDouble(bits)
    inline def apply(inline double: Double): F64 = double

    inline given F64 is Orderable as orderable:

      inline def compare
         (inline left: F64, inline right: F64, inline strict: Boolean, inline greater: Boolean)
              : Boolean =

        inline if greater
        then inline if strict then left > right else left >= right
        else inline if strict then left < right else left <= right

    inline given F64 is Commensurable as commensurableInt:
      type Operand = Int

      inline def compare
         (inline left: F64, inline right: Int, inline strict: Boolean, inline greaterThan: Boolean)
              : Boolean =

        inline if greaterThan
        then inline if strict then left > right else left >= right
        else inline if strict then left < right else left <= right

    inline given F64 is Commensurable as commensurable:
      type Operand = Double

      inline def compare(inline left: F64, inline right: Double, inline strict: Boolean,
          inline greaterThan: Boolean): Boolean =

        inline if greaterThan
        then inline if strict then left > right else left >= right
        else inline if strict then left < right else left <= right

    inline given Conversion[Double, F64] as doubleConversion:
      inline def apply(value: Double): F64 = value

    inline given Conversion[Float, F64] as floatConversion:
      def apply(value: Float): F64 = value.toDouble

    inline given Conversion[Int, F64] as intConversion:
      def apply(value: Int): F64 = value.toDouble

    inline given Conversion[Short, F64] as shortConversion:
      def apply(value: Short): F64 = value.toDouble

    inline given Conversion[Byte, F64] as byteConversion:
      def apply(value: Byte): F64 = value.toDouble

    inline given Conversion[U32, F64] as u32Conversion:
      def apply(value: U32): F64 = JInt.toUnsignedLong(value).toDouble

    inline given Conversion[S32, F64] as s32Conversion:
      def apply(value: S32): F64 = value.toDouble

    inline given Conversion[U16, F64] as u16Conversion:
      def apply(value: U16): F64 = JShort.toUnsignedInt(value).toDouble

    inline given Conversion[U8, F64] as u8Conversion:
      def apply(value: U8): F64 = JShort.toUnsignedInt(value).toDouble

    inline given Conversion[S16, F64] as s16Conversion:
      def apply(value: S16): F64 = value.toDouble

    inline given Conversion[S8, F64] as s8Conversion:
      def apply(value: S8): F64 = value.toDouble

  object F32:
    erased given Underlying[F32, Float] as underlying = erasedValue

    inline given CanEqual[F32, F32 | S64 | S32 | S16 | S8 | Float | Long | Int | Short | Byte] as canEqual =
      erasedValue

    inline given F32 is Orderable as orderable:

      inline def compare
         (inline left: F32, inline right: F32, inline strict: Boolean, inline greaterThan: Boolean)
              : Boolean =

        inline if greaterThan
        then inline if strict then left > right else left >= right
        else inline if strict then left < right else left <= right

    inline def apply(inline sign: Boolean, inline exponent: B16, inline mantissa: B32): F32 =
      val signBit = if sign then 0 else 1 << 31
      F32(if sign then Int.MinValue else 0 | ((exponent & 0xff) << 22) | (mantissa & 0x3fffff))

    inline def apply(inline bits: B32): F32 = JFloat.intBitsToFloat(bits)
    inline def apply(inline float: Float): F32 = float

    inline given Conversion[Float, F32] as floatConversion:
      def apply(value: Float): F32 = value

    inline given Conversion[Short, F32] as shortConversion:
      def apply(value: Short): F32 = value.toFloat

    inline given Conversion[Byte, F32] as byteConversion:
      def apply(value: Byte): F32 = value.toFloat

    inline given Conversion[U16, F32] as u16Conversion:
      def apply(value: U16): F32 = JShort.toUnsignedInt(value).toFloat

    inline given Conversion[U8, F32] as u8Conversion:
      def apply(value: U8): F32 = JShort.toUnsignedInt(value).toFloat

    inline given Conversion[S16, F32] as s16Conversion:
      def apply(value: S16): F32 = value.toFloat

    inline given Conversion[S8, F32] as s8Conversion:
      def apply(value: S8): F32 = value.toFloat

  object U64:
    erased given Underlying[U64, Long] as underlying = erasedValue
    inline given CanEqual[U64, U64] as canEqual = erasedValue

    given FromDigits[U64] as fromDigits:
      inline def fromDigits(digits: String): U64 = ${Hypotenuse2.parseU64('digits)}

    given U64 is Textualizer = JLong.toUnsignedString(_).nn.tt
    inline def apply(inline bits: B64): U64 = bits

    inline given U64 is Orderable as orderable:

      inline def compare
         (inline left: U64, inline right: U64, inline strict: Boolean, inline greaterThan: Boolean)
              : Boolean =

        inline if greaterThan then
          inline if strict then JLong.compareUnsigned(left, right) == 1
          else JLong.compareUnsigned(left, right) != -1
        else
          inline if strict then JLong.compareUnsigned(left, right) == -1
          else JLong.compareUnsigned(left, right) != 1

  object S64:
    erased given Underlying[S64, Long] as underlying = erasedValue
    inline given CanEqual[S64, F64 | F32 | S64 | S32 | S16 | S8 | Float | Double | Long | Int | Short | Byte] as canEqual =
      erasedValue

    given FromDigits[S64] as fromDigits:
      inline def fromDigits(digits: String): S64 = ${Hypotenuse2.parseS64('digits)}

    given S64 is Textualizer = _.toString.tt
    inline def apply(inline bits: B64): S64 = bits

    inline given S64 is Orderable as orderable:

      inline def compare
         (inline left: S64, inline right: S64, inline strict: Boolean, inline greaterThan: Boolean)
              : Boolean =

        inline if greaterThan
        then inline if strict then left > right else left >= right
        else inline if strict then left < right else left <= right


  object U32:
    erased given Underlying[U32, Int] as underlying = erasedValue
    inline given CanEqual[U32, U32] as canEqual = erasedValue

    given FromDigits[U32] as fromDigits:
      inline def fromDigits(digits: String): U32 = ${Hypotenuse2.parseU32('digits)}

    given U32 is Textualizer = JInt.toUnsignedString(_).nn.tt
    inline def apply(inline bits: B32): U32 = bits

    inline given U32 is Orderable as orderable:
      inline def compare
         (inline left: U32, inline right: U32, inline strict: Boolean, inline greaterThan: Boolean)
              : Boolean =

        inline if greaterThan then
          inline if strict then JLong.compareUnsigned(left, right) == 1
          else JInt.compareUnsigned(left, right) != -1
        else
          inline if strict then JLong.compareUnsigned(left, right) == -1
          else JInt.compareUnsigned(left, right) != 1

  object S32:
    erased given Underlying[S32, Int] as underlying = erasedValue
    inline given CanEqual[S32, F64 | F32 | S64 | S32 | S16 | S8 | Float | Double | Long | Int | Short | Byte] as canEqual =
      erasedValue

    given FromDigits[S32] as fromDigits:
      inline def fromDigits(digits: String): S32 = ${Hypotenuse2.parseS32('digits)}

    given S32 is Textualizer = _.toString.tt
    inline def apply(inline bits: B32): S32 = bits

    inline given S32 is Orderable as orderable:

      inline def compare
         (inline left: S32, inline right: S32, inline strict: Boolean, inline greaterThan: Boolean)
              : Boolean =

        inline if greaterThan
        then inline if strict then (left: Int) > (right: Int) else (left: Int) >= (right: Int)
        else inline if strict then (left: Int) < (right: Int) else (left: Int) <= (right: Int)

  object U16:
    erased given Underlying[U16, Short] as underlying = erasedValue
    inline given CanEqual[U16, U16] as canEqual = erasedValue

    given FromDigits[U16] as fromDigits:
      inline def fromDigits(digits: String): U16 = ${Hypotenuse2.parseU16('digits)}

    given U16 is Textualizer = u16 => JShort.toUnsignedInt(u16).toString.nn.tt
    inline def apply(inline bits: B16): U16 = bits

    inline given U16 is Orderable as orderable:

      inline def compare
         (inline left: U16, inline right: U16, inline strict: Boolean, inline greaterThan: Boolean)
              : Boolean =

        val left2 = JShort.toUnsignedInt(left)
        val right2 = JShort.toUnsignedInt(right)

        inline if greaterThan
        then inline if strict then left2 > right2 else left.toInt >= right2
        else inline if strict then left2 < right2 else left.toInt <= right2

  object S16:
    erased given Underlying[S16, Short] as underlying = erasedValue

    inline given CanEqual[S16, F64 | F32 | S64 | S32 | S16 | S8 | Float | Double | Long | Int | Short | Byte] as canEqual =
      erasedValue

    given FromDigits[S16] as fromDigits:
      inline def fromDigits(digits: String): S16 = ${Hypotenuse2.parseS16('digits)}

    given S16 is Textualizer = _.toString.tt
    inline def apply(inline bits: B16): S16 = bits

    inline given S16 is Orderable as orderable:

      inline def compare
         (inline left: S16, inline right: S16, inline strict: Boolean, inline greaterThan: Boolean)
              : Boolean =

        inline if greaterThan
        then inline if strict then left > right else left >= right
        else inline if strict then left < right else left <= right

  object U8:
    erased given Underlying[U8, Byte] as underlying = erasedValue
    inline given CanEqual[U8, U8] as canEqual = erasedValue
    given FromDigits[U8] as fromDigits:
      inline def fromDigits(digits: String): U8 = ${Hypotenuse2.parseU8('digits)}

    given U8 is Textualizer = u8 => JByte.toUnsignedInt(u8).toString.nn.tt
    inline def apply(inline bits: B8): U8 = bits

    inline given U8 is Orderable as orderable:

      inline def compare
         (inline left: U8, inline right: U8, inline strict: Boolean, inline greaterThan: Boolean)
              : Boolean =

        val left2 = JByte.toUnsignedInt(left)
        val right2 = JByte.toUnsignedInt(right)

        inline if greaterThan
        then inline if strict then left2 > right2 else left2 >= right2
        else inline if strict then left2 < right2 else left2 <= right2

  object S8:
    erased given Underlying[S8, Byte] as underlying = erasedValue

    inline given CanEqual[S8, F64 | F32 | S64 | S32 | S16 | S8 | Float | Double | Long | Int | Short | Byte] as canEqual =
      erasedValue

    given FromDigits[S8] as fromDigits:
      inline def fromDigits(digits: String): S8 = ${Hypotenuse2.parseS8('digits)}

    given S8 is Textualizer = _.toString.tt
    inline def apply(inline bits: B8): S8 = bits

    inline given S8 is Orderable as inquality:

      inline def compare
         (inline left: S8, inline right: S8, inline strict: Boolean, inline greaterThan: Boolean)
              : Boolean =

        inline if greaterThan
        then inline if strict then left > right else left >= right
        else inline if strict then left < right else left <= right

  object B64:
    erased given Underlying[B64, Long] as underlying = erasedValue
    inline def block(inline n: Int): B64 = (1L << n) - 1
    inline def set(inline ordinal: Ordinal): B64 = 1L << ordinal.n0

    inline def set(inline interval: Interval): B64 =
      val b = block(interval.size)
      (b: Long) << (interval.start.n0: Int)

    @targetName("set2")
    inline def set(inline bounds: Bounds): B64 = set(bounds.of(64))

    inline def apply(inline long: Long): B64 = long
    inline def apply(inline int: Int): B64 = int.toLong
    inline def apply(inline short: Short): B64 = short.toLong
    inline def apply(inline byte: Byte): B64 = byte.toLong

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
    erased given Underlying[B32, Int] as underlying = erasedValue
    inline def block(inline n: Int): B32 = (1 << n) - 1
    inline def set(inline ordinal: Ordinal): B32 = 1 << ordinal.n0
    inline def set(inline interval: Interval): B32 = block(interval.size) << interval.start.n0

    @targetName("set2")
    inline def set(inline bounds: Bounds): B32 = set(bounds.of(32))

    inline def apply(inline int: Int): B32 = int
    inline def apply(inline short: Short): B32 = short.toInt
    inline def apply(inline byte: Byte): B32 = byte.toInt

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
    erased given Underlying[B16, Short] as underlying = erasedValue
    inline def block(inline n: Int): B16 = ((1 << n) - 1).toShort
    inline def set(inline ordinal: Ordinal): B16 = (1 << ordinal.n0).toShort

    inline def set(inline interval: Interval): B16 =
      (block(interval.size) << interval.start.n0).toShort

    @targetName("set2")
    inline def set(inline bounds: Bounds): B16 = set(bounds.of(16))

    inline def apply(inline short: Short): B16 = short
    inline def apply(inline byte: Byte): B16 = byte.toShort

    def apply(bytes: IArray[Byte], offset: Int = 0): B16 =
      var b16: Int = (bytes(offset) & 0xFF)
      b16 <<= 8
      b16 |= (bytes(offset + 1) & 0xFF)

      b16.toShort

  object B8:
    erased given Underlying[B8, Byte] as underlying = erasedValue
    inline def block(inline n: Int): B8 = ((1 << n) - 1).toByte
    inline def set(inline ordinal: Ordinal): B8 = (1 << ordinal.n0).toByte

    inline def set(inline interval: Interval): B8 =
      (block(interval.size) << interval.start.n0).toByte

    @targetName("set2")
    inline def set(inline bounds: Bounds): B8 = set(bounds.of(8))

    inline def apply(inline byte: Byte): B8 = byte

  extension (s64: S64)
    @targetName("absS64")
    inline def abs: S64 = math.abs(s64)

    @targetName("longS64")
    inline def long: Long = s64

    @targetName("octalS64")
    inline def octal: Text = JLong.toOctalString(s64).nn.tt

    @targetName("hexS64")
    inline def hex: Text = JLong.toHexString(s64).nn.tt

    @targetName("base32S64")
    inline def base32: Text = JLong.toString(s64, 32).nn.tt

    @targetName("binaryS64")
    inline def binary: Text = JLong.toBinaryString(s64).nn.tt

    @targetName("floorModS64")
    inline infix def %% (right: into S64): S64 = math.floorMod(s64, right)

    @targetName("floorDivS64")
    inline infix def \ (right: into S64): S64 = math.floorDiv(s64, right)

    @targetName("powerS64")
    inline infix def ** (exponent: Double): Double = math.pow(s64.toDouble, exponent)

    @targetName("divS64")
    inline infix def / (right: into S64)(using division: DivisionByZero): division.Wrap[S64] =
      division.divideS64(s64, right)

    @targetName("modS64")
    inline infix def % (right: into S64): S64 = s64%right

    @targetName("bitsS64")
    inline def bits: B64 = s64

    @targetName("gcdS64")
    def gcd(right: S64): S64 =
      @tailrec
      def recur(left: S64, right: S64): S64 =
        if right == 0 then left else recur(right, left%right)

      recur(s64, right)

  extension (s32: S32)
    @targetName("plusS32")
    inline infix def + (right: into S32)(using overflow: CheckOverflow): overflow.Wrap[S32] =
      overflow.addS32(s32, right)

    @targetName("intS32")
    inline def int: Int = s32

    @targetName("longS32")
    inline def long: Long = s32.toLong

    @targetName("absS32")
    inline def abs: S32 = math.abs(s32)

    @targetName("powerS32")
    inline infix def ** (exponent: Double): Double = math.pow(s32.toDouble, exponent)

    @targetName("octalS32")
    inline def octal: Text = JInt.toOctalString(s32).nn.tt

    @targetName("hexS32")
    inline def hex: Text = JInt.toHexString(s32).nn.tt

    @targetName("base32S32")
    inline def base32: Text = JInt.toString(s32, 32).nn.tt

    @targetName("binaryS32")
    inline def binary: Text = JInt.toBinaryString(s32).nn.tt

    @targetName("floorModS32")
    inline infix def %% (right: into S32): S32 = math.floorMod(s32, right)

    @targetName("floorDivS32")
    inline infix def \ (right: into S32): S32 = math.floorDiv(s32, right)

    @targetName("divS32")
    inline infix def / (right: into S32)(using division: DivisionByZero): division.Wrap[S32] =
      division.divideS32(s32, right)

    @targetName("modS32")
    inline infix def % (right: into S32): S32 = s32%right

    @targetName("bitsS32")
    inline def bits: B32 = s32

    @targetName("gcdS32")
    def gcd(right: S32): S32 =
      @tailrec
      def recur(left: S32, right: S32): S32 =
        if right == 0 then left else recur(right, left%right)

      recur(s32, right)

  extension (s16: S16)
    @targetName("plusS16")
    inline infix def + (right: into S16)(using overflow: CheckOverflow): overflow.Wrap[S16] =
      overflow.addS16(s16, right)

    @targetName("shortS16")
    inline def short: Short = s16

    @targetName("intS16")
    inline def int: Int = s16.toInt

    @targetName("longS16")
    inline def long: Long = s16.toLong

    @targetName("absS16")
    inline def abs: S16 = math.abs(s16).toShort

    @targetName("powerS16")
    inline infix def ** (exponent: Double): Double = math.pow(s16.toDouble, exponent)

    @targetName("octalS16")
    inline def octal: Text = JInt.toOctalString(s16).nn.tt

    @targetName("hexS16")
    inline def hex: Text = JInt.toHexString(s16).nn.tt

    @targetName("base32S16")
    inline def base32: Text = JInt.toString(s16, 32).nn.tt

    @targetName("binaryS16")
    inline def binary: Text = JInt.toBinaryString(s16).nn.tt

    @targetName("floorModS16")
    inline infix def %% (right: into S16): S16 = math.floorMod(s16, right).toShort

    @targetName("floorDivS16")
    inline infix def \ (right: into S16): S16 = math.floorDiv(s16, right).toShort

    @targetName("divS16")
    inline infix def / (right: into S16)(using division: DivisionByZero): division.Wrap[S16] =
      division.divideS16(s16, right)

    @targetName("modS16")
    inline infix def % (right: into S16): S16 = (s16%right).toShort

    @targetName("bitsS16")
    inline def bits: B16 = s16

    @targetName("gcdS16")
    def gcd(right: S16): S16 =
      @tailrec
      def recur(left: S16, right: S16): S16 =
        if right == 0 then left else recur(right, (left%right).toShort)

      recur(s16, right)

  extension (s8: S8)
    @targetName("plusS8")
    inline infix def + (right: into S8)(using overflow: CheckOverflow): overflow.Wrap[S8] =
      overflow.addS8(s8, right)

    @targetName("byteS8")
    inline def byte: Byte = s8

    @targetName("shortS8")
    inline def short: Short = s8.toShort

    @targetName("intS8")
    inline def int: Int = s8.toInt

    @targetName("longS8")
    inline def long: Long = s8.toLong

    @targetName("absS8")
    inline def abs: S8 = math.abs(s8).toByte

    @targetName("powerS8")
    inline infix def ** (exponent: Double): Double = math.pow(s8.toDouble, exponent)

    @targetName("octalS8")
    inline def octal: Text = JInt.toOctalString(s8).nn.tt

    @targetName("hexS8")
    inline def hex: Text = JInt.toHexString(s8).nn.tt

    @targetName("base32S8")
    inline def base32: Text = JInt.toString(s8, 32).nn.tt

    @targetName("binaryS8")
    inline def binary: Text = JInt.toBinaryString(s8).nn.tt

    @targetName("floorModS8")
    inline infix def %% (right: into S8): S8 = math.floorMod(s8, right).toByte

    @targetName("floorDivS8")
    inline infix def \ (right: into S8): S8 = math.floorDiv(s8, right).toByte

    @targetName("divS8")
    inline infix def / (right: into S8)(using division: DivisionByZero): division.Wrap[S8] =
      division.divideS8(s8, right)

    @targetName("modS8")
    inline infix def % (right: into S8): S8 = (s8%right).toByte

    @targetName("bitsS8")
    inline def bits: B8 = s8


    @targetName("gcdS8")
    def gcd(right: S8): S8 =
      @tailrec
      def recur(left: S8, right: S8): S8 =
        if right == 0 then left else recur(right, (left%right).toByte)

      recur(s8, right)

  extension (bitmap: B8)
    @targetName("bitsB8")
    inline def apply(inline interval: Interval): B8 =
      ((bitmap >> interval.start.n0) & B8.block(interval.size)).toByte

    @targetName("setB8")
    inline def set(inline index: Ordinal): B8 = (bitmap | B8.set(index)).toByte

    @targetName("setIntervalB8")
    inline def set(inline interval: Interval): B8 = (bitmap | B8.set(interval)).toByte

    @targetName("clearB8")
    inline def clear(inline index: Ordinal): B8 = (bitmap & ~B8.set(index)).toByte

    @targetName("clearIntervalB8")
    inline def clear(inline interval: Interval): B8 = (bitmap & ~B8.set(interval)).toByte

    @targetName("flipB8")
    inline def flip(inline index: Ordinal): B8 = (bitmap ^ B8.set(index)).toByte

    @targetName("flipIntervalB8")
    inline def flip(inline interval: Interval): B8 = (bitmap ^ B8.set(interval)).toByte

    @targetName("bitB8")
    inline def apply(inline index: Ordinal): Boolean = ((bitmap >> index.n0) & 1) == 1

    @targetName("rotateLeftB8")
    inline infix def <<< (inline count: Int): B8 = ((bitmap << count%%8) | (bitmap >>> (8 - count%%8))).toByte

    @targetName("rotateRightB8")
    inline infix def >>> (inline count: Int): B8 = ((bitmap >>> count%%8) | (bitmap << (8 - count%%8))).toByte

    @targetName("shiftLeftB8")
    inline infix def << (inline count: Int): B8 = (bitmap << count).toByte

    @targetName("shiftRightB8")
    inline infix def >> (inline count: Int): B8 = (bitmap >>> count).toByte

    @targetName("andB8")
    inline infix def & (inline right: into B8): B8 = (bitmap & right).toByte

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
    inline def ones: S32 = JInt.bitCount(bitmap.toInt)

    @targetName("zerosB8")
    inline def zeros: S32 = 8 - JInt.bitCount(bitmap.toInt)

    @targetName("reverseB8")
    inline def reverse: B8 = (JInt.reverse(bitmap.toInt) >>> 24).toByte

    @targetName("hexB8")
    inline def hex: Text = String.format("%02x", bitmap).nn.tt

    @targetName("octalB8")
    inline def octal: Text = String.format("%03o", bitmap).nn.tt

    inline def apply(inline bit: Int): Boolean = ((bitmap >> bit) & 1) == 1

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

    def s8: S8 = bitmap
    def u8: U8 = bitmap

  extension (bitmap: B16)
    @targetName("bitsB16")
    inline def apply(inline interval: Interval): B16 =
      ((bitmap >> interval.start.n0) & B16.block(interval.size)).toShort

    @targetName("setB16")
    inline def set(inline index: Ordinal): B16 = (bitmap | B16.set(index)).toShort

    @targetName("setIntervalB16")
    inline def set(inline interval: Interval): B16 = (bitmap | B16.set(interval)).toShort

    @targetName("clearB16")
    inline def clear(inline index: Ordinal): B16 = (bitmap & ~B16.set(index)).toShort

    @targetName("clearIntervalB16")
    inline def clear(inline interval: Interval): B16 = (bitmap & ~B16.set(interval)).toShort

    @targetName("flipB16")
    inline def flip(inline index: Ordinal): B16 = (bitmap ^ B16.set(index)).toShort

    @targetName("flipIntervalB16")
    inline def flip(inline interval: Interval): B16 = (bitmap ^ B16.set(interval)).toShort

    @targetName("bitB16")
    inline def apply(inline index: Ordinal): Boolean = ((bitmap >> index.n0) & 1) == 1

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
    inline def ones: S32 = JInt.bitCount(bitmap.toInt)

    @targetName("zerosB16")
    inline def zeros: S32 = 16 - JInt.bitCount(bitmap.toInt)

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

    inline def apply(inline bit: Int): Boolean = ((bitmap >> bit) & 1) == 1

    def s16: S16 = bitmap
    def u16: U16 = bitmap

  extension (bitmap: B32)
    @targetName("bitsB32")
    inline def apply(inline interval: Interval): B32 =
      (bitmap >> interval.start.n0) & B32.block(interval.size)

    @targetName("setB32")
    inline def set(inline index: Ordinal): B32 = bitmap | B32.set(index)

    @targetName("setIntervalB32")
    inline def set(inline interval: Interval): B32 = bitmap | B32.set(interval)

    @targetName("clearB32")
    inline def clear(inline index: Ordinal): B32 = bitmap & ~B32.set(index)

    @targetName("clearIntervalB32")
    inline def clear(inline interval: Interval): B32 = bitmap & ~B32.set(interval)

    @targetName("flipB32")
    inline def flip(inline index: Ordinal): B32 = bitmap ^ B32.set(index)

    @targetName("flipIntervalB32")
    inline def flip(inline interval: Interval): B32 = bitmap ^ B32.set(interval)

    @targetName("bitB32")
    inline def apply(inline index: Ordinal): Boolean = ((bitmap >> index.n0) & 1) == 1

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
    inline def ones: S32 = JInt.bitCount(bitmap.toInt)

    @targetName("zerosB32")
    inline def zeros: S32 = 32 - JInt.bitCount(bitmap.toInt)

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

    inline def apply(inline bit: Int): Boolean = ((bitmap >> bit) & 1) == 1

    def s32: S32 = bitmap
    def u32: U32 = bitmap

  extension (bitmap: B64)
    @targetName("bitsB64")
    inline def apply(inline interval: Interval): B64 =
      (bitmap >> interval.start.n0) & B64.block(interval.size)

    @targetName("setB64")
    inline def set(inline index: Ordinal): B64 = bitmap | B64.set(index)

    @targetName("setIntervalB64")
    inline def set(inline interval: Interval): B64 = bitmap | B64.set(interval)

    @targetName("clearB64")
    inline def clear(inline index: Ordinal): B64 = bitmap & ~B64.set(index)

    @targetName("clearIntervalB64")
    inline def clear(inline interval: Interval): B64 = bitmap & ~B64.set(interval)

    @targetName("flipB64")
    inline def flip(inline index: Ordinal): B64 = bitmap ^ B64.set(index)

    @targetName("flipIntervalB64")
    inline def flip(inline interval: Interval): B64 = bitmap ^ B64.set(interval)

    @targetName("bitB32")
    inline def apply(inline index: Ordinal): Boolean = ((bitmap >> index.n0) & 1) == 1

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
    inline def ones: S32 = JLong.bitCount(bitmap.toInt)

    @targetName("zerosB64")
    inline def zeros: S32 = 64 - JLong.bitCount(bitmap.toInt)

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

    inline def apply(inline bit: Int): Boolean = ((bitmap >> bit) & 1) == 1

    def s64: S64 = bitmap
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
    inline def scalb(inline scale: Int): F64 = math.scalb(double, scale)

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
    inline def scalb(inline scale: Int): F32 = math.scalb(float, scale)

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

    @targetName("gcdU64")
    def gcd(right: U64): U64 =
      @tailrec
      def recur(left: U64, right: U64): U64 = if right == 0 then left else recur(right, left%right)
      recur(u64, right)

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

    @targetName("u32ToS64")
    inline def s64: S64 = JInt.toUnsignedLong(u32)

    @targetName("u32ToU64")
    inline def u64: U64 = JInt.toUnsignedLong(u32)

    @targetName("gcdU32")
    def gcd(right: U32): U32 =
      @tailrec
      def recur(left: U32, right: U32): U32 = if right == 0 then left else recur(right, left%right)
      recur(u32, right)

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

    @targetName("u16ToS32")
    inline def s32: S32 = JShort.toUnsignedInt(u16)

    @targetName("u16ToS64")
    inline def s64: S64 = JShort.toUnsignedLong(u16)

    @targetName("u16ToU32")
    inline def u32: U32 = JShort.toUnsignedInt(u16)

    @targetName("u16ToU64")
    inline def u64: U64 = JShort.toUnsignedLong(u16)

    @targetName("gcdU16")
    def gcd(right: U16): U16 =
      @tailrec
      def recur(left: U16, right: U16): U16 =
        if right == 0 then left else recur(right, (left%right).toShort)

      recur(u16, right)

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

    @targetName("u8ToS16")
    inline def s16: S16 = JByte.toUnsignedInt(u8).toShort

    @targetName("u8ToS32")
    inline def s32: S32 = JByte.toUnsignedInt(u8)

    @targetName("u8ToS64")
    inline def s64: S64 = JByte.toUnsignedLong(u8)

    @targetName("u8ToU16")
    inline def u16: U16 = JByte.toUnsignedInt(u8).toShort

    @targetName("u8ToU32")
    inline def u32: U32 = JByte.toUnsignedInt(u8)

    @targetName("u8ToU64")
    inline def u64: U64 = JByte.toUnsignedLong(u8)

    @targetName("gcdU8")
    def gcd(right: U8): U8 =
      @tailrec
      def recur(left: U8, right: U8): U8 =
        if right == 0 then left else recur(right, (left%right).toByte)

      recur(u8, right)
