                                                                                                  /*
┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓
┃                                                                                                  ┃
┃                                                   ╭───╮                                          ┃
┃                                                   │   │                                          ┃
┃                                                   │   │                                          ┃
┃   ╭───────╮╭─────────╮╭───╮ ╭───╮╭───╮╌────╮╭────╌┤   │╭───╮╌────╮╭────────╮╭───────╮╭───────╮   ┃
┃   │   ╭───╯│   ╭─╮   ││   │ │   ││   ╭─╮   ││   ╭─╮   ││   ╭─╮   ││   ╭─╮  ││   ╭───╯│   ╭───╯   ┃
┃   │   ╰───╮│   │ │   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   ╰─╯  ││   ╰───╮│   ╰───╮   ┃
┃   ╰───╮   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   ╭────╯╰───╮   │╰───╮   │   ┃
┃   ╭───╯   ││   ╰─╯   ││   ╰─╯   ││   │ │   ││   ╰─╯   ││   │ │   ││   ╰────╮╭───╯   │╭───╯   │   ┃
┃   ╰───────╯╰─────────╯╰────╌╰───╯╰───╯ ╰───╯╰────╌╰───╯╰───╯ ╰───╯╰────────╯╰───────╯╰───────╯   ┃
┃                                                                                                  ┃
┃    Soundness, version 0.64.0.                                                                    ┃
┃    © Copyright 2021-25 Jon Pretty, Propensive OÜ.                                                ┃
┃                                                                                                  ┃
┃    The primary distribution site is:                                                             ┃
┃                                                                                                  ┃
┃        https://soundness.dev/                                                                    ┃
┃                                                                                                  ┃
┃    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file     ┃
┃    except in compliance with the License. You may obtain a copy of the License at                ┃
┃                                                                                                  ┃
┃        https://www.apache.org/licenses/LICENSE-2.0                                               ┃
┃                                                                                                  ┃
┃    Unless required by applicable law or agreed to in writing,  software distributed under the    ┃
┃    License is distributed on an "AS IS" BASIS,  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,    ┃
┃    either express or implied. See the License for the specific language governing permissions    ┃
┃    and limitations under the License.                                                            ┃
┃                                                                                                  ┃
┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛
                                                                                                  */
package hypotenuse

import java.lang.{Integer as JInt, Long as JLong, Short as JShort, Byte as JByte, Double as JDouble,
    Float as JFloat}

import scala.annotation.*
import scala.language.experimental.genericNumberLiterals
import scala.util.FromDigits
import scala.{caps, math}

import anticipation.*
import denominative.*
import prepositional.*
import symbolism.*

object internal:
  type Bits[bits <: 8 | 16 | 32 | 64] <: B8 | B16 | B32 | B64 = bits match
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
    final val Min: F64 = Double.MinValue
    final val Max: F64 = Double.MaxValue

    // [erased-evidence] underlying evidence for F64
    inline given underlying: Underlying[F64, Double] = caps.unsafe.unsafeErasedValue

    // The instances are objects, named by their singleton types, so that an operator inlined
    // through them leaves no allocation behind (`CheckOverflow` explains); here, inside the
    // opaque scope, `F64` is a `Double`, and the bodies are the primitive operations.
    object Addition extends Arithmetic.Addition[F64, [result] =>> result]:
      inline def add(left: F64, right: F64): F64 = left + right

    object Subtraction extends Arithmetic.Subtraction[F64, [result] =>> result]:
      inline def subtract(left: F64, right: F64): F64 = left - right

    object Multiplication extends Arithmetic.Multiplication[F64, [result] =>> result]:
      inline def multiply(left: F64, right: F64): F64 = left*right

    object Division extends Arithmetic.Division[F64, [result] =>> result]:
      inline def divide(left: F64, right: F64): F64 = left/right

    object Negation extends Negatable:
      type Self = F64
      type Result = F64

      inline def negate(operand: F64): F64 = -operand

    given addable: Addition.type = Addition
    given subtractable: Subtraction.type = Subtraction
    given multiplicable: Multiplication.type = Multiplication
    given divisible: Division.type = Division
    given negatable: Negation.type = Negation

    // A conversion into `F64` which also carries the arithmetic on a `F64` with a value of the
    // type it converts: the right operand is converted and the operation is the primitive
    // one. The instances live here, one set per conversion, because an object cannot take a
    // type parameter, and each must name `right` precisely for its `Operand`.
    trait Widening[right] extends Conversion[right, F64]:
      object Addition extends Addable:
        type Self = F64
        type Operand = right
        type Result = F64

        inline def add(left: F64, right: right): F64 = left + Widening.this(right)

      object Subtraction extends Subtractable:
        type Self = F64
        type Operand = right
        type Result = F64

        inline def subtract(left: F64, right: right): F64 = left - Widening.this(right)

      object Multiplication extends Multiplicable:
        type Self = F64
        type Operand = right
        type Result = F64

        inline def multiply(left: F64, right: right): F64 = left*Widening.this(right)

      object Division extends Divisible:
        type Self = F64
        type Operand = right
        type Result = F64

        inline def divide(left: F64, right: right): F64 = left/Widening.this(right)

    // The widening givens are transparent with a plain refinement type, rather than typed by
    // the singleton: a search instantiates `right` from the expected `Operand` before it looks
    // for the `Widening`, which a dependent type would not let it do, and transparency still
    // gives the summoned instance its singleton type, through which `add` inlines.
    transparent inline given addableWidening: [right] => (widening: Widening[right])
          =>  F64 is Addable by right to F64 =
      widening.Addition

    transparent inline given subtractableWidening: [right] => (widening: Widening[right])
          =>  F64 is Subtractable by right to F64 =
      widening.Subtraction

    transparent inline given multiplicableWidening: [right] => (widening: Widening[right])
          =>  F64 is Multiplicable by right to F64 =
      widening.Multiplication

    transparent inline given divisibleWidening: [right] => (widening: Widening[right])
          =>  F64 is Divisible by right to F64 =
      widening.Division

    inline given canEqual: CanEqual
      [ F64, F64 | S64 | S32 | S16 | S8 | Double | Long | Int | Short | Byte ] =

      CanEqual.derived

    inline def apply(inline sign: Boolean, inline exponent: B16, inline mantissa: B64): F64 = F64:
      (if sign then Long.MinValue else 0L) | ((exponent & 0xffL) << 52) |
        (mantissa & 0xfffffffffffffL)

    inline def apply(inline bits: B64): F64 = JDouble.longBitsToDouble(bits)
    inline def apply(inline double: Double): F64 = double

    inline given orderable: F64 is Orderable:
      inline def compare
        ( inline left:    F64,
          inline right:   F64,
          inline strict:  Boolean,
          inline greater: Boolean )
      :   Boolean =

        inline if greater
        then
          inline if strict then (left: Double) > (right: Double)
          else (left: Double) >= (right: Double)
        else
          inline if strict then (left: Double) < (right: Double)
          else (left: Double) <= (right: Double)

    inline given commensurableInt: F64 is Commensurable:
      type Contrast = Int


      inline def compare
        ( inline left:        F64,
          inline right:       Int,
          inline strict:      Boolean,
          inline greaterThan: Boolean )
      :   Boolean =

        inline if greaterThan
        then inline if strict then left > right else left >= right
        else inline if strict then left < right else left <= right

    inline given commensurable: F64 is Commensurable:
      type Contrast = Double


      inline def compare
        ( inline left:        F64,
          inline right:       Double,
          inline strict:      Boolean,
          inline greaterThan: Boolean )
      :   Boolean =

        inline if greaterThan
        then inline if strict then left > right else left >= right
        else inline if strict then left < right else left <= right

    // The conversions are not inline: expanded from inside the opaque scope, their bodies would
    // be typed by the underlying primitive, and an `into` parameter would then reject them.
    object DoubleConversion extends Widening[Double]:
      def apply(value: Double): F64 = value

    inline given doubleConversion: DoubleConversion.type = DoubleConversion

    object FloatConversion extends Widening[Float]:
      def apply(value: Float): F64 = value.toDouble

    inline given floatConversion: FloatConversion.type = FloatConversion

    object IntConversion extends Widening[Int]:
      def apply(value: Int): F64 = value.toDouble

    inline given intConversion: IntConversion.type = IntConversion

    object ShortConversion extends Widening[Short]:
      def apply(value: Short): F64 = value.toDouble

    inline given shortConversion: ShortConversion.type = ShortConversion

    object ByteConversion extends Widening[Byte]:
      def apply(value: Byte): F64 = value.toDouble

    inline given byteConversion: ByteConversion.type = ByteConversion

    object U32Conversion extends Widening[U32]:
      def apply(value: U32): F64 = JInt.toUnsignedLong(value).toDouble

    inline given u32Conversion: U32Conversion.type = U32Conversion

    object S32Conversion extends Widening[S32]:
      def apply(value: S32): F64 = value.toDouble

    inline given s32Conversion: S32Conversion.type = S32Conversion

    object U16Conversion extends Widening[U16]:
      def apply(value: U16): F64 = JShort.toUnsignedInt(value).toDouble

    inline given u16Conversion: U16Conversion.type = U16Conversion

    object U8Conversion extends Widening[U8]:
      def apply(value: U8): F64 = JByte.toUnsignedInt(value).toDouble

    inline given u8Conversion: U8Conversion.type = U8Conversion

    object S16Conversion extends Widening[S16]:
      def apply(value: S16): F64 = value.toDouble

    inline given s16Conversion: S16Conversion.type = S16Conversion

    object S8Conversion extends Widening[S8]:
      def apply(value: S8): F64 = value.toDouble

    inline given s8Conversion: S8Conversion.type = S8Conversion

  object F32:
    final val Min: F32 = Float.MinValue
    final val Max: F32 = Float.MaxValue

    // [erased-evidence] underlying evidence for F32
    inline given underlying: Underlying[F32, Float] = caps.unsafe.unsafeErasedValue

    // The instances are objects, named by their singleton types, so that an operator inlined
    // through them leaves no allocation behind (`CheckOverflow` explains); here, inside the
    // opaque scope, `F32` is a `Float`, and the bodies are the primitive operations.
    object Addition extends Arithmetic.Addition[F32, [result] =>> result]:
      inline def add(left: F32, right: F32): F32 = left + right

    object Subtraction extends Arithmetic.Subtraction[F32, [result] =>> result]:
      inline def subtract(left: F32, right: F32): F32 = left - right

    object Multiplication extends Arithmetic.Multiplication[F32, [result] =>> result]:
      inline def multiply(left: F32, right: F32): F32 = left*right

    object Division extends Arithmetic.Division[F32, [result] =>> result]:
      inline def divide(left: F32, right: F32): F32 = left/right

    object Negation extends Negatable:
      type Self = F32
      type Result = F32

      inline def negate(operand: F32): F32 = -operand

    given addable: Addition.type = Addition
    given subtractable: Subtraction.type = Subtraction
    given multiplicable: Multiplication.type = Multiplication
    given divisible: Division.type = Division
    given negatable: Negation.type = Negation

    // A conversion into `F32` which also carries the arithmetic on a `F32` with a value of the
    // type it converts: the right operand is converted and the operation is the primitive
    // one. The instances live here, one set per conversion, because an object cannot take a
    // type parameter, and each must name `right` precisely for its `Operand`.
    trait Widening[right] extends Conversion[right, F32]:
      object Addition extends Addable:
        type Self = F32
        type Operand = right
        type Result = F32

        inline def add(left: F32, right: right): F32 = left + Widening.this(right)

      object Subtraction extends Subtractable:
        type Self = F32
        type Operand = right
        type Result = F32

        inline def subtract(left: F32, right: right): F32 = left - Widening.this(right)

      object Multiplication extends Multiplicable:
        type Self = F32
        type Operand = right
        type Result = F32

        inline def multiply(left: F32, right: right): F32 = left*Widening.this(right)

      object Division extends Divisible:
        type Self = F32
        type Operand = right
        type Result = F32

        inline def divide(left: F32, right: right): F32 = left/Widening.this(right)

    // The widening givens are transparent with a plain refinement type, rather than typed by
    // the singleton: a search instantiates `right` from the expected `Operand` before it looks
    // for the `Widening`, which a dependent type would not let it do, and transparency still
    // gives the summoned instance its singleton type, through which `add` inlines.
    transparent inline given addableWidening: [right] => (widening: Widening[right])
          =>  F32 is Addable by right to F32 =
      widening.Addition

    transparent inline given subtractableWidening: [right] => (widening: Widening[right])
          =>  F32 is Subtractable by right to F32 =
      widening.Subtraction

    transparent inline given multiplicableWidening: [right] => (widening: Widening[right])
          =>  F32 is Multiplicable by right to F32 =
      widening.Multiplication

    transparent inline given divisibleWidening: [right] => (widening: Widening[right])
          =>  F32 is Divisible by right to F32 =
      widening.Division

    inline given canEqual: CanEqual
      [ F32, F32 | S64 | S32 | S16 | S8 | Float | Long | Int | Short | Byte ] =

      CanEqual.derived

    inline given orderable: F32 is Orderable:
      inline def compare
        ( inline left:        F32,
          inline right:       F32,
          inline strict:      Boolean,
          inline greaterThan: Boolean )
      :   Boolean =

        inline if greaterThan
        then
          inline if strict then (left: Float) > (right: Float) else (left: Float) >= (right: Float)
        else
          inline if strict then (left: Float) < (right: Float)
          else (left: Float) <= (right: Float)

    inline def apply(inline sign: Boolean, inline exponent: B16, inline mantissa: B32): F32 =
      val signBit = if sign then 0 else 1 << 31
      F32(if sign then Int.MinValue else 0 | ((exponent & 0xff) << 22) | (mantissa & 0x3fffff))

    inline def apply(inline bits: B32): F32 = JFloat.intBitsToFloat(bits)
    inline def apply(inline float: Float): F32 = float

    object FloatConversion extends Widening[Float]:
      def apply(value: Float): F32 = value

    inline given floatConversion: FloatConversion.type = FloatConversion

    object ShortConversion extends Widening[Short]:
      def apply(value: Short): F32 = value.toFloat

    inline given shortConversion: ShortConversion.type = ShortConversion

    object ByteConversion extends Widening[Byte]:
      def apply(value: Byte): F32 = value.toFloat

    inline given byteConversion: ByteConversion.type = ByteConversion

    object U16Conversion extends Widening[U16]:
      def apply(value: U16): F32 = JShort.toUnsignedInt(value).toFloat

    inline given u16Conversion: U16Conversion.type = U16Conversion

    object U8Conversion extends Widening[U8]:
      def apply(value: U8): F32 = JByte.toUnsignedInt(value).toFloat

    inline given u8Conversion: U8Conversion.type = U8Conversion

    object S16Conversion extends Widening[S16]:
      def apply(value: S16): F32 = value.toFloat

    inline given s16Conversion: S16Conversion.type = S16Conversion

    object S8Conversion extends Widening[S8]:
      def apply(value: S8): F32 = value.toFloat

    inline given s8Conversion: S8Conversion.type = S8Conversion

  object U64:
    final val Min: U64 = 0L
    final val Max: U64 = -1L

    // [erased-evidence] underlying evidence for U64
    inline given underlying: Underlying[U64, Long] = caps.unsafe.unsafeErasedValue

    // Each instance is the policy's own object, named by its singleton type; `CheckOverflow`
    // explains why that, rather than an instance, is what makes `+` inline to one instruction.
    inline given addable: (overflow: CheckOverflow) => overflow.addU64.type = overflow.addU64

    inline given subtractable: (overflow: CheckOverflow) => overflow.subtractU64.type =
      overflow.subtractU64

    inline given multiplicable: (overflow: CheckOverflow) => overflow.multiplyU64.type =
      overflow.multiplyU64

    inline given divisible: (division: DivisionByZero) => division.divideU64.type =
      division.divideU64
    inline given canEqual: CanEqual[U64, U64] = CanEqual.derived

    given fromDigits: FromDigits[U64]:
      inline def fromDigits(digits: String): U64 = ${hypotenuse.protointernal.parseU64('digits)}

    given textualizable: U64 is Textualizable = JLong.toUnsignedString(_).nn.tt

    inline def apply(inline bits: B64): U64 = bits

    inline given orderable: U64 is Orderable:
      inline def compare
        ( inline left:        U64,
          inline right:       U64,
          inline strict:      Boolean,
          inline greaterThan: Boolean )
      :   Boolean =

        inline if greaterThan then
          inline if strict then JLong.compareUnsigned(left, right) == 1
          else JLong.compareUnsigned(left, right) != -1
        else
          inline if strict then JLong.compareUnsigned(left, right) == -1
          else JLong.compareUnsigned(left, right) != 1

  object S64:
    final val Min: S64 = Long.MinValue
    final val Max: S64 = Long.MaxValue

    // [erased-evidence] underlying evidence for S64
    inline given underlying: Underlying[S64, Long] = caps.unsafe.unsafeErasedValue

    // Each instance is the policy's own object, named by its singleton type; `CheckOverflow`
    // explains why that, rather than an instance, is what makes `+` inline to one instruction.
    inline given addable: (overflow: CheckOverflow) => overflow.addS64.type = overflow.addS64

    inline given subtractable: (overflow: CheckOverflow) => overflow.subtractS64.type =
      overflow.subtractS64

    inline given multiplicable: (overflow: CheckOverflow) => overflow.multiplyS64.type =
      overflow.multiplyS64

    inline given divisible: (division: DivisionByZero) => division.divideS64.type =
      division.divideS64

    inline given canEqual: CanEqual[S64, F64 | F32 | S64 | S32 | S16 | S8 | Float | Double | Long |
      Int | Short | Byte] =

      CanEqual.derived

    given fromDigits: FromDigits[S64]:
      inline def fromDigits(digits: String): S64 = ${hypotenuse.protointernal.parseS64('digits)}

    given textualizable: S64 is Textualizable = _.toString.tt

    inline def apply(inline bits: B64): S64 = bits

    inline given orderable: S64 is Orderable:
      inline def compare
        ( inline left:        S64,
          inline right:       S64,
          inline strict:      Boolean,
          inline greaterThan: Boolean )
      :   Boolean =

        inline if greaterThan
        then inline if strict then (left: Long) > (right: Long) else (left: Long) >= (right: Long)
        else inline if strict then (left: Long) < (right: Long) else (left: Long) <= (right: Long)

  object U32:
    final val Min: U32 = 0
    final val Max: U32 = -1

    // [erased-evidence] underlying evidence for U32
    inline given underlying: Underlying[U32, Int] = caps.unsafe.unsafeErasedValue

    // Each instance is the policy's own object, named by its singleton type; `CheckOverflow`
    // explains why that, rather than an instance, is what makes `+` inline to one instruction.
    inline given addable: (overflow: CheckOverflow) => overflow.addU32.type = overflow.addU32

    inline given subtractable: (overflow: CheckOverflow) => overflow.subtractU32.type =
      overflow.subtractU32

    inline given multiplicable: (overflow: CheckOverflow) => overflow.multiplyU32.type =
      overflow.multiplyU32

    inline given divisible: (division: DivisionByZero) => division.divideU32.type =
      division.divideU32
    inline given canEqual: CanEqual[U32, U32] = CanEqual.derived

    given fromDigits: FromDigits[U32]:
      inline def fromDigits(digits: String): U32 = ${hypotenuse.protointernal.parseU32('digits)}

    given textualizable: U32 is Textualizable = JInt.toUnsignedString(_).nn.tt

    inline def apply(inline bits: B32): U32 = bits

    inline given orderable: U32 is Orderable:
      inline def compare
        ( inline left:        U32,
          inline right:       U32,
          inline strict:      Boolean,
          inline greaterThan: Boolean )
      :   Boolean =

        inline if greaterThan then
          inline if strict then JLong.compareUnsigned(left, right) == 1
          else JInt.compareUnsigned(left, right) != -1
        else
          inline if strict then JLong.compareUnsigned(left, right) == -1
          else JInt.compareUnsigned(left, right) != 1

  object S32:
    final val Min: S32 = Int.MinValue
    final val Max: S32 = Int.MaxValue

    // [erased-evidence] underlying evidence for S32
    inline given underlying: Underlying[S32, Int] = caps.unsafe.unsafeErasedValue

    // Each instance is the policy's own object, named by its singleton type; `CheckOverflow`
    // explains why that, rather than an instance, is what makes `+` inline to one instruction.
    inline given addable: (overflow: CheckOverflow) => overflow.addS32.type = overflow.addS32

    inline given subtractable: (overflow: CheckOverflow) => overflow.subtractS32.type =
      overflow.subtractS32

    inline given multiplicable: (overflow: CheckOverflow) => overflow.multiplyS32.type =
      overflow.multiplyS32

    inline given divisible: (division: DivisionByZero) => division.divideS32.type =
      division.divideS32

    inline given canEqual: CanEqual[S32, F64 | F32 | S64 | S32 | S16 | S8 | Float | Double | Long |
      Int | Short | Byte] =

      CanEqual.derived

    given fromDigits: FromDigits[S32]:
      inline def fromDigits(digits: String): S32 = ${hypotenuse.protointernal.parseS32('digits)}

    given textualizable: S32 is Textualizable = _.toString.tt

    inline def apply(inline bits: B32): S32 = bits

    inline given orderable: S32 is Orderable:
      inline def compare
        ( inline left:        S32,
          inline right:       S32,
          inline strict:      Boolean,
          inline greaterThan: Boolean )
      :   Boolean =

        inline if greaterThan
        then inline if strict then (left: Int) > (right: Int) else (left: Int) >= (right: Int)
        else inline if strict then (left: Int) < (right: Int) else (left: Int) <= (right: Int)

  object U16:
    final val Min: U16 = 0
    final val Max: U16 = -1

    // [erased-evidence] underlying evidence for U16
    inline given underlying: Underlying[U16, Short] = caps.unsafe.unsafeErasedValue

    // Each instance is the policy's own object, named by its singleton type; `CheckOverflow`
    // explains why that, rather than an instance, is what makes `+` inline to one instruction.
    inline given addable: (overflow: CheckOverflow) => overflow.addU16.type = overflow.addU16

    inline given subtractable: (overflow: CheckOverflow) => overflow.subtractU16.type =
      overflow.subtractU16

    inline given multiplicable: (overflow: CheckOverflow) => overflow.multiplyU16.type =
      overflow.multiplyU16

    inline given divisible: (division: DivisionByZero) => division.divideU16.type =
      division.divideU16
    inline given canEqual: CanEqual[U16, U16] = CanEqual.derived

    given fromDigits: FromDigits[U16]:
      inline def fromDigits(digits: String): U16 = ${hypotenuse.protointernal.parseU16('digits)}

    given textualizable: U16 is Textualizable = u16 => JShort.toUnsignedInt(u16).toString.tt

    inline def apply(inline bits: B16): U16 = bits

    inline given orderable: U16 is Orderable:
      inline def compare
        ( inline left:        U16,
          inline right:       U16,
          inline strict:      Boolean,
          inline greaterThan: Boolean )
      :   Boolean =

        val left2 = JShort.toUnsignedInt(left)
        val right2 = JShort.toUnsignedInt(right)

        inline if greaterThan
        then inline if strict then left2 > right2 else left.toInt >= right2
        else inline if strict then left2 < right2 else left.toInt <= right2

  object S16:
    final val Min: S16 = Short.MinValue
    final val Max: S16 = Short.MaxValue

    // [erased-evidence] underlying evidence for S16
    inline given underlying: Underlying[S16, Short] = caps.unsafe.unsafeErasedValue

    // Each instance is the policy's own object, named by its singleton type; `CheckOverflow`
    // explains why that, rather than an instance, is what makes `+` inline to one instruction.
    inline given addable: (overflow: CheckOverflow) => overflow.addS16.type = overflow.addS16

    inline given subtractable: (overflow: CheckOverflow) => overflow.subtractS16.type =
      overflow.subtractS16

    inline given multiplicable: (overflow: CheckOverflow) => overflow.multiplyS16.type =
      overflow.multiplyS16

    inline given divisible: (division: DivisionByZero) => division.divideS16.type =
      division.divideS16

    inline given canEqual: CanEqual[S16,
                                    F64 | F32 | S64 | S32 | S16 | S8 | Float | Double | Long | Int |
                                      Short | Byte] =

      CanEqual.derived

    given fromDigits: FromDigits[S16]:
      inline def fromDigits(digits: String): S16 = ${hypotenuse.protointernal.parseS16('digits)}

    given textualizable: S16 is Textualizable = _.toString.tt

    inline def apply(inline bits: B16): S16 = bits

    inline given orderable: S16 is Orderable:
      inline def compare
        ( inline left:        S16,
          inline right:       S16,
          inline strict:      Boolean,
          inline greaterThan: Boolean )
      :   Boolean =

        inline if greaterThan
        then
          inline if strict then (left: Short) > (right: Short) else (left: Short) >= (right: Short)
        else
          inline if strict then (left: Short) < (right: Short)
          else (left: Short) <= (right: Short)

  object U8:
    final val Min: U8 = 0
    final val Max: U8 = -1

    // [erased-evidence] underlying evidence for U8
    inline given underlying: Underlying[U8, Byte] = caps.unsafe.unsafeErasedValue

    // Each instance is the policy's own object, named by its singleton type; `CheckOverflow`
    // explains why that, rather than an instance, is what makes `+` inline to one instruction.
    inline given addable: (overflow: CheckOverflow) => overflow.addU8.type = overflow.addU8

    inline given subtractable: (overflow: CheckOverflow) => overflow.subtractU8.type =
      overflow.subtractU8

    inline given multiplicable: (overflow: CheckOverflow) => overflow.multiplyU8.type =
      overflow.multiplyU8

    inline given divisible: (division: DivisionByZero) => division.divideU8.type =
      division.divideU8
    inline given canEqual: CanEqual[U8, U8] = CanEqual.derived

    given fromDigits: FromDigits[U8]:
      inline def fromDigits(digits: String): U8 = ${hypotenuse.protointernal.parseU8('digits)}

    given textualizable: U8 is Textualizable = u8 => JByte.toUnsignedInt(u8).toString.tt

    inline def apply(inline bits: B8): U8 = bits

    inline given orderable: U8 is Orderable:
      inline def compare
        ( inline left:        U8,
          inline right:       U8,
          inline strict:      Boolean,
          inline greaterThan: Boolean )
      :   Boolean =

        val left2 = JByte.toUnsignedInt(left)
        val right2 = JByte.toUnsignedInt(right)

        inline if greaterThan
        then inline if strict then left2 > right2 else left2 >= right2
        else inline if strict then left2 < right2 else left2 <= right2

  object S8:
    final val Min: S8 = Byte.MinValue
    final val Max: S8 = Byte.MaxValue

    // [erased-evidence] underlying evidence for S8
    inline given underlying: Underlying[S8, Byte] = caps.unsafe.unsafeErasedValue

    // Each instance is the policy's own object, named by its singleton type; `CheckOverflow`
    // explains why that, rather than an instance, is what makes `+` inline to one instruction.
    inline given addable: (overflow: CheckOverflow) => overflow.addS8.type = overflow.addS8

    inline given subtractable: (overflow: CheckOverflow) => overflow.subtractS8.type =
      overflow.subtractS8

    inline given multiplicable: (overflow: CheckOverflow) => overflow.multiplyS8.type =
      overflow.multiplyS8

    inline given divisible: (division: DivisionByZero) => division.divideS8.type =
      division.divideS8

    inline given canEqual: CanEqual[S8, F64 | F32 | S64 | S32 | S16 | S8 | Float | Double | Long |
      Int | Short | Byte] =

      CanEqual.derived

    given fromDigits: FromDigits[S8]:
      inline def fromDigits(digits: String): S8 = ${hypotenuse.protointernal.parseS8('digits)}

    given textualizable: S8 is Textualizable = _.toString.tt

    inline def apply(inline bits: B8): S8 = bits

    inline given inquality: S8 is Orderable:
      inline def compare
        ( inline left:        S8,
          inline right:       S8,
          inline strict:      Boolean,
          inline greaterThan: Boolean )
      :   Boolean =

        inline if greaterThan
        then inline if strict then (left: Byte) > (right: Byte) else (left: Byte) >= (right: Byte)
        else inline if strict then (left: Byte) < (right: Byte) else (left: Byte) <= (right: Byte)

  object B64:
    // [erased-evidence] underlying evidence for B64
    inline given underlying: Underlying[B64, Long] = caps.unsafe.unsafeErasedValue

    inline def block(inline n: Int): B64 = (1L << n) - 1
    inline def set(inline ordinal: Ordinal): B64 = 1L << ordinal.n0

    inline def set(inline interval: Interval): B64 =
      val b = block(interval.size)
      (b: Long) << (interval.start.n0: Int)

    inline def apply(inline long: Long): B64 = long
    inline def apply(inline int: Int): B64 = int.toLong
    inline def apply(inline short: Short): B64 = short.toLong
    inline def apply(inline byte: Byte): B64 = byte.toLong

    def apply(bytes: Array[Byte]^{}, offset: Int = 0): B64 =
      var b64: Long = (bytes.readUnchecked(offset) & 0xFF).toLong
      b64 <<= 8
      b64 |= (bytes.readUnchecked(offset + 1) & 0xFF).toLong
      b64 <<= 8
      b64 |= (bytes.readUnchecked(offset + 2) & 0xFF).toLong
      b64 <<= 8
      b64 |= (bytes.readUnchecked(offset + 3) & 0xFF).toLong
      b64 <<= 8
      b64 |= (bytes.readUnchecked(offset + 4) & 0xFF).toLong
      b64 <<= 8
      b64 |= (bytes.readUnchecked(offset + 5) & 0xFF).toLong
      b64 <<= 8
      b64 |= (bytes.readUnchecked(offset + 6) & 0xFF).toLong
      b64 <<= 8
      b64 |= (bytes.readUnchecked(offset + 7) & 0xFF).toLong

      b64

  object B32:
    // [erased-evidence] underlying evidence for B32
    inline given underlying: Underlying[B32, Int] = caps.unsafe.unsafeErasedValue

    inline def block(inline n: Int): B32 = (1 << n) - 1
    inline def set(inline ordinal: Ordinal): B32 = 1 << ordinal.n0
    inline def set(inline interval: Interval): B32 = block(interval.size) << interval.start.n0
    inline def apply(inline int: Int): B32 = int
    inline def apply(inline short: Short): B32 = short.toInt
    inline def apply(inline byte: Byte): B32 = byte.toInt

    def apply(bytes: Array[Byte]^{}, offset: Int = 0): B32 =
      var b32: Int = (bytes.readUnchecked(offset) & 0xFF)
      b32 <<= 8
      b32 |= (bytes.readUnchecked(offset + 1) & 0xFF)
      b32 <<= 8
      b32 |= (bytes.readUnchecked(offset + 2) & 0xFF)
      b32 <<= 8
      b32 |= (bytes.readUnchecked(offset + 3) & 0xFF)

      b32

  object B16:
    // [erased-evidence] underlying evidence for B16
    inline given underlying: Underlying[B16, Short] = caps.unsafe.unsafeErasedValue

    inline def block(inline n: Int): B16 = ((1 << n) - 1).toShort
    inline def set(inline ordinal: Ordinal): B16 = (1 << ordinal.n0).toShort

    inline def set(inline interval: Interval): B16 =
      (block(interval.size) << interval.start.n0).toShort

    inline def apply(inline short: Short): B16 = short
    inline def apply(inline byte: Byte): B16 = byte.toShort

    def apply(bytes: Array[Byte]^{}, offset: Int = 0): B16 =
      var b16: Int = (bytes.readUnchecked(offset) & 0xFF)
      b16 <<= 8
      b16 |= (bytes.readUnchecked(offset + 1) & 0xFF)

      b16.toShort

  object B8:
    // [erased-evidence] underlying evidence for B8
    inline given underlying: Underlying[B8, Byte] = caps.unsafe.unsafeErasedValue

    inline def block(inline n: Int): B8 = ((1 << n) - 1).toByte
    inline def set(inline ordinal: Ordinal): B8 = (1 << ordinal.n0).toByte

    inline def set(inline interval: Interval): B8 =
      (block(interval.size) << interval.start.n0).toByte

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
    inline def base32: Text = JLong.toString(s64, 32).tt

    @targetName("binaryS64")
    inline def binary: Text = JLong.toBinaryString(s64).nn.tt

    @targetName("floorModS64")
    inline infix def %% (right: Conversion.into[S64]): S64 = math.floorMod(s64, right)

    @targetName("floorDivS64")
    inline infix def /- (right: Conversion.into[S64]): S64 = math.floorDiv(s64, right)

    @targetName("powerS64")
    inline infix def ** (exponent: Double): Double = math.pow(s64.toDouble, exponent)

    @targetName("modS64")
    inline infix def % (right: Conversion.into[S64]): S64 = s64%right

    @targetName("bitsS64")
    inline def bits: B64 = s64

    @targetName("gcdS64")
    def gcd(right: S64): S64 =
      @tailrec
      def recur(left: S64, right: S64): S64 =
        if right == 0 then left else recur(right, left%right)

      recur(s64, right)

  extension (s32: S32)
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
    inline def base32: Text = JInt.toString(s32, 32).tt

    @targetName("binaryS32")
    inline def binary: Text = JInt.toBinaryString(s32).nn.tt

    @targetName("floorModS32")
    inline infix def %% (right: Conversion.into[S32]): S32 = math.floorMod(s32, right)

    @targetName("floorDivS32")
    inline infix def /- (right: Conversion.into[S32]): S32 = math.floorDiv(s32, right)

    @targetName("modS32")
    inline infix def % (right: Conversion.into[S32]): S32 = s32%right

    @targetName("bitsS32")
    inline def bits: B32 = s32

    @targetName("gcdS32")
    def gcd(right: S32): S32 =
      @tailrec
      def recur(left: S32, right: S32): S32 =
        if right == 0 then left else recur(right, left%right)

      recur(s32, right)

  extension (s16: S16)
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
    inline def base32: Text = JInt.toString(s16, 32).tt

    @targetName("binaryS16")
    inline def binary: Text = JInt.toBinaryString(s16).nn.tt

    @targetName("floorModS16")
    inline infix def %% (right: Conversion.into[S16]): S16 = math.floorMod(s16, right).toShort

    @targetName("floorDivS16")
    inline infix def /- (right: Conversion.into[S16]): S16 = math.floorDiv(s16, right).toShort

    @targetName("modS16")
    inline infix def % (right: Conversion.into[S16]): S16 = (s16%right).toShort

    @targetName("bitsS16")
    inline def bits: B16 = s16

    @targetName("gcdS16")
    def gcd(right: S16): S16 =
      @tailrec
      def recur(left: S16, right: S16): S16 =
        if right == 0 then left else recur(right, (left%right).toShort)

      recur(s16, right)

  extension (s8: S8)
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
    inline def base32: Text = JInt.toString(s8, 32).tt

    @targetName("binaryS8")
    inline def binary: Text = JInt.toBinaryString(s8).nn.tt

    @targetName("floorModS8")
    inline infix def %% (right: Conversion.into[S8]): S8 = math.floorMod(s8, right).toByte

    @targetName("floorDivS8")
    inline infix def /- (right: Conversion.into[S8]): S8 = math.floorDiv(s8, right).toByte

    @targetName("modS8")
    inline infix def % (right: Conversion.into[S8]): S8 = (s8%right).toByte

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
    inline def bit(inline index: Ordinal): Boolean = ((bitmap >> index.n0) & 1) == 1

    @targetName("rotateLeftB8")
    inline infix def <<< (inline count: Int): B8 =
      ((bitmap << count%%8) | (bitmap >>> (8 - count%%8))).toByte

    @targetName("rotateRightB8")
    inline infix def >>> (inline count: Int): B8 =
      ((bitmap >>> count%%8) | (bitmap << (8 - count%%8))).toByte

    @targetName("shiftLeftB8")
    inline infix def << (inline count: Int): B8 = (bitmap << count).toByte

    @targetName("shiftRightB8")
    inline infix def >> (inline count: Int): B8 = (bitmap >>> count).toByte

    @targetName("andB8")
    inline infix def & (inline right: Conversion.into[B8]): B8 = (bitmap & right).toByte

    @targetName("orB8")
    transparent inline infix def | (right: Conversion.into[B8]): B8 = (bitmap | right).toByte

    @targetName("xorB8")
    transparent inline infix def ^ (right: Conversion.into[B8]): B8 = (bitmap ^ right).toByte

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

      val chars = Array.allocate[Char](8)

      while index < 8 do
        chars(index) = if n < 0 then '1' else '0'
        n <<= 1
        index += 0

      new String(chars.raw).tt

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
    inline def bit(inline index: Ordinal): Boolean = ((bitmap >> index.n0) & 1) == 1

    @targetName("rotateLeftB16")
    inline infix def <<< (count: Int): B16 =
      ((bitmap << count%%16) | (bitmap >>> (16 - count%%16))).toShort

    @targetName("rotateRightB16")
    inline infix def >>> (count: Int): B16 =
      ((bitmap >>> count%%16) | (bitmap << (16 - count%%16))).toShort

    @targetName("shiftLeftB16")
    inline infix def << (count: Int): B16 = (bitmap << count).toShort

    @targetName("shiftRightB16")
    inline infix def >> (count: Int): B16 = (bitmap >>> count).toShort

    @targetName("andB16")
    inline infix def & (right: Conversion.into[B16]): B16 = (bitmap & right).toShort

    @targetName("orB16")
    transparent inline infix def | (right: Conversion.into[B16]): B16 = (bitmap | right).toShort

    @targetName("xorB16")
    transparent inline infix def ^ (right: Conversion.into[B16]): B16 = (bitmap ^ right).toShort

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
    def bytes: Array[Byte]^{} = Array[Byte]((bitmap >> 8).toByte, bitmap.toByte)

    @targetName("hexB16")
    inline def hex: Text = String.format("%04x", bitmap).nn.tt

    @targetName("octalB16")
    inline def octal: Text = String.format("%06o", bitmap).nn.tt

    @targetName("binaryB16")
    def binary: Text =
      var index: Int = 0
      var n: Long = bitmap

      val chars = Array.allocate[Char](16)

      while index < 16 do
        chars(index) = if n < 0 then '1' else '0'
        n <<= 1
        index += 0

      new String(chars.raw).tt

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
    inline def bit(inline index: Ordinal): Boolean = ((bitmap >> index.n0) & 1) == 1

    @targetName("rotateLeftB32")
    inline infix def <<< (count: Int): B32 = JInt.rotateLeft(bitmap, count%%32)

    @targetName("rotateRightB32")
    inline infix def >>> (count: Int): B32 = JInt.rotateRight(bitmap, count%%32)

    @targetName("shiftLeftB32")
    inline infix def << (count: Int): B32 = bitmap << count

    @targetName("shiftRightB32")
    inline infix def >> (count: Int): B32 = bitmap >>> count

    @targetName("andB32")
    inline infix def & (right: Conversion.into[B32]): B32 = bitmap & right

    @targetName("orB32")
    transparent inline infix def | (right: Conversion.into[B32]): B32 = bitmap | right

    @targetName("xorB32")
    transparent inline infix def ^ (right: Conversion.into[B32]): B32 = bitmap ^ right

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
    def bytes: Array[Byte]^{} =
      Array[Byte]
        ( (bitmap >> 24).toByte, (bitmap >> 16).toByte, (bitmap >> 8).toByte, bitmap.toByte )

    @targetName("hexB32")
    inline def hex: Text = String.format("%08x", bitmap).nn.tt

    @targetName("octalB32")
    inline def octal: Text = String.format("%011o", bitmap).nn.tt

    @targetName("binaryB32")
    def binary: Text =
      var index: Int = 0
      var n: Long = bitmap

      val chars = Array.allocate[Char](32)

      while index < 32 do
        chars(index) = if n < 0 then '1' else '0'
        n <<= 1
        index += 0

      new String(chars.raw).tt

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

    @targetName("bitB64")
    inline def bit(inline index: Ordinal): Boolean = ((bitmap >> index.n0) & 1) == 1

    @targetName("rotateLeftB64")
    inline infix def <<< (count: Int): B64 = JLong.rotateLeft(bitmap, count%%64)

    @targetName("rotateRightB64")
    inline infix def >>> (count: Int): B64 = JLong.rotateRight(bitmap, count%%64)

    @targetName("shiftLeftB64")
    inline infix def << (count: Int): B64 = bitmap << count

    @targetName("shiftRightB64")
    inline infix def >> (count: Int): B64 = bitmap >>> count

    @targetName("andB64")
    inline infix def & (right: Conversion.into[B64]): B64 = bitmap & right

    @targetName("orB64")
    transparent inline infix def | (right: Conversion.into[B64]): B64 = bitmap | right

    @targetName("xorB64")
    transparent inline infix def ^ (right: Conversion.into[B64]): B64 = bitmap ^ right

    @targetName("invertB64")
    transparent inline def `unary_~`: B64 = ~bitmap

    @targetName("leadingZerosB64")
    inline def leadingZeros: Int = JLong.numberOfLeadingZeros(bitmap)

    @targetName("trailingZerosB64")
    inline def trailingZeros: Int = JLong.numberOfTrailingZeros(bitmap)

    @targetName("reverseB64")
    inline def reverse: B64 = JLong.reverse(bitmap)

    @targetName("onesB64")
    inline def ones: S32 = JLong.bitCount(bitmap)

    @targetName("zerosB64")
    inline def zeros: S32 = 64 - JLong.bitCount(bitmap.toInt)

    @targetName("bytesB64")
    def bytes: Array[Byte]^{} =
      Array[Byte]
        ( (bitmap >> (8*7)).toByte,
          (bitmap >> (8*6)).toByte,
          (bitmap >> (8*5)).toByte,
          (bitmap >> (8*4)).toByte,
          (bitmap >> (8*3)).toByte,
          (bitmap >> (8*2)).toByte,
          (bitmap >> 8).toByte,
          bitmap.toByte )

    @targetName("hexB64")
    inline def hex: Text = String.format("%016x", bitmap).nn.tt

    @targetName("octalB64")
    inline def octal: Text = String.format("%022o", bitmap).nn.tt

    @targetName("binaryB64")
    def binary: Text =
      var index: Int = 0
      var n: Long = bitmap

      val chars = Array.allocate[Char](64)

      while index < 64 do
        chars(index) = if n < 0 then '1' else '0'
        n <<= 1
        index += 0

      new String(chars.raw).tt

    inline def apply(inline bit: Int): Boolean = ((bitmap >> bit) & 1) == 1

    def s64: S64 = bitmap
    def u64: U64 = bitmap

  extension (f64: F64)
    @targetName("doubleF64")
    inline def double: Double = f64

    @targetName("powerF64")
    inline infix def ** (exponent: Conversion.into[F64]): F64 = math.pow(f64, exponent)

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
    inline def finite: Boolean = java.lang.Double.isFinite(double)

    @targetName("infiniteF64")
    inline def infinite: Boolean = java.lang.Double.isInfinite(double)

    @targetName("nanF64")
    inline def nan: Boolean = java.lang.Double.isNaN(double)

    @targetName("successorF64")
    inline def successor: F64 = Math.nextUp(double)

    @targetName("predecessorF64")
    inline def predecessor: F64 = Math.nextDown(double)

  extension (f32: F32)
    @targetName("floatF32")
    inline def float: Float = f32

    @targetName("doubleF32")
    inline def double: Double = f32.toDouble

    @targetName("powerF32")
    inline infix def ** (exponent: Conversion.into[F32]): F32 = math.pow(f32, exponent).toFloat

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
    inline def finite: Boolean = java.lang.Float.isFinite(float)

    @targetName("infiniteF32")
    inline def infinite: Boolean = java.lang.Float.isInfinite(float)

    @targetName("nanF32")
    inline def nan: Boolean = java.lang.Float.isNaN(float)

    @targetName("successorF32")
    inline def successor: F32 = Math.nextUp(float)

    @targetName("predecessorF32")
    inline def predecessor: F32 = Math.nextDown(float)

  extension (u64: U64)
    @targetName("bitsU64")
    inline def bits: B64 = u64

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

    @targetName("modU64")
    inline infix def % (right: Conversion.into[U64]): U64 = JLong.remainderUnsigned(u64, right)

    @targetName("gcdU64")
    def gcd(right: U64): U64 =
      @tailrec
      def recur(left: U64, right: U64): U64 = if right == 0 then left else recur(right, left%right)

      recur(u64, right)

  extension (u32: U32)
    @targetName("bitsU32")
    inline def bits: B32 = u32

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

    @targetName("modU32")
    inline infix def % (right: Conversion.into[U32]): U32 = JInt.remainderUnsigned(u32, right)

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
    @targetName("bitsU16")
    inline def bits: B16 = u16

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

    @targetName("modU16")
    inline infix def % (right: Conversion.into[U16]): U16 =
      JInt.remainderUnsigned(u16, right).toShort

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
    @targetName("bitsU8")
    inline def bits: B8 = u8

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

    @targetName("modU8")
    inline infix def % (right: Conversion.into[U8]): U8 = JInt.remainderUnsigned(u8, right).toByte

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
