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

import contingency.*
import prepositional.*
import symbolism.*

// A policy for `/` on the fixed-width integers, the counterpart of `CheckOverflow` (which
// explains why each instance is an `object`).
trait DivisionByZero:
  type Wrap[result]

  val divideU64: U64 is Divisible by U64 to Wrap[U64]
  val divideS64: S64 is Divisible by S64 to Wrap[S64]
  val divideU32: U32 is Divisible by U32 to Wrap[U32]
  val divideS32: S32 is Divisible by S32 to Wrap[S32]
  val divideU16: U16 is Divisible by U16 to Wrap[U16]
  val divideS16: S16 is Divisible by S16 to Wrap[S16]
  val divideU8: U8 is Divisible by U8 to Wrap[U8]
  val divideS8: S8 is Divisible by S8 to Wrap[S8]

object DivisionByZero:
  import Arithmetic.Division

  // Divides as the hardware does, so a zero divisor throws `java.lang.ArithmeticException`.
  object Unchecked extends DivisionByZero:
    type Wrap[result] = result

    object divideU64 extends Division[U64, Wrap]:
      inline def divide(left: U64, right: U64): U64 =
        U64(java.lang.Long.divideUnsigned(Long(left.bits), Long(right.bits)).bits)

    object divideS64 extends Division[S64, Wrap]:
      inline def divide(left: S64, right: S64): S64 = S64((left.long/right.long).bits)

    object divideU32 extends Division[U32, Wrap]:
      inline def divide(left: U32, right: U32): U32 =
        U32(java.lang.Integer.divideUnsigned(Int(left.bits), Int(right.bits)).bits)

    object divideS32 extends Division[S32, Wrap]:
      inline def divide(left: S32, right: S32): S32 = S32((left.int/right.int).bits)

    object divideU16 extends Division[U16, Wrap]:
      inline def divide(left: U16, right: U16): U16 = U16((left.int/right.int).toShort.bits)

    object divideS16 extends Division[S16, Wrap]:
      inline def divide(left: S16, right: S16): S16 = S16((left.short/right.short).toShort.bits)

    object divideU8 extends Division[U8, Wrap]:
      inline def divide(left: U8, right: U8): U8 = U8((left.int/right.int).toByte.bits)

    object divideS8 extends Division[S8, Wrap]:
      inline def divide(left: S8, right: S8): S8 = S8((left.byte/right.byte).toByte.bits)

  // Raises `Arithmetic.Error` on a zero divisor.
  object Checked extends DivisionByZero:
    // Pure, for the reason `CheckOverflow.Checked` gives.
    type Wrap[result] = Tactic[Arithmetic.Error]^ ?->{} result

    private inline def zero(): Nothing raises Arithmetic.Error =
      abort(Arithmetic.Error(Arithmetic.Error.Reason.DivisionByZero))

    object divideU64 extends Division[U64, Wrap]:
      inline def divide(left: U64, right: U64): Result =
        if Long(right.bits) == 0L then zero()
        else U64(java.lang.Long.divideUnsigned(Long(left.bits), Long(right.bits)).bits)

    object divideS64 extends Division[S64, Wrap]:
      inline def divide(left: S64, right: S64): Result =
        if right.long == 0L then zero() else S64((left.long/right.long).bits)

    object divideU32 extends Division[U32, Wrap]:
      inline def divide(left: U32, right: U32): Result =
        if Int(right.bits) == 0 then zero()
        else U32(java.lang.Integer.divideUnsigned(Int(left.bits), Int(right.bits)).bits)

    object divideS32 extends Division[S32, Wrap]:
      inline def divide(left: S32, right: S32): Result =
        if right.int == 0 then zero() else S32((left.int/right.int).bits)

    object divideU16 extends Division[U16, Wrap]:
      inline def divide(left: U16, right: U16): Result =
        if right.int == 0 then zero() else U16((left.int/right.int).toShort.bits)

    object divideS16 extends Division[S16, Wrap]:
      inline def divide(left: S16, right: S16): Result =
        if right.short == 0 then zero() else S16((left.short/right.short).toShort.bits)

    object divideU8 extends Division[U8, Wrap]:
      inline def divide(left: U8, right: U8): Result =
        if right.int == 0 then zero() else U8((left.int/right.int).toByte.bits)

    object divideS8 extends Division[S8, Wrap]:
      inline def divide(left: S8, right: S8): Result =
        if right.byte == 0 then zero() else S8((left.byte/right.byte).toByte.bits)
