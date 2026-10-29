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

import java.lang.{Integer as JInt, Long as JLong}

import contingency.*
import prepositional.*
import symbolism.*

// A policy for the arithmetic of the fixed-width integers: `+`, `-` and `*` on a `U*` or an `S*`
// take their `Addable`, `Subtractable` and `Multiplicable` instances from the policy in scope,
// and `Wrap` is what that policy makes of each result — the bare type, or one that `raises
// Arithmetic.Error`.
//
// Every instance is an `object` reached by a stable path from its given (`U64.addable` is
// `overflow.addU64`), so that the proxies the inliner introduces for them are pure paths, which
// it drops: `a + b` compiles to the one instruction the hardware has for it. An instance
// constructed with `new`, whether an anonymous class or a final one, survives inlining as an
// allocation per operation.
trait CheckOverflow:
  type Wrap[result]

  val addU64: U64 is Addable by U64 to Wrap[U64]
  val addS64: S64 is Addable by S64 to Wrap[S64]
  val addU32: U32 is Addable by U32 to Wrap[U32]
  val addS32: S32 is Addable by S32 to Wrap[S32]
  val addU16: U16 is Addable by U16 to Wrap[U16]
  val addS16: S16 is Addable by S16 to Wrap[S16]
  val addU8: U8 is Addable by U8 to Wrap[U8]
  val addS8: S8 is Addable by S8 to Wrap[S8]

  val subtractU64: U64 is Subtractable by U64 to Wrap[U64]
  val subtractS64: S64 is Subtractable by S64 to Wrap[S64]
  val subtractU32: U32 is Subtractable by U32 to Wrap[U32]
  val subtractS32: S32 is Subtractable by S32 to Wrap[S32]
  val subtractU16: U16 is Subtractable by U16 to Wrap[U16]
  val subtractS16: S16 is Subtractable by S16 to Wrap[S16]
  val subtractU8: U8 is Subtractable by U8 to Wrap[U8]
  val subtractS8: S8 is Subtractable by S8 to Wrap[S8]

  val multiplyU64: U64 is Multiplicable by U64 to Wrap[U64]
  val multiplyS64: S64 is Multiplicable by S64 to Wrap[S64]
  val multiplyU32: U32 is Multiplicable by U32 to Wrap[U32]
  val multiplyS32: S32 is Multiplicable by S32 to Wrap[S32]
  val multiplyU16: U16 is Multiplicable by U16 to Wrap[U16]
  val multiplyS16: S16 is Multiplicable by S16 to Wrap[S16]
  val multiplyU8: U8 is Multiplicable by U8 to Wrap[U8]
  val multiplyS8: S8 is Multiplicable by S8 to Wrap[S8]

object CheckOverflow:
  import Arithmetic.{Addition, Subtraction, Multiplication}

  // Wraps, as the hardware does.
  object Unchecked extends CheckOverflow:
    type Wrap[result] = result

    object addU64 extends Addition[U64, Wrap]:
      inline def add(left: U64, right: U64): U64 = U64((Long(left.bits) + Long(right.bits)).bits)

    object addS64 extends Addition[S64, Wrap]:
      inline def add(left: S64, right: S64): S64 = S64((left.long + right.long).bits)

    object addU32 extends Addition[U32, Wrap]:
      inline def add(left: U32, right: U32): U32 = U32((Int(left.bits) + Int(right.bits)).bits)

    object addS32 extends Addition[S32, Wrap]:
      inline def add(left: S32, right: S32): S32 = S32((left.int + right.int).bits)

    object addU16 extends Addition[U16, Wrap]:
      inline def add(left: U16, right: U16): U16 =
        U16((Short(left.bits) + Short(right.bits)).toShort.bits)

    object addS16 extends Addition[S16, Wrap]:
      inline def add(left: S16, right: S16): S16 = S16((left.short + right.short).toShort.bits)

    object addU8 extends Addition[U8, Wrap]:
      inline def add(left: U8, right: U8): U8 = U8((left.byte + right.byte).toByte.bits)

    object addS8 extends Addition[S8, Wrap]:
      inline def add(left: S8, right: S8): S8 = S8((left.byte + right.byte).toByte.bits)

    object subtractU64 extends Subtraction[U64, Wrap]:
      inline def subtract(left: U64, right: U64): U64 =
        U64((Long(left.bits) - Long(right.bits)).bits)

    object subtractS64 extends Subtraction[S64, Wrap]:
      inline def subtract(left: S64, right: S64): S64 = S64((left.long - right.long).bits)

    object subtractU32 extends Subtraction[U32, Wrap]:
      inline def subtract(left: U32, right: U32): U32 = U32((Int(left.bits) - Int(right.bits)).bits)

    object subtractS32 extends Subtraction[S32, Wrap]:
      inline def subtract(left: S32, right: S32): S32 = S32((left.int - right.int).bits)

    object subtractU16 extends Subtraction[U16, Wrap]:
      inline def subtract(left: U16, right: U16): U16 =
        U16((Short(left.bits) - Short(right.bits)).toShort.bits)

    object subtractS16 extends Subtraction[S16, Wrap]:
      inline def subtract(left: S16, right: S16): S16 =
        S16((left.short - right.short).toShort.bits)

    object subtractU8 extends Subtraction[U8, Wrap]:
      inline def subtract(left: U8, right: U8): U8 = U8((left.byte - right.byte).toByte.bits)

    object subtractS8 extends Subtraction[S8, Wrap]:
      inline def subtract(left: S8, right: S8): S8 = S8((left.byte - right.byte).toByte.bits)

    object multiplyU64 extends Multiplication[U64, Wrap]:
      inline def multiply(left: U64, right: U64): U64 =
        U64((Long(left.bits)*Long(right.bits)).bits)

    object multiplyS64 extends Multiplication[S64, Wrap]:
      inline def multiply(left: S64, right: S64): S64 = S64((left.long*right.long).bits)

    object multiplyU32 extends Multiplication[U32, Wrap]:
      inline def multiply(left: U32, right: U32): U32 = U32((Int(left.bits)*Int(right.bits)).bits)

    object multiplyS32 extends Multiplication[S32, Wrap]:
      inline def multiply(left: S32, right: S32): S32 = S32((left.int*right.int).bits)

    object multiplyU16 extends Multiplication[U16, Wrap]:
      inline def multiply(left: U16, right: U16): U16 =
        U16((Short(left.bits)*Short(right.bits)).toShort.bits)

    object multiplyS16 extends Multiplication[S16, Wrap]:
      inline def multiply(left: S16, right: S16): S16 =
        S16((left.short*right.short).toShort.bits)

    object multiplyU8 extends Multiplication[U8, Wrap]:
      inline def multiply(left: U8, right: U8): U8 = U8((left.byte*right.byte).toByte.bits)

    object multiplyS8 extends Multiplication[S8, Wrap]:
      inline def multiply(left: S8, right: S8): S8 = S8((left.byte*right.byte).toByte.bits)

  // Raises `Arithmetic.Error` where the result does not fit. The detection is exact: a signed
  // sum overflows when both operands have the sign the sum lacks; an unsigned sum, when it
  // comes out below either operand; a product, when its high word is not the sign extension
  // of its low word (or, for the narrower widths, when the product computed in a wider type
  // falls outside the range).
  object Checked extends CheckOverflow:
    // A context function that captures nothing, rather than `result raises Arithmetic.Error`
    // (which is one that may capture anything): under capture checking, an implementation of
    // `Addable#add` whose result captures is freshened and no longer implements it. The pure
    // form is a subtype of the `raises` one, so `a + b` still satisfies `S32 raises
    // Arithmetic.Error`, `safely(a + b)` and the rest.
    type Wrap[result] = Tactic[Arithmetic.Error]^ ?->{} result

    private inline def overflow(): Nothing raises Arithmetic.Error =
      abort(Arithmetic.Error(Arithmetic.Error.Reason.Overflow))

    object addU64 extends Addition[U64, Wrap]:
      inline def add(left: U64, right: U64): Result =
        val sum: Long = Long(left.bits) + Long(right.bits)
        if JLong.compareUnsigned(sum, Long(left.bits)) < 0 then overflow() else U64(sum.bits)

    object addS64 extends Addition[S64, Wrap]:
      inline def add(left: S64, right: S64): Result =
        val sum: Long = left.long + right.long
        if ((left.long^sum) & (right.long^sum)) < 0L then overflow() else S64(sum.bits)

    object addU32 extends Addition[U32, Wrap]:
      inline def add(left: U32, right: U32): Result =
        val sum: Long = left.long + right.long
        if sum > 0xffffffffL then overflow() else U32(sum.toInt.bits)

    object addS32 extends Addition[S32, Wrap]:
      inline def add(left: S32, right: S32): Result =
        val sum: Int = left.int + right.int
        if ((left.int^sum) & (right.int^sum)) < 0 then overflow() else S32(sum.bits)

    object addU16 extends Addition[U16, Wrap]:
      inline def add(left: U16, right: U16): Result =
        val sum: Int = left.int + right.int
        if sum > 0xffff then overflow() else U16(sum.toShort.bits)

    object addS16 extends Addition[S16, Wrap]:
      inline def add(left: S16, right: S16): Result =
        val sum: Int = left.short + right.short
        if sum < Short.MinValue || sum > Short.MaxValue then overflow() else S16(sum.toShort.bits)

    object addU8 extends Addition[U8, Wrap]:
      inline def add(left: U8, right: U8): Result =
        val sum: Int = left.int + right.int
        if sum > 0xff then overflow() else U8(sum.toByte.bits)

    object addS8 extends Addition[S8, Wrap]:
      inline def add(left: S8, right: S8): Result =
        val sum: Int = left.byte + right.byte
        if sum < Byte.MinValue || sum > Byte.MaxValue then overflow() else S8(sum.toByte.bits)

    object subtractU64 extends Subtraction[U64, Wrap]:
      inline def subtract(left: U64, right: U64): Result =
        if JLong.compareUnsigned(Long(left.bits), Long(right.bits)) < 0 then overflow()
        else U64((Long(left.bits) - Long(right.bits)).bits)

    object subtractS64 extends Subtraction[S64, Wrap]:
      inline def subtract(left: S64, right: S64): Result =
        val difference: Long = left.long - right.long

        if ((left.long^right.long) & (left.long^difference)) < 0L then overflow()
        else S64(difference.bits)

    object subtractU32 extends Subtraction[U32, Wrap]:
      inline def subtract(left: U32, right: U32): Result =
        if JInt.compareUnsigned(Int(left.bits), Int(right.bits)) < 0 then overflow()
        else U32((Int(left.bits) - Int(right.bits)).bits)

    object subtractS32 extends Subtraction[S32, Wrap]:
      inline def subtract(left: S32, right: S32): Result =
        val difference: Int = left.int - right.int

        if ((left.int^right.int) & (left.int^difference)) < 0 then overflow()
        else S32(difference.bits)

    object subtractU16 extends Subtraction[U16, Wrap]:
      inline def subtract(left: U16, right: U16): Result =
        val difference: Int = left.int - right.int
        if difference < 0 then overflow() else U16(difference.toShort.bits)

    object subtractS16 extends Subtraction[S16, Wrap]:
      inline def subtract(left: S16, right: S16): Result =
        val difference: Int = left.short - right.short

        if difference < Short.MinValue || difference > Short.MaxValue then overflow()
        else S16(difference.toShort.bits)

    object subtractU8 extends Subtraction[U8, Wrap]:
      inline def subtract(left: U8, right: U8): Result =
        val difference: Int = left.int - right.int
        if difference < 0 then overflow() else U8(difference.toByte.bits)

    object subtractS8 extends Subtraction[S8, Wrap]:
      inline def subtract(left: S8, right: S8): Result =
        val difference: Int = left.byte - right.byte

        if difference < Byte.MinValue || difference > Byte.MaxValue then overflow()
        else S8(difference.toByte.bits)

    object multiplyU64 extends Multiplication[U64, Wrap]:
      inline def multiply(left: U64, right: U64): Result =
        if Math.unsignedMultiplyHigh(Long(left.bits), Long(right.bits)) != 0L then overflow()
        else U64((Long(left.bits)*Long(right.bits)).bits)

    object multiplyS64 extends Multiplication[S64, Wrap]:
      inline def multiply(left: S64, right: S64): Result =
        val product: Long = left.long*right.long

        if Math.multiplyHigh(left.long, right.long) != (product >> 63) then overflow()
        else S64(product.bits)

    object multiplyU32 extends Multiplication[U32, Wrap]:
      inline def multiply(left: U32, right: U32): Result =
        val product: Long = left.long*right.long
        if product > 0xffffffffL then overflow() else U32(product.toInt.bits)

    object multiplyS32 extends Multiplication[S32, Wrap]:
      inline def multiply(left: S32, right: S32): Result =
        val product: Long = left.long*right.long
        if product != product.toInt.toLong then overflow() else S32(product.toInt.bits)

    object multiplyU16 extends Multiplication[U16, Wrap]:
      inline def multiply(left: U16, right: U16): Result =
        val product: Int = left.int*right.int
        if product > 0xffff then overflow() else U16(product.toShort.bits)

    object multiplyS16 extends Multiplication[S16, Wrap]:
      inline def multiply(left: S16, right: S16): Result =
        val product: Int = left.short*right.short

        if product < Short.MinValue || product > Short.MaxValue then overflow()
        else S16(product.toShort.bits)

    object multiplyU8 extends Multiplication[U8, Wrap]:
      inline def multiply(left: U8, right: U8): Result =
        val product: Int = left.int*right.int
        if product > 0xff then overflow() else U8(product.toByte.bits)

    object multiplyS8 extends Multiplication[S8, Wrap]:
      inline def multiply(left: S8, right: S8): Result =
        val product: Int = left.byte*right.byte

        if product < Byte.MinValue || product > Byte.MaxValue then overflow()
        else S8(product.toByte.bits)
