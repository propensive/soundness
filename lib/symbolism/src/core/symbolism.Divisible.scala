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
package symbolism

import prepositional.*

object Divisible:
  def apply[dividend, divisor, result](lambda: (dividend, divisor) -> result)
  :   dividend is Divisible by divisor to result =

    new Divisible:
      type Self = dividend
      type Result = result
      type Operand = divisor

      def divide(dividend: dividend, divisor: divisor): result = lambda(dividend, divisor)


  // Objects named by their singleton types, as `Addable`'s: `a/b` inlines to one instruction.
  object DoubleDivision extends Divisible:
    type Self = Double
    type Operand = Double
    type Result = Double

    inline def divide(dividend: Double, divisor: Double): Double = dividend/divisor

  object DoubleIntDivision extends Divisible:
    type Self = Double
    type Operand = Int
    type Result = Double

    inline def divide(dividend: Double, divisor: Int): Double = dividend/divisor

  object FloatDivision extends Divisible:
    type Self = Float
    type Operand = Float
    type Result = Float

    inline def divide(dividend: Float, divisor: Float): Float = dividend/divisor

  object FloatIntDivision extends Divisible:
    type Self = Float
    type Operand = Int
    type Result = Float

    inline def divide(dividend: Float, divisor: Int): Float = dividend/divisor

  object FloatDoubleDivision extends Divisible:
    type Self = Float
    type Operand = Double
    type Result = Double

    inline def divide(dividend: Float, divisor: Double): Double = dividend/divisor

  object LongDivision extends Divisible:
    type Self = Long
    type Operand = Long
    type Result = Long

    inline def divide(dividend: Long, divisor: Long): Long = dividend/divisor

  object LongDoubleDivision extends Divisible:
    type Self = Long
    type Operand = Double
    type Result = Double

    inline def divide(dividend: Long, divisor: Double): Double = dividend/divisor

  object IntDivision extends Divisible:
    type Self = Int
    type Operand = Int
    type Result = Int

    inline def divide(dividend: Int, divisor: Int): Int = dividend/divisor

  object IntDoubleDivision extends Divisible:
    type Self = Int
    type Operand = Double
    type Result = Double

    inline def divide(dividend: Int, divisor: Double): Double = dividend/divisor

  object ShortDivision extends Divisible:
    type Self = Short
    type Operand = Short
    type Result = Short

    inline def divide(dividend: Short, divisor: Short): Short = (dividend/divisor).toShort

  object ShortDoubleDivision extends Divisible:
    type Self = Short
    type Operand = Double
    type Result = Double

    inline def divide(dividend: Short, divisor: Double): Double = dividend/divisor

  object ByteDivision extends Divisible:
    type Self = Byte
    type Operand = Byte
    type Result = Byte

    inline def divide(dividend: Byte, divisor: Byte): Byte = (dividend/divisor).toByte

  object ByteDoubleDivision extends Divisible:
    type Self = Byte
    type Operand = Double
    type Result = Double

    inline def divide(dividend: Byte, divisor: Double): Double = dividend/divisor

  inline given double: DoubleDivision.type = DoubleDivision
  inline given double2: DoubleIntDivision.type = DoubleIntDivision
  inline given float: FloatDivision.type = FloatDivision
  inline given float2: FloatIntDivision.type = FloatIntDivision
  inline given float3: FloatDoubleDivision.type = FloatDoubleDivision
  inline given long: LongDivision.type = LongDivision
  inline given long2: LongDoubleDivision.type = LongDoubleDivision
  inline given int: IntDivision.type = IntDivision
  inline given int2: IntDoubleDivision.type = IntDoubleDivision
  inline given short: ShortDivision.type = ShortDivision
  inline given short2: ShortDoubleDivision.type = ShortDoubleDivision
  inline given byte: ByteDivision.type = ByteDivision
  inline given byte2: ByteDoubleDivision.type = ByteDoubleDivision

trait Divisible extends Typeclass.Pure, Operable, Resultant:
  type Dividend = Self
  type Divisor = Operand

  def divide(dividend: Self, divisor: Divisor): Result

  // extension (dividend: Self)
  //   @targetName("divide")
  //   inline infix def / (divisor: Divisor): Result = divide(dividend, divisor)
