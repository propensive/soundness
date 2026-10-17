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

object Negatable:
  def apply[operand, result](lambda: operand -> result): operand is Negatable to result =
    new Negatable:
      type Self = operand
      type Result = result

      def negate(operand: Self): Result = lambda(operand)

  // Objects named by their singleton types, as `Addable`'s: `-a` inlines to one instruction.
  object DoubleNegation extends Negatable:
    type Self = Double
    type Result = Double

    inline def negate(operand: Double): Double = -operand

  object FloatNegation extends Negatable:
    type Self = Float
    type Result = Float

    inline def negate(operand: Float): Float = -operand

  object LongNegation extends Negatable:
    type Self = Long
    type Result = Long

    inline def negate(operand: Long): Long = -operand

  object IntNegation extends Negatable:
    type Self = Int
    type Result = Int

    inline def negate(operand: Int): Int = -operand

  object ShortNegation extends Negatable:
    type Self = Short
    type Result = Short

    inline def negate(operand: Short): Short = (-operand).toShort

  object ByteNegation extends Negatable:
    type Self = Byte
    type Result = Byte

    inline def negate(operand: Byte): Byte = (-operand).toByte

  inline given double: DoubleNegation.type = DoubleNegation
  inline given float: FloatNegation.type = FloatNegation
  inline given long: LongNegation.type = LongNegation
  inline given int: IntNegation.type = IntNegation
  inline given short: ShortNegation.type = ShortNegation
  inline given byte: ByteNegation.type = ByteNegation

trait Negatable extends Typeclass.Pure, Resultant:
  def negate(operand: Self): Result
