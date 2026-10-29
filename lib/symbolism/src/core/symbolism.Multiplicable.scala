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

object Multiplicable:
  def apply[multiplicand, multiplier, result](lambda: (multiplicand, multiplier) -> result)
  :   multiplicand is Multiplicable by multiplier to result =

    new Multiplicable:
      type Self = multiplicand
      type Result = result
      type Operand = multiplier

      def multiply(multiplicand: multiplicand, multiplier: multiplier): result =
        lambda(multiplicand, multiplier)


  // Objects named by their singleton types, as `Addable`'s: `a*b` inlines to one instruction.
  object DoubleMultiplication extends Multiplicable:
    type Self = Double
    type Operand = Double
    type Result = Double

    inline def multiply(multiplicand: Double, multiplier: Double): Double = multiplicand*multiplier

  object LongMultiplication extends Multiplicable:
    type Self = Long
    type Operand = Long
    type Result = Long

    inline def multiply(multiplicand: Long, multiplier: Long): Long = multiplicand*multiplier

  object IntMultiplication extends Multiplicable:
    type Self = Int
    type Operand = Int
    type Result = Int

    inline def multiply(multiplicand: Int, multiplier: Int): Int = multiplicand*multiplier

  object FloatMultiplication extends Multiplicable:
    type Self = Float
    type Operand = Float
    type Result = Float

    inline def multiply(multiplicand: Float, multiplier: Float): Float = multiplicand*multiplier

  object ShortMultiplication extends Multiplicable:
    type Self = Short
    type Operand = Short
    type Result = Short

    inline def multiply(multiplicand: Short, multiplier: Short): Short =
      (multiplicand*multiplier).toShort

  object ByteMultiplication extends Multiplicable:
    type Self = Byte
    type Operand = Byte
    type Result = Byte

    inline def multiply(multiplicand: Byte, multiplier: Byte): Byte =
      (multiplicand*multiplier).toByte

  inline given double: DoubleMultiplication.type = DoubleMultiplication
  inline given long: LongMultiplication.type = LongMultiplication
  inline given int: IntMultiplication.type = IntMultiplication
  inline given float: FloatMultiplication.type = FloatMultiplication
  inline given short: ShortMultiplication.type = ShortMultiplication
  inline given byte: ByteMultiplication.type = ByteMultiplication

  given concatenable: [textual: Concatenable by textual to textual: Zeroic]
  =>  textual is Multiplicable:
    type Self = textual
    type Operand = Int
    type Result = textual

    def multiply(text: textual, count: Int): textual =
      var result: textual = textual.zero
      var i = 0

      while i < count do
        i += 1
        result = result+text

      result

trait Multiplicable extends Typeclass.Pure, Operable, Resultant:
  type Multiplicand = Self
  type Multiplier = Operand

  def multiply(multiplicand: Multiplicand, multiplier: Multiplier): Result
