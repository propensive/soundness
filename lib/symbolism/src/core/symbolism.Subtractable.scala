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

object Subtractable:
  def apply[minuend, subtrahend, result](lambda: (minuend, subtrahend) -> result)
  :   minuend is Subtractable by subtrahend to result =

    new Subtractable:
      type Self = minuend
      type Result = result
      type Operand = subtrahend

      def subtract(minuend: minuend, subtrahend: subtrahend): result =
        lambda(minuend, subtrahend)


  // Objects named by their singleton types, as `Addable`'s: `a - b` inlines to one instruction.
  object DoubleSubtraction extends Subtractable:
    type Self = Double
    type Operand = Double
    type Result = Double

    inline def subtract(minuend: Double, subtrahend: Double): Double = minuend - subtrahend

  object FloatSubtraction extends Subtractable:
    type Self = Float
    type Operand = Float
    type Result = Float

    inline def subtract(minuend: Float, subtrahend: Float): Float = minuend - subtrahend

  object LongSubtraction extends Subtractable:
    type Self = Long
    type Operand = Long
    type Result = Long

    inline def subtract(minuend: Long, subtrahend: Long): Long = minuend - subtrahend

  object IntSubtraction extends Subtractable:
    type Self = Int
    type Operand = Int
    type Result = Int

    inline def subtract(minuend: Int, subtrahend: Int): Int = minuend - subtrahend

  object ShortSubtraction extends Subtractable:
    type Self = Short
    type Operand = Short
    type Result = Short

    inline def subtract(minuend: Short, subtrahend: Short): Short = (minuend - subtrahend).toShort

  object ByteSubtraction extends Subtractable:
    type Self = Byte
    type Operand = Byte
    type Result = Byte

    inline def subtract(minuend: Byte, subtrahend: Byte): Byte = (minuend - subtrahend).toByte

  inline given double: DoubleSubtraction.type = DoubleSubtraction
  inline given float: FloatSubtraction.type = FloatSubtraction
  inline given long: LongSubtraction.type = LongSubtraction
  inline given int: IntSubtraction.type = IntSubtraction
  inline given short: ShortSubtraction.type = ShortSubtraction
  inline given byte: ByteSubtraction.type = ByteSubtraction

trait Subtractable extends Typeclass.Pure, Operable, Resultant:
  type Minuend = Self
  type Subtrahend = Operand

  def subtract(minuend: Minuend, subtrahend: Subtrahend): Result
