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

object Addable:
  def apply[augend, addend, result](lambda: (augend, addend) -> result)
  :   augend is Addable by addend to result =

    new Addable:

      type Self = augend
      type Result = result
      type Operand = addend

      def add(augend: augend, addend: addend): result = lambda(augend, addend)


  // The primitive instances are objects, named by their singleton types: an operator inlined
  // through one of them resolves `add` to the `inline def` here, so `a + b` on two `Double`s
  // compiles to the one instruction, with no instance and no boxing (hypotenuse's
  // `CheckOverflow` explains the mechanism). Through the trait type — a `using` parameter of a
  // non-inline method — they still dispatch, as any instance does.
  object DoubleAddition extends Addable:
    type Self = Double
    type Operand = Double
    type Result = Double

    inline def add(augend: Double, addend: Double): Double = augend + addend

  object FloatAddition extends Addable:
    type Self = Float
    type Operand = Float
    type Result = Float

    inline def add(augend: Float, addend: Float): Float = augend + addend

  object LongAddition extends Addable:
    type Self = Long
    type Operand = Long
    type Result = Long

    inline def add(augend: Long, addend: Long): Long = augend + addend

  object IntAddition extends Addable:
    type Self = Int
    type Operand = Int
    type Result = Int

    inline def add(augend: Int, addend: Int): Int = augend + addend

  object IntDoubleAddition extends Addable:
    type Self = Int
    type Operand = Double
    type Result = Double

    inline def add(augend: Int, addend: Double): Double = augend + addend

  object DoubleIntAddition extends Addable:
    type Self = Double
    type Operand = Int
    type Result = Double

    inline def add(augend: Double, addend: Int): Double = augend + addend

  object ShortAddition extends Addable:
    type Self = Short
    type Operand = Short
    type Result = Short

    inline def add(augend: Short, addend: Short): Short = (augend + addend).toShort

  object ByteAddition extends Addable:
    type Self = Byte
    type Operand = Byte
    type Result = Byte

    inline def add(augend: Byte, addend: Byte): Byte = (augend + addend).toByte

  inline given double: DoubleAddition.type = DoubleAddition
  inline given float: FloatAddition.type = FloatAddition
  inline given long: LongAddition.type = LongAddition
  inline given int: IntAddition.type = IntAddition
  inline given int2: IntDoubleAddition.type = IntDoubleAddition
  inline given int3: DoubleIntAddition.type = DoubleIntAddition
  inline given short: ShortAddition.type = ShortAddition
  inline given byte: ByteAddition.type = ByteAddition

  given concatenable: [left, right] => (concatenable: left is Concatenable by right)
  =>  left is Addable by right to concatenable.Result =
    (left, right) => concatenable.concat(left, right)

trait Addable extends Typeclass, Operable, Resultant:
  type Augend = Self
  type Addend = Operand

  def add(augend: Augend, addend: Addend): Result
