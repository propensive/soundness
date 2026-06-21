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
┃    Soundness, version 0.54.0.                                                                    ┃
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


  given double: Double is Divisible by Double to Double = Divisible:
    (dividend, divisor) => dividend/divisor

  given double2: Double is Divisible by Int to Double = Divisible:
    (dividend, divisor) => dividend/divisor

  given float: Float is Divisible by Float to Float = Divisible:
    (dividend, divisor) => dividend/divisor

  given float2: Float is Divisible by Int to Float = Divisible:
    (dividend, divisor) => dividend/divisor

  given float3: Float is Divisible by Double to Double = Divisible:
    (dividend, divisor) => dividend/divisor

  given long: Long is Divisible by Long to Long = Divisible:
    (dividend, divisor) => dividend/divisor

  given long2: Long is Divisible by Double to Double = Divisible:
    (dividend, divisor) => dividend/divisor

  given int: Int is Divisible by Int to Int = Divisible:
    (dividend, divisor) => dividend/divisor

  given int2: Int is Divisible by Double to Double = Divisible:
    (dividend, divisor) => dividend/divisor

  given short: Short is Divisible by Short to Short = Divisible:
    (dividend, divisor) => (dividend/divisor).toShort

  given short2: Short is Divisible by Double to Double = Divisible:
    (dividend, divisor) => dividend/divisor

  given byte: Byte is Divisible by Byte to Byte = Divisible:
    (dividend, divisor) => (dividend/divisor).toByte

  given byte2: Byte is Divisible by Double to Double = Divisible:
    (dividend, divisor) => dividend/divisor

trait Divisible extends Typeclass, Operable, Resultant:
  type Dividend = Self
  type Divisor = Operand

  def divide(dividend: Self, divisor: Divisor): Result

  // extension (dividend: Self)
  //   @targetName("divide")
  //   inline infix def / (divisor: Divisor): Result = divide(dividend, divisor)
