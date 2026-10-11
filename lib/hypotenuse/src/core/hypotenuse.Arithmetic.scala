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

import anticipation.*
import fulminate.*
import prepositional.*
import symbolism.*

// The failure of an arithmetic *operation*, as distinct from a value that cannot be
// represented (`Decimal.Error`, `Rational.Error`). Division and overflow are separate
// `Reason`s rather than separate types because they are one concept — an operation with no
// representable result — and `arithmeticOptions.checkedDivision` / `checkedOverflow` remain
// independent imports, so a caller still chooses which checks to switch on.
object Arithmetic:
  object Error:
    enum Reason(val number: Int) extends Clarification:
      case DivisionByZero extends Reason(1)
      case Overflow       extends Reason(2)

    given communicable: Reason is Communicable =
      case Reason.DivisionByZero => m"the divisor was zero"
      case Reason.Overflow       => m"the result exceeded the range of its type"

  case class Error(reason: Arithmetic.Error.Reason)(using Diagnostics)
  extends fulminate.Error(868, reason.number)
    ( m"the arithmetic operation could not be completed because $reason" )

  // The shapes of the fixed-width arithmetic instances: a number with itself, to a result the
  // policy in force wraps. Each policy implements them as `object`s (see `CheckOverflow`).
  trait Addition[number, wrap[_]] extends Addable:
    type Self = number
    type Operand = number
    type Result = wrap[number]

  trait Subtraction[number, wrap[_]] extends Subtractable:
    type Self = number
    type Operand = number
    type Result = wrap[number]

  trait Multiplication[number, wrap[_]] extends Multiplicable:
    type Self = number
    type Operand = number
    type Result = wrap[number]

  trait Division[number, wrap[_]] extends Divisible:
    type Self = number
    type Operand = number
    type Result = wrap[number]
