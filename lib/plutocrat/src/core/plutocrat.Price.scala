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
package plutocrat

import prepositional.*
import symbolism.*

object Price:
  given [currency <: Label, price <: Price in currency] => price is Divisible:
    type Self = price
    type Operand = Double
    type Result = Price in currency

    def divide(left: price, right: Double): Price in currency =
      Price(left.principal/right, left.tax/right)

  given addable: [currency <: Label, price <: Price in currency] => price is Addable:
    type Operand = Price in currency
    type Result = Price in currency

    def add(left: price, right: Price in currency): Price in currency =
      Price(left.principal + right.principal, left.tax + right.tax)

  given subtractable: [currency <: Label, price <: Price in currency] => price is Subtractable:
    type Operand = Price in currency
    type Result = Price in currency

    def subtract(left: price, right: Price in currency): Price in currency =
      Price(left.principal - right.principal, left.tax - right.tax)

  given negatable: [currency <: Label, price <: Price in currency] => price is Negatable:
    type Result = Price in currency
    def negate(price: price): Price in currency = Price(-price.principal, -price.tax)


  def apply[currency <: Label](principal0: Money in currency, tax0: Money in currency)
  :   Price in currency =

    new Price:
      type Form = currency
      val principal = principal0
      val tax = tax0

trait Price:
  type Form <: Label

  val principal: Money in Form
  val tax: Money in Form

  def effectiveTaxRate: Double = tax/principal

  def inclusive: Money in Form = principal + tax
  override def hashCode(): Int = (principal.asInstanceOf[Long] ^ tax.asInstanceOf[Long]*31).hashCode

  override def toString(): String =
    s"Price(${principal.value} ${principal.currency}, ${tax.value} ${tax.currency})"

  override def equals(that: Any): Boolean = that match
    case that: Price => principal == that.principal && tax == that.tax
    case _           => false
