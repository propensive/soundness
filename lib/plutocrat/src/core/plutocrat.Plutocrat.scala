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
┃    Soundness, version 0.27.0.                                                                    ┃
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

import anticipation.*
import gossamer.*
import hieroglyph.*, textMetrics.uniform
import hypotenuse.*
import prepositional.*
import proscenium.*
import rudiments.*
import spectacular.*
import symbolism.*

object Plutocrat:
  opaque type Money[+currency <: Currency & Singleton] = Long

  object Money:
    erased given underlying[currency <: Currency & Singleton]
    :     Underlying[Money[currency], Int] =
      !!

    def apply(currency: Currency & Singleton)(wholePart: Long, subunit: Int): Money[currency.type] =
      wholePart*currency.modulus + subunit

    given ordering: [currency <: Currency & Singleton] => Ordering[Money[currency]] =
      Ordering.Long match
        case ordering: Ordering[Money[currency]] => ordering

    given showable: [currency <: Currency & Singleton: ValueOf] => (currencyStyle: CurrencyStyle)
          =>  Money[currency] is Showable =

      money =>
        val currency = valueOf[currency]
        val units = (money/currency.modulus).toString.show
        val subunit = (money%currency.modulus).toString.show.pad(2, Rtl, '0')

        currencyStyle.format(currency, units, subunit)

    given addable: [currency <: Currency & Singleton]
          =>  Money[currency] is Addable by Money[currency] into Money[currency] =
      _ + _

    given subtractable: [currency <: Currency & Singleton]
          =>  Money[currency] is Subtractable by Money[currency] into Money[currency] =
      _ - _

    given multiplicable: [currency <: Currency & Singleton]
          =>  Money[currency] is Multiplicable by Double into Money[currency] =
      (left, right) =>
        val value = left*right
        (value + value.signum/2).toLong

  extension [currency <: Currency & Singleton: ValueOf](left: Money[currency])
    @targetName("greaterThan")
    infix def > (right: Money[currency]): Boolean = (left: Long) > (right: Long)

    @targetName("greaterThanOrEqual")
    infix def >= (right: Money[currency]): Boolean = (left: Long) >= (right: Long)

    @targetName("lessThan")
    infix def < (right: Money[currency]): Boolean = (left: Long) < (right: Long)

    @targetName("lessThanOrEqual")
    infix def <= (right: Money[currency]): Boolean = (left: Long) <= (right: Long)

    @targetName("divide")
    infix def / (right: Double): Money[currency] =
      val value = left/right
      (value + value.signum/2).toLong

    @targetName("divide2")
    infix def / (right: Money[currency]): Double = left.toDouble/right.toDouble

    @targetName("negate")
    def `unary_-`: Money[currency] = -left
    def tax(rate: Double): Price[currency] = Price(left, (left*rate + 0.5).toLong)

    @tailrec
    def split(right: Int, result: List[Money[currency]] = Nil): List[Money[currency]] =
      if right == 1 then left :: result else
        val share: Money[currency] = left/right
        val remainder: Money[currency] = (left - share)
        remainder.split(right - 1, share :: result)
