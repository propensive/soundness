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
package plutocrat

import scala.quoted.*

import anticipation.*
import contingency.*
import fulminate.*
import gossamer.*
import hieroglyph.*, textMetrics.uniform
import hypotenuse.*
import prepositional.*
import proscenium.*
import rudiments.*
import spectacular.*
import symbolism.*

object Plutocrat:
  private given realm: Realm = realm"plutocrat"
  opaque type Money = Long
  opaque type Isin = Long

  extension (value: Long) protected def in[currency <: Label]: Money in currency =
    value.asInstanceOf[Money in currency]

  object Isin:
    def apply(isin: Text): Isin raises IsinError =
      if isin.length != 12 then abort(IsinError(IsinError.WrongLength(isin.length))) else
        var result: Long = 0L
        val cc0 = isin.s(0)

        if 'A' <= cc0 <= 'Z' then result |= ((cc0.toLong - 'A') & 0b11111) << 59
        else raise(IsinError(IsinError.BadCountryCode(isin.keep(2))))

        val cc1 = isin.s(1)

        if 'A' <= cc1 <= 'Z' then result |= ((cc1.toLong - 'A') & 0b11111) << 54
        else raise(IsinError(IsinError.BadCountryCode(isin.keep(2))))

        for index <- 2 until 11 do
          val char = isin.s(index)
          if '0' <= char <= '9' || 'A' <= char <= 'Z'
          then result |= ((char.toLong - '0') & 0b111111) << (60 - index*6)
          else raise(IsinError(IsinError.InvalidCharacter(index, char)))

        if result.isin != isin then abort(IsinError(IsinError.LuhnCheck))
        result

  extension (isin: Isin)
    private[Plutocrat] def payload: Text =
      val chars: Array[Char] = new Array(9)
      var numeric = 0L

      for index <- 2 until 11 do
        val char = (isin >>> (60 - index*6)) & 0b111111
        numeric = numeric*10 + char
        chars(index - 2) = (char + '0').toChar

      String(chars).tt

    def countryCode: Text =
      t"${((isin >>> 59) + 'A').toChar}${(((isin >>> 54) & 0b11111) + 'A').toChar}"

    def nsin: Text =
      val data = t"$countryCode$payload".s.flatMap:
        case char if char.isDigit => char.toString
        case letter               => (letter - 55).toString

      t"$payload${Luhn.digit(data)}"

    def isin: Text = t"$countryCode$nsin"

  def interpolator(context: Expr[StringContext]): Macro[Isin] =
    abortive:
      val isin = Isin(context.valueOrAbort.parts.head)
      '{${Expr[Long](isin)}.asInstanceOf[Isin]}

  object Money:
    inline given underlying: [currency <: Label] => Underlying[Money in currency, Int] = !!

    protected[plutocrat] def apply[currency <: Label](currency: Text, value: Long)
    :   Money in currency =

      val c1 = ((currency.s(0) - 'A') & 0b11111L) << 59
      val c2 = ((currency.s(1) - 'A') & 0b11111L) << 54
      val c3 = ((currency.s(2) - 'A') & 0b11111L) << 49

      (value & ((1L << 49) - 1L) | c1 | c2 | c3).asInstanceOf[Money in currency]


    protected[plutocrat] def apply[currency <: Label](currency: Text, value: Double)
    :   Money in currency =

      apply[currency](currency, Math.round(value))


    def apply[currency <: Label: ValueOf](value: Long): Money in currency =
      apply(valueOf[currency], value).in[currency]

    given showable: [currency: Currency] => (currencyStyle: CurrencyStyle)
    =>  Money in currency is Showable =

      money =>
        val units = (money.value/currency.modulus).toString.show
        val subunit = (money.value%currency.modulus).toString.show.pad(2, Rtl, '0')

        currencyStyle.format(currency.code, currency.symbol, units, subunit)

    given addable: [currency <: Label]
    =>  (Money in currency) is Addable by (Money in currency) to (Money in currency) =
      (left, right) =>
        Money(left.currency, left.value + right.value).in[currency]

    given subtractable: [currency <: Label]
    =>  (Money in currency) is Subtractable by (Money in currency) to (Money in currency) =

      (left, right) =>
        Money(left.currency, left.value - right.value).in[currency]

    given multiplicable: [currency <: Label]
    =>  (Money in currency) is Multiplicable by Double to (Money in currency) =

      (left, right) =>
        Money(left.currency, left.value*right).in[currency]

    given divisible: [currency <: Label, money <: (Money in currency)]
    =>  money is Divisible:
      type Self = money
      type Operand = Double
      type Result = Money in currency

      def divide(left: money, right: Double): Money in currency =
        Money(left.currency, left.value/right).in[currency]

    given divisible2: [currency <: Label, left <: Money in currency, right <: Money in currency]
    =>  left is Divisible:

      type Self = left
      type Operand = right
      type Result = Double

      def divide(left: left, right: right): Double = left.value.toDouble/right.value.toDouble


    given divisible3: [currency <: Label, money <: Money in currency] => money is Divisible:
      type Self = money
      type Operand = Int
      type Result = Money in currency

      def divide(left: money, right: Int): Money in currency =
        Money(left.currency, left.value/right).in[currency]


    inline given orderable: [currency <: Label] => (Money in currency) is Orderable:
      inline def compare
        ( inline left:        Money in currency,
          inline right:       Money in currency,
          inline strict:      Boolean,
          inline greaterThan: Boolean )
      :   Boolean =

        if left.value == right.value then !strict else (left.value < right.value)^greaterThan


    given zeroic: [currency <: Label: Currency] => (Money in currency) is Zeroic:
      def zero: Money in currency = Money(currency.code, 0L).in[currency]

    given negatable: [currency <: Label] => (Money in currency) is Negatable:
      type Result = Money in currency

      def negate(money: Money in currency): Money in currency =
        Money(money.currency, -money.value).in[currency]

  extension (money: Money)
    def currency: Text =
      val c1: Char = (((money >> 59) & 0b00011111) + 'A').toChar
      val c2: Char = (((money >> 54) & 0b00011111) + 'A').toChar
      val c3: Char = (((money >> 49) & 0b00011111) + 'A').toChar

      new String(Array(c1, c2, c3)).tt

    def value: Long = (money << 15) >> 15

  extension [currency <: Label: ValueOf](left: Money in currency)
    def tax(rate: Double): Price in currency =
      Price(left, Money(left.currency, left.value*rate).in[currency])

    @tailrec
    def share(right: Int, result: List[Money in currency] = Nil): List[Money in currency] =
      if right == 1 then left :: result else
        val share: Money in currency = (left/right).in[currency]
        val remainder: Money in currency = (left - share).in[currency]
        remainder.share(right - 1, share :: result)
