/*
    Plutocrat, version [unreleased]. Copyright 2023 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package plutocrat

import gossamer.*
import anticipation.*
import hieroglyph.*, textWidthCalculation.uniform
import spectacular.*
import rudiments.*

import language.experimental.captureChecking

open case class Currency(isoCode: Text, symbol: Text, name: Text, modulus: Int):
  this: Currency =>
  def apply(value: Double): Money[this.type] =
    val integral = value.toLong
    val tweak = (if integral < 0 then -0.5 else 0.5)/modulus
    Money(this)(integral, ((value - integral + tweak)*modulus).toInt)
  
  def zero: Money[this.type] = apply(0.00)

case class Price[CurrencyType <: Currency & Singleton: ValueOf](principal: Money[CurrencyType], tax: Money[CurrencyType]):
  def effectiveTaxRate: Double = tax/principal

  @targetName("add")
  def +(right: Price[CurrencyType]): Price[CurrencyType] = Price(principal + right.principal, tax + right.tax)
  
  @targetName("subtract")
  def -(right: Price[CurrencyType]): Price[CurrencyType] = Price(principal - right.principal, tax - right.tax)
  
  @targetName("negate")
  def unary_- : Price[CurrencyType] = Price(-principal, -tax)
  
  @targetName("multiply")
  def *(right: Double): Price[CurrencyType] = Price(principal*right, tax*right)
  
  @targetName("divide")
  def /(right: Double): Price[CurrencyType] = Price(principal/right, tax/right)

  def inclusive: Money[CurrencyType] = principal + tax

trait CurrencyStyle:
  def format(currency: Currency, unit: Text, subunit: Text): Text

package currencyStyles:
  given local: CurrencyStyle = (currency, unit, subunit) => t"${currency.symbol}$unit.$subunit"
  given generic: CurrencyStyle = (currency, unit, subunit) => t"$unit.$subunit ${currency.isoCode}"

object Plutocrat:
  opaque type Money[+CurrencyType <: Currency & Singleton] = Long

  object Money:
    def apply(currency: Currency & Singleton)(wholePart: Long, subunit: Int): Money[currency.type] =
      wholePart*currency.modulus + subunit
    
    given [CurrencyType <: Currency & Singleton]: Ordering[Money[CurrencyType]] =
      Ordering.Long match
        case ordering: Ordering[Money[CurrencyType]] => ordering

    given [CurrencyType <: Currency & Singleton: ValueOf](using currencyStyle: CurrencyStyle): Show[Money[CurrencyType]] = money =>
      val currency = valueOf[CurrencyType]
      val units = (money/currency.modulus).toString.show
      val subunit = (money%currency.modulus).toString.show.pad(2, Rtl, '0')
      
      currencyStyle.format(currency, units, subunit)
  
  extension [CurrencyType <: Currency & Singleton: ValueOf](left: Money[CurrencyType])

    @targetName("greaterThan")
    def >(right: Money[CurrencyType]): Boolean = (left: Long) > (right: Long)

    @targetName("greaterThanOrEqual")
    def >=(right: Money[CurrencyType]): Boolean = (left: Long) >= (right: Long)

    @targetName("lessThan")
    def <(right: Money[CurrencyType]): Boolean = (left: Long) < (right: Long)

    @targetName("lessThanOrEqual")
    def <=(right: Money[CurrencyType]): Boolean = (left: Long) <= (right: Long)

    @targetName("add")
    def +(right: Money[CurrencyType]): Money[CurrencyType] = left + right
    
    @targetName("subtract")
    def -(right: Money[CurrencyType]): Money[CurrencyType] = left - right
    
    @targetName("multiply")
    def *(right: Int): Money[CurrencyType] = left*right
    
    @targetName("multiply2")
    def *(right: Double): Money[CurrencyType] =
      val value = left*right
      (value + math.signum(value)/2).toLong
    
    @targetName("divide")
    def /(right: Double): Money[CurrencyType] =
      val value = left/right
      (value + math.signum(value)/2).toLong

    @targetName("divide2")
    def /(right: Money[CurrencyType]): Double = left.toDouble/right.toDouble

    @targetName("negate")
    def `unary_-`: Money[CurrencyType] = -left

    def tax(rate: Double): Price[CurrencyType] = Price(left, (left*rate + 0.5).toLong)

    @tailrec
    def split(right: Int, result: List[Money[CurrencyType]] = Nil): List[Money[CurrencyType]] =
      if right == 1 then left :: result else
        val share: Money[CurrencyType] = left/right
        val remainder: Money[CurrencyType] = (left - share)
        remainder.split(right - 1, share :: result)

export Plutocrat.Money

extension [CurrencyType <: Currency & Singleton: ValueOf](seq: Iterable[Money[CurrencyType]])
  def total: Money[CurrencyType] =
    def recur(seq: Iterable[Money[CurrencyType]], total: Money[CurrencyType]): Money[CurrencyType] =
      if seq.isEmpty then total else recur(seq.tail, total + seq.head)
    
    val currency: CurrencyType = summon[ValueOf[CurrencyType]].value
    recur(seq, currency.zero)
