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

import gossamer.*, textWidthCalculation.uniform
import rudiments.*

case class Currency(isoCode: String, symbol: String, name: String, fractionalName: String, modulo: Int):
  def apply(value: Double): Money[this.type] =
    val integral = value.toLong
    Money(this)(integral, ((value - integral + (0.5/modulo))*modulo).toInt)
  
  def zero: Money[this.type] = apply(0.00)

object Eur extends Currency("EUR", "€", "Euro", "Cent", 100)
object Usd extends Currency("USD", "$", "US Dollar", "Cent", 100)
object Gbp extends Currency("GBP", "£", "Pounds Sterling", "Pence", 100)

object PlutocratOpaques:
  opaque type Money[+CurrencyType <: Currency & Singleton] = Long

  object Money:
    def apply(currency: Currency & Singleton)(wholePart: Long, fractional: Int): Money[currency.type] =
      wholePart*currency.modulo + fractional
    
    given [CurrencyType <: Currency & Singleton]: Ordering[Money[CurrencyType]] =
      (a, b) => if a < b then 1 else if b < a then -1 else 0

    given [CurrencyType <: Currency & Singleton: ValueOf]: Show[Money[CurrencyType]] = money =>
      val currency = valueOf[CurrencyType]
      val integral = money/currency.modulo
      val fractional = money%currency.modulo
        
      t"${currency.symbol+integral}.${fractional.toString.show.pad(2, Rtl, '0')}"
  
  extension [CurrencyType <: Currency & Singleton: ValueOf](left: Money[CurrencyType])
    
    @targetName("add")
    def +(right: Money[CurrencyType]): Money[CurrencyType] = left + right
    
    @targetName("subtract")
    def -(right: Money[CurrencyType]): Money[CurrencyType] = left - right
    
    @targetName("multiply")
    def *(right: Int): Money[CurrencyType] = left*right
    
    @targetName("divide")
    def /(right: Int): Money[CurrencyType] = (left + right/2)/right
    
    @tailrec
    def split(right: Int, result: List[Money[CurrencyType]] = Nil): List[Money[CurrencyType]] =
      if right == 1 then left :: result else
        val share: Money[CurrencyType] = left/right
        val remainder: Money[CurrencyType] = (left - share)
        remainder.split(right - 1, share :: result)

export PlutocratOpaques.Money

extension [CurrencyType <: Currency & Singleton: ValueOf](seq: Iterable[Money[CurrencyType]])
  def total: Money[CurrencyType] =
    def recur(seq: Iterable[Money[CurrencyType]], total: Money[CurrencyType]): Money[CurrencyType] =
      if seq.isEmpty then total else recur(seq.tail, total + seq.head)
    
    val currency: CurrencyType = summon[ValueOf[CurrencyType]].value
    recur(seq, currency.zero)