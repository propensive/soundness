/*
    Plutocrat, version [unreleased]. Copyright -23 Jon Pretty, Propensive OÜ.

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

object Eur extends Currency("EUR", "€", "Euro", "Cent", 100)
object Usd extends Currency("USD", "$", "US Dollar", "Cent", 100)

object PlutocratOpaques:
  opaque type Money[CurrencyType <: Currency & Singleton] = Long

  object Money:
    def apply(currency: Currency & Singleton)(wholePart: Long, fractional: Int): Money[currency.type] =
      wholePart*currency.modulo + fractional

    given [CurrencyType <: Currency & Singleton: ValueOf]: Show[Money[CurrencyType]] = money =>
      val currency = valueOf[CurrencyType]
      val integral = money/currency.modulo
      val fractional = money%currency.modulo
        
      t"${currency.symbol+integral}.${fractional.toString.show.pad(2, Rtl, '0')}"

export PlutocratOpaques.Money
