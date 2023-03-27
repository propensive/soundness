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
