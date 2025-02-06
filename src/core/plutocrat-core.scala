/*
    Plutocrat, version [unreleased]. Copyright 2025 Jon Pretty, Propensive OÜ.

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

package currencyStyles:
  given local: CurrencyStyle = (currency, unit, subunit) => t"${currency.symbol}$unit.$subunit"
  given generic: CurrencyStyle = (currency, unit, subunit) => t"$unit.$subunit ${currency.isoCode}"

package currencies:
  type Eur = Eur.type
  object Eur extends Currency(t"EUR", t"€",    t"Euro",               100)

  type Usd = Usd.type
  object Usd extends Currency(t"USD", t"$$",   t"US Dollar",          100)

  type Jpy = Jpy.type
  object Jpy extends Currency(t"JPY", t"¥",    t"Japanese Yen",       100)

  type Gbp = Gbp.type
  object Gbp extends Currency(t"GBP", t"£",    t"Pounds Sterling",    100)

  type Cny = Cny.type
  object Cny extends Currency(t"CNY", t"¥",    t"Renminbi",           100)

  type Aud = Aud.type
  object Aud extends Currency(t"AUD", t"A$$",  t"Australian Dollar",  100)

  type Cad = Cad.type
  object Cad extends Currency(t"CAD", t"C$$",  t"Canadian Dollar",    100)

  type Chf = Chf.type
  object Chf extends Currency(t"CHF", t"CHF",  t"Swiss Franc",        100)

  type Hkd = Hkd.type
  object Hkd extends Currency(t"HKD", t"HK$$", t"Hong Kong Dollar",   100)

  type Sgd = Sgd.type
  object Sgd extends Currency(t"SGD", t"S$$",  t"Singapore Dollar",   100)

  type Sek = Sek.type
  object Sek extends Currency(t"SEK", t"kr",   t"Swedish Krona",      100)

  type Krw = Krw.type
  object Krw extends Currency(t"KRW", t"₩",    t"South Korean Won",   100)

  type Nok = Nok.type
  object Nok extends Currency(t"NOK", t"kr",   t"Norwegian Krone",    100)

  type Nzd = Nzd.type
  object Nzd extends Currency(t"NZD", t"NZ$$", t"New Zealand Dollar", 100)

  type Inr = Inr.type
  object Inr extends Currency(t"INR", t"₹",    t"Indian Rupee",       100)

  type Mxn = Mxn.type
  object Mxn extends Currency(t"MXN", t"$$",   t"Mexican Peso",       100)

export Plutocrat.Money

extension [CurrencyType <: Currency & Singleton: ValueOf](seq: Iterable[Money[CurrencyType]])
  def total: Money[CurrencyType] =
    def recur(seq: Iterable[Money[CurrencyType]], total: Money[CurrencyType]): Money[CurrencyType] =
      if seq.isEmpty then total else recur(seq.tail, total + seq.head)

    val currency: CurrencyType = summon[ValueOf[CurrencyType]].value
    recur(seq, currency.zero)
