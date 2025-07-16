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
┃    Soundness, version 0.39.0.                                                                    ┃
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

import gossamer.*
import symbolism.*

package currencyStyles:
  given local: CurrencyStyle = (currency, unit, subunit) => t"${currency.symbol}$unit.$subunit"
  given generic: CurrencyStyle = (currency, unit, subunit) => t"$unit.$subunit ${currency.isoCode}"

package currencies:
  // These are listed as the "most traded currencies" on Wikipedia
  // Source: https://en.wikipedia.org/wiki/Template:Most_traded_currencies

  type Usd = Usd.type
  object Usd extends Currency(t"USD", t"$$",   t"US Dollar",          100)

  type Eur = Eur.type
  object Eur extends Currency(t"EUR", t"€",    t"Euro",               100)

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

  type Twd = Twd.type
  object Twd extends Currency(t"TWD", t"NT$$", t"New Taiwan Dollar",  100)

  type Zar = Zar.type
  object Zar extends Currency(t"ZAR", t"R",    t"South African Rand", 100)

  type Brl = Brl.type
  object Brl extends Currency(t"BRL", t"R$$",  t"Brazilian Real",     100)

  type Dkk = Dkk.type
  object Dkk extends Currency(t"DKK", t"kr",   t"Danish Krone",       100)

  type Pln = Pln.type
  object Pln extends Currency(t"PLN", t"zł",   t"Polish Złoty",       100)

  type Thb = Thb.type
  object Thb extends Currency(t"THB", t"฿",    t"Thai Baht",          100)

  type Ils = Ils.type
  object Ils extends Currency(t"ILS", t"₪",    t"Israeli New Shekel", 100)

  type Idr = Idr.type
  object Idr extends Currency(t"IDR", t"Rp",   t"Indonesian Rupiah",  100)

  type Czk = Czk.type
  object Czk extends Currency(t"CZK", t"Kč",   t"Czech Koruna",       100)

  type Aed = Aed.type
  object Aed extends Currency(t"AED", t"Dh",   t"UAE Dirham",         100)

  type Try = Try.type
  object Try extends Currency(t"TRY", t"₺",    t"Turkish Lira",       100)

  type Huf = Huf.type
  object Huf extends Currency(t"HUF", t"Ft",   t"Hungarian Forint",   100)

  type Clp = Clp.type
  object Clp extends Currency(t"CLP", t"$$",   t"Chilean Peso",       100)

  type Sar = Sar.type
  object Sar extends Currency(t"SAR", t"SAR",  t"Saudi Ryial",        100)

  type Php = Php.type
  object Php extends Currency(t"PHP", t"₱",    t"Philippine Peso",    100)

  type Myr = Myr.type
  object Myr extends Currency(t"MYR", t"RM",   t"Malaysian Ringgit",  100)

  type Cop = Cop.type
  object Cop extends Currency(t"COP", t"$$",   t"Colombian Peso",     100)

  type Rub = Rub.type
  object Rub extends Currency(t"RUB", t"₽",    t"Russian Ruble",      100)

  type Ron = Ron.type
  object Ron extends Currency(t"RON", t"lei",  t"Romanian Leu",       100)

  type Pen = Pen.type
  object Pen extends Currency(t"PEN", t"S/",   t"Peruvian Sol",       100)

export Plutocrat.Money

extension [currency <: Currency & Singleton: ValueOf](seq: Iterable[Money[currency]])
  def total: Money[currency] =
    def recur(seq: Iterable[Money[currency]], total: Money[currency]): Money[currency] =
      if seq.isEmpty then total else recur(seq.tail, total + seq.head)

    val currency: currency = summon[ValueOf[currency]].value
    recur(seq, currency.zero)
