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

import gossamer.*
import prepositional.*

package currencyStyles:
  given localCurrencyStyle: CurrencyStyle =
    (code, symbol, unit, subunit) => t"$symbol$unit.$subunit"

  given genericCurrencyStyle: CurrencyStyle =
    (code, symbol, unit, subunit) => t"$unit.$subunit $code"

extension (inline context: StringContext)
  inline def isin(): Isin = ${plutocrat.internal.interpolator('context)}

package currencies:
  // These are listed as the "most traded currencies" on Wikipedia
  // Source: https://en.wikipedia.org/wiki/Template:Most_traded_currencies

  given usd: ("USD" is Currency of "US Dollar" in "$" over 100) = Currency()
  given eur: ("EUR" is Currency of "Euro" in "€" over 100) = Currency()
  given jpy: ("JPY" is Currency of "Japanese Yen" in "¥" over 100) = Currency()
  given gbp: ("GBP" is Currency of "Pounds Sterling" in "£" over 100) = Currency()
  given cny: ("CNY" is Currency of "Renminbi" in "¥" over 100) = Currency()
  given aud: ("AUD" is Currency of "Australian Dollar" in "A$" over 100) = Currency()
  given cad: ("CAD" is Currency of "Canadian Dollar" in "C$" over 100) = Currency()
  given chf: ("CHF" is Currency of "Swiss Franc" in "CHF" over 100) = Currency()
  given hkd: ("HKD" is Currency of "Hong Kong Dollar" in "HK$" over 100) = Currency()
  given sgd: ("SGD" is Currency of "Singapore Dollar" in "S$" over 100) = Currency()
  given sek: ("SEK" is Currency of "Swedish Krona" in "kr" over 100) = Currency()
  given krw: ("KRW" is Currency of "South Korean Won" in "₩" over 100) = Currency()
  given nok: ("NOK" is Currency of "Norwegian Krone" in "kr" over 100) = Currency()
  given nzd: ("NZD" is Currency of "New Zealand Dollar" in "NZ$" over 100) = Currency()
  given inr: ("INR" is Currency of "Indian Rupee" in "₹" over 100) = Currency()
  given mxn: ("MXN" is Currency of "Mexican Peso" in "$" over 100) = Currency()
  given twd: ("TWD" is Currency of "New Taiwan Dollar" in "NT$" over 100) = Currency()
  given zar: ("ZAR" is Currency of "South African Rand" in "R" over 100) = Currency()
  given brl: ("BRL" is Currency of "Brazilian Real" in "R$" over 100) = Currency()
  given dkk: ("DKK" is Currency of "Danish Krone" in "kr" over 100) = Currency()
  given pln: ("PLN" is Currency of "Polish Złoty" in "zł" over 100) = Currency()
  given thb: ("THB" is Currency of "Thai Baht" in "฿" over 100) = Currency()
  given ils: ("ILS" is Currency of "Israeli New Shekel" in "₪" over 100) = Currency()
  given idr: ("IDR" is Currency of "Indonesian Rupiah" in "Rp" over 100) = Currency()
  given czk: ("CZK" is Currency of "Czech Koruna" in "Kč" over 100) = Currency()
  given aed: ("AED" is Currency of "United Arab Emirates Dirham" in "AED" over 100) = Currency()
  given `try`: ("TRY" is Currency of "Turkish Lira" in "₺" over 100) = Currency()
  given huf: ("HUF" is Currency of "Hungarian Forint" in "Ft" over 100) = Currency()
  given clp: ("CLP" is Currency of "Chilean Peso" in "$" over 100) = Currency()
  given sar: ("SAR" is Currency of "Saudi Riyal" in "SR" over 100) = Currency()
  given php: ("PHP" is Currency of "Philippine Peso" in "₱" over 100) = Currency()
  given myr: ("MYR" is Currency of "Malaysian Ringgit" in "RM" over 100) = Currency()
  given cop: ("COP" is Currency of "Colombian Peso" in "$$" over 100) = Currency()
  given rub: ("RUB" is Currency of "Russian Ruble" in "₽" over 100) = Currency()
  given ron: ("RON" is Currency of "Romanian Leu" in "lei" over 100) = Currency()
  given pen: ("PEN" is Currency of "Peruvian Sol" in "S/" over 100) = Currency()

export plutocrat.internal.{Money, Isin}
