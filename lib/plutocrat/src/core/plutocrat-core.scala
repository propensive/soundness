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
┃    Soundness, version 0.53.0.                                                                    ┃
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

import denominative.*
import gossamer.*
import prepositional.*
import proscenium.*
import rudiments.*
import symbolism.*

package currencyStyles:
  given local: CurrencyStyle = (code, symbol, unit, subunit) => t"$symbol$unit.$subunit"
  given generic: CurrencyStyle = (code, symbol, unit, subunit) => t"$unit.$subunit $code"

package currencies:
  // These are listed as the "most traded currencies" on Wikipedia
  // Source: https://en.wikipedia.org/wiki/Template:Most_traded_currencies

  given Usd: ("USD" is Currency of "US Dollar" in "$" over 100) = Currency()
  given Eur: ("EUR" is Currency of "Euro" in "€" over 100) = Currency()
  given Jpy: ("JPY" is Currency of "Japanese Yen" in "¥" over 100) = Currency()
  given Gbp: ("GBP" is Currency of "Pounds Sterling" in "£" over 100) = Currency()
  given Cny: ("CNY" is Currency of "Renminbi" in "¥" over 100) = Currency()
  given Aud: ("AUD" is Currency of "Australian Dollar" in "A$" over 100) = Currency()
  given Cad: ("CAD" is Currency of "Canadian Dollar" in "C$" over 100) = Currency()
  given Chf: ("CHF" is Currency of "Swiss Franc" in "CHF" over 100) = Currency()
  given Hkd: ("HKD" is Currency of "Hong Kong Dollar" in "HK$" over 100) = Currency()
  given Sgd: ("SGD" is Currency of "Singapore Dollar" in "S$" over 100) = Currency()
  given Sek: ("SEK" is Currency of "Swedish Krona" in "kr" over 100) = Currency()
  given Krw: ("KRW" is Currency of "South Korean Won" in "₩" over 100) = Currency()
  given Nok: ("NOK" is Currency of "Norwegian Krone" in "kr" over 100) = Currency()
  given Nzd: ("NZD" is Currency of "New Zealand Dollar" in "NZ$" over 100) = Currency()
  given Inr: ("INR" is Currency of "Indian Rupee" in "₹" over 100) = Currency()
  given Mxn: ("MXN" is Currency of "Mexican Peso" in "$" over 100) = Currency()
  given Twd: ("TWD" is Currency of "New Taiwan Dollar" in "NT$" over 100) = Currency()
  given Zar: ("ZAR" is Currency of "South African Rand" in "R" over 100) = Currency()
  given Brl: ("BRL" is Currency of "Brazilian Real" in "R$" over 100) = Currency()
  given Dkk: ("DKK" is Currency of "Danish Krone" in "kr" over 100) = Currency()
  given Pln: ("PLN" is Currency of "Polish Złoty" in "zł" over 100) = Currency()
  given Thb: ("THB" is Currency of "Thai Baht" in "฿" over 100) = Currency()
  given Ils: ("ILS" is Currency of "Israeli New Shekel" in "₪" over 100) = Currency()
  given Idr: ("IDR" is Currency of "Indonesian Rupiah" in "Rp" over 100) = Currency()
  given Czk: ("CZK" is Currency of "Czech Koruna" in "Kč" over 100) = Currency()
  given Aed: ("AED" is Currency of "United Arab Emirates Dirham" in "AED" over 100) = Currency()
  given Try: ("TRY" is Currency of "Turkish Lira" in "₺" over 100) = Currency()
  given Huf: ("HUF" is Currency of "Hungarian Forint" in "Ft" over 100) = Currency()
  given Clp: ("CLP" is Currency of "Chilean Peso" in "$" over 100) = Currency()
  given Sar: ("SAR" is Currency of "Saudi Riyal" in "SR" over 100) = Currency()
  given Php: ("PHP" is Currency of "Philippine Peso" in "₱" over 100) = Currency()
  given Myr: ("MYR" is Currency of "Malaysian Ringgit" in "RM" over 100) = Currency()
  given Cop: ("COP" is Currency of "Colombian Peso" in "$$" over 100) = Currency()
  given Rub: ("RUB" is Currency of "Russian Ruble" in "₽" over 100) = Currency()
  given Ron: ("RON" is Currency of "Romanian Leu" in "lei" over 100) = Currency()
  given Pen: ("PEN" is Currency of "Peruvian Sol" in "S/" over 100) = Currency()

export Plutocrat.Money
