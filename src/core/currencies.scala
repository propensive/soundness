/*
    Plutocrat, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

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

import language.experimental.captureChecking

object Eur extends Currency(t"EUR", t"€", t"Euro", 100)
object Usd extends Currency(t"USD", t"$$", t"US Dollar", 100)
object Jpy extends Currency(t"JPY", t"¥", t"Japanese Yen", 100)
object Gbp extends Currency(t"GBP", t"£", t"Pounds Sterling", 100)
object Cny extends Currency(t"CNY", t"¥", t"Renminbi", 100)
object Aud extends Currency(t"AUD", t"A$$", t"Australian Dollar", 100)
object Cad extends Currency(t"CAD", t"C$$", t"Canadian Dollar", 100)
object Chf extends Currency(t"CHF", t"CHF", t"Swiss Franc", 100)
object Hkd extends Currency(t"HKD", t"HK$$", t"Hong Kong Dollar", 100)
object Sgd extends Currency(t"SGD", t"S$$", t"Singapore Dollar", 100)
object Sek extends Currency(t"SEK", t"kr", t"Swedish Krona", 100)
object Krw extends Currency(t"KRW", t"₩", t"South Korean Won", 100)
object Nok extends Currency(t"NOK", t"kr", t"Norwegian Krone", 100)
object Nzd extends Currency(t"NZD", t"NZ$$", t"New Zealand Dollar", 100)
object Inr extends Currency(t"INR", t"₹", t"Indian Rupee", 100)
object Mxn extends Currency(t"MXN", t"$$", t"Mexican Peso", 100)
