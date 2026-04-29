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

import soundness.*

import currencies.{Gbp, Eur}

object Tests extends Suite(m"Plutocrat tests"):
  def run(): Unit =
    suite(m"Money tests"):
      test(m"Show a local monetary value"):
        import currencyStyles.local
        val amount: Money in "EUR" = Eur(3.01)
        t"Received $amount"

      . assert(_ == t"Received €3.01")

      test(m"Type error for different currencies"):
        demilitarize:
          val amount: Money in "EUR" = Gbp(3.01)

      . assert(!_.nil)

      test(m"No type error for unspecified currency"):
        demilitarize:
          val amount: Money = Gbp(3.01)

      . assert(_.nil)

      test(m"Currency is recoverable from runtime"):
        val amount: Money = Gbp(3.01)
        amount.currency

      . assert(_ == "GBP")

      test(m"Show a monetary value"):
        import currencyStyles.generic
        val amount = Eur(3.01)
        t"Received $amount"

      . assert(_ == t"Received 3.01 EUR")

      test(m"Add two amounts"):
        Eur(3.01) + Eur(0.02)

      . assert(_ == Eur(3.03))

      test(m"Subtract an amount"):
        Eur(3.01) - Eur(0.02)

      . assert(_ == Eur(2.99))

      test(m"Multiply an amount"):
        Eur(3.01)*3.0

      . assert(_ == Eur(9.03))

      test(m"Divide an amount"):
        Eur(3.01)/3

      . assert(_ == Eur(1.00))

      test(m"Split an amount"):
        Eur(3.01).share(3).total

      . assert(_ == Eur(3.01))

      test(m"Different currencies cannot be combined"):
        demilitarize:
          Eur(1.00) + Gbp(1.00)

      . assert(_.map(_.reason) == List(CompileError.Reason.MissingImplicitArgument))

      test(m"Monetary values can be negated"):
        -Eur(1.99)

      . assert(_ == Eur(-1.99))

      test(m"Compare amounts"):
        Eur(1.01) > Eur(2.10)

      . assert(_ == false)

      test(m"Compare equal amounts with >"):
        Eur(1.01) > Eur(1.01)

      . assert(_ == false)

      test(m"Compare equal amounts with >="):
        Eur(1.01) >= Eur(1.01)

      . assert(_ == true)

    suite(m"Price tests"):
      test(m"Construct new price"):
        Gbp(2.30).tax(0.2)

      . assert(_ == Price(Gbp(2.30), Gbp(0.46)))

      test(m"Tax amount is rounded up correctly"):
        Gbp(2.94).tax(0.2)

      . assert(_ == Price(Gbp(2.94), Gbp(0.59)))

      test(m"Prices in different currencies cannot be combined"):
        demilitarize(Eur(1.00).tax(0.175) + Gbp(1.00).tax(0.2))
      . assert(!_.nil)


    suite(m"ISINs"):
      test(m"Luhn check"):
        Luhn.check(17893729974L)
      . assert(_ == true)

      test(m"Luhn check 2"):
        Luhn.check(49927398716L)
      . assert(_ == true)

      test(m"Luhn check 3"):
        Luhn.check("79927398713")
      . assert(_ == true)

      test(m"Create an ISIN and get country code"):
        unsafely(Isin("US0378331005")).countryCode
      . assert(_ == t"US")

      test(m"Create an ISIN and get NSIN"):
        unsafely(Isin("US0378331005")).nsin
      . assert(_ == t"0378331005")

      test(m"Create an ISIN and get it back"):
        unsafely(Isin("US0378331005")).isin
      . assert(_ == t"US0378331005")

      test(m"Create a more complex ISIN and get it back"):
        unsafely(Isin("GB00BH4HKS39")).isin
      . assert(_ == t"GB00BH4HKS39")

      test(m"Compiletime ISIN"):
        isin"GB00BH4HKS39"
      . assert(_ == unsafely(Isin(t"GB00BH4HKS39")))

      test(m"Compiletime ISIN length error"):
        demilitarize(isin"GB00BH4HKS3").map(_.message)
      . assert(_ == List(t"[↯SN-pl/1.3] the ISIN number is not valid because it had length 11, but it should be 12 characters long"))

      test(m"Compiletime ISIN bad country code"):
        demilitarize(isin"5500BH4HKS30").map(_.message)
      . assert(_ == List(t"[↯SN-pl/1.2] the ISIN number is not valid because its country code 55 was not valid"))

      test(m"Compiletime ISIN invalid character"):
        demilitarize(isin"US00Bx4HKS30").map(_.message)
      . assert(_ == List(t"[↯SN-pl/1.1] the ISIN number is not valid because the character x at position 5 is not a digit or uppercase letter"))

      test(m"Compiletime ISIN Luhn check failure"):
        demilitarize(isin"GB00BH4HKS34").map(_.message)
      . assert(_ == List(t"[↯SN-pl/1.4] the ISIN number is not valid because its last digit failed the Luhn check"))
