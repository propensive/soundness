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
┃    Soundness, version 0.40.0.                                                                    ┃
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

import currencies.*

object Tests extends Suite(m"Plutocrat tests"):
  def run(): Unit =
    suite(m"Money tests"):
      test(m"Show a local monetary value"):
        import currencyStyles.local
        val amount: Money[Eur] = Eur(3.01)
        t"Received $amount"

      . assert(_ == t"Received €3.01")

      test(m"Show a monetary value"):
        import currencyStyles.generic
        val amount = Eur(3.01)
        t"Received $amount"

      . assert(_ == t"Received 3.01 EUR")

      test(m"Add two amounts"):
        Eur(3.01) + Eur(0.02)

      . assert(_ == Eur(3.03))

      test(m"Subtract an amounts"):
        Eur(3.01) - Eur(0.02)

      . assert(_ == Eur(2.99))

      test(m"Multiply an amount"):
        Eur(3.01)*3

      . assert(_ == Eur(9.03))

      test(m"Divide an amount"):
        Eur(3.01)/3

      . assert(_ == Eur(1.00))

      test(m"Split an amount"):
        Eur(3.01).split(3).total

      . assert(_ == Eur(3.01))

      /*test(m"Different currencies cannot be combined"):
        demilitarize:
          Eur(1.00) + Gbp(1.00)
        .map(_.id)

      . assert(_ == List(CompileErrorId.MissingImplicitArgument))*/

      test(m"Monetary values can be negated"):
        -Eur(1.99)

      . assert(_ == Eur(-1.99))

      test(m"Compare amounts"):
        Eur(1.01) > Eur(2.10)

      . assert(_ == false)

      test(m"Compare equal amounts"):
        Eur(1.01) > Eur(1.01)

      . assert(_ == false)

      test(m"Compare equal amounts"):
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
        demilitarize(Eur(1.00).tax(0.175) + Gbp(1.00).tax(0.2)).map(_.message)

      . assert(_ == List(t"Found:    plutocrat.Price[plutocrat.currencies.Gbp.type]\nRequired: plutocrat.Price[plutocrat.currencies.Eur.type]"))
