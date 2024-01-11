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

import probably.*
import gossamer.*
import larceny.*

object Tests extends Suite(t"Plutocrat tests"):
  def run(): Unit =
    suite(t"Money tests"):
      test(t"Show a local monetary value"):
        import currencyStyles.local
        val amount = Eur(3.01)
        t"Received $amount"
      .assert(_ == t"Received €3.01")
      
      test(t"Show a monetary value"):
        import currencyStyles.generic
        val amount = Eur(3.01)
        t"Received $amount"
      .assert(_ == t"Received 3.01 EUR")
      
      test(t"Add two amounts"):
        Eur(3.01) + Eur(0.02)
      .assert(_ == Eur(3.03))
      
      test(t"Subtract an amounts"):
        Eur(3.01) - Eur(0.02)
      .assert(_ == Eur(2.99))
      
      test(t"Multiply an amount"):
        Eur(3.01)*3
      .assert(_ == Eur(9.03))
      
      test(t"Divide an amount"):
        Eur(3.01)/3
      .assert(_ == Eur(1.00))
      
      test(t"Split an amount"):
        Eur(3.01).split(3).total
      .assert(_ == Eur(3.01))

      test(t"Different currencies cannot be combined"):
        demilitarize:
          Eur(1.00) + Gbp(1.00)
        .map(_.errorId)
      .assert(_ == List(ErrorId.MissingImplicitArgumentID))

      test(t"Monetary values can be negated"):
        -Eur(1.99)
      .assert(_ == Eur(-1.99))

      test(t"Compare amounts"):
        Eur(1.01) > Eur(2.10)
      .assert(_ == false)
      
      test(t"Compare equal amounts"):
        Eur(1.01) > Eur(1.01)
      .assert(_ == false)
      
      test(t"Compare equal amounts"):
        Eur(1.01) >= Eur(1.01)
      .assert(_ == true)

    suite(t"Price tests"):
      test(t"Construct new price"):
        Gbp(2.30).tax(0.2)
      .assert(_ == Price(Gbp(2.30), Gbp(0.46)))
      
      test(t"Tax amount is rounded up correctly"):
        Gbp(2.94).tax(0.2)
      .assert(_ == Price(Gbp(2.94), Gbp(0.59)))

      test(t"Prices in different currencies cannot be combined"):
        demilitarize:
          Eur(1.00).tax(0.175) + Gbp(1.00).tax(0.2)
        .map(_.errorId)
      .assert(_ == List(ErrorId.TypeMismatchID))
