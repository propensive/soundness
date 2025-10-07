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
┃    Soundness, version 0.43.0.                                                                    ┃
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
package abacist

import anticipation.*
import fulminate.*
import gossamer.{t, Decimalizer}
import larceny.*
import prepositional.*
import probably.*
import quantitative.*
import rudiments.*
import spectacular.*
import symbolism.*

given decimalizer: Decimalizer = Decimalizer(3)

object Tests extends Suite(m"Abacist Tests"):
  def run(): Unit =
    suite(m"Quanta tests"):
      type Height = (Feet[1], Inches[1])

      test(m"Access seconds in an HMS time"):
        val hmsTime = Quanta[TimeSeconds](27, 18, 9)
        hmsTime[Seconds]
      .assert(_ == 9)

      test(m"Access minutes in an HMS time"):
        val hmsTime = Quanta[TimeSeconds](27, 18, 9)
        hmsTime[Minutes]
      .assert(_ == 18)

      test(m"Access hours in an HMS time"):
        val hmsTime = Quanta[TimeSeconds](27, 18, 9)
        hmsTime[Hours]
      .assert(_ == 27)

      test(m"Access inches in an imperial distance"):
        val imperialDistance = Quanta[(Miles[1], Yards[1], Feet[1], Inches[1])](1800, 4, 2, 11)
        imperialDistance[Feet]
      .assert(_ == 2)

      test(m"Units of different dimensions cannot be mixed"):
        demilitarize:
          Quanta[(Miles[1], Yards[1], Seconds[1], Inches[1])](1, 2, 3)
      .assert(_.nonEmpty)

      test(m"Convert a length to a Quanta"):
        val length: Quantity[Metres[1]] = (5.9*Foot + 10.0*Inch)
        val count = length.quanta[Height]
        (count[Feet], count[Inches])
      .assert(_ == (6, 9))

      type Weight = (Stones[1], Pounds[1], Ounces[1])

      test(m"Convert a mass Quantity to a Quanta"):
        val weight: Quantity[Kilograms[1]] = 20.0*Kilo(Gram)
        val count = weight.quanta[Weight]
        (count[Stones], count[Pounds], count[Ounces])
      .assert(_ == (3, 2, 1))

      test(m"Convert a Quanta to a Quantity"):
        val weight: Quanta[Weight] = Quanta(5, 6)
        weight.quantity
      .assert(_ == 2.438057*Kilo(Gram))

      test(m"Convert a Quanta to a Quantity in pounds"):
        val weight: Quanta[Weight] = Quanta(5, 6)
        weight.quantity.in[Pounds]
      .assert(_ == 5.375*Pound)

      test(m"Add two Quantas"):
        val weight: Quanta[Weight] = Quanta(12, 9)
        val sum: Quanta[Weight] = weight + Quanta[Weight](1)
        (sum[Stones], sum[Pounds], sum[Ounces])
      .assert(_ == (0, 12, 10))

      test(m"Add two Quantas 2"):
        val weight: Quanta[Weight] = Quanta(12, 9)
        val sum: Quanta[Weight] = weight + Quanta[Weight](2)
        (sum[Stones], sum[Pounds], sum[Ounces])
      .assert(_ == (0, 12, 11))

      test(m"Add two Quantas 3"):
        val weight: Quanta[Weight] = Quanta(12, 9)
        val sum: Quanta[Weight] = weight + Quanta[Weight](5)
        (sum[Stones], sum[Pounds], sum[Ounces])
      .assert(_ == (0, 12, 14))

      test(m"Add two Quantas 4"):
        val weight: Quanta[Weight] = Quanta(12, 9)
        val sum: Quanta[Weight] = weight + Quanta[Weight](7)
        (sum[Stones], sum[Pounds], sum[Ounces])
      .assert(_ == (0, 13, 0))

      test(m"Add two Quantas 5"):
        val weight: Quanta[Weight] = Quanta(12, 9)
        val sum: Quanta[Weight] = weight + Quanta[Weight](8)
        (sum[Stones], sum[Pounds], sum[Ounces])
      .assert(_ == (0, 13, 1))

      test(m"Subtract two Quantas 1"):
        val weight: Quanta[Weight] = Quanta(12, 9)
        val weight2: Quanta[Weight] = Quanta(0, 2)
        val result = weight - weight2
        (result[Stones], result[Pounds], result[Ounces])
      .assert(_ == (0, 12, 7))

      test(m"Subtract two Quantas 2"):
        val weight: Quanta[Weight] = Quanta(12, 9)
        val weight2: Quanta[Weight] = Quanta(0, 2)
        val result = weight2 - weight
        (result[Stones], result[Pounds], result[Ounces])
      .assert(_ == (0, -12, -7))

      test(m"Multiply a Quanta by a double"):
        val weight: Quanta[Weight] = Quanta(12, 9)
        val result = weight*2.5
        (result[Stones], result[Pounds], result[Ounces])
      .assert(_ == (2, 3, 7))

      test(m"Adding with double carry"):
        val weight: Quanta[Weight] = Quanta(100, 13, 15)
        val sum: Quanta[Weight] = weight + Quanta[Weight](1)
        (sum[Stones], sum[Pounds], sum[Ounces])
      .assert(_ == (101, 0, 0))

      test(m"Collapse a weight value"):
        val weight: Quanta[Weight] = Quanta(2, 3, 4)
        val weight2 = weight.collapse(1)
        (weight2[Pounds], weight2[Ounces])
      .assert(_ == (31, 4))

      test(m"Collapse a weight value 2"):
        val weight: Quanta[Weight] = Quanta(2, 3, 4)
        val weight2 = weight.collapse(2)
        weight2[Ounces]
      .assert(_ == 500)

      test(m"Cannot collapse beyond last unit"):
        demilitarize:
          val weight: Quanta[Weight] = Quanta(2, 3, 4)
          weight.collapse(3)
      .assert(_.length == 1)

      suite(m"Showing Quanta values"):
        test(m"Show a single-unit weight"):
          Quanta[Weight](2).show
        .assert(_ == t"2oz")

        test(m"Show a more complex weight"):
          Quanta[Weight](3, 2).show
        .assert(_ == t"3lb 2oz")

        test(m"Show a weight of three parts"):
          Quanta[Weight](1, 3, 2).show
        .assert(_ == t"1st 3lb 2oz")

        test(m"Show a weight of three parts"):
          Quanta[Weight](1, 3, 2).show
        .assert(_ == t"1st 3lb 2oz")


        test(m"Show with custom unit rendering"):
          given UnitsNames[Height] = () => List(t"'", t"\"")
          Quanta[Height](5, 9).show
        .assert(_ == t"5' 9\"")

      suite(m"Aggregate tests"):
        test(m"Total of several values"):
          List[Quanta[Weight]](Quanta(10), Quanta(1, 6), Quanta(2, 4, 1)).total
        . assert(_ == Quanta(2, 6, 1))

        test(m"Mean of several values"):
          List[Quanta[Weight]](Quanta(10), Quanta(1, 6), Quanta(2, 4, 1)).mean
        . assert(_ == Quanta(11, 6))
