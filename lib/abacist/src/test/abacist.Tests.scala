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
┃    Soundness, version 0.27.0.                                                                    ┃
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

object Tests extends Suite(m"Quantitative Tests"):
  def run(): Unit =
    suite(m"Count tests"):

      type Height = (Feet[1], Inches[1])

      test(m"Access seconds in an HMS time"):
        val hmsTime = Count[TimeSeconds](27, 18, 9)
        hmsTime[Seconds]
      .assert(_ == 9)

      test(m"Access minutes in an HMS time"):
        val hmsTime = Count[TimeSeconds](27, 18, 9)
        hmsTime[Minutes]
      .assert(_ == 18)

      test(m"Access hours in an HMS time"):
        val hmsTime = Count[TimeSeconds](27, 18, 9)
        hmsTime[Hours]
      .assert(_ == 27)

      test(m"Access inches in an imperial distance"):
        val imperialDistance = Count[(Miles[1], Yards[1], Feet[1], Inches[1])](1800, 4, 2, 11)
        imperialDistance[Feet]
      .assert(_ == 2)

      test(m"Units of different dimensions cannot be mixed"):
        demilitarize:
          Count[(Miles[1], Yards[1], Seconds[1], Inches[1])](1, 2, 3)
      .assert(_.nonEmpty)

      test(m"Convert a length to a Count"):
        val length: Quantity[Metres[1]] = (5.9*Foot + 10.0*Inch)
        val count = length.count[Height]
        (count[Feet], count[Inches])
      .assert(_ == (5, 10))

      type Weight = (Stones[1], Pounds[1], Ounces[1])

      test(m"Convert a mass Quantity to a Count"):
        val weight: Quantity[Kilograms[1]] = 20.0*Kilo(Gram)
        val count = weight.count[Weight]
        (count[Stones], count[Pounds], count[Ounces])
      .assert(_ == (3, 2, 1))

      test(m"Convert a Count to a Quantity"):
        val weight: Count[Weight] = Count(5, 6)
        weight.quantity
      .assert(_ == 2.438057*Kilo(Gram))

      test(m"Convert a Count to a Quantity in pounds"):
        val weight: Count[Weight] = Count(5, 6)
        weight.quantity.in[Pounds]
      .assert(_ == 5.375*Pound)

      test(m"Add two Counts"):
        val weight: Count[Weight] = Count(12, 9)
        val sum: Count[Weight] = weight + Count[Weight](1)
        (sum[Stones], sum[Pounds], sum[Ounces])
      .assert(_ == (0, 12, 10))

      test(m"Add two Counts 2"):
        val weight: Count[Weight] = Count(12, 9)
        val sum: Count[Weight] = weight + Count[Weight](2)
        (sum[Stones], sum[Pounds], sum[Ounces])
      .assert(_ == (0, 12, 11))

      test(m"Add two Counts 3"):
        val weight: Count[Weight] = Count(12, 9)
        val sum: Count[Weight] = weight + Count[Weight](5)
        (sum[Stones], sum[Pounds], sum[Ounces])
      .assert(_ == (0, 12, 14))

      test(m"Add two Counts 4"):
        val weight: Count[Weight] = Count(12, 9)
        val sum: Count[Weight] = weight + Count[Weight](7)
        (sum[Stones], sum[Pounds], sum[Ounces])
      .assert(_ == (0, 13, 0))

      test(m"Add two Counts 5"):
        val weight: Count[Weight] = Count(12, 9)
        val sum: Count[Weight] = weight + Count[Weight](8)
        (sum[Stones], sum[Pounds], sum[Ounces])
      .assert(_ == (0, 13, 1))

      test(m"Subtract two Counts 1"):
        val weight: Count[Weight] = Count(12, 9)
        val weight2: Count[Weight] = Count(0, 2)
        val result = weight - weight2
        (result[Stones], result[Pounds], result[Ounces])
      .assert(_ == (0, 12, 7))

      test(m"Subtract two Counts 2"):
        val weight: Count[Weight] = Count(12, 9)
        val weight2: Count[Weight] = Count(0, 2)
        val result = weight2 - weight
        (result[Stones], result[Pounds], result[Ounces])
      .assert(_ == (0, -12, -7))

      test(m"Multiply a count by a double"):
        val weight: Count[Weight] = Count(12, 9)
        val result = weight*2.5
        (result[Stones], result[Pounds], result[Ounces])
      .assert(_ == (2, 3, 7))

      test(m"Adding with double carry"):
        val weight: Count[Weight] = Count(100, 13, 15)
        val sum: Count[Weight] = weight + Count[Weight](1)
        (sum[Stones], sum[Pounds], sum[Ounces])
      .assert(_ == (101, 0, 0))

      test(m"Collapse a weight value"):
        val weight: Count[Weight] = Count(2, 3, 4)
        val weight2 = weight.collapse(1)
        (weight2[Pounds], weight2[Ounces])
      .assert(_ == (31, 4))

      test(m"Collapse a weight value 2"):
        val weight: Count[Weight] = Count(2, 3, 4)
        val weight2 = weight.collapse(2)
        weight2[Ounces]
      .assert(_ == 500)

      test(m"Cannot collapse beyond last unit"):
        demilitarize:
          val weight: Count[Weight] = Count(2, 3, 4)
          weight.collapse(3)
      .assert(_.length == 1)

      suite(m"Showing Count values"):
        test(m"Show a single-unit weight"):
          Count[Weight](2).show
        .assert(_ == t"2oz")

        test(m"Show a more complex weight"):
          Count[Weight](3, 2).show
        .assert(_ == t"3lb 2oz")

        test(m"Show a weight of three parts"):
          Count[Weight](1, 3, 2).show
        .assert(_ == t"1st 3lb 2oz")

        test(m"Show a weight of three parts"):
          Count[Weight](1, 3, 2).show
        .assert(_ == t"1st 3lb 2oz")


        test(m"Show with custom unit rendering"):
          given UnitsNames[Height] = () => List(t"'", t"\"")
          Count[Height](5, 9).show
        .assert(_ == t"5' 9\"")

      suite(m"Aggregate tests"):
        test(m"Total of several values"):
          List[Count[Weight]](Count(10), Count(1, 6), Count(2, 4, 1)).total
        . assert(_ == Count(2, 6, 1))

        test(m"Mean of several values"):
          List[Count[Weight]](Count(10), Count(1, 6), Count(2, 4, 1)).mean
        . assert(_ == Count(11, 6))
