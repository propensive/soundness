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
┃    Soundness, version 0.64.0.                                                                    ┃
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

import soundness.*

given decimalizer: Decimalizer = Decimalizer(3)

object Tests extends Suite(m"Abacist Tests"):
  def run(): Unit =
    suite(m"Quanta tests"):
      type Height = Quanta[Inches[1]] in (Feet[1])

      test(m"Access seconds in an HMS time"):
        val hmsTime: TimeSeconds = Quanta(27, 18, 9)
        hmsTime[Seconds]
      . assert(_ == 9)

      test(m"Access minutes in an HMS time"):
        val hmsTime: TimeSeconds = Quanta(27, 18, 9)
        hmsTime[Minutes]
      . assert(_ == 18)

      test(m"Access hours in an HMS time"):
        val hmsTime: TimeSeconds = Quanta(27, 18, 9)
        hmsTime[Hours]
      . assert(_ == 27)

      type ImperialDistance = Quanta[Inches[1]] in (Feet[1], Yards[1], Miles[1])

      test(m"Access inches in an imperial distance"):
        val imperialDistance: ImperialDistance = Quanta(1800, 4, 2, 11)
        imperialDistance[Feet]
      . assert(_ == 2)

      test(m"Units of different dimensions cannot be mixed"):
        demilitarize:
          (Quanta(1, 2, 3): Quanta[Inches[1]] in (Seconds[1], Yards[1], Miles[1]))
      . assert(_.nonEmpty)

      test(m"A non-unit base is rejected by the type bound"):
        demilitarize:
          val bad: Quanta[String] = ???
      . assert(_.nonEmpty)

      test(m"Convert a length to a Quanta"):
        val length: Quantity[Metres[1]] = (5.9*Foot + 10.0*Inch)
        val count = length.quanta[Height]
        (count[Feet], count[Inches])
      . assert(_ == (6, 9))

      type Weight = Quanta[Ounces[1]] in (Pounds[1], Stones[1])

      test(m"Convert a mass Quantity to a Quanta"):
        val weight: Quantity[Kilograms[1]] = 20.0*Kilo(Gram)
        val count = weight.quanta[Weight]
        (count[Stones], count[Pounds], count[Ounces])
      . assert(_ == (3, 2, 1))

      test(m"Convert a Quanta to a Quantity"):
        val weight: Weight = Quanta(5, 6)
        weight.quantity
      . assert(_ == 2.438057*Kilo(Gram))

      test(m"Convert a Quanta to a Quantity in pounds"):
        val weight: Weight = Quanta(5, 6)
        weight.quantity.to[Pounds]
      . assert(_ == 5.375*Pound)

      test(m"Add two Quantas"):
        val weight: Weight = Quanta(12, 9)
        val sum: Weight = weight + Quanta(1)
        (sum[Stones], sum[Pounds], sum[Ounces])
      . assert(_ == (0, 12, 10))

      test(m"Add two Quantas 2"):
        val weight: Weight = Quanta(12, 9)
        val sum: Weight = weight + Quanta(2)
        (sum[Stones], sum[Pounds], sum[Ounces])
      . assert(_ == (0, 12, 11))

      test(m"Add two Quantas 3"):
        val weight: Weight = Quanta(12, 9)
        val sum: Weight = weight + Quanta(5)
        (sum[Stones], sum[Pounds], sum[Ounces])
      . assert(_ == (0, 12, 14))

      test(m"Add two Quantas 4"):
        val weight: Weight = Quanta(12, 9)
        val sum: Weight = weight + Quanta(7)
        (sum[Stones], sum[Pounds], sum[Ounces])
      . assert(_ == (0, 13, 0))

      test(m"Add two Quantas 5"):
        val weight: Weight = Quanta(12, 9)
        val sum: Weight = weight + Quanta(8)
        (sum[Stones], sum[Pounds], sum[Ounces])
      . assert(_ == (0, 13, 1))

      test(m"Subtract two Quantas 1"):
        val weight: Weight = Quanta(12, 9)
        val weight2: Weight = Quanta(0, 2)
        val result = weight - weight2
        (result[Stones], result[Pounds], result[Ounces])
      . assert(_ == (0, 12, 7))

      test(m"Subtract two Quantas 2"):
        val weight: Weight = Quanta(12, 9)
        val weight2: Weight = Quanta(0, 2)
        val result = weight2 - weight
        (result[Stones], result[Pounds], result[Ounces])
      . assert(_ == (0, -12, -7))

      test(m"Multiply a Quanta by a double"):
        val weight: Weight = Quanta(12, 9)
        val result = weight*2.5
        (result[Stones], result[Pounds], result[Ounces])
      . assert(_ == (2, 3, 7))

      test(m"Adding with double carry"):
        val weight: Weight = Quanta(100, 13, 15)
        val sum: Weight = weight + Quanta(1)
        (sum[Stones], sum[Pounds], sum[Ounces])
      . assert(_ == (101, 0, 0))

      test(m"Collapse a weight value"):
        val weight: Weight = Quanta(2, 3, 4)
        val weight2 = weight.collapse(1)
        (weight2[Pounds], weight2[Ounces])
      . assert(_ == (31, 4))

      test(m"Collapse a weight value 2"):
        val weight: Weight = Quanta(2, 3, 4)
        val weight2 = weight.collapse(2)
        weight2[Ounces]
      . assert(_ == 500)

      test(m"Cannot collapse beyond last unit"):
        demilitarize:
          val weight: Weight = Quanta(2, 3, 4)
          weight.collapse(3)
      . assert(_.length == 1)

      suite(m"Shared base-unit supertype"):
        type Hms = Quanta[Seconds[1]] in (Minutes[1], Hours[1])
        type Ms = Quanta[Seconds[1]] in (Minutes[1])

        test(m"Differently-divided quanta share a common base-unit supertype"):
          val a: Hms = Quanta(0, 1, 0)
          val b: Ms = Quanta(0, 30)
          List[Quanta[Seconds[1]]](a, b).length
        . assert(_ == 2)

        test(m"Quanta with the same base unit but different divisions are comparable"):
          val a: Quanta[Seconds[1]] = (Quanta(0, 1, 0): Hms)
          val b: Quanta[Seconds[1]] = (Quanta(0, 30): Ms)
          a > b
        . assert(_ == true)

      suite(m"Showing Quanta values"):
        test(m"Show a single-unit weight"):
          (Quanta(2): Weight).show
        . assert(_ == t"2oz")

        test(m"Show a more complex weight"):
          (Quanta(3, 2): Weight).show
        . assert(_ == t"3lb 2oz")

        test(m"Show a weight of three parts"):
          (Quanta(1, 3, 2): Weight).show
        . assert(_ == t"1st 3lb 2oz")

        test(m"Show with custom unit rendering"):
          given UnitsNames[Height] = () => List(t"'", t"\"")
          (Quanta(5, 9): Height).show
        . assert(_ == t"5' 9\"")

      suite(m"Aggregate tests"):
        test(m"Total of several values"):
          List[Weight](Quanta(10), Quanta(1, 6), Quanta(2, 4, 1)).total
        . assert(_ == (Quanta(2, 6, 1): Weight))

        test(m"Mean of several values"):
          List[Weight](Quanta(10), Quanta(1, 6), Quanta(2, 4, 1)).mean
        . assert(_ == (Quanta(11, 6): Weight))
