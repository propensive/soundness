/*
    Abacist, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package abacist

import probably.*
import rudiments.*
import gossamer.{t, Decimalizer}
import larceny.*
import spectacular.*
import quantitative.*

import language.strictEquality

given decimalizer: Decimalizer = Decimalizer(3)

object Tests extends Suite(t"Quantitative Tests"):
  def run(): Unit =
    suite(t"Count tests"):
      
      type Height = (Feet[1], Inches[1])

      test(t"Access seconds in an HMS time"):
        val hmsTime = Count[TimeSeconds](27, 18, 9)
        hmsTime[Seconds]
      .assert(_ == 9)
      
      test(t"Access minutes in an HMS time"):
        val hmsTime = Count[TimeSeconds](27, 18, 9)
        println("underlying = "+hmsTime)
        hmsTime[Minutes]
      .assert(_ == 18)
      
      test(t"Access hours in an HMS time"):
        val hmsTime = Count[TimeSeconds](27, 18, 9)
        hmsTime[Hours]
      .assert(_ == 27)
      
      test(t"Access inches in an imperial distance"):
        val imperialDistance = Count[(Miles[1], Yards[1], Feet[1], Inches[1])](1800, 4, 2, 11)
        imperialDistance[Feet]
      .assert(_ == 2)
      
      test(t"Units of different dimensions cannot be mixed"):
        demilitarize:
          Count[(Miles[1], Yards[1], Seconds[1], Inches[1])](1, 2, 3)
      .assert(_.length == 1)

      test(t"Convert a length to a Count"):
        val length: Quantity[Metres[1]] = (5*Foot + 10*Inch)
        val count = length.count[Height]
        (count[Feet], count[Inches])
      .assert(_ == (5, 10))
      
      type Weight = (Stones[1], Pounds[1], Ounces[1])
      
      test(t"Convert a mass Quantity to a Count"):
        val weight: Quantity[Kilograms[1]] = 20*Kilo(Gram)
        val count = weight.count[Weight]
        (count[Stones], count[Pounds], count[Ounces])
      .assert(_ == (3, 2, 1))
      
      test(t"Convert a Count to a Quantity"):
        val weight: Count[Weight] = Count(5, 6)
        weight.quantity
      .assert(_ == 2.438057*Kilo(Gram))
      
      test(t"Convert a Count to a Quantity in pounds"):
        val weight: Count[Weight] = Count(5, 6)
        weight.quantity.in[Pounds]
      .assert(_ == 5.375*Pound)

      test(t"Add two Counts"):
        val weight: Count[Weight] = Count(12, 9)
        val sum: Count[Weight] = weight + Count[Weight](1)
        (sum[Stones], sum[Pounds], sum[Ounces])
      .assert(_ == (0, 12, 10))
      
      test(t"Add two Counts 2"):
        val weight: Count[Weight] = Count(12, 9)
        val sum: Count[Weight] = weight + Count[Weight](2)
        (sum[Stones], sum[Pounds], sum[Ounces])
      .assert(_ == (0, 12, 11))
      
      test(t"Add two Counts 3"):
        val weight: Count[Weight] = Count(12, 9)
        val sum: Count[Weight] = weight + Count[Weight](5)
        (sum[Stones], sum[Pounds], sum[Ounces])
      .assert(_ == (0, 12, 14))
      
      test(t"Add two Counts 4"):
        val weight: Count[Weight] = Count(12, 9)
        val sum: Count[Weight] = weight + Count[Weight](7)
        (sum[Stones], sum[Pounds], sum[Ounces])
      .assert(_ == (0, 13, 0))
      
      test(t"Add two Counts 5"):
        val weight: Count[Weight] = Count(12, 9)
        val sum: Count[Weight] = weight + Count[Weight](8)
        (sum[Stones], sum[Pounds], sum[Ounces])
      .assert(_ == (0, 13, 1))
      
      test(t"Subtract two Counts 1"):
        val weight: Count[Weight] = Count(12, 9)
        val weight2: Count[Weight] = Count(0, 2)
        val result = weight - weight2
        (result[Stones], result[Pounds], result[Ounces])
      .assert(_ == (0, 12, 7))
      
      test(t"Subtract two Counts 2"):
        val weight: Count[Weight] = Count(12, 9)
        val weight2: Count[Weight] = Count(0, 2)
        val result = weight2 - weight
        (result[Stones], result[Pounds], result[Ounces])
      .assert(_ == (0, -12, -7))
      
      test(t"Multiply a count by a double"):
        val weight: Count[Weight] = Count(12, 9)
        val result = weight*2.5
        (result[Stones], result[Pounds], result[Ounces])
      .assert(_ == (2, 3, 7))
      
      test(t"Adding with double carry"):
        val weight: Count[Weight] = Count(100, 13, 15)
        val sum: Count[Weight] = weight + Count[Weight](1)
        (sum[Stones], sum[Pounds], sum[Ounces])
      .assert(_ == (101, 0, 0))

      test(t"Collapse a weight value"):
        val weight: Count[Weight] = Count(2, 3, 4)
        val weight2 = weight.collapse(1)
        (weight2[Pounds], weight2[Ounces])
      .assert(_ == (31, 4))
      
      test(t"Collapse a weight value 2"):
        val weight: Count[Weight] = Count(2, 3, 4)
        val weight2 = weight.collapse(2)
        weight2[Ounces]
      .assert(_ == 500)
      
      test(t"Cannot collapse beyond last unit"):
        demilitarize:
          val weight: Count[Weight] = Count(2, 3, 4)
          weight.collapse(3)
      .assert(_.length == 1)

      suite(t"Showing Count values"):
        test(t"Show a single-unit weight"):
          Count[Weight](2).show
        .assert(_ == t"2oz")
        
        test(t"Show a more complex weight"):
          Count[Weight](3, 2).show
        .assert(_ == t"3lb 2oz")
        
        test(t"Show a weight of three parts"):
          Count[Weight](1, 3, 2).show
        .assert(_ == t"1st 3lb 2oz")
        
        test(t"Show a weight of three parts"):
          Count[Weight](1, 3, 2).show
        .assert(_ == t"1st 3lb 2oz")

        test(t"Show with custom unit rendering"):
          given UnitsNames[Height] = () => List(t"'", t"\"")
          Count[Height](5, 9).show
        .assert(_ == t"5' 9\"")