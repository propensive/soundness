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
package quantitative

import fulminate.*
import symbolism.*
import rudiments.*
import gossamer.*
import hypotenuse.*
import larceny.*
import probably.*
import spectacular.*

import language.strictEquality
import language.experimental.into

given decimalizer: Decimalizer = Decimalizer(3)

object Tests extends Suite(m"Quantitative Tests"):
  def run(): Unit =
    suite(m"Arithmetic tests"):
      test(m"Add two distances"):
        Metre + Metre*2.0
      .assert(_ == Metre*3.0)

      test(m"Multiply two different units"):
        2.0*Second * 3.0*Metre
      .assert(_ == 6.0*Metre*Second)

      test(m"Invert a quantity"):
        (2.0*Metre/Second).invert
      .assert(_ == 0.5*Second/Metre)

      test(m"Divide a double by a quantity"):
        Quantity(1.0)/(2.0*Metre/Second)
      .assert(_ == 0.5*Second/Metre)

    suite(m"Compile errors"):
      test(m"Cannot add quantities of different units"):
        demilitarize:
          Metre + 2.0*Second
      .assert(_.nonEmpty)

      test(m"Cannot specify a quantity with a double value"):
        demilitarize:
          val x: Quantity[Metres[1]] = 10.0
      . assert(_.nonEmpty)

      test(m"Specify a quantity"):
        case class Speed(value: Quantity[Metres[1] & Hours[-1]])
        Speed(10.0*Kilo(Metre)/Hour)

      . assert()

      test(m"Specify a quantity"):
        case class Speed(value: Quantity[Metres[1] & Hours[-1]])
        Speed(10.0*Kilo(Metre)/Hour)

      . assert()

      test(m"Cannot subtract quantities of different units"):
        demilitarize:
          Metre - 2.0*Second
        .map(_.message)
      .assert(_ == List("quantitative: the left operand represents distance, but the right operand represents time; these are incompatible physical quantities"))

      test(m"Add two different units"):
        demilitarize:
          Second*2.0 + Metre*3.0
        .map(_.message)
      .assert(_.contains(t"quantitative: the left operand represents time, but the right operand represents distance; these are incompatible physical quantities"))

      test(m"Units cancel out"):
        demilitarize:
          (20.0*Metre*Second)/(Metre*Second): Double
        .map(_.message)
      .assert(_.isEmpty)

      test(m"Principal units are preferred"):
        demilitarize:
          val x = 2.0*Metre
          val y = 3.0*Foot
          val z: Quantity[Metres[2]] = x*y
      .assert(_.isEmpty)

      test(m"Non-principal units are not preferred"):
        demilitarize:
          val x = 2.0*Metre
          val y = 3.0*Foot
          val z: Quantity[Feet[2]] = x*y
      .assert(_.nonEmpty)

      test(m"Units of different dimension cannot be added"):
        demilitarize:
          2.0*Metre + 2.0*Joule
        .map(_.message)
      .assert(_ == List("quantitative: the left operand represents distance, but the right operand represents energy; these are incompatible physical quantities"))

      test(m"Different dimensions are incomparable"):
        demilitarize:
          7.0*Metre >= 2.0*Kilo(Gram)
        .map(_.message)
      .assert(_ == List("quantitative: the left operand represents distance, but the right operand represents mass; these are incompatible physical quantities"))

      test(m"Different powers of the same dimension are incomparable"):
        demilitarize:
          7.0*Metre >= 2.0*Metre*Metre
        .map(_.message)
      .assert(_ == List("quantitative: the left operand represents distance, but the right operand represents area; these are incompatible physical quantities"))

    suite(m"Automatic conversions"):
      test(m"Conversions are applied automatically to RHS in multiplication"):
        val x = 2.0*Metre
        val y = 3.0*Foot
        x*y
      .assert(_ == 1.8288000000000002*Metre*Metre)

      test(m"Conversions are applied automatically to LHS in multiplication"):
        val x = 2.0*Metre
        val y = 3.0*Foot
        y*x
      .assert(_ == 1.8288000000000002*Metre*Metre)

      test(m"Conversions are applied automatically in division"):
        val x = 2.0*Metre*Metre
        val y = 3.0*Foot
        x/y
      .assert(_ == 2.187226596675415*Metre)

      test(m"Conversions are applied automatically to LHS in division"):
        val x = 2.0*Metre
        val y = 3.0*Foot*Foot
        y/x
      .assert(_ == 0.13935456000000002*Metre)

      test(m"Mixed units of the same dimension can be added"):
        2.0*Metre + 2.0*Foot
      .assert(_ == 2.6096*Metre)

      test(m"Mixed units of the same dimension can be subtracted"):
        2.0*Metre - 2.0*Foot
      .assert(_ == 1.3904*Metre)

      test(m"Mixed units of the same type can be added (reverse order)"):
        2.0*Foot + 2.0*Metre
      .assert(_ == 2.6096*Metre)

    suite(m"Normalization tests"):
      test(m"Normalize minutes as seconds"):
        (60.0*Second).normalize[Minutes[1]]
      . assert(_ == Minute)

      test(m"Normalize hours as seconds"):
        (2.0*Hour).normalize[Seconds[1]]
      . assert(_ == 7200.0*Second)

      test(m"Normalize inches as metres"):
        (1.0*Inch).normalize[Metres[1]]
      . assert(_ == 2.54*Centi(Metre))

    suite(m"Metric prefixes"):
      test(m"Metric kilo prefix multiplies by 10^3"):
        15.0*Kilo(Metre)
      .assert(_ == 15000.0*Metre)

      test(m"Metric mega prefix multiplies by 10^6"):
        15.0*Mega(Metre)
      .assert(_ == 15000000.0*Metre)

      test(m"Metric giga prefix multiplies by 10^9"):
        15.0*Giga(Metre)
      .assert(_ == 15000000000.0*Metre)

      test(m"Metric kibi prefix multiplies by 2^10"):
        10.0*Kibi(Metre)
      .assert(_ == 10240.0*Metre)

      test(m"Metric mebi prefix multiplies by 2^20"):
        10.0*Mebi(Metre)
      .assert(_ == (1024*1024*10.0)*Metre)

      test(m"Metric milli prefix multiplies by 10^-3"):
        1.5*Milli(Metre)
      .assert(_ == 0.0015*Metre)

      test(m"Metric micro prefix multiplies by 10^-6"):
        1.5*Micro(Metre)
      .assert(_ == 0.0000015*Metre)

      test(m"Metric nano prefix multiplies by 10^-9"):
        2.5*Nano(Metre)
      .assert(_ == 0.0000000025*Metre)

    suite(m"Explicit conversion tests"):
      test(m"Convert feet to metres"):
        (3.0*Foot).in[Metres]
      .assert(_ == 0.9144000000000001*Metre)

      test(m"Convert metres to feet"):
        (3.0*Metre).in[Feet]
      .assert(_ == 9.842519685039369*Foot)

      test(m"Convert m² to ft²"):
        (Metre*Metre).in[Feet]
      .assert(_ == 10.76391041670972*Foot*Foot)

      test(m"Conversion to seconds does nothing"):
        (3.0*Metre).in[Seconds]
      .assert(_ == 3.0*Metre)

    suite(m"Inequalities"):
      test(m"6ft < 2m"):
        6.0*Foot < 2.0*Metre
      .assert(_ == true)

      test(m"6ft <= 2m"):
        6.0*Foot < 2.0*Metre
      .assert(_ == true)

      test(m"7ft > 2m"):
        7.0*Foot > 2.0*Metre
      .assert(_ == true)

      test(m"7ft >= 2m"):
        7.0*Foot >= 2.0*Metre
      .assert(_ == true)

      test(m"9ft² < 1m²"):
        9.0*Foot*Foot < Metre*Metre
      .assert(_ == true)

      test(m"10ft² < 1m²"):
        10.0*Foot*Foot < Metre*Metre
      .assert(_ == true)

      test(m"11ft² > 1m²"):
        11.0*Foot*Foot > Metre*Metre
      .assert(_ == true)

    suite(m"Rendering tests"):
      test(m"Show a value in metres"):
        (7.567*Metre).show
      .assert(_ == t"7.57 m")

      test(m"Show a value in square metres"):
        (1.4*Metre*Metre).show
      .assert(_ == t"1.40 m²")

      test(m"Show a value in metres per second"):
        (8.54*Metre/Second).show
      .assert(_ == t"8.54 m·s¯¹")

      test(m"Show a value in kilometres per second"):
        (8.54*Kilo(Metre)/Second).show
      .assert(_ == t"8.54×10³ m·s¯¹")

      test(m"Show a value in kilograms"):
        (10.4*Kilo(Gram)/Second).show
      .assert(_ == t"10.4 kg·s¯¹")

      test(m"Show the speed of light"):
        constants.SpeedOfLightInVacuum.show
      .assert(_ == t"3.00×10⁸ m·s¯¹")

      test(m"Show Planck's constant"):
        constants.PlanckConstant.show
      .assert(_ == t"6.63×10¯³⁴ m²·kg·s¯¹")

      test(m"Show an energy using custom units"):
        (45.0*Joule).show
      .assert(_ == t"45.0 J")

      test(m"Show a force in Newtons"):
        (100.0*Newton).show
      .assert(_ == t"100 N")

    suite(m"Quantity descriptions"):
      test(m"describe a base dimension"):
        Metre.dimension
      .assert(_ == t"distance")

      test(m"describe a compound dimension"):
        (Metre/Second).dimension
      .assert(_ == t"velocity")

      test(m"describe a complex compound dimension"):
        (Foot*Foot*Kilo(Gram)/(Second*Second*Mole)).dimension
      .assert(_ == t"chemical potential")

    suite(m"Quantifiability tests"):
      case class Pts(value: Double)
      given Quantifiable[Pts, Inches[1]] = pts => (Inch*pts.value)/72.0

      test(m"quantify a distance"):
        Pts(71).quantify < Inch
      .assert(_ == true)

      test(m"quantify a distance"):
        Pts(73).quantify > Inch
      .assert(_ == true)

    suite(m"Offset quantities"):
      test(m"Get Celsius value"):
        (300.0*Kelvin).in[Celsius].show
      .assert(_ == t"26.9 °C")

      test(m"Get Fahrenheit value"):
        (300.0*Kelvin).in[Fahrenheit].show
      .assert(_ == t"80.3 °F")

      test(m"Create Celsius value"):
        Celsius(30.0).show
      .assert(_ == t"30.0 °C")

      test(m"Create Fahrenheit value"):
        Fahrenheit(30.0).show
      .assert(_ == t"30.0 °F")

      test(m"Check stored Celsius value"):
        Celsius(30.0).toString.show
      .assert(_ == t"303.15")

      test(m"Check stored Fahrenheit value"):
        Fahrenheit(30.0).toString.show
      .assert(_ == t"489.67")

      test(m"Convert Fahrenheit value to Kelvin"):
        Fahrenheit(0.0).in[Kelvins].show
      .assert(_ == t"255 K")

      test(m"Convert Fahrenheit value to Celsius via Kelvin"):
        Fahrenheit(100.0).in[Kelvins].in[Celsius].show
      .assert(_ == t"37.8 °C")

      test(m"Convert Fahrenheit directly to Celsius"):
        Fahrenheit(100.0).in[Celsius].show
      .assert(_ == t"37.8 °C")

    suite(m"Aggregation tests"):
      test(m"Total some values"):
        List(1.0*Second, 2.0*Second, 3.0*Second).total
      . assert(_ == 6.0*Second)

      test(m"Average some values"):
        List(1.0*Second, 2.0*Second, 3.0*Second).mean
      . assert(_ == 2.0*Second)

      test(m"Variance of some values"):
        List(1.0*Second, 2.0*Second, 3.0*Second).variance
      . assert(_ == (2.0/3.0)*Second*Second)

      test(m"Standard deviation of some values"):
        summon[Quantity[Seconds[2]] is Rootable[2]]
        List(1.0*Second, 2.0*Second, 3.0*Second).standardDeviation
      . assert(_ == (2.0/3.0).sqrt*Second)
