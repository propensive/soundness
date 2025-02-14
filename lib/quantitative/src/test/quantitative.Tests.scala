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

import gossamer.{t, Decimalizer}
import hypotenuse.*
import larceny.*
import probably.*
import spectacular.*

import language.strictEquality
import language.experimental.into

given decimalizer: Decimalizer = Decimalizer(3)

object Tests extends Suite(t"Quantitative Tests"):
  def run(): Unit =
    suite(t"Arithmetic tests"):
      test(t"Add two lengths"):
        Metre + Metre*2
      .assert(_ == Metre*3)

      test(t"Multiply two different units"):
        2*Second * 3*Metre
      .assert(_ == 6*Metre*Second)

      test(t"Invert a quantity"):
        (2*Metre/Second).invert
      .assert(_ == 0.5*Second/Metre)

      test(t"Divide a double by a quantity"):
        Quantity(1.0)/(2.0*Metre/Second)
      .assert(_ == 0.5*Second/Metre)

    suite(t"Compile errors"):
      test(t"Cannot add quantities of different units"):
        demilitarize:
          Metre + 2*Second
      .assert(_.nonEmpty)

      test(t"Cannot specify a quantity with a double value"):
        demilitarize:
          val x: Quantity[Metres[1]] = 10.0
      . assert(_.nonEmpty)

      test(t"Specify a quantity"):
        case class Speed(value: Quantity[Metres[1] & Hours[-1]])
        Speed(10.0*Kilo(Metre)/Hour)

      . assert()

      test(t"Specify a quantity"):
        case class Speed(value: Quantity[Metres[1] & Hours[-1]])
        Speed(10.0*Kilo(Metre)/Hour)

      . assert()

      test(t"Cannot subtract quantities of different units"):
        demilitarize:
          Metre - 2*Second
        .map(_.message)
      .assert(_.contains(t"quantitative: the left operand represents length, but the right operand represents time; these are incompatible physical quantities"))

      test(t"Add two different units"):
        demilitarize:
          Second*2 + Metre*3
        .map(_.message)
      .assert(_.contains(t"quantitative: the left operand represents time, but the right operand represents length; these are incompatible physical quantities"))

      test(t"Units cancel out"):
        demilitarize:
          (20*Metre*Second)/(Metre*Second): Double
        .map(_.message)
      .assert(_.isEmpty)

      test(t"Principal units are preferred"):
        demilitarize:
          val x = 2*Metre
          val y = 3*Foot
          val z: Quantity[Metres[2]] = x*y
      .assert(_.isEmpty)

      test(t"Non-principal units are not preferred"):
        demilitarize:
          val x = 2*Metre
          val y = 3*Foot
          val z: Quantity[Feet[2]] = x*y
      .assert(_.nonEmpty)

      test(t"Units of different dimension cannot be added"):
        demilitarize:
          2*Metre + 2*Joule
        .map(_.message)
      .assert(_ == List("quantitative: the left operand represents length, but the right operand represents energy; these are incompatible physical quantities"))

      test(t"Different dimensions are incomparable"):
        demilitarize:
          7*Metre >= 2*Kilo(Gram)
        .map(_.message)
      .assert(_ == List("quantitative: the left operand represents length, but the right operand represents mass; these are incompatible physical quantities"))

      test(t"Different powers of the same dimension are incomparable"):
        demilitarize:
          7*Metre >= 2*Metre*Metre
        .map(_.message)
      .assert(_ == List("quantitative: the left operand represents length, but the right operand represents area; these are incompatible physical quantities"))

    suite(t"Automatic conversions"):
      test(t"Conversions are applied automatically to RHS in multiplication"):
        val x = 2*Metre
        val y = 3*Foot
        x*y
      .assert(_ == 1.8288000000000002*Metre*Metre)

      test(t"Conversions are applied automatically to LHS in multiplication"):
        val x = 2*Metre
        val y = 3*Foot
        y*x
      .assert(_ == 1.8288000000000002*Metre*Metre)

      test(t"Conversions are applied automatically in division"):
        val x = 2*Metre*Metre
        val y = 3*Foot
        x/y
      .assert(_ == 2.187226596675415*Metre)

      test(t"Conversions are applied automatically to LHS in division"):
        val x = 2*Metre
        val y = 3*Foot*Foot
        y/x
      .assert(_ == 0.13935456000000002*Metre)

      test(t"Mixed units of the same dimension can be added"):
        2*Metre + 2*Foot
      .assert(_ == 2.6096*Metre)

      test(t"Mixed units of the same dimension can be subtracted"):
        2*Metre - 2*Foot
      .assert(_ == 1.3904*Metre)

      test(t"Mixed units of the same type can be added (reverse order)"):
        2*Foot + 2*Metre
      .assert(_ == 2.6096*Metre)


    suite(t"Metric prefixes"):
      test(t"Metric kilo prefix multiplies by 10^3"):
        15.0*Kilo(Metre)
      .assert(_ == 15000.0*Metre)

      test(t"Metric mega prefix multiplies by 10^6"):
        15.0*Mega(Metre)
      .assert(_ == 15000000.0*Metre)

      test(t"Metric giga prefix multiplies by 10^9"):
        15.0*Giga(Metre)
      .assert(_ == 15000000000.0*Metre)

      test(t"Metric kibi prefix multiplies by 2^10"):
        10*Kibi(Metre)
      .assert(_ == 10240*Metre)

      test(t"Metric mebi prefix multiplies by 2^20"):
        10*Mebi(Metre)
      .assert(_ == (1024*1024*10)*Metre)

      test(t"Metric milli prefix multiplies by 10^-3"):
        1.5*Milli(Metre)
      .assert(_ == 0.0015*Metre)

      test(t"Metric micro prefix multiplies by 10^-6"):
        1.5*Micro(Metre)
      .assert(_ == 0.0000015*Metre)

      test(t"Metric nano prefix multiplies by 10^-9"):
        2.5*Nano(Metre)
      .assert(_ == 0.0000000025*Metre)

    suite(t"Explicit conversion tests"):
      test(t"Convert feet to metres"):
        (3.0*Foot).in[Metres]
      .assert(_ == 0.9144000000000001*Metre)

      test(t"Convert metres to feet"):
        (3.0*Metre).in[Feet]
      .assert(_ == 9.842519685039369*Foot)

      test(t"Convert m² to ft²"):
        (Metre*Metre).in[Feet]
      .assert(_ == 10.76391041670972*Foot*Foot)

      test(t"Conversion to seconds does nothing"):
        (3.0*Metre).in[Seconds]
      .assert(_ == 3.0*Metre)

    suite(t"Inequalities"):
      test(t"6ft < 2m"):
        6*Foot < 2*Metre
      .assert(_ == true)

      test(t"6ft <= 2m"):
        6*Foot < 2*Metre
      .assert(_ == true)

      test(t"7ft > 2m"):
        7*Foot > 2*Metre
      .assert(_ == true)

      test(t"7ft >= 2m"):
        7*Foot >= 2*Metre
      .assert(_ == true)

      test(t"9ft² < 1m²"):
        9*Foot*Foot < Metre*Metre
      .assert(_ == true)

      test(t"10ft² < 1m²"):
        10*Foot*Foot < Metre*Metre
      .assert(_ == true)

      test(t"11ft² > 1m²"):
        11*Foot*Foot > Metre*Metre
      .assert(_ == true)

    suite(t"Rendering tests"):
      test(t"Show a value in metres"):
        (7.567*Metre).show
      .assert(_ == t"7.57 m")

      test(t"Show a value in square metres"):
        (1.4*Metre*Metre).show
      .assert(_ == t"1.40 m²")

      test(t"Show a value in metres per second"):
        (8.54*Metre/Second).show
      .assert(_ == t"8.54 m·s¯¹")

      test(t"Show a value in kilometres per second"):
        (8.54*Kilo(Metre)/Second).show
      .assert(_ == t"8.54×10³ m·s¯¹")

      test(t"Show a value in kilograms"):
        (10.4*Kilo(Gram)/Second).show
      .assert(_ == t"10.4 kg·s¯¹")

      test(t"Show the speed of light"):
        constants.SpeedOfLightInVacuum.show
      .assert(_ == t"3.00×10⁸ m·s¯¹")

      test(t"Show Planck's constant"):
        constants.PlanckConstant.show
      .assert(_ == t"6.63×10¯³⁴ m²·kg·s¯¹")

      test(t"Show an energy using custom units"):
        (45*Joule).show
      .assert(_ == t"45.0 J")

      test(t"Show a force in Newtons"):
        (100*Newton).show
      .assert(_ == t"100 N")

    suite(t"Quantity descriptions"):
      test(t"describe a base dimension"):
        Metre.dimension
      .assert(_ == t"length")

      test(t"describe a compound dimension"):
        (Metre/Second).dimension
      .assert(_ == t"velocity")

      test(t"describe a complex compound dimension"):
        (Foot*Foot*Kilo(Gram)/(Second*Second*Mole)).dimension
      .assert(_ == t"chemical potential")

    suite(t"Quantifiability tests"):
      case class Pts(value: Double)
      given Quantifiable[Pts, Inches[1]] = pts => (Inch*pts.value)/72

      test(t"quantify a length"):
        Pts(71).quantify < Inch
      .assert(_ == true)

      test(t"quantify a length"):
        Pts(73).quantify > Inch
      .assert(_ == true)

    suite(t"Offset quantities"):
      test(t"Get Celsius value"):
        (300*Kelvin).in[Celsius].show
      .assert(_ == t"26.9 °C")

      test(t"Get Fahrenheit value"):
        (300*Kelvin).in[Fahrenheit].show
      .assert(_ == t"80.3 °F")

      test(t"Create Celsius value"):
        Celsius(30.0).show
      .assert(_ == t"30.0 °C")

      test(t"Create Fahrenheit value"):
        Fahrenheit(30.0).show
      .assert(_ == t"30.0 °F")

      test(t"Check stored Celsius value"):
        Celsius(30.0).toString.show
      .assert(_ == t"303.15")

      test(t"Check stored Fahrenheit value"):
        Fahrenheit(30.0).toString.show
      .assert(_ == t"489.67")

      test(t"Convert Fahrenheit value to Kelvin"):
        Fahrenheit(0.0).in[Kelvins].show
      .assert(_ == t"255 K")

      test(t"Convert Fahrenheit value to Celsius via Kelvin"):
        Fahrenheit(100.0).in[Kelvins].in[Celsius].show
      .assert(_ == t"37.8 °C")

      test(t"Convert Fahrenheit directly to Celsius"):
        Fahrenheit(100.0).in[Celsius].show
      .assert(_ == t"37.8 °C")
