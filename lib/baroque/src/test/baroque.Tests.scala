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
┃    Soundness, version 0.51.0.                                                                    ┃
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
package baroque

import abacist.*
import fulminate.*
import gossamer.*
import probably.*
import quantitative.*
import spectacular.*
import symbolism.*

import autopsies.contrastExpectations

given Decimalizer = Decimalizer(3)

object Tests extends Suite(m"Baroque tests"):
  def run(): Unit =
    test(m"Show a complex number"):
      Complex(1, 3).show
    .assert(_ == t"1 + 3ℐ")

    test(m"Show a complex number with negative imaginary"):
      Complex(1, -3).show
    .assert(_ == t"1 - 3ℐ")

    test(m"Show a complex number with no imaginary part"):
      Complex(1, 0).show
    .assert(_ == t"1")

    test(m"Show a complex number with no real part"):
      Complex(0, 3).show
    .assert(_ == t"3ℐ")

    test(m"Show a quantity complex number"):
      val re = 1.0*Metre/Second
      val im = Metre*9.0/Second
      Complex[Quantity[Metres[1] & Seconds[-1]]](1.0*Metre/Second, Metre*9.0/Second).show
    .assert(_ == t"(1.00 + 9.00ℐ) m·s¯¹")

    test(m"Show a quantity complex number with only a real part"):
      val re = 1.0*Metre/Second
      val im = 0.0*Metre/Second
      Complex[Quantity[Metres[1] & Seconds[-1]]](re, im).show
    .assert(_ == t"1.00 m·s¯¹")

    test(m"Show a quantity value in feet and inches"):
      type Distance = Quanta[(Yards[1], Feet[1], Inches[1])]
      val re: Distance = Quanta(1, 2, 0)
      val im: Distance = Quanta(2, 0, 6)
      Complex[Distance](re, im).show
    .assert(_ == t"(1 + 2ℐ) yd, 2 ft, (6ℐ) in")

    test(m"Add two int-complex numbers"):
      Complex(1, 2) + Complex(8, 2)
    .assert(_ == Complex(9, 4))

    test(m"Add two double-complex numbers"):
      Complex(0.1, 0.2) + Complex(0.8, 0.2)
    .assert(_ == Complex(0.9, 0.4))

    test(m"Add two quantity-complex numbers"):
      Complex(1.0*Metre, 3.0*Metre) + Complex(2.0*Metre, 8.0*Metre)
    .assert(_ == Complex(3.0*Metre, 11.0*Metre))

    test(m"Add heterogeneous quantity complex numbers"):
      Complex(1.0*Inch, 3.0*Inch) + Complex(2.0*Foot, 8.0*Foot)
    .assert(_ == Complex(0.635*Metre, 2.5146*Metre))

    test(m"Subtract two int-complex numbers"):
      Complex(1, 2) - Complex(8, 2)
    .assert(_ == Complex(-7, 0))

    test(m"Subtract two double-complex numbers"):
      Complex(0.1, 0.2) - Complex(1.1, 0.2)
    .assert(_ == Complex(-1.0, 0.0))

    test(m"Subtract two quantity-complex numbers"):
      Complex(1.0*Metre, 3.0*Metre) - Complex(2.0*Metre, 8.0*Metre)
    .assert(_ == Complex(-Metre, -5*Metre))

    test(m"Subtract heterogeneous quantity complex numbers"):
      Complex(1.0*Inch, 3.0*Inch) - Complex(2.0*Foot, 8.0*Foot)
    .assert(_ == Complex(-0.5842*Metre, -2.3622*Metre))

    test(m"Multiply complex numbers"):
      Complex(1, 3)*Complex(2, 4)
    .assert(_ == Complex(-10, 10))

    test(m"Multiply complex quantities"):
      Complex(1*Metre, 3*Metre)*Complex(2*Metre, 4*Metre)
    .assert(_ == Complex(-10*Metre*Metre, 10*Metre*Metre))

    test(m"Multiply complex quantities of different units"):
      Complex(1*Metre, 3*Metre)*Complex(2*Second, 4*Second)
    .assert(_ == Complex(-10*Metre*Second, 10*Metre*Second))

    test(m"Divide complex numbers"):
      Complex(3.0, 2.0)/Complex(1.0, -4.0)
    .assert { c => c.real == -0.29411764705882354 && c.imaginary == 0.8235294117647058 }

    test(m"Divide complex quantities"):
      Complex(3.0*Metre, 2.0*Metre)/Complex(1.0*Foot, -4.0*Foot)
    .assert(_ == Complex(-0.9649529102979775,2.7018681488343375))

    test(m"Multiply complex quantity numbers"):
      Complex(18.0*Foot, 1.4*Foot)*Complex(4.0*Kilo(Gram), 2.0*Kilo(Gram))
    .assert(_ == Complex(69.2*Foot*Kilo(Gram), 41.6*Foot*Kilo(Gram)))

    test(m"Negate a complex number"):
      -Complex(10.0, 7.0)
    .assert(_ == Complex(-10.0, -7.0))

    test(m"Negate a complex quantity"):
      -Complex(10*Inch, 7*Inch)
    .assert(_ == Complex(-10*Inch, -7*Inch))
