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
┃    Soundness, version 0.30.0.                                                                    ┃
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

import fulminate.*
import gossamer.*
import probably.*
import quantitative.*
import spectacular.*
import symbolism.*
import java.awt.SecondaryLoop

given Decimalizer = Decimalizer(3)

object Tests extends Suite(m"Baroque tests"):
  def run(): Unit =
    test(m"Show a complex number"):
      Complex(1, 3).show
    .assert(_ == t"1 + 3𝕚")

    test(m"Show a quantity complex number"):
      val re = 1.0*Metre/Second
      val im = Metre*9.0/Second
      Complex[Quantity[Metres[1] & Seconds[-1]]](1.0*Metre/Second, Metre*9.0/Second).show
    .assert(_ == t"(1.00 + 9.00𝕚) m·s¯¹")

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

    test(m"Multiply complex numbers"):
      Complex(1, 3)*Complex(2, 4)
    .assert(_ == Complex(-10, 10))

    test(m"Divide complex numbers"):
      Complex(3.0, 2.0)/Complex(1.0, -4.0)
    .assert { c => c.real == -0.29411764705882354 && c.imaginary == 0.8235294117647058 }

    test(m"Divide complex quantities"):
      Complex(3.0*Metre, 2.0*Metre)/Complex(1.0*Foot, -4.0*Foot)
    .assert(_ == Complex(-0.9649529102979775,2.7018681488343375))

    test(m"Multiply complex quantity numbers"):
      Complex(18.0*Foot, 1.4*Foot)*Complex(4.0*Kilo(Gram), 2.0*Kilo(Gram))
    .assert(_ == Complex(69.2*Foot*Kilo(Gram), 41.6*Foot*Kilo(Gram)))
