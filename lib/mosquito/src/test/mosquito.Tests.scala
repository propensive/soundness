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
┃    Soundness, version 0.53.0.                                                                    ┃
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
package mosquito

import baroque.*
import fulminate.*
import gossamer.*
import hieroglyph.*, textMetrics.uniform
import larceny.*
import probably.*
import quantitative.*
import rudiments.*
import spectacular.*
import symbolism.*

import autopsies.contrastExpectations

given Decimalizer(4)

object Tests extends Suite(m"Mosquito tests"):
  def run(): Unit =
    test(m"Create a Tensor of Ints"):
      Tensor(1, 2, 3)
    .assert(_ == Tensor(1, 2, 3))

    test(m"A Tensor of Ints infers the correct size"):
      demilitarize:
        val tensor: Tensor[Int, 3] = Tensor(1, 3, 4)
      .map(_.message)
    .assert(_ == Nil)

    test(m"Type error if size is incorrect"):
      demilitarize:
        val tensor: Tensor[Int, 2] = Tensor(1, 3, 4)
    .assert(_.nonEmpty)

    test(m"Type error if type is incorrect"):
      demilitarize:
        val tensor: Tensor[String, 3] = Tensor(1, 3, 4)
    .assert(_.nonEmpty)

    test(m"Calculate integer dot-product"):
      Tensor(1, 2, 3).dot(Tensor(4, 3, 7))
    .assert(_ == 31)

    test(m"Calculate Double dot-product"):
      Tensor(0.1, 0.2, 0.3).dot(Tensor(0.4, 0.3, 0.7))
    .assert(_ === 0.31 +/- 0.000001)

    test(m"Calculate integer cross-product"):
      Tensor(1, 2, 3).cross(Tensor(4, 3, 7))
    .assert(_ == Tensor(5, 5, -5))

    test(m"Calculate Double cross-product"):
      Tensor(1.4, 2.4, 3.8).cross(Tensor(4.9, 3.6, 0.7))
    .assert(_ == Tensor(-12.0, 17.64, -6.72))

    test(m"Show Tensor 3-tensor"):
      Tensor(1, 3, 6).show
    .assert(_ == t"\u239b 1 \u239e\n\u239c 3 \u239f\n\u239d 6 \u23a0")

    test(m"Add two tensors"):
      Tensor(1, 2, 3) + Tensor(3, 4, 5)
    .assert(_ == Tensor(4, 6, 8))

    suite(m"Quantity operations"):
      test(m"Add two quantity tensors"):
        Tensor(1.0*Metre, 2.0*Metre, 3.0*Metre) + Tensor(3.0*Metre, 4.0*Metre, 5.0*Metre)
      .assert(_ == Tensor(4.0*Metre, 6.0*Metre, 8.0*Metre))

      test(m"Add two mixed-quantity tensors"):
        Tensor(1.0*Foot, 1.0*Foot, 1.0*Foot) + Tensor(3.0*Metre, 4.0*Metre, 5.0*Metre)
      .assert(_ == Tensor(3.3048*Metre, 4.3048*Metre, 5.3048*Metre))

      test(m"Map from m to m²"):
        Tensor(1.0*Metre, 2.0*Metre, 3.0*Metre, 4.0*Metre).map(_*Metre)
      .assert(_ == Tensor(1.0*Metre*Metre, 2.0*Metre*Metre, 3.0*Metre*Metre, 4.0*Metre*Metre))

    suite(m"Matrix tests"):
      val m1 = Matrix[2, 3]((1, 2, 3), (4, 5, 6))
      val m2 = Matrix[3, 2]((7, 8), (9, 10), (11, 12))

      test(m"Access matrix elements"):
        m1(0, 0)
      .assert(_ == 1)

      test(m"Access matrix elements2"):
        m1(1, 1)
      .assert(_ == 5)

      test(m"Access matrix elements 3"):
        m1(1, 2)
      .assert(_ == 6)

      test(m"Multiply matrices"):
        m1*m2
      .assert(_ == Matrix[2, 2]((58, 139), (64, 154)))

      test(m"Scalar multiply matrices"):
        m1*10
      .assert(_ == Matrix[2, 3]((10, 20, 30), (40, 50, 60)))

      test(m"Scalar divide matrices"):
        m1/2
      .assert(_ == Matrix[2, 3]((0, 1, 1), (2, 2, 3)))

    suite(m"Interesting types"):
      test(m"Dot product of a tensor of quantities"):
        val v1 = Tensor(5*Inch, 2*Inch, Inch)
        val v2 = Tensor(2*Inch, 3*Inch, 6*Inch)
        v1.dot(v2)
      .assert(_ == 22*Inch*Inch)

      test(m"Cross product of a tensor of quantities"):
        val v1 = Tensor(5*Inch, 2*Inch, Inch)
        val v2 = Tensor(2*Inch, 3*Inch, 6*Inch)
        v1.cross(v2)
      .assert(_ == Tensor(9*Inch, -28*Inch, 11*Inch))

      test(m"Sum of two tensors of quantities"):
        val v1 = Tensor(5*Inch, 2*Inch, Inch)
        val v2 = Tensor(2*Inch, 3*Inch, 6*Inch)
        v1 + v2
      .assert(_ == Tensor(7*Inch, 5*Inch, 7*Inch))

      test(m"Sum of two tensors of different quantities"):
        val v1 = Tensor(5*Inch, 2*Inch, Inch)
        val v2 = Tensor(2*Metre, 3*Metre, 6*Metre)

        val sum = v1 + v1
      .assert()
