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
package cardinality

import fulminate.*
import larceny.*
import probably.*

object Tests extends Suite(m"Cardinality tests"):
  def run(): Unit =
    suite(m"Compile-time tests"):

      test(m"Value is less than lower bound"):
        demilitarize:
          val x: -1.0 ~ 1.0 = -1.01
      . assert(_.nonEmpty)

      test(m"Value is greater than upper bound"):
        demilitarize:
          val x: -1.0 ~ 1.0 = 1.01
      . assert(_.nonEmpty)

      test(m"Doubling a number doubles its range"):
        demilitarize:
          val x: -1.0 ~ 1.0 = 0.0
          val y: -2.0 ~ 2.0 = x*2.0
      . assert(_ == Nil)

      test(m"A value on the lower bound is accepted"):
        demilitarize:
          val x: -1.0 ~ 1.0 = -1.0
      . assert(_ == Nil)

      test(m"A value on the upper bound is accepted"):
        demilitarize:
          val x: -1.0 ~ 1.0 = 1.0
      . assert(_ == Nil)

    suite(m"Addition"):
      test(m"Adding two ranges adds their bounds"):
        demilitarize:
          val x: 0.0 ~ 1.0 = 0.5
          val y: 0.0 ~ 2.0 = 1.0
          val z: 0.0 ~ 3.0 = x + y
      . assert(_ == Nil)

      test(m"The sum of two ranges does not fit the wider operand's range"):
        demilitarize:
          val x: 0.0 ~ 1.0 = 0.5
          val y: 0.0 ~ 2.0 = 1.0
          val z: 0.0 ~ 2.0 = x + y
      . assert(_.nonEmpty)

      test(m"Adding a singleton shifts both bounds"):
        demilitarize:
          val x: 0.0 ~ 1.0 = 0.5
          val y: 1.0 ~ 2.0 = x + 1.0
      . assert(_ == Nil)

      test(m"Adding a singleton shifts the bounds by that amount, not another"):
        demilitarize:
          val x: 0.0 ~ 1.0 = 0.5
          val y: 0.0 ~ 1.0 = x + 1.0
      . assert(_.nonEmpty)

    suite(m"Subtraction"):
      test(m"Subtracting a range widens downwards"):
        demilitarize:
          val x: 0.0 ~ 1.0 = 0.5
          val y: 0.0 ~ 2.0 = 1.0
          val z: -2.0 ~ 1.0 = x - y
      . assert(_ == Nil)

      test(m"A difference that can go negative does not fit a non-negative range"):
        demilitarize:
          val x: 0.0 ~ 1.0 = 0.5
          val y: 0.0 ~ 2.0 = 1.0
          val z: 0.0 ~ 1.0 = x - y
      . assert(_.nonEmpty)

      test(m"Subtracting a singleton shifts both bounds down"):
        demilitarize:
          val x: 2.0 ~ 3.0 = 2.5
          val y: 0.0 ~ 1.0 = x - 2.0
      . assert(_ == Nil)

    suite(m"Multiplication across a sign boundary"):
      // Both operands straddle zero, so the extreme products are the two negative-times-positive
      // corners, not `leftMin*rightMin ~ leftMax*rightMax`. This is what `Min4`/`Max4` are for,
      // and it is the corner most likely to be got wrong.
      test(m"The product's bounds are the extremes of all four corner products"):
        demilitarize:
          val x: -2.0 ~ 1.0 = 0.0
          val y: -3.0 ~ 4.0 = 0.0
          val z: -8.0 ~ 6.0 = x*y
      . assert(_ == Nil)

      // Pinned on the inferred bound rather than merely "an error occurred", so that the test
      // still discriminates if the product type changes to something else that is also wrong.
      test(m"The product does not fit a range truncated at the naive upper bound"):
        demilitarize:
          val x: -2.0 ~ 1.0 = 0.0
          val y: -3.0 ~ 4.0 = 0.0
          val z: -8.0 ~ 4.0 = x*y
      . assert(_.exists(_.message.toString.contains("(-8.0d : scala.Double) ~ (6.0d : scala.Double)")))

      test(m"The product does not fit a range truncated at the naive lower bound"):
        demilitarize:
          val x: -2.0 ~ 1.0 = 0.0
          val y: -3.0 ~ 4.0 = 0.0
          val z: -6.0 ~ 6.0 = x*y
      . assert(_.nonEmpty)

      test(m"Multiplying by a negative singleton swaps the bounds"):
        demilitarize:
          val x: 1.0 ~ 2.0 = 1.5
          val y: -4.0 ~ -2.0 = x*(-2.0)
      . assert(_ == Nil)

    suite(m"Division"):
      test(m"Dividing by a positive singleton scales both bounds"):
        demilitarize:
          val x: 2.0 ~ 4.0 = 3.0
          val y: 1.0 ~ 2.0 = x/2.0
      . assert(_ == Nil)

      test(m"Dividing by a wholly-positive range takes the extremes of the four quotients"):
        demilitarize:
          val x: 2.0 ~ 4.0 = 3.0
          val y: 1.0 ~ 2.0 = 1.5
          val z: 1.0 ~ 4.0 = x/y
      . assert(_ == Nil)

      // When the divisor's range straddles zero, `rightMin*rightMax` is negative and `Asym`
      // takes its falsehood branch, widening the quotient to the infinities. No finite range
      // can then hold the result.
      test(m"Dividing by a range straddling zero admits no finite bound"):
        demilitarize:
          val x: 2.0 ~ 4.0 = 3.0
          val y: -1.0 ~ 1.0 = 0.5
          val z: 0.0 ~ 100.0 = x/y
      . assert:
          _.exists:
            _.message.toString.contains
              ("(-Infinityd : scala.Double) ~ (Infinityd : scala.Double)")

    suite(m"Runtime behaviour"):
      test(m"The underlying value survives the opaque type"):
        val x: 0.0 ~ 1.0 = 0.5
        x.double
      . assert(_ == 0.5)

      test(m"An in-range value is forced without change"):
        0.75.force[0.0, 1.0].double
      . assert(_ == 0.75)

      test(m"Arithmetic computes the value, not just the type"):
        val x: 0.0 ~ 1.0 = 0.5
        val y: 0.0 ~ 2.0 = 1.25
        (x + y).double
      . assert(_ == 1.75)

    suite(m"Type test"):
      // The `comparable` given is what lets a range type be used in pattern position; without it
      // a `Double` could never be narrowed to a checked range at runtime.
      test(m"A value inside the range matches"):
        val value: Double = 0.5

        value match
          case narrowed: (0.0 ~ 1.0) => narrowed.double
          case _                     => -1.0

      . assert(_ == 0.5)

      test(m"A value above the range does not match"):
        val value: Double = 1.5

        value match
          case narrowed: (0.0 ~ 1.0) => narrowed.double
          case _                     => -1.0

      . assert(_ == -1.0)

      test(m"A value below the range does not match"):
        val value: Double = -0.5

        value match
          case narrowed: (0.0 ~ 1.0) => narrowed.double
          case _                     => -1.0

      . assert(_ == -1.0)

      test(m"A value on the boundary matches"):
        val value: Double = 1.0

        value match
          case narrowed: (0.0 ~ 1.0) => narrowed.double
          case _                     => -1.0

      . assert(_ == 1.0)
