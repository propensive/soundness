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
┃    Soundness, version 0.54.0.                                                                    ┃
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
package acyclicity

import soundness.*

object Tests extends Suite(m"Acyclicity Tests"):
  def run(): Unit =
    // Divisibility poset on the divisors of 12: `a ≤ b` iff `a` divides `b`.
    val divisors = Set(1, 2, 3, 4, 6, 12)
    def divides(a: Int, b: Int): Boolean = b%a == 0

    suite(m"Hasse construction"):
      val hasse = Hasse(divisors)(divides)

      test(m"immediate supertypes are the covering multiples"):
        hasse.parents(2)
      . assert(_ == Set(4, 6))

      test(m"immediate subtypes are the covering divisors"):
        hasse.children(6)
      . assert(_ == Set(2, 3))

      test(m"the top element covers its two predecessors"):
        hasse.children(12)
      . assert(_ == Set(4, 6))

      test(m"a transitive edge is not a cover"):
        hasse.children(12).contains(2)
      . assert(_ == false)

      test(m"the maximum is the whole set's top"):
        hasse.maxima
      . assert(_ == Set(12))

      test(m"the minimum is the whole set's bottom"):
        hasse.minima
      . assert(_ == Set(1))

    suite(m"Deferred bottom"):
      val hasse = Hasse(divisors)(divides).bottom(0)

      test(m"the bottom becomes the sole minimum"):
        hasse.minima
      . assert(_ == Set(0))

      test(m"the former minimum now covers the bottom"):
        hasse.children(1)
      . assert(_ == Set(0))

      test(m"the bottom's parents are the former minima"):
        hasse.parents(0)
      . assert(_ == Set(1))

    suite(m"Comparison frugality"):
      test(m"construction compares fewer than all ordered pairs"):
        var count = 0
        Hasse(divisors): (a, b) =>
          count += 1
          divides(a, b)

        count
      . assert(_ < divisors.size*(divisors.size - 1))
