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
┃    Soundness, version 0.43.0.                                                                    ┃
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
package metamorphose

import contingency.*, strategies.throwUnsafely
import gossamer.*
import language.experimental.genericNumberLiterals
import probably.*
import fulminate.*
import rudiments.*

import errorDiagnostics.stackTraces

object Tests extends Suite(m"Metamorphose tests"):
  def run(): Unit =
    suite(m"Factorial tests"):
      test(m"Calculate 0!")(Factorial(0)).assert(_ == 1)
      test(m"Calculate 1!")(Factorial(1)).assert(_ == 1)
      test(m"Calculate 2!")(Factorial(2)).assert(_ == 2)
      test(m"Calculate 3!")(Factorial(3)).assert(_ == 6)
      test(m"Calculate 4!")(Factorial(4)).assert(_ == 24)

      val factorial21: BigInt = 51090942171709440000
      test(m"Calculate 21!")(Factorial(21)).assert(_ == factorial21)

      val factorial42: BigInt = 1405006117752879898543142606244511569936384000000000
      test(m"Calculate 42!")(Factorial(42)).assert(_ == factorial42)

    suite(m"Factoradic encoding tests"):
      test(m"2-element identity permutation")(Factoradic(List(0, 0))).assert(_ == Factoradic(0))
      test(m"4-element identity permutation")(Factoradic(List(0, 0, 0, 0))).assert(_ == Factoradic(0))
      test(m"(1, 0, 0, 0) permutation")(Factoradic(List(1, 0, 0, 0))).assert(_ == Factoradic(6))
      test(m"(2, 0, 0, 0) permutation")(Factoradic(List(2, 0, 0, 0))).assert(_ == Factoradic(12))
      test(m"(3, 0, 0, 0) permutation")(Factoradic(List(3, 0, 0, 0))).assert(_ == Factoradic(18))
      test(m"(0, 1, 0, 0) permutation")(Factoradic(List(0, 1, 0, 0))).assert(_ == Factoradic(2))
      test(m"(1, 1, 0, 0) permutation")(Factoradic(List(1, 1, 0, 0))).assert(_ == Factoradic(8))
      test(m"(2, 1, 0, 0) permutation")(Factoradic(List(2, 1, 0, 0))).assert(_ == Factoradic(14))
      test(m"(3, 1, 0, 0) permutation")(Factoradic(List(3, 1, 0, 0))).assert(_ == Factoradic(20))
      test(m"(0, 2, 0, 0) permutation")(Factoradic(List(0, 2, 0, 0))).assert(_ == Factoradic(4))
      test(m"(1, 2, 0, 0) permutation")(Factoradic(List(1, 2, 0, 0))).assert(_ == Factoradic(10))
      test(m"(2, 2, 0, 0) permutation")(Factoradic(List(2, 2, 0, 0))).assert(_ == Factoradic(16))
      test(m"(3, 2, 0, 0) permutation")(Factoradic(List(3, 2, 0, 0))).assert(_ == Factoradic(22))

      test(m"(0, 0, 1, 0) permutation")(Factoradic(List(0, 0, 1, 0))).assert(_ == Factoradic(1))
      test(m"(1, 0, 1, 0) permutation")(Factoradic(List(1, 0, 1, 0))).assert(_ == Factoradic(7))
      test(m"(2, 0, 1, 0) permutation")(Factoradic(List(2, 0, 1, 0))).assert(_ == Factoradic(13))
      test(m"(3, 0, 1, 0) permutation")(Factoradic(List(3, 0, 1, 0))).assert(_ == Factoradic(19))
      test(m"(0, 1, 1, 0) permutation")(Factoradic(List(0, 1, 1, 0))).assert(_ == Factoradic(3))
      test(m"(1, 1, 1, 0) permutation")(Factoradic(List(1, 1, 1, 0))).assert(_ == Factoradic(9))
      test(m"(2, 1, 1, 0) permutation")(Factoradic(List(2, 1, 1, 0))).assert(_ == Factoradic(15))
      test(m"(3, 1, 1, 0) permutation")(Factoradic(List(3, 1, 1, 0))).assert(_ == Factoradic(21))
      test(m"(0, 2, 1, 0) permutation")(Factoradic(List(0, 2, 1, 0))).assert(_ == Factoradic(5))
      test(m"(1, 2, 1, 0) permutation")(Factoradic(List(1, 2, 1, 0))).assert(_ == Factoradic(11))
      test(m"(2, 2, 1, 0) permutation")(Factoradic(List(2, 2, 1, 0))).assert(_ == Factoradic(17))
      test(m"(3, 2, 1, 0) permutation")(Factoradic(List(3, 2, 1, 0))).assert(_ == Factoradic(23))

      test(m"six-element reversal permutation")(Factoradic(List(5, 4, 3, 2, 1, 0)))
       .assert(_ == Factoradic(719))

      test(m"ensure that an error occurs for out-of-range bases"):
        capture[PermutationError](Factoradic(List(2, 1)))
      .assert(_ == PermutationError(PermutationError.Reason.BaseRange(2, 2)))

      test(m"ensure that an error occurs for more complex out-of-range base"):
        capture[PermutationError](Factoradic(List(0, 0, 8, 3, 0, 1, 0)))
      .assert(_ == PermutationError(PermutationError.Reason.BaseRange(8, 5)))

    suite(m"Factoradic decoding"):
      test(m"Check distinctness of factoradic expansions"):
        (0 to 1000).map(Factoradic(_).expand).to(Set).size
      .assert(_ == 1001)

    suite(m"Permutations"):
      test(m"Construct an identity permutation"):
        Permutation(Vector(0, 1, 2, 3, 4, 5))
      .assert(_ == Permutation(Factoradic(0)))

      test(m"Construct a reversal permutation"):
        Permutation(Vector(5, 4, 3, 2, 1, 0))
      .assert(_ == Permutation(Factoradic(719)))

      test(m"Reverse a list of numbers"):
        Permutation(Vector(5, 4, 3, 2, 1, 0))(List("one", "two", "three", "four", "five", "six"))
      .assert(_ == List("six", "five", "four", "three", "two", "one"))

      test(m"Reorder a list of numbers"):
        Permutation(Vector(3, 1, 4, 2, 0, 5))(List("zero", "one", "two", "three", "four", "five"))
      .assert(_ == List("three", "one", "four", "two", "zero", "five"))

      test(m"Check duplicate indexes are caught"):
        capture[PermutationError](Permutation(Vector(3, 1, 4, 2, 3, 5)))
      .assert(_ == PermutationError(PermutationError.Reason.DuplicateIndex(3, 4)))

      test(m"Check negative indexes are caught"):
        capture[PermutationError](Permutation(Vector(3, 1, 4, 2, -3, 5)))
      .assert(_ == PermutationError(PermutationError.Reason.InvalidIndex(-3, 5)))

      test(m"Check high indexes are caught"):
        capture[PermutationError](Permutation(Vector(3, 1, 4, 6, 0, 5)))
      .assert(_ == PermutationError(PermutationError.Reason.InvalidIndex(6, 5)))

      test(m"Check input is long enough"):
        val permutation = Permutation(Vector(3, 1, 4, 2, 0, 5))
        capture[PermutationError](permutation(List(1, 2, 3)))
      .assert(_ == PermutationError(PermutationError.Reason.TooShort(3, 6)))

      test(m"Check uniqueness of permutations"):
        val list = List("one", "two", "three", "four", "five", "six", "seven")
        Permutation.bySize(7).map(_(list)).to(Set).size
      .assert(_ == 5040)

      Permutation.bySize(6).each: permutation =>
        val list = List("one", "two", "three", "four", "five", "six")
        test(m"Inversion"):
          val applied = permutation(list)
          val inversion = permutation.inverse
          inversion(applied)
        .assert(_ == list)


      val indices = Vector(6, 2, 1, 0, 3, 5, 4)
      val permutation = Permutation(indices)
      for i <- 0 to 6 do
        test(m"Apply permutation indexwise"):
          permutation(i)
        .assert(_ == indices(i))
