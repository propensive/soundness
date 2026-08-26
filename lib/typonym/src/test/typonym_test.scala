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
package typonym

import soundness.*

object Tests extends Suite(m"Typonym tests"):
  def run(): Unit =
    test(m"Get a list of strings"):
      reify[TypeList[("one", "two", "three")]]
    . assert(_ == List("one", "two", "three"))

    test(m"Get a map of strings"):
      reify[TypeMap[((1, "one"), (2, "two"), (3, "three"))]]
    . assert(_ == Map(1 -> "one", 2 -> "two", 3 -> "three"))

    test(m"Get a multimap of strings"):
      reify[TypeMap[((1, TypeList[("one", "un", "ein")]), (2, TypeList[("two", "zwei", "deux")]))]]
    . assert(_ == Map(1 -> List("one", "un", "ein"), 2 -> List("two", "zwei", "deux")))

    test(m"Reify a set of strings"):
      reify[TypeSet["one" | "two" | "three" | "four"]]
    . assert(_ == List("one", "two", "three", "four"))

    test(m"use Reifiable typeclass"):
      def foo[T](using reifiable: T is Reifiable to List[String]): List[String] =
        reifiable.reification()
      foo["yes" | "no" | "maybe"]
    . assert(_ == List("yes", "no", "maybe"))

    suite(m"Primitive constants"):
      // Only `String` and `Int` constants were reached by the tests above, via the list and map
      // cases; `constant` handles four kinds and the other two had no coverage.
      test(m"Reify a string literal directly"):
        reify["solo"]
      . assert(_ == "solo")

      test(m"Reify an integer literal directly"):
        reify[42]
      . assert(_ == 42)

      test(m"Reify a boolean literal"):
        reify[true]
      . assert(_ == true)

      test(m"Reify a double literal"):
        reify[3.5]
      . assert(_ == 3.5)

      test(m"Reify a list of booleans"):
        reify[TypeList[(true, false, true)]]
      . assert(_ == List(true, false, true))

      test(m"Reify a list of doubles"):
        reify[TypeList[(1.5, 2.5)]]
      . assert(_ == List(1.5, 2.5))

    suite(m"reifyAs"):
      test(m"Reify a set at an explicitly-stated result type"):
        reifyAs[TypeSet["a" | "b"], List[String]]
      . assert(_ == List("a", "b"))

      test(m"The stated result type is checked against the reification"):
        demilitarize:
          reifyAs[TypeSet["a" | "b"], List[Int]]
      . assert(_.nonEmpty)

      // Aspirational: only the `TypeSet` case reifies at its element type, building `List[set]`.
      // The `TypeList` case reifies each element independently and so produces a `List[Any]`,
      // making `reifyAs` at a narrower element type fail its cast — which is why
      // `Reifiable.listUnion` routes through `TypeSet`. A type list knows its element types just
      // as well as a type set does, so this asymmetry looks like a bug rather than a limit.
      test(m"A type list can be reified at its element type"):
        demilitarize:
          reifyAs[TypeList[("a", "b")], List[String]]
        . map(_.message)
      . aspire(_.isEmpty)

    suite(m"Reifiable"):
      test(m"A directly-constructed Reifiable returns its value"):
        Reifiable[Any, List[String]](List("x", "y")).reification()
      . assert(_ == List("x", "y"))

      test(m"reify is an alias for reification()"):
        Reifiable[Any, List[String]](List("x", "y")).reify
      . assert(_ == List("x", "y"))

    suite(m"Compile errors"):
      // `constant` matches only the four literal kinds, so anything else must be rejected at
      // expansion time rather than reified as something unintended.
      test(m"A non-literal type cannot be reified"):
        demilitarize:
          reify[Int]
      . assert(_.nonEmpty)

      test(m"A non-literal element of a type list cannot be reified"):
        demilitarize:
          reify[TypeList[(String, "two")]]
      . assert(_.nonEmpty)

      test(m"A non-literal member of a type set cannot be reified"):
        demilitarize:
          reify[TypeSet["one" | Int]]
      . assert(_.nonEmpty)

