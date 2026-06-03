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
package murmuration

// Imported narrowly rather than via `soundness.*`: the umbrella pulls in Mercator's
// wildcard functor `.map` extension, whose `Identity`-deriving macro aborts on this opaque
// `List`. That clash (extension `.map` vs the standard library's native member) is exactly
// what flipping the project-wide export must contend with; here we test the type in isolation.
import probably.*
import fulminate.*
import rudiments.*

import murmuration.{List, Nil, `::`, Series}

object Tests extends Suite(m"Murmuration tests"):
  def run(): Unit =
    test(m"Construct a list and measure its length"):
      List(1, 2, 3).length
    . assert(_ == 3)

    test(m"List.empty is empty"):
      List.empty[Int].scala.isEmpty
    . assert(_ == true)

    test(m"map transforms every element"):
      List(1, 2, 3).map(_ + 1)
    . assert(_ == List(2, 3, 4))

    test(m"filter keeps matching elements"):
      List(1, 2, 3, 4).filter(_ % 2 == 0)
    . assert(_ == List(2, 4))

    // `flatMap` expands the lambda's result via the `Expandable` typeclass, so the result may be
    // our own `List`, any standard-library collection, or an `Option` — no subtyping required.
    test(m"flatMap over our own List"):
      List(1, 2, 3).flatMap(n => List(n, n*10))
    . assert(_ == List(1, 10, 2, 20, 3, 30))

    test(m"flatMap over a standard-library collection"):
      List(1, 2, 3).flatMap(n => Vector(n, -n))
    . assert(_ == List(1, -1, 2, -2, 3, -3))

    test(m"flatMap over an Option filters absent results"):
      List(1, 2, 3).flatMap(n => if n > 1 then Some(n) else None)
    . assert(_ == List(2, 3))

    test(m"foldLeft accumulates"):
      List(1, 2, 3, 4).foldLeft(0)(_ + _)
    . assert(_ == 10)

    test(m"reverse reverses order"):
      List(1, 2, 3).reverse
    . assert(_ == List(3, 2, 1))

    test(m"cons prepends an element"):
      0 :: List(1, 2)
    . assert(_ == List(0, 1, 2))

    test(m"headOption of a non-empty list"):
      List(1, 2, 3).headOption
    . assert(_ == Some(1))

    // Pattern matching: Nil and head :: tail
    def sum(values: List[Int]): Int = values match
      case Nil          => 0
      case head :: tail => head + sum(tail)

    test(m"Recursive sum via Nil and :: patterns"):
      sum(List(1, 2, 3, 4))
    . assert(_ == 10)

    test(m"Fixed-arity List(...) pattern binds elements"):
      List(1, 2, 3) match
        case List(a, b, c) => a + b + c
        case _             => -1
    . assert(_ == 6)

    test(m"Construct a Series and measure its length"):
      Series(1, 2, 3).length
    . assert(_ == 3)

    test(m"Series indexes by position"):
      Series(10, 20, 30)(1)
    . assert(_ == 20)

    test(m"map transforms every Series element"):
      Series(1, 2, 3).map(_ + 1)
    . assert(_ == Series(2, 3, 4))

    test(m"filter keeps matching Series elements"):
      Series(1, 2, 3, 4).filter(_ % 2 == 0)
    . assert(_ == Series(2, 4))

    test(m"reverse reverses a Series"):
      Series(1, 2, 3).reverse
    . assert(_ == Series(3, 2, 1))

    test(m"sortBy orders a Series"):
      Series(3, 1, 2).sortBy(identity)
    . assert(_ == Series(1, 2, 3))

    test(m"prepend an element to a Series"):
      0 +: Series(1, 2)
    . assert(_ == Series(0, 1, 2))

    test(m"append an element to a Series"):
      Series(1, 2) :+ 3
    . assert(_ == Series(1, 2, 3))

    // The iteration vocabulary is provided by the `Traversable` typeclass, exactly as for `List`.
    test(m"foldLeft accumulates over a Series"):
      Series(1, 2, 3, 4).foldLeft(0)(_ + _)
    . assert(_ == 10)

    test(m"convert a List to a Series"):
      List(1, 2, 3).to[Series]
    . assert(_ == Series(1, 2, 3))

    test(m"convert a Series to a List"):
      Series(1, 2, 3).to[List]
    . assert(_ == List(1, 2, 3))

    test(m"Fixed-arity Series(...) pattern binds elements"):
      Series(1, 2, 3) match
        case Series(a, b, c) => a + b + c
        case _               => -1
    . assert(_ == 6)
