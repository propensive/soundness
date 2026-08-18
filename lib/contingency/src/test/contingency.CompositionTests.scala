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
package contingency

import soundness.*

import proscenium.compat.*
import contingency.strategies.throwUnsafely

import errorDiagnostics.stackTracesDiagnostics
import denominative.asymptotics.linearSizeComplexity

case class CErrorA(value: Int)(using Diagnostics) extends Error(m"composition error a: $value")
case class CErrorB(value: Int)(using Diagnostics) extends Error(m"composition error b: $value")

case class Tally(values: List[Int])(using Diagnostics)
extends Error(m"tally of ${values.length} errors")

case class Located(items: List[(Text, Int)])(using Diagnostics)
extends Error(m"${items.length} located errors")

object CompositionTests extends Suite(m"Contingency composition"):

  def failA(n: Int): Int raises CErrorA = raise(CErrorA(n)) yet -1
  def abortA(n: Int): Int raises CErrorA = abort(CErrorA(n))

  // Direct `Validate` construction: a `raises … tracks …` function VALUE cannot be typed under
  // capture checking, so the body must beta-reduce away into `protect`'s inline position. See
  // rep/DECISIONS.md and jacinta.ValidationTests.
  private inline def collect(inline body: Unit raises CErrorA tracks Pointer): Tally =
    Validate[Tally, [r] =>> r raises CErrorA, Pointer]
      ( Tally(Nil), { case CErrorA(n) => Tally(accrual.values :+ n) } )
    . protect(body)

  private inline def collectB(inline body: Unit raises CErrorB tracks Pointer): Tally =
    Validate[Tally, [r] =>> r raises CErrorB, Pointer]
      ( Tally(Nil), { case CErrorB(n) => Tally(accrual.values :+ n) } )
    . protect(body)

  private inline def collectLocated(inline body: Unit raises CErrorA tracks Pointer): Located =
    Validate[Located, [r] =>> r raises CErrorA, Pointer]
      ( Located(Nil),
        { case CErrorA(n) => Located(accrual.items :+ (prior.let(_.text).or(t"?"), n)) } )
    . protect(body)

  def run(): Unit =
    suite(m"Eager evaluation and independence"):
      test(m"Two failing sibling ventures both collected, never forced"):
        collect:
          val a: Venture[Int] = venture(failA(1))
          val b: Venture[Int] = venture(failA(2))
          ()
        . values
      . assert(_ == List(1, 2))

      test(m"A clean venture evaluates once; forcing twice reuses the value"):
        var count = 0
        var total = 0

        collect:
          val a = venture { count += 1; 5 }
          guard:
            total = a() + a()
          ()

        (count, total)
      . assert(_ == (1, 10))

      test(m"abort inside a venture is delimited; siblings still contribute"):
        collect:
          val a = venture(abortA(1))
          val b = venture(failA(2))
          ()
        . values
      . assert(_ == List(1, 2))

    suite(m"Dependency skipping without cascades"):
      test(m"A step depending on a failed venture is skipped, with no cascade error"):
        var ran = false

        collect:
          val a = venture(failA(1))
          val b = venture(7)

          venture:
            val sum = a() + b()
            ran = true
            if sum > 0 then raise(CErrorA(99))

          ()
        . values -> ran
      . assert(_ == List(1) -> false)

      test(m"A dependent step with clean inputs runs and contributes its own errors"):
        collect:
          val a = venture(3)
          val b = venture(4)

          venture:
            if a() + b() > 0 then raise(CErrorA(99))

          val c = venture(failA(3))
          ()
        . values
      . assert(_ == List(99, 3))

      test(m"Forcing a Venture without a skip-scope in context does not compile"):
        demilitarize:
          def force(v: Venture[Int]): Int = v()
      . assert(_.nonEmpty)

      test(m"A nested venture's skip is delimited; later siblings still contribute"):
        collect:
          val a = venture(failA(1))
          val b = venture { a() + 1 }
          val c = venture(failA(4))
          ()
        . values
      . assert(_ == List(1, 4))

    suite(m"guard"):
      test(m"guard runs when the tactic is clean and yields the block's value"):
        recover:
          case Tally(values) => values.sum
        . protect:
            track[Pointer](Tally(Nil)):
              case CErrorA(n) => Tally(accrual.values :+ n)
            . protect:
                val a = venture(3)
                guard(a() * 2)
      . assert(_ == 6)

      test(m"guard is skipped when tainted; the full accrual is reported"):
        var probe = false

        val outcome = recover:
          case Tally(values) => values
        . protect:
            track[Pointer](Tally(Nil)):
              case CErrorA(n) => Tally(accrual.values :+ n)
            . protect:
                val a = venture(failA(1))
                val b = venture(failA(2))

                guard:
                  probe = true
                  List(a(), b())

        outcome -> probe
      . assert(_ == List(1, 2) -> false)

      test(m"A raise inside an outer guard makes an inner guard certify out"):
        var inner = false
        var after = false

        val outcome = recover:
          case Tally(values) => values
        . protect:
            track[Pointer](Tally(Nil)):
              case CErrorA(n) => Tally(accrual.values :+ n)
            . protect:
                guard:
                  raise(CErrorA(5))
                  guard { inner = true; List.empty[Int] }
                  after = true
                  List.empty[Int]

        (outcome, inner, after)
      . assert(_ == (List(5), false, false))

      test(m"guard is the identity under safely"):
        safely[CErrorA](guard(5))
      . assert(_ == 5)

      test(m"guard is the identity under unsafely"):
        unsafely[CErrorA](guard(9))
      . assert(_ == 9)

      test(m"guard is the identity under attempt"):
        attempt[CErrorA](guard(5))
      . assert(_ == Attempt.Success(5))

      test(m"Under safely, a failing venture escapes eagerly, before any guard"):
        safely[CErrorA]:
          val a = venture(failA(1))
          guard(9)
      . assert(_ == Unset)

    suite(m"Nested aggregation scopes"):
      test(m"validate inside validate: inner aggregation is fully delimited"):
        var innerValues: List[Int] = List(-1)

        collect:
          val inner = collect:
            failA(1)
            failA(2)
            ()

          innerValues = inner.values
          ()
        . values -> innerValues
      . assert(_ == List() -> List(1, 2))

      test(m"ventures and guard under accrue"):
        var probe = false

        val outcome = capture[Tally]:
          accrue(Tally(Nil)) { (tally, error) => error match
            case CErrorA(n) => Tally(tally.values :+ n)
            case _          => tally
          } { case CErrorA(_) => () }
          . protect:
              val a = venture(failA(1))
              val b = venture(failA(2))
              guard { probe = true; () }
              ()

        outcome.values -> probe
      . assert(_ == List(1, 2) -> false)

      test(m"track aborts the outer tactic with the fold, with ventures"):
        recover:
          case Tally(values) => values
        . protect:
            track[Pointer](Tally(Nil)):
              case CErrorA(n) => Tally(accrual.values :+ n)
            . protect:
                venture(failA(1))
                venture(failA(2))
                List.empty[Int]
      . assert(_ == List(1, 2))

    suite(m"Composition across raises boundaries"):
      test(m"Both inner-library failures surface through mitigate, transformed"):
        def innerLibrary(): Unit raises CErrorA =
          venture(failA(1))
          venture(failA(2))
          ()

        collectB:
          mitigate:
            case CErrorA(n) => CErrorB(n + 100)
          . protect(innerLibrary())
          ()
        . values
      . assert(_ == List(101, 102))

      test(m"Taint crosses a mitigation boundary: inner guard is skipped"):
        var probe = false

        def innerLibrary(): Unit raises CErrorA =
          guard { probe = true; () }
          ()

        collectB:
          raise(CErrorB(1))

          mitigate:
            case CErrorA(n) => CErrorB(n + 100)
          . protect(innerLibrary())
          ()
        . values -> probe
      . assert(_ == List(1) -> false)

    suite(m"Decoding shapes"):
      test(m"Choice point: a failed discriminator reports one error and no branch runs"):
        var branchA = false
        var branchB = false

        collect:
          val tag = venture(failA(10))

          venture:
            tag() match
              case 1 => branchA = true
              case _ => branchB = true

          ()
        . values -> (branchA, branchB)
      . assert(_ == List(10) -> (false, false))

      test(m"Choice point: clean discriminator, failing branch contributes its errors"):
        var branchA = false
        var branchB = false

        collect:
          val tag = venture(1)

          venture:
            if tag() == 1 then
              branchA = true
              failA(7)
              ()
            else branchB = true

          ()
        . values -> (branchA, branchB)
      . assert(_ == List(7) -> (true, false))

      test(m"Collection decode: failures carry element pointers; assembly is skipped"):
        var assembled = false

        val outcome = collectLocated:
          val items = List(0, 1, 2, 3, 4).map: index =>
            focus(prior.or(Pointer.Self)(index.toString.tt)):
              venture(if index % 2 == 0 then index else failA(index))

          guard:
            val values = items.map(_())
            assembled = true

          ()

        outcome.items -> assembled
      . assert(_ == List(t"1" -> 1, t"3" -> 3) -> false)

      test(m"focus supplements venture-recorded errors"):
        collectLocated:
          focus(prior.or(Pointer.Self)(t"field")):
            venture(failA(9))
          ()
        . items
      . assert(_ == List(t"field" -> 9))

    suite(m"Existing semantics unchanged"):
      test(m"raise-with-ersatz still folds every error under validate"):
        collect:
          failA(1)
          failA(2)
          ()
        . values
      . assert(_ == List(1, 2))

      test(m"safely still returns Unset on the first raise"):
        safely(failA(2))
      . assert(_ == Unset)
