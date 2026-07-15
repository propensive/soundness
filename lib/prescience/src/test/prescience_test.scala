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
┃    Soundness, version 0.63.0.                                                                    ┃
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
package prescience

import scala.quoted.*

import soundness.*

import prescience.inlinables.{int, string, boolean}

case class Point(x: Int, y: Int)
case class Order(id: Int, name: String, active: Boolean)
case class Rank(value: Int)
case class Reading(celsius: Celsius, station: String)

// A same-compilation-run counterpart of `Celsius`: its conditional companion
// given has the shape the staging tier evaluates, but it is compiled in the
// same run as its use site, so no trustworthy classfile exists at expansion
// time (an incremental rebuild leaves a STALE one from the previous compile
// in the output directory, which sits on the macro classpath). Both tiers
// therefore refuse same-run definitions outright — the evaluator through a
// current-run symbol check, the staging tier by that check plus excluding
// the output directory from its inner classpath — and the macro
// deterministically degrades to the runtime tier, whose sibling here is
// real so the observable behavior is identical on clean and incremental
// builds.
case class Fahrenheit(degrees: Int)
case class Sample(fahrenheit: Fahrenheit)

object Fahrenheit:
  given inlinable: (dummy: DummyImplicit) => (Fahrenheit is Inlinable) = new Inlinable:
    type Self = Fahrenheit

    def read(input: Expr[String])(using Quotes): Expr[Fahrenheit] =
      '{ Fahrenheit(java.lang.Integer.parseInt($input.trim.nn)) }

    def readRuntime(input: String): Fahrenheit =
      Fahrenheit(java.lang.Integer.parseInt(input.trim.nn))

object Tests extends Suite(m"Prescience tests"):
  def run(): Unit =
    suite(m"Tier A: static instances evaluated at expansion time"):
      // The leaf instances' runtime siblings throw, so these passing at all
      // proves every field was generated by the instances' `read` methods at
      // compile time.
      test(m"A two-field record reads through inlined instance code"):
        Prescience.read[Point]("3,4")
      . assert(_ == Point(3, 4))

      test(m"Mixed field types each inline their own instance's code"):
        Prescience.read[Order]("7,widget,true")
      . assert(_ == Order(7, "widget", true))

    suite(m"Runtime fallback: same-run and lexical instances degrade"):
      test(m"A lexically-scoped instance routes through the runtime sibling"):
        // Defined in the compilation run that expands the macro, so its class
        // cannot be loaded at expansion time; the macro splices a call to
        // `readRuntime` instead of failing.
        given rank: (Rank is Inlinable) = new Inlinable:
          type Self = Rank

          def read(input: Expr[String])(using Quotes): Expr[Rank] =
            '{ Rank(java.lang.Integer.parseInt($input.trim.nn)) }

          def readRuntime(input: String): Rank =
            Rank(java.lang.Integer.parseInt(input.trim.nn))

        Prescience.read[Rank]("42")
      . assert(_ == Rank(42))

    suite(m"Tier B: staging evaluates a conditional given at expansion time"):
      test(m"A non-static instance inlines through the staging summon"):
        // `Celsius`'s given is conditional (not a static path), so only the
        // staging tier can evaluate it at expansion time — and its runtime
        // sibling throws, so this passing proves it did.
        Prescience.readStaging[Reading]("21,Kew")
      . assert(_ == Reading(Celsius(21), "Kew"))

      test(m"A same-run instance deterministically takes the runtime tier"):
        // Would be nondeterministic without the same-run guards (see the
        // comment on `Fahrenheit`); with them, clean and incremental builds
        // agree: the runtime tier serves it.
        Prescience.readStaging[Sample]("70")
      . assert(_ == Sample(Fahrenheit(70)))
