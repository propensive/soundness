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
package prescience

import scala.quoted.*

// The tier-A subjects: static singletons compiled in this component, one
// phase before the `test` use site, so the deriving macro can load them
// through the macro classloader at expansion time. Their runtime siblings
// throw, so a test that silently fell back to the runtime tier fails loudly —
// the passing tests *prove* the generated code came from `read`.
object IntInlinable extends Inlinable:
  type Self = Int
  def read(input: Expr[String])(using Quotes): Expr[Int] =
    '{ java.lang.Integer.parseInt($input.trim.nn) }

  def readRuntime(input: String): Int =
    throw AssertionError("prescience: runtime tier invoked for a static instance")

object StringInlinable extends Inlinable:
  type Self = String
  def read(input: Expr[String])(using Quotes): Expr[String] = input

  def readRuntime(input: String): String =
    throw AssertionError("prescience: runtime tier invoked for a static instance")

object BooleanInlinable extends Inlinable:
  type Self = Boolean
  def read(input: Expr[String])(using Quotes): Expr[Boolean] =
    '{ java.lang.Boolean.parseBoolean($input.trim.nn) }

  def readRuntime(input: String): Boolean =
    throw AssertionError("prescience: runtime tier invoked for a static instance")

// Given aliases as members of a static object: the member-access path of the
// evaluator (load the module, invoke the parameterless accessor).
object inlinables:
  given int: (Int is Inlinable) = IntInlinable
  given string: (String is Inlinable) = StringInlinable
  given boolean: (Boolean is Inlinable) = BooleanInlinable

// The tier-B subject: its given is conditional, so the summoned tree is an
// application — not a static path — and tier A cannot evaluate it. The inner
// implicit search of the staging tier resolves and *runs* it, yielding a live
// instance at expansion time; the throwing runtime sibling again proves that
// a passing test inlined at compile time.
case class Celsius(degrees: Int)

object Celsius:
  given inlinable: (dummy: DummyImplicit) => (Celsius is Inlinable) = new Inlinable:
    type Self = Celsius

    def read(input: Expr[String])(using Quotes): Expr[Celsius] =
      '{ Celsius(java.lang.Integer.parseInt($input.trim.nn)) }

    def readRuntime(input: String): Celsius =
      throw AssertionError("prescience: runtime tier invoked for a staging-tier instance")
