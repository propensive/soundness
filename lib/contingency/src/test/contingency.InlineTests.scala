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

import errorDiagnostics.stackTracesDiagnostics

// No ambient strategy is imported here: with `strategies.throwUnsafely` in scope, as in the main
// suite, every `Tactic[…]` is satisfied and a raise the `defer…` forms should reject cannot be
// shown to be rejected. The errors and raisers are this file's own, since `demilitarize`
// compiles the file's source alone.
case class InlineErrorA(value: Int)(using Diagnostics) extends Error(m"inline error a: $value")
case class InlineErrorB(value: Int)(using Diagnostics) extends Error(m"inline error b: $value")

def inlineFailA(n: Int): String raises InlineErrorA =
  raise(InlineErrorA(n))
  "fallback"

def inlineFailB(n: Int): String raises InlineErrorB =
  raise(InlineErrorB(n))
  "fallback"

object InlineTests extends Suite("errors-inline", m"Contingency in inline bodies"):
  def run(): Unit =
    suite(m"defer… forms inside inline bodies"):
      test(m"a deferRecover rejects a raise its type argument does not admit"):
        demilitarize:
          inline def inlineRecover: String =
            deferRecover[InlineErrorA]:
              case InlineErrorA(n) => s"recovered:$n"
            . protect(inlineFailB(8))
          inlineRecover
        . exists { error =>
            error.message.contains("No given instance") && error.message.contains("InlineErrorB") }
      . assert(_ == true)

      test(m"a deferRecover over a union admits each member's raise"):
        demilitarize:
          inline def inlineRecover: String =
            deferRecover[InlineErrorA | InlineErrorB]:
              case InlineErrorA(n) => s"a:$n"
              case InlineErrorB(n) => s"b:$n"
            . protect(inlineFailA(1) + inlineFailB(2))
          inlineRecover
      . assert(_.nil)
