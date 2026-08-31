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
package probably

import anticipation.*
import chiaroscuro.*
import digression.*
import fulminate.*
import gossamer.*
import hypotenuse.*
import iridescence.*
import nomenclature.*
import prepositional.*
import symbolism.*

// The default four-decimal-place rendering for numeric comparisons, imported
// decisively rather than silently supplied by `import probably.*`.
package decimalizers:
  given fourDecimalPlaces: Decimalizer = Decimalizer(4)

export Baseline.Compare.{Min, Mean, Max}
export Baseline.Metric.{Cadential, Temporal}
export Baseline.Mode.{Arithmetic, Geometric}

// Exported at package level so that `n"…"` moniker literals work wherever probably is
// imported, without a separate `import Probing.nominative` in every suite.
export Probing.nominative

  // A real trait, not a structural refinement of `Palette`: structural member selection goes
  // through `iridescence.Palette.selectDynamic` — runtime reflection, which Scala Native does not
  // support — whereas these are ordinary virtual calls.


trait TestPalette extends JuxtapositionPalette:
  type Form = Srgb
  def warning: Color in Srgb
  def critical: Color in Srgb
  def benchmark: Color in Srgb
  def mixed: Color in Srgb
  def informative: Color in Srgb
  def cold: Color in Srgb
  def warm: Color in Srgb
  def hot: Color in Srgb
  def accented: Color in Srgb
  def highlight: Color in Srgb
  def detail: Color in Srgb
  def pass: Color in Srgb
  def fail: Color in Srgb
  def aspirePass: Color in Srgb
  def aspireFail: Color in Srgb
  def subdued: Color in Srgb
  def unaccented: Color in Srgb
  def positive: Color in Srgb
  def negative: Color in Srgb

// The checking vocabulary now lives in `anticipation.check`, so that modules which only need to
// compare values (notably `quantitative`) do not depend on the test framework. It is re-exported
// here so that `import probably.*` still provides it.
export anticipation.{!==, +/-, ===, Checkable, Tolerance, ±}


def test[report](name: Message)(using suite: Testable, codepoint: Codepoint): Test.Id =
  Test.Id(name, suite, codepoint)

// Declares a test with a stable moniker (a compile-time-checked Java identifier) alongside
// its description. The moniker addresses the test in selections and charts, independently
// of edits to the description.
def test[report](name: Name[Probing], description: Message)
  ( using suite: Testable, codepoint: Codepoint )
:   Test.Id =

  Test.Id(description, suite, codepoint, name)


def suite[report](name: Message)(using suite: Testable, runner: Runner[report])
  ( block: Testable ?=> Unit )
:   Unit =

  runner.suite(Testable(name, suite), block)


def suite[report](name: Name[Probing], description: Message)
  ( using suite: Testable, runner: Runner[report] )
  ( block: Testable ?=> Unit )
:   Unit =

  runner.suite(Testable(description, suite, name), block)


package harnesses:
  given threadLocal: Harness:
    private val delegate: Option[Harness] =
      Option(Runner.harnessThreadLocal.get()).map(_.nn).flatten

    override def capture[value: Decomposable](name: Text, value: value): value =
      delegate.map(_.capture[value](name, value)).getOrElse(value)

package autopsies:
  given contrastExpectations: Autopsy:
    type Analyse = true

  given none: Autopsy:
    type Analyse = false
