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
package probably

import anticipation.*
import chiaroscuro.*
import digression.*
import fulminate.*
import gossamer.*
import hypotenuse.*
import iridescence.*
import prepositional.*
import proscenium.*
import symbolism.*

private given realm: Realm = realm"probably"

given decimalizer: Decimalizer = Decimalizer(4)

export Baseline.Compare.{Min, Mean, Max}
export Baseline.Metric.{Cadential, Temporal}
export Baseline.Mode.{Arithmetic, Geometric}

type TestPalette = Palette:
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
  def subdued: Color in Srgb

extension [left](left: left)
  infix def === [right](right: right)(using checkable: left is Checkable against right): Boolean =
    checkable.check(left, right)

  infix def !== [right](right: right)(using checkable: left is Checkable against right): Boolean =
    !checkable.check(left, right)

extension [value](value: value)
  @targetName("plusOrMinus")
  inline infix def +/- (tolerance: value)
  ( using inline commensurable: value is Commensurable against value,
          addable:              value is Addable by value,
          equality:             addable.Result =:= value,
          subtractable:         value is Subtractable by value,
          equality2:            subtractable.Result =:= value )
  :   Tolerance[value] =

    Tolerance[value](value, tolerance)(_ >= _, _ + _, _ - _)


  @targetName("plusOrMinus2")
  inline infix def ± (tolerance: value)
    ( using inline commensurable: value is Commensurable against value,
                   addable:       value is Addable by value,
                   equality:      addable.Result =:= value,
                   subtractable:  value is Subtractable by value,
                   equality2:     subtractable.Result =:= value )
  :   Tolerance[value] =

    value +/- (tolerance)


def test[report](name: Message)(using suite: Testable, codepoint: Codepoint): TestId =
  TestId(name, suite, codepoint)


def suite[report](name: Message)(using suite: Testable, runner: Runner[report])
  ( block: Testable ?=> Unit )
:   Unit =

  runner.suite(Testable(name, suite), block)


extension [test](test: Test[test])
  inline def aspire(inline predicate: test => Boolean): Unit =
    ${probably.internal.aspire[test]('test)}


  inline def assert(inline predicate: test => Boolean): Unit =
    ${probably.internal.assert[test]('test, 'predicate)}


  inline def check(inline predicate: test => Boolean): test =
    ${probably.internal.check[test]('test, 'predicate)}

  inline def assert(): Unit = ${probably.internal.assert[test]('test, '{probably.internal.succeed})}
  inline def check(): test = ${probably.internal.check[test]('test, '{probably.internal.succeed})}
  inline def matches(inline pf: test ~> Any): Unit = assert(pf.isDefinedAt(_))

extension [value](inline value: value)(using inline test: Harness)
  inline def debug: value = ${probably.internal.debug('value, 'test)}

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
