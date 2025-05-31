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
┃    Soundness, version 0.32.0.                                                                    ┃
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
import prepositional.*
import proscenium.*

given realm: Realm = realm"probably"

given decimalizer: Decimalizer = Decimalizer(3)

export Baseline.Compare.{Min, Mean, Max}
export Baseline.Metric.{BySpeed, ByTime}
export Baseline.Mode.{Arithmetic, Geometric}

extension [left](left: left)
  infix def === [right](right: right)(using checkable: left is Checkable against right): Boolean =
    checkable.check(left, right)

  infix def !== [right](right: right)(using checkable: left is Checkable against right): Boolean =
    !checkable.check(left, right)

extension (value: Double)
  @targetName("plusOrMinus")
  infix def +/- (tolerance: Double): Tolerance = Tolerance(value, tolerance)

def test[report](name: Message)(using suite: Testable, codepoint: Codepoint): TestId =
  TestId(name, suite, codepoint)


def suite[report](name: Message)(using suite: Testable, runner: Runner[report])
   (block: Testable ?=> Unit)
: Unit =

    runner.suite(Testable(name, suite), block)


extension [test](test: Test[test])
  inline def aspire[report](inline predicate: test => Boolean)
              (using runner: Runner[report],
                     inc:    Inclusion[report, Verdict],
                     inc2:   Inclusion[report, Verdict.Detail])
  : Unit =

      ${Probably.aspire[test, report]('test, 'runner, 'inc, 'inc2)}


  inline def assert[report]
              (inline predicate: test => Boolean)
              (using runner:     Runner[report],
                     inclusion:  Inclusion[report, Verdict],
                     inclusion2: Inclusion[report, Verdict.Detail])
  : Unit =

      ${Probably.assert[test, report]('test, 'predicate, 'runner, 'inclusion, 'inclusion2)}


  inline def check[report]
              (inline predicate: test => Boolean)
              (using runner:     Runner[report],
                     inclusion:  Inclusion[report, Verdict],
                     inclusion2: Inclusion[report, Verdict.Detail])
  : test =

      ${Probably.check[test, report]('test, 'predicate, 'runner, 'inclusion, 'inclusion2)}


  inline def assert[report]()
              (using runner:     Runner[report],
                     inclusion:  Inclusion[report, Verdict],
                     inclusion2: Inclusion[report, Verdict.Detail])
  : Unit =

      ${
          Probably.assert[test, report]
           ('test, '{Probably.succeed}, 'runner, 'inclusion, 'inclusion2) }


  inline def check[report]()
              (using runner:     Runner[report],
                     inclusion:  Inclusion[report, Verdict],
                     inclusion2: Inclusion[report, Verdict.Detail])
  : test =

      ${
          Probably.check[test, report]
           ('test, '{Probably.succeed}, 'runner, 'inclusion, 'inclusion2) }


  inline def matches[report](inline pf: test ~> Any)
              (using runner: Runner[report],
                     inc:    Inclusion[report, Verdict],
                     inc2:   Inclusion[report, Verdict.Detail])
  : Unit =

      assert[report](pf.isDefinedAt(_))


extension [value](inline value: value)(using inline test: Harness)
  inline def debug: value = ${Probably.debug('value, 'test)}

package harnesses:
  given threadLocal: Harness = new Harness():
    private val delegate: Option[Harness] =
      Option(Runner.harnessThreadLocal.get()).map(_.nn).flatten

    override def capture[value: Decomposable](name: Text, value: value): value =
      delegate.map(_.capture[value](name, value)).getOrElse(value)
