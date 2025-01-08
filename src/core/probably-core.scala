/*
    Probably, version [unreleased]. Copyright 2025 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package probably

import anticipation.*
import digression.*
import fulminate.*
import gossamer.*
import rudiments.*
import spectacular.*

given realm: Realm = realm"probably"

given Decimalizer = Decimalizer(3)

export Baseline.Compare.{Min, Mean, Max}
export Baseline.Metric.{BySpeed, ByTime}
export Baseline.Calc.{Ratio, Difference}

extension (value: Double)
  @targetName("plusOrMinus")
  infix def +/- (tolerance: Double): Tolerance = Tolerance(value, tolerance)
  infix def meets (tolerance: Tolerance): Boolean = tolerance.covers(value)

def test[ReportType](name: Text)(using suite: TestSuite, codepoint: Codepoint): TestId =
  TestId(name, suite, codepoint)

def suite[ReportType](name: Text)(using suite: TestSuite, runner: Runner[ReportType])
   (block: TestSuite ?=> Unit)
        : Unit =

  runner.suite(TestSuite(name, suite), block)

extension [TestType](test: Test[TestType])
  inline def aspire[ReportType](inline predicate: TestType => Boolean)
     (using runner: Runner[ReportType],
            inc: Inclusion[ReportType, Outcome],
            inc2: Inclusion[ReportType, Details])
      : Unit =
    ${Probably.aspire[TestType, ReportType]('test, 'runner, 'inc, 'inc2)}

  inline def assert[ReportType]
     (inline predicate: TestType => Boolean)
     (using runner:     Runner[ReportType],
            inclusion:  Inclusion[ReportType, Outcome],
            inclusion2: Inclusion[ReportType, Details])
          : Unit =
    ${Probably.assert[TestType, ReportType]('test, 'predicate, 'runner, 'inclusion, 'inclusion2)}

  inline def check[ReportType]
     (inline predicate: TestType => Boolean)
     (using runner:     Runner[ReportType],
            inclusion:  Inclusion[ReportType, Outcome],
            inclusion2: Inclusion[ReportType, Details])
          : TestType =
    ${Probably.check[TestType, ReportType]('test, 'predicate, 'runner, 'inclusion, 'inclusion2)}

  inline def assert[ReportType]()
     (using runner:     Runner[ReportType],
            inclusion:  Inclusion[ReportType, Outcome],
            inclusion2: Inclusion[ReportType, Details])
          : Unit =
    ${
        Probably.assert[TestType, ReportType]
         ('test, '{Probably.succeed}, 'runner, 'inclusion, 'inclusion2) }

  inline def check[ReportType]()
     (using runner:     Runner[ReportType],
            inclusion:  Inclusion[ReportType, Outcome],
            inclusion2: Inclusion[ReportType, Details])
          : TestType =

    ${
        Probably.check[TestType, ReportType]
         ('test, '{Probably.succeed}, 'runner, 'inclusion, 'inclusion2) }

  inline def matches[ReportType](inline pf: TestType ~> Any)
     (using runner: Runner[ReportType],
            inc:    Inclusion[ReportType, Outcome],
            inc2:   Inclusion[ReportType, Details])
          : Unit =

    assert[ReportType](pf.isDefinedAt(_))

extension [ValueType](inline value: ValueType)(using inline test: Harness)
  inline def debug: ValueType = ${Probably.debug('value, 'test)}

package harnesses:
  given threadLocal: Harness = new Harness():
    private val delegate: Option[Harness] =
      Option(Runner.harnessThreadLocal.get()).map(_.nn).flatten

    override def capture[ValueType: Inspectable](name: Text, value: ValueType): ValueType =
      delegate.map(_.capture[ValueType](name, value)).getOrElse(value)
