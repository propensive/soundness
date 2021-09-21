/*
    Probably, version 0.8.0. Copyright 2017-21 Jon Pretty, Propensive OÜ.

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

import probably.Runner.*
import probably.*
import scala.util.Try

object Tests extends Suite("Probably Tests"):

  def reportTest(fn: Runner => Unit): Report =
    val runner = Runner()
    fn(runner)
    runner.report()

  def run(using Runner): Unit =
    test("tests with the same name get grouped") {
      reportTest { runner =>
        runner("test")(2 + 2).assert(_ == 4)
        runner("test")(2 + 2).assert(_ == 4)
      }
    }.assert(_.results.size == 1)

    test("tests with different names displayed separately") {
      reportTest { runner =>
        runner("alpha")(2 + 2).assert(_ == 4)
        runner("beta")(2 + 2).assert(_ == 4)
      }
    }.assert(_.results.size == 2)

    test("tests can fail") {
      reportTest(_("failing")(2 + 2).assert(_ == 5))
    }.assert(_.results.head.outcome.failed)

    test("tests can succeed") {
      reportTest(_("failing")(2 + 2).assert(_ == 4))
    }.assert(_.results.head.outcome.passed)

    test("tests can throw an exception") {
      val runner = Runner()
      runner("failing") {
        throw new Exception()
        4
      }.assert(_ == 4)
      runner.report().results.head.outcome
    }.assert {
      case Outcome.FailsAt(_, _) => true
      case _                     => false
    }

    test("assertion can throw an exception") {
      val runner = Runner()
      runner("failing") { 2 + 2 }.assert { r =>
        throw new Exception()
        r == 4
      }

      runner.report().results.head.outcome
    }.assert {
      case Outcome.FailsAt(_, _) => true
      case _                     => false
    }

    test("repetition fails on nth attempt") {
      val report = reportTest { runner =>
        for i <- 1 to 10 do runner("integers are less than six")(i).assert(_ < 6)
      }

      report.results.head.outcome
    }.assert {
      case Outcome.FailsAt(_, 6) => true
      case _ => false
    }

    test("repetition captures failed value") {
      val report = reportTest { runner =>
        for i <- 1 to 10 do runner("integers are less than six") {
          i
        }.assert(_ < 6)
      }
      
      report.results.head.outcome
    }.assert {
      case Outcome.FailsAt(Datapoint.Fail(Debug(_, _, _, _, map), _), _) if map("i") == "6" => true
      case _                                                                                => false
    }

    //test.assert("assertion-only test")(1 + 1 == 2)

    test("time-only test") {
      reportTest(_.time("wait for 100ms")(Thread.sleep(100)))
    }.assert(_.results.head.ttot >= 100)

    test("repetition test") {
      reportTest(runner => for(i <- 1 to 100) runner("simple test")(1 + 1).assert(_ == 2))
    }.assert(_.results.head.count == 100)

    test("assert without predicate should succeed") {
      reportTest(_("test division")(5/2).assert(_ => true))
    }.assert { x => x.passed == 1 && x.total == 1 }

    test("assert without predicate should fail") {
      reportTest(_("test division")(5/0).assert(_ => true))
    }.assert { r => r.passed == 0 && r.failed == 1 }

    test("check without predicate should succeed") {
      reportTest(_("test division")(5/2).check(_ => true))
    }.assert { x => x.passed == 1 && x.total == 1 }

    test("check without predicate should fail") {
      reportTest(runner => Try(runner("test division")(5/0).check(_ => true)))
    }.assert { x => x.failed == 1 && x.total == 1 }

    test("assert with 2 successful predicates") {
      reportTest(_("test double")(0.001).assert { x => x >= 0.0 && x <= 1.0 })
    }.assert { r => r.passed == 1 && r.failed == 1 }

    test("assert with 2 predicates") {
      val report = reportTest(_("test double")(0.001).assert { x => x >= 0.0 && x < 0.0 })
      report.results.head.outcome
    }.assert(_ == Outcome.FailsAt(Datapoint.Fail(Debug(None), 1), 1))

    suite("Tolerance tests") {
      test("Compare numbers which are not similar enough") {
        3.14159 ~~ 3.4
      }.assert(_ == false)
      
      test("Compare numbers which are similar") {
        3.141593 ~~ 3.1415926
      }.assert(_ == true)

      test("Compare different case class instances") {
        case class Person(name: String, height: Float)
        Person("John Smith", 1.0) ~~ Person("John Smith", 1.1)
      }.assert(_ == false)
      
      test("Compare equal case class instances") {
        case class Person(name: String, height: Float)
        Person("John Smith", 1.0) ~~ Person("John Smith", 1.0)
      }.assert(_ == true)
      
      test("Compare similar case class instances") {
        case class Person(name: String, height: Float)
        Person("John Smith", 1.001) ~~ Person("John Smith", 1.0)
      }.assert(_ == true)
    }
