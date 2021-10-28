/*
    Probably, version 0.18.0. Copyright 2017-21 Jon Pretty, Propensive OÜ.

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
import gossamer.*
import rudiments.*
import eucalyptus.*
import scala.util.Try

import unsafeExceptions.canThrowAny

given Log(Everything |-> Stdout)

object Tests extends Suite(str"Probably Tests"):

  def reportTest(fn: Runner => Unit): Report =
    val runner = Runner()
    fn(runner)
    runner.report()

  def run(using Runner): Unit =
    test(str"tests with the same name get grouped") {
      reportTest { runner =>
        runner(str"test")(2 + 2).assert(_ == 4)
        runner(str"test")(2 + 2).assert(_ == 4)
      }
    }.assert(_.results.size == 1)

    test(str"tests with different names displayed separately") {
      reportTest { runner =>
        runner(str"alpha")(2 + 2).assert(_ == 4)
        runner(str"beta")(2 + 2).assert(_ == 4)
      }
    }.assert(_.results.size == 2)

    test(str"tests can fail") {
      reportTest(_(str"failing")(2 + 2).assert(_ == 5))
    }.assert(_.results.head.outcome.failed)

    test(str"tests can succeed") {
      reportTest(_(str"failing")(2 + 2).assert(_ == 4))
    }.assert(_.results.head.outcome.passed)

    test(str"tests can throw an exception") {
      val runner = Runner()
      runner(str"failing") {
        throw new Exception()
        4
      }.assert(_ == 4)
      runner.report().results.head.outcome
    }.assert {
      case Outcome.FailsAt(_, _) => true
      case _                     => false
    }

    test(str"assertion can throw an exception") {
      val runner = Runner()
      runner(str"failing") { 2 + 2 }.assert { r =>
        throw new Exception()
        r == 4
      }

      runner.report().results.head.outcome
    }.assert {
      case Outcome.FailsAt(_, _) => true
      case _                     => false
    }

    test(str"repetition fails on nth attempt") {
      val report = reportTest { runner =>
        for i <- 1 to 10 do runner(str"integers are less than six")(i).assert(_ < 6)
      }

      report.results.head.outcome
    }.assert {
      case Outcome.FailsAt(_, 6) => true
      case _                     => false
    }

    test(str"time-only test") {
      reportTest(_.time(str"wait for 100ms")(Thread.sleep(100)))
    }.assert(_.results.head.ttot >= 100)

    test(str"repetition test") {
      reportTest(runner => for(i <- 1 to 100) runner(str"simple test")(1 + 1).assert(_ == 2))
    }.assert(_.results.head.count == 100)

    test(str"assert without predicate should succeed") {
      reportTest(_(str"test division")(5/2).assert(_ => true))
    }.assert { x => x.passed == 1 && x.total == 1 }

    test(str"assert without predicate should fail") {
      reportTest(_(str"test division")(5/0).assert(_ => true))
    }.assert { r => r.passed == 0 && r.failed == 1 }

    test(str"check without predicate should succeed") {
      reportTest(_(str"test division")(5/2).check(_ => true))
    }.assert { x => x.passed == 1 && x.total == 1 }

    test(str"check without predicate should fail") {
      reportTest(runner => Try(runner(str"test division")(5/0).check(_ => true)))
    }.assert { x => x.failed == 1 && x.total == 1 }

    test(str"assert with 2 successful predicates") {
      reportTest(_(str"test double")(0.001).assert { x => x >= 0.0 && x <= 1.0 })
    }.assert { r => r.passed == 1 && r.failed == 0 && r.total == 1 }

    suite(str"Tolerance tests") {
      test(str"Compare numbers which are not similar enough") {
        3.14159 ~~ 3.4
      }.assert(_ == false)
      
      test(str"Compare numbers which are similar") {
        3.141593 ~~ 3.1415926
      }.assert(_ == true)

      test(str"Compare different case class instances") {
        case class Person(name: Txt, height: Float)
        Person(str"John Smith", 1.0) ~~ Person(str"John Smith", 1.1)
      }.assert(_ == false)
      
      test(str"Compare equal case class instances") {
        case class Person(name: Txt, height: Float)
        Person(str"John Smith", 1.0) ~~ Person(str"John Smith", 1.0)
      }.assert(_ == true)
      
      test(str"Compare similar case class instances") {
        case class Person(name: Txt, height: Float)
        Person(str"John Smith", 1.001) ~~ Person(str"John Smith", 1.0)
      }.assert(_ == true)
    }
