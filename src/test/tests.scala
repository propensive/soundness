/*

    Probably, version 0.3.0. Copyright 2017-20 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
    compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the License is
    distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and limitations under the License.

*/
package probably.tests

import probably._

object Main extends Suite() {

  def run(test: Runner): Unit = {
    test("tests with the same name get grouped") {
      val runner = new Runner()
      runner("test") { 2 + 2 }.assert(_ == 4)
      runner("test") { 2 + 2 }.assert(_ == 4)
      runner.report()
    }.assert(_.results.size == 1)
    
    test("tests with different names displayed separately") {
      val runner = new Runner()
      runner("alpha") { 2 + 2 }.assert(_ == 4)
      runner("beta") { 2 + 2 }.assert(_ == 4)
      runner.report()
    }.assert(_.results.size == 2)
    
    test("tests can fail") {
      val runner = new Runner()
      runner("failing") { 2 + 2 }.assert(_ == 5)
      runner.report()
    }.assert(_.results.head.outcome.failed)
    
    test("tests can succeed") {
      val runner = new Runner()
      runner("failing") { 2 + 2 }.assert(_ == 4)
      runner.report()
    }.assert(_.results.head.outcome.passed)
    
    test("tests can throw an exception") {
      val runner = new Runner()
      runner("failing") {
        throw new Exception()
        4
      }.assert(_ == 4)
      runner.report().results.head.outcome
    }.assert {
      case Runner.FailsAt(_, _) => true
      case _ => false
    }
    
    test("assertion can throw an exception") {
      val runner = new Runner()
      runner("failing") { 2 + 2 }.assert { r =>
        throw new Exception()
        r == 4
      }
      
      runner.report().results.head.outcome
    }.assert {
      case Runner.FailsAt(_, _) => true
      case _             => false
    }

    test("repetition fails on nth attempt") {
      val runner = new Runner()
      for(i <- 1 to 10) runner("integers are less than six")(i).assert(_ < 6)
      runner.report().results.head.outcome
    }.assert {
      case Runner.FailsAt(_, 6) => true
      case x => false
    }
    
    test("repetition captures failed value") {
      val runner = new Runner()
      for(i <- 1 to 10) runner("integers are less than six", i = i)(i).assert(_ < 6)
      runner.report().results.head.outcome
    }.assert {
      case Runner.FailsAt(Runner.Fail(map), _) if map("i") == "6" => true
      case x => false
    }

    test.assert("assertion-only test") {
      1 + 1 == 2
    }

    test("time-only test") {
      val runner = new Runner()
      runner.time("wait for 100ms") {
        Thread.sleep(100)
      }
      runner.report()
    }.assert(_.results.head.ttot >= 100)

    test("repetition test") {
      val runner = new Runner()
      for(i <- 1 to 100) runner("simple test") {
        1 + 1
      }.assert(_ == 2)
      runner.report
    }.assert(_.results.head.count == 100)
  }
}