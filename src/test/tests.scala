package probably.tests

import probably._

object Main extends Tests {

  def run(): Unit = {
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
      case FailsAt(_, _) => true
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
      case FailsAt(_, _) => true
      case _             => false
    }

    test("repetition fails on nth attempt") {
      val runner = new Runner()
      for(i <- 1 to 10) runner("integers are less than six")(i).assert(_ < 6)
      runner.report().results.head.outcome
    }.assert {
      case FailsAt(_, 6) => true
      case x => false
    }
    
    test("repetition captures failed value") {
      val runner = new Runner()
      for(i <- 1 to 10) runner("integers are less than six", i = i)(i).assert(_ < 6)
      runner.report().results.head.outcome
    }.assert {
      case FailsAt(Fail(map), _) if map("i") == "6" => true
      case x => false
    }
  }
}