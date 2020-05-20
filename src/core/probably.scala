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
package probably

import gastronomy._

import scala.util._
import scala.collection.immutable.ListMap
import scala.util.control.NonFatal

import language.dynamics

import Runner._

object Runner {
  object Showable {
    implicit def show[T: Show](value: T): Showable[T] = Showable[T](value, implicitly[Show[T]])
  }

  case class Showable[T](value: T, show: Show[T]) { def apply(): String = show.show(value) }

  case class TestId private[probably](value: String)
  sealed abstract class Outcome(val passed: Boolean) {
    def failed: Boolean = !passed
    def debug: String = ""
  }

  case class FailsAt(datapoint: Datapoint, count: Int) extends Outcome(false) {
    override def debug: String = datapoint.map.map { case (k, v) => s"$k=$v" }.mkString(" ")
  }

  case object Passed extends Outcome(true)
  case object Mixed extends Outcome(false)

  sealed abstract class Datapoint(val map: Map[String, String])
  case object Pass extends Datapoint(Map())

  case class Fail(failMap: Map[String, String]) extends Datapoint(failMap)
  case class Throws(exception: Throwable, throwMap: Map[String, String]) extends Datapoint(throwMap)
  case class ThrowsInCheck(exception: Exception, throwMap: Map[String, String]) extends Datapoint(throwMap)

  object Show {
    implicit val int: Show[Int] = _.toString
    implicit val string: Show[String] = identity
    implicit val boolean: Show[Boolean] = _.toString
    implicit val long: Show[Long] = _.toString
    implicit val byte: Show[Byte] = _.toString
    implicit val short: Show[Short] = _.toString
    implicit val char: Show[Char] = _.toString
  }

  trait Show[T] { def show(value: T): String }
}


class Runner(specifiedTests: Set[TestId] = Set()) extends Dynamic {

  final def runTest(testId: TestId): Boolean = specifiedTests.isEmpty || specifiedTests(testId)

  def applyDynamic[T](method: String)(name: String)(fn: => T): Test { type Type = T } =
    applyDynamicNamed[T]("")("" -> name)(fn)
  
  def applyDynamicNamed[T]
                       (method: String)
                       (name: (String, String), args: (String, Showable[_])*)
                       (fn: => T)
                       : Test { type Type = T } =
    new Test(name._2, args.toMap.mapValues(_())) {
      type Type = T
      def action(): T = fn
    }

  def time[T](name: String)(fn: => T): T = applyDynamicNamed[T]("")("" -> name)(fn).check { _ => true }

  def suite(name: String)(fn: Runner => Unit): Unit = {
    val report = new Test(name, Map()) {
      type Type = Report

      def action(): Report = {
        val runner = new Runner()
        fn(runner)
        runner.report()
      }
    }.check(_.results.forall(_.outcome.passed))

    if(report.results.exists(_.outcome.passed) && report.results.exists(_.outcome.failed))
      synchronized { results = results.updated(name, results(name).copy(outcome = Mixed)) }

    report.results.foreach { result => record(result.copy(indent = result.indent + 1)) }
  }

  def assert(name: String)(fn: => Boolean): Unit = applyDynamicNamed("")("" -> name)(fn).assert(identity)

  abstract class Test(val name: String, map: => Map[String, String]) {
    type Type
    
    def id: TestId = TestId(name.digest[Sha256].encoded[Hex].take(6).toLowerCase)
    def action(): Type
    
    def assert(predicate: Type => Boolean): Unit =
      try if(runTest(id)) check(predicate) catch { case NonFatal(e) => () }
    
    def check(predicate: Type => Boolean): Type = {
      val t0 = System.currentTimeMillis()
      val value = Try(action())
      val time = System.currentTimeMillis() - t0

      value match {
        case Success(value) =>
          def handler: PartialFunction[Throwable, Datapoint] = { case e: Exception => ThrowsInCheck(e, map) }
          val datapoint: Datapoint = try(if(predicate(value)) Pass else Fail(map)) catch handler
          record(this, time, datapoint)
          value
        case Failure(exception) =>
          record(this, time, Throws(exception, map))
          throw exception
      }
    }
  }

  def report(): Report = Report(results.values.to[List])

  def clear(): Unit = results = emptyResults()

  protected def record(test: Test, duration: Long, datapoint: Datapoint): Unit = synchronized {
    results = results.updated(test.name, results(test.name).append(test.name, duration, datapoint))
  }

  protected def record(summary: Summary) = synchronized {
    results = results.updated(summary.name, summary)
  }

  private[this] def emptyResults(): Map[String, Summary] = ListMap[String, Summary]().withDefault { name =>
    Summary(TestId(name.digest[Sha256].encoded[Hex].take(6).toLowerCase), name, 0, Int.MaxValue, 0L,
        Int.MinValue, Passed)
  }

  @volatile
  protected var results: Map[String, Summary] = emptyResults()
}

case class Summary(id: TestId,
                   name: String,
                   count: Int,
                   tmin: Long,
                   ttot: Long,
                   tmax: Long,
                   outcome: Outcome,
                   indent: Int = 0) {
  def avg: Double = ttot.toDouble/count/1000.0
  def min: Double = tmin.toDouble/1000.0
  def max: Double = tmax.toDouble/1000.0
  
  def append(test: String, duration: Long, datapoint: Datapoint): Summary =
    Summary(id, name, count + 1, tmin min duration, ttot + duration, tmax max duration, outcome match {
      case FailsAt(dp, c) => FailsAt(dp, c)
      case Passed         => datapoint match {
        case Pass           => Passed
        case other          => FailsAt(other, count + 1)
      }
      case Mixed          => FailsAt(datapoint, 0)
    })
}

case class Report(results: List[Summary]) {
  val passed: Int = results.count(_.outcome == Passed)
  val failed: Int = results.count(_.outcome != Passed)
  val total: Int = failed + passed
}
