package probably

import scala.util._
import scala.collection.immutable.ListMap

import language.dynamics

sealed trait TestStatus
sealed trait TestResult
case class FailsAt(status: TestStatus, count: Int, map: Map[String, String]) extends TestResult
case object Pass extends TestStatus with TestResult
case object Fail extends TestStatus
case class Throws(exception: Throwable) extends TestStatus
case class ThrowsInCheck(exception: Exception) extends TestStatus

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

object Showable {
  implicit def show[T: Show](value: T): Showable = Showable(implicitly[Show[T]].show(value))
}

case class Showable(string: String) extends AnyVal

class Runner() extends Dynamic {
  def applyDynamicNamed[T]
                       (method: String)
                       (name: String, args: (String, Showable)*)
                       (fn: => T)
                       : Test { type Type = T } =
    new Test(name, args.toMap.mapValues(_.string)) {
      type Type = T
      def action(): T = fn
    }

  abstract class Test(val name: String, values: Map[String, String]) {
    type Type
    def action(): Type
    def assert(check: Type => Boolean): Type = {
      val t0 = System.currentTimeMillis()
      val result = Try(action())
      val time = System.currentTimeMillis() - t0

      result match {
        case Success(value) =>
          val outcome = try { if(check(value)) Pass else Fail } catch { case e: Exception => ThrowsInCheck(e) }
          record(this, Point(time, outcome))
          value
        case Failure(exception) =>
          record(this, Point(time, Throws(exception)))
          throw exception
      }
    }
  }

  def report(): Report = synchronized {
    Report { results.to[List].map { case (name, points) =>
      var result: TestResult = Pass
      var total: Long = 0L
      var min: Long = Long.MaxValue
      var max: Long = Long.MinValue
      var count: Int = 0

      while(count < points.length) {
        val point = points(count)
        count += 1
        total += point.duration
        if(point.duration > max) max = point.duration
        if(point.duration < min) min = point.duration
        result = result match {
          case Pass => point.status match {
            case Pass => Pass
            case fail => FailsAt(fail, count, Map())
          }
          case failed => failed
        }
      }

      TestSummary(name, count, min/1000.0, (total/1000.0)/count, max/1000.0, result)
    } }
  }

  protected def record(test: Test, point: Point): Unit =
    synchronized(results = results.updated(test.name, point :: results(test.name)))
  
  @volatile
  protected var results: Map[String, List[Point]] =
    ListMap[String, List[Point]]().withDefault { _ => List() }
}

case class Point(duration: Long, status: TestStatus)

case class TestSummary(name: String, count: Int, min: Double, avg: Double, max: Double, result: TestResult)
case class Report(results: List[TestSummary])