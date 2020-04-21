package probably

import scala.util._
import scala.collection.immutable.ListMap

import language.dynamics

sealed trait Status
case object Pass extends Status
case object Fail extends Status
case class Throws(exception: Exception) extends Status
case class ThrowsInCheck(exception: Exception) extends Status
case class FailsAt(count: Int, map: Map[String, String])

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
          val outcome = try { if(check(value)) Pass else Fail } catch { case e: Exception => CheckThrew }
          record(this, Point(time, outcome))
          value
        case Failure(exception) =>
          record(this, Point(time, Threw))
          throw exception
      }
    }
  }

  def report(): Report = synchronized {
    Report { results.to[List].map { case (name, points) =>
      var status: Status = Incomplete
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
        if(status == Incomplete) status = point.status
        else if(status != point.status) status = Fickle
      }

      TestResult(name, count, min/1000.0, (total/1000.0)/count, max/1000.0, status)
    } }
  }

  protected def record(test: Test, point: Point): Unit =
    synchronized(results = results.updated(test.name, point :: results(test.name)))
  
  @volatile
  protected var results: Map[String, List[Point]] =
    ListMap[String, List[Point]]().withDefault { _ => List() }
}

object Point { def apply(duration: Long, status: Status.Value): Point = Point(duration << 2 | status.id) }

case class Point(data: Long) extends AnyVal {
  def duration: Long = data >> 2
  def status: Status.Value = Status(data.toInt & 3)
}

case class TestResult(name: String, count: Int, min: Double, avg: Double, max: Double, status: Status.Value)
case class Report(results: List[TestResult])