package probably

import magnolia._

import scala.util._
import scala.collection.immutable.ListMap
import scala.util.control.NonFatal

import language.dynamics
import language.experimental.macros

sealed abstract class TestResult(val passed: Boolean) {
  def failed: Boolean = !passed
  def debug: String
}

sealed trait TestStatus { def map: Map[String, String] }

case class FailsAt(status: TestStatus, count: Int) extends TestResult(false) {
  def debug: String = status.map.map { case (k, v) => s"$k=$v" }.mkString(" ")
}

case object Pass extends TestResult(true) with TestStatus {
  def map: Map[String, String] = Map()
  def debug: String = ""
}

case class Fail(map: Map[String, String]) extends TestStatus

case class Throws(exception: Throwable, map: Map[String, String]) extends TestStatus

case class ThrowsInCheck(exception: Exception, map: Map[String, String]) extends TestStatus

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
  implicit def show[T: Show](value: T): Showable[T] = Showable[T](value, implicitly[Show[T]])
}

case class Showable[T](value: T, show: Show[T]) { def apply(): String = show.show(value) }

class Runner() extends Dynamic {

  def only(test: Test): Boolean = true

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

  abstract class Test(val name: String, map: => Map[String, String]) {
    type Type
    
    def action(): Type
    
    def assert(predicate: Type => Boolean): Unit =
      try if(only(this)) check(predicate) catch { case NonFatal(e) => () }
    
    def check(predicate: Type => Boolean): Type = {
      val t0 = System.currentTimeMillis()
      val result = Try(action())
      val time = System.currentTimeMillis() - t0

      result match {
        case Success(value) =>
          val outcome = try {
            if(predicate(value)) Pass else Fail(map)
          } catch { case e: Exception => ThrowsInCheck(e, map) }
          record(this, Point(time, outcome))
          value
        case Failure(exception) =>
          record(this, Point(time, Throws(exception, map)))
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

      points.reverse.foreach { point =>
        count += 1
        total += point.duration
        if(point.duration > max) max = point.duration
        if(point.duration < min) min = point.duration
        result = result match {
          case Pass => point.status match {
            case Pass => Pass
            case fail => FailsAt(fail, count)
          }
          case failed => failed
        }
      }

      TestSummary(name, count, min/1000.0, (total/1000.0)/count, max/1000.0, result)
    } }
  }

  protected def record(test: Test, point: Point): Unit = {
    synchronized(results = results.updated(test.name, point :: results(test.name)))
  }
  
  @volatile
  protected var results: Map[String, List[Point]] =
    ListMap[String, List[Point]]().withDefault { _ => List() }
}

case class Seed(value: Long) {
  def apply(): Long = value
  def stream: Stream[Seed] = {
    val rnd = new java.util.Random(value)
    Stream.continually(Seed(rnd.nextLong))
  }
}

object Arbitrary {
  type Typeclass[T] = Arbitrary[T]

  def combine[T](ctx: CaseClass[Arbitrary, T]): Arbitrary[T] = (seed, n) => ctx.rawConstruct {
    ctx.parameters.zip(spread(seed, n, ctx.parameters.size)).zip(seed.stream.take(ctx.parameters.size)).map {
      case ((param, i), s) => param.typeclass(s, i)
    } }


  val interestingInts = Vector(0, 1, -1, 2, -2, 42, Int.MaxValue, Int.MinValue, Int.MaxValue - 1,
      Int.MinValue + 1)

  implicit val int: Arbitrary[Int] = (seed, n) => interestingInts.lift(n).getOrElse(seed.stream(n).value.toInt)


  val interestingStrings = Vector("", "a", "z", "\n", "0", "_", "\"", "\'", " ", "abcdefghijklmnopqrstuvwxyz")
  implicit def string: Arbitrary[String] = (seed, n) => interestingStrings.lift(n).getOrElse {
    val chars = seed.stream(n).stream.map(_()).map(_.toByte).filter(_ > 31).filter(_ < 128).take(8)
    new String(chars.to[Array], "UTF-8")
  }

  implicit def gen[T]: Arbitrary[T] = macro Magnolia.gen[T]

  private def spread(seed: Seed, total: Int, count: Int): List[Int] = {
    val sample = seed.stream.map(_.value.toDouble).map(math.abs(_)).take(count).to[List]
    sample.tails.foldLeft(List[Int]()) { case (acc, tail) => tail.headOption.fold(acc) { v =>
      (((v/(tail.sum))*(total - acc.sum)) + 0.5).toInt :: acc
    } }
  }
}

trait Arbitrary[T] { def apply(seed: Seed, n: Int): T }

object Generate {
  def stream[T](implicit arbitrary: Arbitrary[T], seed: Seed = Seed(0L)): Stream[T] =
    Stream.from(0).map(arbitrary(seed, _))
}

case class Point(duration: Long, status: TestStatus)
case class TestSummary(name: String, count: Int, min: Double, avg: Double, max: Double, result: TestResult)
case class Report(results: List[TestSummary])