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

import scala.collection.immutable.ListMap
import scala.collection.mutable.HashMap
import scala.util.*, control.NonFatal
import scala.quoted.*

import wisteria.*
import escritoire.*
import rudiments.*
import gossamer.*

import language.dynamics

import Runner.*

trait LowPriorityDebugString:
  given [T: Show]: DebugString[T] = summon[Show[T]].show(_).string


object DebugString extends Derivation[DebugString], LowPriorityDebugString:
  given DebugString[String] = "\""+_.flatMap {
    case '\n' => "\\n"
    case '\t' => "\\t"
    case '\r' => "\\r"
    case '\\' => "\\\\"
    case '\"' => "\\\""
    case '\'' => "\\\'"
    case '\b' => "\\b"
    case '\f' => "\\f"
    case ch   => if ch < 128 && ch >= 32 then ch.toString else String.format("\\u%04x", ch.toInt)
  }+"\""

  val debugAny: DebugString[Any] = _.toString

  given DebugString[Char] =
    ch => "'"+summon[DebugString[String]].show(ch.toString).drop(1).dropRight(1)+"'"

  given [Coll[X] <: Seq[X], T: DebugString]: DebugString[Coll[T]] =
    xs => xs.map(summon[DebugString[T]].show(_)).join("Seq(", ", ", ")")

  def join[T](ctx: CaseClass[DebugString, T]): DebugString[T] = t =>
    ctx.params.map {
      param => param.typeclass.show(param.deref(t))
    }.join(str"${ctx.typeInfo.short}(", ", ", ")")
  
  def split[T](ctx: SealedTrait[DebugString, T]): DebugString[T] = t =>
    ctx.choose(t) { subtype => subtype.typeclass.show(subtype.cast(t)) }


trait DebugString[T]:
  def show(value: T): String

object Runner:
  case class TestId private[probably](value: String)
  
  enum Outcome:
    case FailsAt(datapoint: Datapoint, count: Int)
    case Passed
    case Mixed

    def failed: Boolean = !passed
    def passed: Boolean = this == Passed
    
    def filename: String = this match
      case FailsAt(datapoint, count) => datapoint.debugValue.filename.otherwise("(no source)")
      case _                         => ""
    
    def line: String = this match
      case FailsAt(datapoint, count) => datapoint.debugValue.line.otherwise(0).toString
      case _                         => ""

    def debug: String = this match
      case FailsAt(datapoint, count) =>
        lazy val padWidth = datapoint.debugValue.allInfo.map(_._1.length).max
        datapoint.debugValue.allInfo.map { case (k, v) =>
          val value = v.cut("\n").join("\n"+" "*(padWidth + 3))
          str"${k.padLeft(padWidth, ' ')} = $value"
        }.join("\n")
      
      case _ =>
        ""

  enum Datapoint(val debugValue: Debug):
    case Pass extends Datapoint(Debug())
    case Fail(debug: Debug, index: Int) extends Datapoint(debug)
    case Throws(exception: Throwable, debug: Debug) extends Datapoint(debug)
    
    case PredicateThrows(exception: Exception, debug: Debug, index: Int) extends Datapoint(debug)

  def shortDigest(text: String): String =
    val md = java.security.MessageDigest.getInstance("SHA-256").nn
    md.update(text.getBytes)
    md.digest.nn.take(3).map(b => f"$b%02x").mkString

class Runner(subset: Set[TestId] = Set()) extends Dynamic:

  val runner: Runner = this

  final def skip(testId: TestId): Boolean = !(subset.isEmpty || subset(testId))

  inline def apply[T](name: String)(inline fn: Test ?=> T): Test { type Type = T } =
    new Test(name):
      type Type = T
      def action(): T = fn(using this)

  def time[T](name: String)(fn: => T): T = apply[T](name)(fn).check { _ => true }

  def suite(testSuite: TestSuite): Unit = suite(testSuite.name)(testSuite.run)

  def suite(name: String)(fn: Runner ?=> Unit): Unit =
    val test = new Test(name):
      type Type = Report

      def action(): Report =
        val runner = Runner()
        fn(using runner)
        runner.report()
    
    val report = test.check(_.results.forall(_.outcome.passed))

    if report.results.exists(_.outcome.passed) && report.results.exists(_.outcome.failed)
    then synchronized {
      results = results.updated(name, results(name).copy(outcome = Outcome.Mixed))
    }

    report.results.foreach { result => record(result.copy(indent = result.indent + 1)) }

  def assert(name: String)(fn: => Boolean): Unit = apply(name)(fn).oldAssert(identity)

  abstract class Test(val name: String):
    type Type

    def id: TestId = TestId(Runner.shortDigest(name))
    def action(): Type
    
    def oldAssert(pred: Type => Boolean): Unit =
      try if !skip(id) then check(pred) catch case NonFatal(_) => ()
    
    private val map: HashMap[String, String] = HashMap()

    def debug[T](name: String, expr: T): T =
      map(name) = expr.toString
      expr

    inline def assert(inline pred: Type => Boolean): Unit =
      ${Macros.assert[Type]('runner, 'this, 'pred)}

    inline def check(pred: Type => Boolean, debug: Option[Type] => Debug = v => Debug(v.map(_.toString))): Type =
      def handler(index: Int): PartialFunction[Throwable, Datapoint] =
        case e: Exception => Datapoint.PredicateThrows(e, Debug(), index)

      def makeDatapoint(pred: Type => Boolean, count: Int, datapoint: Datapoint, value: Type)
          : Datapoint =
        try if pred(value) then Datapoint.Pass else Datapoint.Fail(debug(Some(value)), count)
        catch handler(count)

      val t0 = System.currentTimeMillis()
      val value = Try(action())
      val time = System.currentTimeMillis() - t0

      value match
        case Success(value) =>
          record(this, time, makeDatapoint(pred, 0, Datapoint.Pass, value))
          value
        case Failure(e) =>
          val info = Option(e.getStackTrace).to(List)
            .flatMap(_.nn.to(List).map(_.nn))
            .takeWhile(_.getClassName != "probably.Suite")
            .map(_.nn.toString)
            .join(Option(e.getMessage).fold("null")(_.nn+"\n  at "), "\n  at ", "")
          
          record(this, time, Datapoint.Throws(e, debug(None).add("exception", info)))
          throw e

  def report(): Report = Report(results.values.to(List))
  def clear(): Unit = results = emptyResults()

  protected def record(test: Test, duration: Long, datapoint: Datapoint): Unit = synchronized {
    results = results.updated(test.name, results(test.name).append(test.name, duration, datapoint))
  }

  protected def record(summary: Summary) = synchronized {
    results = results.updated(summary.name, summary)
  }

  private def emptyResults(): Map[String, Summary] = ListMap().withDefault { name =>
    Summary(TestId(shortDigest(name)), name, 0, Int.MaxValue, 0L, Int.MinValue, Outcome.Passed, 0)
  }

  @volatile
  protected var results: Map[String, Summary] = emptyResults()
end Runner

case class Debug(found: Option[String] = None, filename: Maybe[String] = Unset, line: Maybe[Int] = Unset,
                     expected: Maybe[String] = Unset, info: Map[String, String] = Map()):
  def add(key: String, value: String): Debug = copy(info = info.updated(key, value))
  
  def allInfo: ListMap[String, String] =
    val basicInfo =
      List("found" -> found.getOrElse(Unset), "expected" -> expected)
        .filter(_._2 != Unset)
        .to(ListMap)
        .view
        .mapValues(_.toString)
        .to(ListMap)
    
    basicInfo ++ info

object Macros:
  import scala.reflect.*
  def assert[T: Type](runner: Expr[Runner], test: Expr[Runner#Test { type Type = T }], pred: Expr[T => Boolean])(using Quotes): Expr[Unit] =
    import quotes.reflect.*

    def interpret(pred: Expr[T => Boolean]): Option[Expr[Option[T] => Debug]] = pred match
      case '{ (x: T) => x == ($expr: T) } =>
        val filename = Expr {
          val absolute = Position.ofMacroExpansion.toString.cut(":").head
          val pwd = Sys.user.dir()
          if absolute.startsWith(pwd) then absolute.drop(pwd.length + 1) else absolute
        }
        val line = Expr(Position.ofMacroExpansion.startLine + 1)

        val debugString = Expr.summon[DebugString[T]].getOrElse('{DebugString.debugAny})

        Expr.summon[Comparison[T]].map { comparison =>
          '{ (x: Option[T]) =>
            Debug(
              found = x.map($debugString.show(_)),
              filename = $filename,
              line = $line,
              expected = $debugString.show($expr),
              info = if x.isEmpty then Map()
                  else Map("structure" -> $comparison.compare(x.get, $expr).toString)
            )
          }
        }.orElse {
          Some { 
            '{ (x: Option[T]) =>
              Debug(found = x.map(_.toString), filename = $filename, line = $line, expected = $expr.toString)
            }
          }
        }

      case '{ (x: T) => ($expr: T) == x } =>
        interpret('{ (x: T) => x == ($expr: T) })
      
      case _ =>
        None
    
    val debug = interpret(pred).getOrElse('{ (x: Option[T]) => Debug(x.map(_.toString)) })
    
    '{
      try if !$runner.skip($test.id) then $test.check($pred, $debug) catch case NonFatal(_) => ()
    }

enum Differences:
  case Same
  case Structural(differences: Map[String, Differences])
  case Diff(left: String, right: String)

  def flatten: Seq[(List[String], String, String)] = this match
    case Same              => Nil
    case Structural(diffs) => diffs.to(List).flatMap { case (label, nested) =>
                                println(nested)
                                nested.flatten.map { case (path, left, right) =>
                                  (label :: path, left, right)
                                }
                              }
    case Diff(left, right) => List((Nil, left, right))

  override def toString(): String =
    val table = Tabulation[(List[String], String, String)](
      Heading("Key", _(0).join(".")),
      Heading("Expected", _(1)),
      Heading("Found", _(2))
    )

    table.tabulate(100, flatten).join("\n")

trait Comparison[T]:
  def compare(left: T, right: T): Differences

object Comparison extends Derivation[Comparison]:
  given Comparison[String] =
    (a, b) => if a == b then Differences.Same else Differences.Diff(a.toString, b.toString)
  
  given Comparison[Int] =
    (a, b) => if a == b then Differences.Same else Differences.Diff(a.toString, b.toString)
  
  given Comparison[Boolean] =
    (a, b) => if a == b then Differences.Same else Differences.Diff(a.toString, b.toString)
  
  given Comparison[Long] =
    (a, b) => if a == b then Differences.Same else Differences.Diff(a.toString, b.toString)
  
  given Comparison[Char] =
    (a, b) => if a == b then Differences.Same else Differences.Diff(a.toString, b.toString)
  
  given Comparison[Short] =
    (a, b) => if a == b then Differences.Same else Differences.Diff(a.toString, b.toString)
  
  given Comparison[Double] =
    (a, b) => if a == b then Differences.Same else Differences.Diff(a.toString, b.toString)
  
  given Comparison[Float] =
    (a, b) => if a == b then Differences.Same else Differences.Diff(a.toString, b.toString)
  

  def join[T](caseClass: CaseClass[Comparison, T]): Comparison[T] = (left, right) =>
    if left == right then Differences.Same
    else Differences.Structural {
      caseClass.params
        .filter { p => p.deref(left) != p.deref(right) }
        .map { p => p.label -> p.typeclass.compare(p.deref(left), p.deref(right)) }
        .to(Map)
    }
  
  def split[T](sealedTrait: SealedTrait[Comparison, T]): Comparison[T] = (left, right) =>
    val leftType = sealedTrait.choose(left)(identity(_))
    sealedTrait.choose(right) { subtype =>
      if leftType == subtype
      then subtype.typeclass.compare(subtype.cast(left), subtype.cast(right))
      else Differences.Diff(str"type: ${leftType.typeInfo.short}", str"type: ${subtype.typeInfo.short}")
    }

case class Summary(id: TestId, name: String, count: Int, tmin: Long, ttot: Long, tmax: Long,
                       outcome: Outcome, indent: Int):
  def avg: Double = ttot.toDouble/count/1000.0
  def min: Double = tmin.toDouble/1000.0
  def max: Double = tmax.toDouble/1000.0

  def aggregate(datapoint: Datapoint) = outcome match
    case Outcome.FailsAt(dp, c) => Outcome.FailsAt(dp, c)
    case Outcome.Passed         => datapoint match
      case Datapoint.Pass         => Outcome.Passed
      case other                  => Outcome.FailsAt(other, count + 1)
    case Outcome.Mixed          => Outcome.FailsAt(datapoint, 0)

  def append(test: String, duration: Long, datapoint: Datapoint): Summary =
    Summary(id, name, count + 1, tmin min duration, ttot + duration, tmax max duration,
        aggregate(datapoint), 0)

case class Report(results: List[Summary]):
  val passed: Int = results.count(_.outcome == Outcome.Passed)
  val failed: Int = results.count(_.outcome != Outcome.Passed)
  val total: Int = failed + passed

trait TestSuite:
  def run(using Runner): Unit
  def name: String

object global:
  object test extends Runner()

inline def test[T](name: String)(inline fn: Runner#Test ?=> T)(using runner: Runner)
    : runner.Test { type Type = T } =
  runner(name)(fn)

def suite(name: String)(fn: Runner ?=> Unit)(using runner: Runner): Unit = runner.suite(name)(fn)
def suite(suite: TestSuite)(using runner: Runner): Unit = runner.suite(suite)
def time[T](name: String)(fn: => T)(using runner: Runner): T = test(name)(fn).check { _ => true }