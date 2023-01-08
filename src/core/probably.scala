/*
    Probably, version 0.4.0. Copyright 2017-23 Jon Pretty, Propensive OÜ.

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

import wisteria.*
import escritoire.*, tableStyles.default
import rudiments.*
import gossamer.*
import eucalyptus.*
import iridescence.*
import escapade.*

import scala.collection.mutable.{HashMap, HashSet}
import scala.util.*
import scala.quoted.*

import language.dynamics

given realm: Realm = Realm(t"probably")

import Runner.*

object Runner:
  case class TestId(value: Text)
  
  enum Outcome:
    case FailsAt(datapoint: Datapoint, count: Int)
    case Passed
    case Mixed

    def failed: Boolean = !passed
    def passed: Boolean = this == Passed
    
    def filename: Text = this match
      case FailsAt(datapoint, count) => datapoint.inspectValue.filename.or(t"(no source)")
      case _                         => t""
    
    def line: Text = this match
      case FailsAt(datapoint, count) => datapoint.inspectValue.line.or(0).show
      case _                         => t""

    def inspect: Text = this match
      case FailsAt(datapoint, count) =>
        val padWidth = datapoint.inspectValue.allInfo.map(_(0).length).max
        datapoint.inspectValue.allInfo.map { case (k, v) =>
          val value: Text = (v.cut(t"\n"): List[Text]).join(t"\n${t" "*(padWidth + 3)}")
          t"${k.pad(padWidth, Ltr)} = $value"
        }.join(t"\n")
      
      case _ =>
        t""

  enum Datapoint(val inspectValue: Inspect):
    case Pass extends Datapoint(Inspect())
    case Fail(inspect: Inspect, index: Int) extends Datapoint(inspect)
    case Throws(exception: Throwable, inspect: Inspect) extends Datapoint(inspect)
    
    case PredicateThrows(exception: Exception, inspect: Inspect, index: Int) extends Datapoint(inspect)

  def shortDigest[T: Show](text: T): Text =
    val md = java.security.MessageDigest.getInstance("SHA-256").nn
    md.update(text.show.s.getBytes)
    md.digest.nn.take(3).immutable(using Unsafe).map { b => Text(f"$b%02x") }.join

class Runner(subset: Set[TestId] = Set()) extends Dynamic:

  val runner: Runner = this

  final def skip(testId: TestId): Boolean = !(subset.isEmpty || subset(testId))

  def apply[T](name: Text)(fn: Test ?=> T): Test { type Type = T } =
    new Test(name):
      type Type = T
      def action(): T = fn(using this)

  def time[T](name: Text)(fn: => T)(using Log): T = apply[T](name)(fn).check(true.waive)
  def suite(testSuite: TestSuite)(using Log): Unit = suite(testSuite.name)(testSuite.run)

  def suite(name: Text)(fn: Runner ?=> Unit)(using Log): Unit =
    Log.info(ansi"Starting test suite ${colors.Gold}($name)")
    val test = new Test(name):
      type Type = Report

      def action(): Report =
        val runner = Runner()
        fn(using runner)
        runner.report()

    val t0 = System.currentTimeMillis
    val report = test.check(_.results.forall(_.outcome.passed))
    val t1 = System.currentTimeMillis - t0

    if report.results.exists(_.outcome.passed) && report.results.exists(_.outcome.failed)
    then synchronized:
      results = results.updated(name, results(name).copy(outcome = Outcome.Mixed))

    report.results.foreach: result =>
      record(result.copy(indent = result.indent + 1))
   
    Log.fine(ansi"Completed test suite ${colors.Gold}($name) in ${t1.show}ms")

  def assert(name: Text)(fn: => Boolean)(using Log): Unit = apply(name)(fn).oldAssert(identity)

  object Test:
    given AnsiShow[Test] = test => ansi"${colors.Khaki}(${test.id.value})"

  abstract class Test(val name: Text):
    type Type

    def id: TestId = TestId(Runner.shortDigest(name))
    def action(): Type
    
    def oldAssert(pred: Type => Boolean)(using Log): Unit =
      try if !skip(id) then check(pred)
      catch
        case NonFatal(e) =>
          Log.warn(ansi"A $e exception was thrown whilst checking the test ${this.ansi}")
        case fatal: Throwable =>
          try Log.fail(ansi"A fatal exception, $fatal, was thrown in test $name")
          catch case _: Throwable => ()
          throw fatal
    
    private val map: HashMap[Text, Text] = HashMap()

    def inspect[T](name: Text, expr: T): T =
      map(name) = Showable(expr).show
      expr

    inline def assert(inline pred: Type => Boolean)(using log: Log): Unit =
      ${ProbablyMacros.assert[Type]('runner, 'this, 'pred, 'log)}

    transparent inline def matches(pf: PartialFunction[Type, Any])(using Log): Unit = assert(pf.isDefinedAt(_))

    def check(pred: Type => Boolean, inspect: Option[Type] => Inspect = v => Inspect(v.map(Showable(_).show)))
             (using Log): Type =
      def handler(index: Int): PartialFunction[Throwable, Datapoint] =
        case e: Exception =>
          Log.warn(ansi"A $e exception was thrown in the test ${this.ansi}")
          Datapoint.PredicateThrows(e, Inspect(), index)

      def makeDatapoint(pred: Type => Boolean, count: Int, datapoint: Datapoint, value: Type)
          : Datapoint =
        try if pred(value) then Datapoint.Pass else Datapoint.Fail(inspect(Some(value)), count)
        catch handler(count)

      val t0 = System.currentTimeMillis()
      val value = Try(action())
      val time = System.currentTimeMillis() - t0

      value match
        case Success(value) =>
          record(this, time, makeDatapoint(pred, 0, Datapoint.Pass, value))
          value

        case Failure(e) =>
          val trace = Option(e.getStackTrace).fold(Nil)(_.nn.immutable(using Unsafe).to(List).map(_.nn))
          
          val info = trace.takeWhile(_.getClassName != "probably.Suite")
            .map { frame => Showable(frame.nn).show }
            .join(Option(e.getMessage).fold(t"null") { x => t"${e.getClass.getName.nn}: ${x.nn}\n  at " }, t"\n  at ", t"")
          
          record(this, time, Datapoint.Throws(e, inspect(None).add(t"exception", info)))
          
          throw e

  def report(): Report = Report(results.values.to(List))
  def clear(): Unit = results = emptyResults()

  protected def record(test: Test, duration: Long, datapoint: Datapoint): Unit = synchronized:
    results = results.updated(test.name, results(test.name).append(test.name, duration, datapoint))

  protected def record(summary: Summary) = synchronized:
    results = results.updated(summary.name, summary)

  private def emptyResults(): Map[Text, Summary] = ListMap().withDefault: name =>
    Summary(TestId(shortDigest(name)), name, 0, Int.MaxValue, 0L, Int.MinValue, Outcome.Passed, 0)

  protected var results: Map[Text, Summary] = emptyResults()
end Runner

case class Inspect(found: Option[Text] = None, filename: Maybe[Text] = Unset, line: Maybe[Int] = Unset,
                     expected: Maybe[Text] = Unset, info: Map[Text, Text] = Map()):
  def add(key: Text, value: Text): Inspect = copy(info = info.updated(key, value))
  
  def allInfo: ListMap[Text, Text] =
    val basicInfo =
      List(t"found" -> found.getOrElse(Unset), t"expected" -> expected)
        .filter(_._2 != Unset)
        .to(ListMap)
        .view
        .mapValues(Showable(_).show)
        .to(ListMap)
    
    basicInfo ++ info

object ProbablyMacros:
  import scala.reflect.*
  def assert[T: Type](runner: Expr[Runner], test: Expr[Runner#Test { type Type = T }], pred: Expr[T => Boolean], log: Expr[Log])(using Quotes): Expr[Unit] =
    import quotes.reflect.*
    
    val filename: Expr[String] = Expr:
      val absolute = Showable(Position.ofMacroExpansion).show.cut(t":").head
      val pwd = try Sys.user.dir().show catch case e: KeyNotFoundError => throw Mistake("should not happen")
      
      if absolute.starts(pwd) then absolute.drop(pwd.length + 1).s else absolute.s
    
    val line = Expr(Position.ofMacroExpansion.startLine + 1)

    def inspectExpr[S: Type](expr: Expr[S]): Expr[Option[S] => Inspect] =
      '{ (result: Option[S]) =>
        Inspect(found = result.map { r => Text(r.toString()) }, filename = Text($filename), line = $line, expected = Text($expr.toString()))
      }

    def interpret(pred: Expr[T => Boolean]): Expr[Option[T] => Inspect] = pred match
      case '{ (x: T) => x == ($expr: T) }             => inspectExpr(expr)
      case '{ (x: T) => ($expr: T) == x }             => inspectExpr(expr)
      case '{ (x: Double) => x == ($expr: Double) }   => inspectExpr(expr)
      case '{ (x: Double) => x == ($expr: Float) }    => inspectExpr(expr)
      case '{ (x: Double) => x == ($expr: Long) }     => inspectExpr(expr)
      case '{ (x: Double) => x == ($expr: Int) }      => inspectExpr(expr)
      case '{ (x: Double) => x == ($expr: Short) }    => inspectExpr(expr)
      case '{ (x: Double) => x == ($expr: Byte) }     => inspectExpr(expr)
      case '{ (x: Float) => x == ($expr: Double) }    => inspectExpr(expr)
      case '{ (x: Float) => x == ($expr: Float) }     => inspectExpr(expr)
      case '{ (x: Float) => x == ($expr: Long) }      => inspectExpr(expr)
      case '{ (x: Float) => x == ($expr: Int) }       => inspectExpr(expr)
      case '{ (x: Float) => x == ($expr: Short) }     => inspectExpr(expr)
      case '{ (x: Float) => x == ($expr: Byte) }      => inspectExpr(expr)
      case '{ (x: Long) => x == ($expr: Double) }     => inspectExpr(expr)
      case '{ (x: Long) => x == ($expr: Float) }      => inspectExpr(expr)
      case '{ (x: Long) => x == ($expr: Long) }       => inspectExpr(expr)
      case '{ (x: Long) => x == ($expr: Int) }        => inspectExpr(expr)
      case '{ (x: Long) => x == ($expr: Short) }      => inspectExpr(expr)
      case '{ (x: Long) => x == ($expr: Byte) }       => inspectExpr(expr)
      case '{ (x: Int) => x == ($expr: Double) }      => inspectExpr(expr)
      case '{ (x: Int) => x == ($expr: Float) }       => inspectExpr(expr)
      case '{ (x: Int) => x == ($expr: Long) }        => inspectExpr(expr)
      case '{ (x: Int) => x == ($expr: Int) }         => inspectExpr(expr)
      case '{ (x: Int) => x == ($expr: Short) }       => inspectExpr(expr)
      case '{ (x: Int) => x == ($expr: Byte) }        => inspectExpr(expr)
      case '{ (x: Short) => x == ($expr: Double) }    => inspectExpr(expr)
      case '{ (x: Short) => x == ($expr: Float) }     => inspectExpr(expr)
      case '{ (x: Short) => x == ($expr: Long) }      => inspectExpr(expr)
      case '{ (x: Short) => x == ($expr: Int) }       => inspectExpr(expr)
      case '{ (x: Short) => x == ($expr: Short) }     => inspectExpr(expr)
      case '{ (x: Short) => x == ($expr: Byte) }      => inspectExpr(expr)
      case '{ (x: Byte) => x == ($expr: Double) }     => inspectExpr(expr)
      case '{ (x: Byte) => x == ($expr: Float) }      => inspectExpr(expr)
      case '{ (x: Byte) => x == ($expr: Long) }       => inspectExpr(expr)
      case '{ (x: Byte) => x == ($expr: Int) }        => inspectExpr(expr)
      case '{ (x: Byte) => x == ($expr: Short) }      => inspectExpr(expr)
      case '{ (x: Byte) => x == ($expr: Byte) }       => inspectExpr(expr)
      case '{ (x: Boolean) => x == ($expr: Boolean) } => inspectExpr(expr)
      case '{ (x: Double) => ($expr: Double) == x }   => inspectExpr(expr)
      case '{ (x: Double) => ($expr: Float) == x }    => inspectExpr(expr)
      case '{ (x: Double) => ($expr: Long) == x }     => inspectExpr(expr)
      case '{ (x: Double) => ($expr: Int) == x }      => inspectExpr(expr)
      case '{ (x: Double) => ($expr: Short) == x }    => inspectExpr(expr)
      case '{ (x: Double) => ($expr: Byte) == x }     => inspectExpr(expr)
      case '{ (x: Float) => ($expr: Double) == x }    => inspectExpr(expr)
      case '{ (x: Float) => ($expr: Float) == x }     => inspectExpr(expr)
      case '{ (x: Float) => ($expr: Long) == x }      => inspectExpr(expr)
      case '{ (x: Float) => ($expr: Int) == x }       => inspectExpr(expr)
      case '{ (x: Float) => ($expr: Short) == x }     => inspectExpr(expr)
      case '{ (x: Float) => ($expr: Byte) == x }      => inspectExpr(expr)
      case '{ (x: Long) => ($expr: Double) == x }     => inspectExpr(expr)
      case '{ (x: Long) => ($expr: Float) == x }      => inspectExpr(expr)
      case '{ (x: Long) => ($expr: Long) == x }       => inspectExpr(expr)
      case '{ (x: Long) => ($expr: Int) == x }        => inspectExpr(expr)
      case '{ (x: Long) => ($expr: Short) == x }      => inspectExpr(expr)
      case '{ (x: Long) => ($expr: Byte) == x }       => inspectExpr(expr)
      case '{ (x: Int) => ($expr: Double) == x }      => inspectExpr(expr)
      case '{ (x: Int) => ($expr: Float) == x }       => inspectExpr(expr)
      case '{ (x: Int) => ($expr: Long) == x }        => inspectExpr(expr)
      case '{ (x: Int) => ($expr: Int) == x }         => inspectExpr(expr)
      case '{ (x: Int) => ($expr: Short) == x }       => inspectExpr(expr)
      case '{ (x: Int) => ($expr: Byte) == x }        => inspectExpr(expr)
      case '{ (x: Short) => ($expr: Double) == x }    => inspectExpr(expr)
      case '{ (x: Short) => ($expr: Float) == x }     => inspectExpr(expr)
      case '{ (x: Short) => ($expr: Long) == x }      => inspectExpr(expr)
      case '{ (x: Short) => ($expr: Int) == x }       => inspectExpr(expr)
      case '{ (x: Short) => ($expr: Short) == x }     => inspectExpr(expr)
      case '{ (x: Short) => ($expr: Byte) == x }      => inspectExpr(expr)
      case '{ (x: Byte) => ($expr: Double) == x }     => inspectExpr(expr)
      case '{ (x: Byte) => ($expr: Float) == x }      => inspectExpr(expr)
      case '{ (x: Byte) => ($expr: Long) == x }       => inspectExpr(expr)
      case '{ (x: Byte) => ($expr: Int) == x }        => inspectExpr(expr)
      case '{ (x: Byte) => ($expr: Short) == x }      => inspectExpr(expr)
      case '{ (x: Byte) => ($expr: Byte) == x }       => inspectExpr(expr)
      case '{ (x: Boolean) => ($expr: Boolean) == x } => inspectExpr(expr)
      
      case expr =>
        '{ (opt: Option[?]) => Inspect(found = opt.map(_.toString.asInstanceOf[Text]), filename = Text($filename),
            line = $line) }
    
    '{
      try if !$runner.skip($test.id) then $test.check($pred, ${interpret(pred)})(using $log)
      catch case NonFatal(_) => ()
    }

object Differences:
  given Show[Differences] = diff =>
    val table = Table[(List[Text], Text, Text)](
      Column(t"Key")(_(0).join(t".")),
      Column(t"Found")(_(1)),
      Column(t"Expected")(_(2)),
    )

    table.tabulate(diff.flatten, 100).join(ansi"\n").render

enum Differences:
  case Same
  case Structural(differences: Map[Text, Differences])
  case Diff(left: Text, right: Text)

  def flatten: Seq[(List[Text], Text, Text)] = this match
    case Same              => Nil
    case Structural(diffs) => diffs.to(List).flatMap { case (label, nested) =>
                                nested.flatten.map { case (path, left, right) =>
                                  (label :: path, left, right)
                                }
                              }
    case Diff(left, right) => List((Nil, left, right))

trait Comparison[-T]:
  def compare(left: T, right: T): Differences

object Comparison: // extends Derivation[Comparison]:
  given Comparison[Text] = (a, b) =>
    if a == b then Differences.Same else Differences.Diff(a, b)
  
  given Comparison[String] = (a, b) =>
    if a == b then Differences.Same else Differences.Diff(Text(a), Text(b))
  
  given [T: Show]: Comparison[T] = (a, b) =>
    if a == b then Differences.Same else Differences.Diff(a.show, b.show)
  
  def join[T](caseClass: CaseClass[Comparison, T]): Comparison[T] = (left, right) =>
    if left == right then Differences.Same
    else Differences.Structural:
      caseClass.params
        .filter { p => p.deref(left) != p.deref(right) }
        .map { p => Text(p.label) -> p.typeclass.compare(p.deref(left), p.deref(right)) }
        .to(Map)
  
  def split[T](sealedTrait: SealedTrait[Comparison, T]): Comparison[T] = (left, right) =>
    val leftType = sealedTrait.choose(left)(identity(_))
    sealedTrait.choose(right): subtype =>
      if leftType == subtype
      then subtype.typeclass.compare(subtype.cast(left), subtype.cast(right))
      else Differences.Diff(t"type: ${leftType.typeInfo.short}", t"type: ${subtype.typeInfo.short}")

case class Summary(id: TestId, name: Text, count: Int, tmin: Long, ttot: Long, tmax: Long, outcome: Outcome,
                       indent: Int):
  def avg: Double = ttot.toDouble/count/1000.0
  def min: Double = tmin.toDouble/1000.0
  def max: Double = tmax.toDouble/1000.0

  def aggregate(datapoint: Datapoint) = outcome match
    case Outcome.FailsAt(dp, c) => Outcome.FailsAt(dp, c)
    case Outcome.Passed         => datapoint match
      case Datapoint.Pass         => Outcome.Passed
      case other                  => Outcome.FailsAt(other, count + 1)
    case Outcome.Mixed          => Outcome.FailsAt(datapoint, 0)

  def append(test: Text, duration: Long, datapoint: Datapoint): Summary =
    Summary(id, name, count + 1, tmin min duration, ttot + duration, tmax max duration,
        aggregate(datapoint), 0)

case class Report(results: List[Summary]):
  val passed: Int = results.count(_.outcome == Outcome.Passed)
  val failed: Int = results.count(_.outcome != Outcome.Passed)
  val total: Int = failed + passed

object TestSuite:
  given AnsiShow[TestSuite] = ts => ansi"${colors.Gold}(${ts.name})"

trait TestSuite:
  def run(using Runner): Unit
  def name: Text

object global:
  object test extends Runner()

def test[T](name: Text)(fn: Runner#Test ?=> T)(using runner: Runner, log: Log)
    : runner.Test { type Type = T } =
  runner(name)(fn)

def suite(name: Text)(fn: Runner ?=> Unit)(using runner: Runner, log: Log): Unit =
  runner.suite(name)(fn)

def suite(suite: TestSuite)(using runner: Runner, log: Log): Unit = runner.suite(suite)
def time[T](name: Text)(fn: => T)(using Runner, Log): T = test(name)(fn).check { _ => true }

case class UnexpectedSuccessError[T](value: T)
extends Error(err"the expression was expected to throw an exception, but instead returned $value")

extension (inline value: Any)
  transparent inline def hasType[T]: Boolean = inline value match
    case _: T => true
    case _    => false

transparent inline def capture[T](inline fn: => CanThrow[Exception] ?=> T)
                              : Exception throws UnexpectedSuccessError[T] =
  try
    val result = fn
    throw UnexpectedSuccessError(result)
  catch
    case error: UnexpectedSuccessError[T] => throw error
    case error: Exception                 => error
