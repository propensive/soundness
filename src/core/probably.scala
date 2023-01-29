package probably

import eucalyptus.*
import gossamer.*
import anticipation.*
import rudiments.*
import deviation.*

import scala.collection.mutable as scm

given realm: Realm = Realm(t"probably")

extension [T](inline value: T)(using inline test: TestContext)
  inline def inspect(using Debug[T]): T = ${ProbablyMacros.inspect('value, 'test)}

package testContexts:
  given threadLocal: TestContext = new TestContext():
    private val delegate: Option[TestContext] = Option(Runner.testContextThreadLocal.get()).map(_.nn).flatten
    
    override def capture[T](name: Text, value: T)(using Debug[T]): T =
      delegate.map(_.capture[T](name, value)).getOrElse(value)

@annotation.capability
class TestContext():
  private[probably] val captured: scm.ArrayBuffer[(Text, Text)] = scm.ArrayBuffer()
  
  def capture[T](name: Text, value: T)(using Debug[T]): T =
    captured.append(name -> value.debug)
    value

object TestId:
  given Ordering[TestId] = math.Ordering.Implicits.seqOrdering[List, Text].on(_.ids.reverse)

case class TestId(name: Text, suite: Maybe[TestSuite]):
  lazy val id: Text = Integer.toHexString(suite.hashCode ^ name.hashCode).nn.show.pad(6, Rtl, '0').take(6, Rtl)
  lazy val ids: List[Text] =  id :: suite.mm(_.id.ids).or(Nil)
  def apply[T](ctx: TestContext ?=> T): Test[T] = Test[T](this, ctx(using _))
  def depth: Int = suite.mm(_.id.depth).or(0) + 1

class TestSuite(val name: Text, val parent: Maybe[TestSuite] = Unset):
  override def equals(that: Any): Boolean = that match
    case that: TestSuite => name == that.name && parent == that.parent
    case _               => false
  
  override def hashCode: Int = name.hashCode + parent.hashCode

  val id: TestId = TestId(name, parent)

enum Outcome:
  case Pass(duration: Long)
  case Fail(duration: Long)
  case Throws(exception: Exception, duration: Long)
  case CheckThrows(exception: Exception, duration: Long)

  def duration: Long

enum TestRun[+T]:
  case Returns(result: T, duration: Long, context: Map[Text, Text])
  case Throws(exception: () => T, duration: Long, context: Map[Text, Text])

  def get: T = this match
    case Returns(result, _, _)   => result
    case Throws(exception, _, _) => exception()

object Runner:
  private[probably] val testContextThreadLocal: ThreadLocal[Option[TestContext]] = ThreadLocal()

class Runner[ReportType]()(using reporter: TestReporter[ReportType]):
  def skip(id: TestId): Boolean = false
  val report: ReportType = reporter.make()

  def maybeRun[T, S](test: Test[T]): Maybe[TestRun[T]] = if skip(test.id) then Unset else run[T, S](test)

  def run[T, S](test: Test[T]): TestRun[T] =
    val ctx = TestContext()
    Runner.testContextThreadLocal.set(Some(ctx))
    val ns0 = System.nanoTime
    
    try
      val ns0: Long = System.nanoTime
      val result: T = test.action(ctx)
      val ns: Long = System.nanoTime - ns0
      TestRun.Returns(result, ns, ctx.captured.to(Map))
    
    catch case err: Exception =>
      val ns: Long = System.nanoTime - ns0
      
      val lazyException = () =>
        import unsafeExceptions.canThrowAny
        throw err

      TestRun.Throws(lazyException, ns, ctx.captured.to(Map))
    finally Runner.testContextThreadLocal.set(None)

  def suite(suite: TestSuite, fn: TestSuite ?=> Unit): Unit =
    if !skip(suite.id) then
      reporter.declareSuite(report, suite)
      fn(using suite)
  
  def complete(): Unit = reporter.complete(report)

case class Test[+Return](id: TestId, action: TestContext => Return)

def test[ReportType](name: Text)(using suite: TestSuite): TestId = TestId(name, suite)

def suite[R](name: Text)(using suite: TestSuite, runner: Runner[R])(fn: TestSuite ?=> Unit): Unit =
  runner.suite(TestSuite(name, suite), fn)

extension [T](test: Test[T])
  inline def assert[R](inline pred: T => Boolean)
                      (using runner: Runner[R], inc: Inclusion[R, Outcome], inc2: Inclusion[R, DebugInfo])
                      : Unit =
    ${ProbablyMacros.assert[T, R]('test, 'pred, 'runner, 'inc, 'inc2)}
  
  inline def check[R](inline pred: T => Boolean)
                     (using runner: Runner[R], inc: Inclusion[R, Outcome], inc2: Inclusion[R, DebugInfo]): T =
    ${ProbablyMacros.check[T, R]('test, 'pred, 'runner, 'inc, 'inc2)}

  inline def assert[R]()(using runner: Runner[R], inc: Inclusion[R, Outcome],
                           inc2: Inclusion[R, DebugInfo]): Unit =
    ${ProbablyMacros.assert[T, R]('test, '{ProbablyMacros.succeed}, 'runner, 'inc, 'inc2)}
  
  inline def check[R]()(using runner: Runner[R], inc: Inclusion[R, Outcome],
                           inc2: Inclusion[R, DebugInfo]): T =
    ${ProbablyMacros.check[T, R]('test, '{ProbablyMacros.succeed}, 'runner, 'inc, 'inc2)}
  
  inline def matches[R](inline pf: PartialFunction[T, Any])
                    (using runner: Runner[R], inc: Inclusion[R, Outcome], inc2: Inclusion[R, DebugInfo])
                    : Unit =
    assert[R](pf.isDefinedAt(_))
  
  inline def typed[R](using runner: Runner[R]): Unit = ${ProbablyMacros.typed[T, R]('test, 'runner)}
  
case class UnexpectedSuccessError(value: Any)
extends Error(err"the expression was expected to throw an exception, but instead returned $value")

transparent inline def capture[E <: Exception](inline fn: => CanThrow[E] ?=> Any)
                              : E throws UnexpectedSuccessError =
  try
    val result = fn(using unsafeExceptions.canThrowAny)
    throw UnexpectedSuccessError(result)
  catch
    case error: E                      => error
    case error: UnexpectedSuccessError => throw error
