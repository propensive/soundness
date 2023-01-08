package probably2

import eucalyptus.*
import gossamer.*
import anticipation.*
import rudiments.*
import turbulence.*

import scala.collection.mutable as scm

given realm: Realm = Realm(t"probably")

extension [T](inline value: T)(using inline test: TestBody)
  inline def inspect(using Debug[T]): T = ${ProbablyMacros.inspect('value, 'test)}

class TestBody():
  private val captured: scm.HashMap[Text, Text] = scm.HashMap()
  def capture[T](name: Text, value: T)(using Debug[T]): T =
    captured(name) = value.debug
    value

case class TestId(name: Text, suite: Maybe[Suite]):
  lazy val id: Text = java.lang.Integer.toHexString(hashCode).nn.show.pad(6, Rtl, '0').take(6)
  def apply[T](body: TestBody ?=> T): Test[T] = Test[T](this, body(using _))

case class Suite(name: Text, parent: Maybe[Suite]):
  lazy val id: TestId = TestId(name, parent)

object BaseSuite extends Suite(t"", Unset)

object Test:
  case class Record(id: TestId)
  
  enum Run[+T]:
    case Returns(result: T, time: Long)
    case Throws(exception: Exception, time: Long)
    case Skipped
  
    def duration: Long = this match
      case Returns(_, time) => time
      case Throws(_, time)  => time
      case Skipped          => 0L

  enum Result:
    case Pass(duration: Long)
    case Fail(duration: Long)
    case Throws(exception: Exception, duration: Long)
    case AssertionThrows(exception: Exception, duration: Long)

class Runner[R]():
  private val funnel: Funnel[Test.Record] = Funnel()
  def skip(id: TestId): Boolean = false

  def run[T](test: Test[T])(fn: Test.Run[T] => Test.Result): Test.Run[T] =
    if skip(test.id) then Test.Run.Skipped else
      val body = TestBody()
      val ns0 = System.nanoTime
      
      val result: Test.Run[T] = 
        try
          val ns0: Long = System.nanoTime
          val result: T = test.action(body)
          val ns: Long = System.nanoTime - ns0
          Test.Run.Returns(result, ns)
        catch case err: Exception =>
          val ns: Long = System.currentTimeMillis - ns0
          Test.Run.Throws(err, ns)
      
      val record = try fn(result) catch case err: Exception => Test.Result.AssertionThrows(err, result.duration)

      funnel.put(Test.Record(test.id))
      
      result

  //lazy val report: R = unsafely(funnel.stream)

case class Test[Return](id: TestId, action: TestBody => Return)

def test(name: Text)(using suite: Suite): TestId = TestId(name, suite)

def suite[T](name: Text)(using suite: Suite)(fn: Suite ?=> T): T =
  val suite2 = Suite(name, suite)
  fn(using suite2)

extension [T](test: Test[T])
  inline def assert[R](inline pred: T => Boolean)(using runner: Runner[R]): Unit =
    ${ProbablyMacros.assert[T, R]('test, 'pred, 'runner)}
  
  inline def typed[R](using runner: Runner[R]): Unit = ${ProbablyMacros.typed[T, R]('test, 'runner)}
  
  // inline def benchmark[R](using t: GenericDuration)
  //                        (warmup: t.Duration, duration: t.Duration)
  //                        (using runner: Runner[R])
  //                     : Unit =
  //   ${ProbablyMacros.benchmark[T, R]('t, 'warmup, 'duration, 'runner, 'reportable)}
//   inline def check(inline pred: T => Boolean): T = action()

case class Assertion(pass: Boolean, exception: Maybe[Exception], time: Long)
case class Benchmark(runs: List[Long], exception: Maybe[Exception])

// enum ReportItem:
//   case SuiteItem(suite: Suite, items: List[ReportItem])
//   case TestItem(test: Test[?], result: Result)