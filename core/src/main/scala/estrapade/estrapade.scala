package estrapade

import language.implicitConversions
import language.experimental.macros

import scala.collection.mutable.HashMap
import scala.util._

object `package` {
  def test[T](name: String)(action: => T): Test.Definition[T] = {
    val hashed = Test.hash(name)
    Test.synchronized {
      if(Test.tests.contains(hashed)) Test.tests(hashed).asInstanceOf[Test.Definition[T]]
      else {
        val newTest = new Test.Definition[T](name, () => action)
        Test.tests(hashed) = newTest
        newTest
      }
    }
  }

  def captureOut(fn: => Unit): String = {
    import java.io._
    val out = scala.Console.out
    val baos = new ByteArrayOutputStream()
    scala.Console.withOut(new PrintStream(baos))(fn)
    baos.toString
  }
}

case class DependencyFailureException(dep: String) extends Exception("a failure occurred in a dependency")

class Test[Result](val value: () => Try[Result], hash: String) {
  private lazy val lazyValue = value()
  def apply(): Result = lazyValue.getOrElse { throw DependencyFailureException(hash) }
}

object Test {
  
  private[estrapade] def hash(name: String) = {
    import javax.xml.bind.DatatypeConverter, java.security.MessageDigest
    val md5 = MessageDigest.getInstance("MD5")
    DatatypeConverter.printHexBinary(md5.digest(name.getBytes("UTF-8"))).toLowerCase.take(6)
  }

  def report()(implicit runner: Runner): runner.Return = runner.report()

  private[estrapade] val tests: HashMap[String, Definition[_]] = new HashMap()

  class Definition[Result](val name: String, val action: () => Result) {
    
    def returns()(implicit runner: Runner): Test[Result] = assert { v => true }
    
    def assert(assertion: Result => Boolean)(implicit runner: Runner): Test[Result] =
      macro Macros.assertion
  
    lazy val hash = Test.hash(name)
      
    def assert(
      assertion: Result => Boolean,
      msg: Result => String)(implicit runner: Runner
    ): Test[Result] =
      if(runner.doTest(hash)) {
        val t0 = System.nanoTime
        val result = Try(action())
        val duration = System.nanoTime - t0
        val outcome = result.map { r =>
          Try(if(assertion(r)) Runner.Passed else Runner.Failed(msg(r))) match {
            case Success(v) => v
            case Failure(e) => Runner.ThrewInCheck(e)
          }
        } match {
          case Success(v) => v
          case Failure(DependencyFailureException(dep)) =>
            Runner.FailedDependency(dep.take(6))
          case Failure(f) =>
            Runner.ThrewInRun(f)
        }
        runner.record(this, outcome, duration)
        val test = new Test(() => result, hash)
        test.lazyValue
        test
      } else new Test(() => Try(action()), hash)
  }
}

object Runner {

  def serialize(throwable: Throwable): String = {
    import java.io._
    import javax.xml.bind.DatatypeConverter
    import java.util.zip._
    val sb = new StringBuilder()
    sb.append(throwable.toString)
    sb.append("\n    at ")
    val stack = throwable.getStackTrace
    stack.init.foreach { frame =>
      sb.append(frame.toString)
      sb.append("\n    at ")
    }
    sb.append(stack.last)
    val baos = new ByteArrayOutputStream()
    val gzips = new GZIPOutputStream(baos)
    gzips.write(sb.toString.getBytes("UTF-8"))
    gzips.close()
    DatatypeConverter.printBase64Binary(baos.toByteArray)
  }

  sealed abstract class Outcome(val string: String, val diagnosis: List[String], val success: Option[Boolean])
  case object Passed extends Outcome("pass", Nil, Some(true))
  case class Failed(reason: String) extends Outcome("fail", List(reason), Some(false))
  case class ThrewInCheck(throwable: Throwable) extends Outcome("fail", List(s"exception thrown during assertion: ${serialize(throwable)}"), None)
  case class ThrewInRun(throwable: Throwable) extends Outcome("fail", List(s"exception thrown during run: ${serialize(throwable)}"), Some(false))
  case class FailedDependency(dep: String) extends Outcome("skip", List(s"dependency $dep failed"), None)
  case object Unstable extends Outcome("vari", Nil, Some(false))
  
  case class Result(definition: Test.Definition[_], outcome: Outcome, duration: Long)
}

/** the runner for tests definitions, which determines if, when and how the tests and assertions on
 *  them are executed
 *
 *  Implementing a custom test runner involves specifying  */
trait Runner {

  type Return
  
  def doTest(test: String): Boolean

  def record(definition: Test.Definition[_], outcome: Runner.Outcome, duration: Long): Unit
  
  def report(): Return
}

abstract class TestApp() {

  /** the code containing the tests to be run */
  def tests(): Unit
  
  /** the instance of the [[Run]] to be available to tests defined when implementing the [[tests]]
    * method
    *
    * Note that this value is initialized explicitly when the [[main]] method is executed, as the
    * instance requires the command-line arguments as parameters to its creation. Prior to running
    * the [[main]] method, this value will be `null`. */
  implicit protected var runner: Runner = _
  
  /** the entry-point of the application */
  def main(args: Array[String]): Unit = {
    import CliRunner.Config

    /* Parses the command-line arguments to specify the parameters for the creation of a [[Config]]
     * instance. */
    def parse(args: List[String], config: Config = Config()): Config = (args.flatMap { arg =>
      // Transform combined forms of arguments and values into separate arguments
      if(arg.startsWith("--")) arg.split("=", 2).to[List]
      else if(arg.startsWith("-") && arg.length > 2) List(arg.substring(0, 2), arg.substring(2))
      else List(arg)
    }) match {
      case Nil =>
        config
      case ("-h" | "--help") :: _ =>
        println("""Usage: estrapade [OPTIONS]
                  |Runs the application, and prints a test report to the console.
                  |
                  |Mandatory arguments to long options are mandatory for short options too.
                  |  -c, --columns=COLS     print output for a terminal width of COLS
                  |  -l, --lazy             only compute tests when their values are used
                  |  -n, --no-color         do not print ANSI color output
                  |  -o, --test-only=TESTS  only run tests with the specified comma-separated hashes
                  |  -T, --tags=TAGS        only run tests with the specified comma-separated tags
                  |""".stripMargin)
        sys.exit(0)
      case ("-c" | "--columns") :: cols :: rest =>
        parse(rest, config.copy(columns = cols.toInt))
      case ("-n" | "--no-color") :: rest =>
        parse(rest, config.copy(color = false))
      case ("-l" | "--lazy") :: rest =>
        parse(rest, config.copy(runLazily = true))
      case ("-o" | "--test-only") :: tests :: rest =>
        parse(rest, config.copy(testOnly = Some(tests.split(",").to[Set])))
      case other :: _ =>
        println(s"Unknown argument: $other")
        sys.exit(1)
    }

    runner = new CliRunner(parse(args.to[List]))
    tests()
    Test.report()
  }
}

