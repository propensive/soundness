package estrapade

import language.implicitConversions
import language.experimental.macros

import scala.collection.mutable.HashMap
import scala.util._

/** a value representing a test */
class Test[Result](val value: () => Try[Result], hash: String) {
  private lazy val lazyValue = value()
  def apply(): Result = lazyValue.getOrElse { throw Test.DependencyFailureException(hash) }
}

/** companion object for [[Test]] */
object Test {
  /** exception which is thrown when attempting to get the value from a dependent test which has
   *  failed */
  case class DependencyFailureException(dep: String) extends
      Exception("a failure occurred in a dependency")
  
  private[estrapade] def hash(name: String): String = {
    import javax.xml.bind.DatatypeConverter, java.security.MessageDigest
    val md5 = MessageDigest.getInstance("MD5")
    DatatypeConverter.printHexBinary(md5.digest(name.getBytes("UTF-8"))).toLowerCase.take(6)
  }

  def report()(implicit runner: Runner): runner.Return = runner.report()

  private[estrapade] val tests: HashMap[String, Definition[_]] = new HashMap()

  class Definition[Result](val name: String, val action: () => Result) {
    
    /** assert that the expression returns without throwing an exception */
    def returns()(implicit runner: Runner): Test[Result] = assert { v => true }
    
    /** make an assertion on the evaluated result of the test expression */
    def assert(assertion: Result => Boolean)(implicit runner: Runner): Test[Result] =
      macro Macros.assertion
  
    lazy val hash: String = Test.hash(name)
      
    def assert(
      assertion: Result => Boolean,
      msg: Result => String)(implicit runner: Runner
    ): Test[Result] =
      if(!runner.skip(hash)) {
        val t0 = System.nanoTime
        val result = Try(action())
        val duration = System.nanoTime - t0
        val outcome = result.map { r =>
          Try(if(assertion(r)) Passed else Failed(msg(r))) match {
            case Success(v) => v
            case Failure(e) => ThrewInCheck(e)
          }
        } match {
          case Success(v) => v
          case Failure(DependencyFailureException(dep)) =>
            FailedDependency(dep.take(6))
          case Failure(f) =>
            ThrewInRun(f)
        }
        runner.record(this, outcome, duration)
        val test = new Test(() => result, hash)
        test.lazyValue
        test
      } else new Test(() => Try(action()), hash)
  }
  
  sealed abstract class Outcome(val string: String, val diagnosis: List[String],
      val success: Option[Boolean])
  
  case object Passed extends Outcome("pass", Nil, Some(true))
  case class Failed(reason: String) extends Outcome("fail", List(reason), Some(false))
  
  case class ThrewInCheck(throwable: Throwable) extends
      Outcome("fail", List(s"exception thrown during assertion: ${throwable}"), None)
  
  case class ThrewInRun(throwable: Throwable) extends
      Outcome("fail", List(s"exception thrown during run: ${throwable}"), Some(false))
  
  case class FailedDependency(dep: String) extends
      Outcome("skip", List(s"dependency $dep failed"), None)
  
  case object Unstable extends Outcome("vari", Nil, Some(false))
  
  case class Result(definition: Test.Definition[_], outcome: Outcome, duration: Long)
}

