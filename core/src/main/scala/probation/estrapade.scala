package probation

import language.implicitConversions
import language.experimental.macros
import language.dynamics

import scala.collection.mutable.HashMap
import scala.util._

/** a value representing a test */
case class Test[Result](
  definition: Test.Definition[Result],
  assertion: Result => Boolean,
  failure: Result => String
)(implicit runner: Runner) {
  private[probation] lazy val result: (Try[Result], Long) = {
    val t0 = System.nanoTime
    try {
      val result = definition.action()
      val duration = System.nanoTime - t0
      (Success[Result](result), duration)
    } catch { case e: Exception => (Failure[Result](e), System.nanoTime - t0) }
  }

  /** gets the value resulting from evaluating this test */
  def apply(): Result = runner.exec(this)
}

/** companion object for [[Test]] */
object Test {

  /** exception which is thrown when attempting to get the value from a dependent test which has
   *  failed */
  case class DependencyFailureException(dep: String) extends
      Exception("a failure occurred in a dependency")
  
  private[probation] def hash(name: String): String = {
    import javax.xml.bind.DatatypeConverter, java.security.MessageDigest
    val md5 = MessageDigest.getInstance("MD5")
    DatatypeConverter.printHexBinary(md5.digest(name.getBytes("UTF-8"))).toLowerCase.take(6)
  }

  /** use the current [[Runner]] to produce a report of all the tests which have been run */
  def report()(implicit runner: Runner): runner.Return = runner.report()

  private[probation] val tests: HashMap[String, Definition[_]] = new HashMap()

  // the name of the method used to constrain calls to `applyDynamicNamed` in [[Definition]].
  final val method = "observe"

  // A definition of a test, without 
  case class Definition[Result](name: String, action: () => Result, observed: Seq[Observed[_]]) extends Dynamic {
   
    def applyDynamicNamed(meth: method.type)(observed: Observed[_]*): Definition[Result] =
      Definition(name, action, observed)

    def applyDynamic(meth: method.type)(observed: Observed[_]*): Definition[Result] =
      Definition(name, action, observed)

    override def hashCode: Int = name.hashCode
    
    override def equals(that: Any): Boolean = that match {
      case that: Definition[_] => name == that.name
      case _ => false
    }

    /** assert that the expression returns some value, without throwing an exception */
    def returns()(implicit runner: Runner): Test[Result] = assert { v => true }
    
    /** make an assertion on the evaluated result of the test expression */
    def assert(assertion: Result => Boolean)(implicit runner: Runner): Test[Result] =
      macro Macros.assertion
  
    /** a short hash representing the test */
    lazy val hash: String = Test.hash(name)
    
    /** makes an assertion on the evaluated result of the test expression, and reports a custom
     *  failure message if the assertion returns false
     *
     *  All calls to the single-parameter overloaded implementation of `assert` are rewritten by a
     *  macro to this implementation, with the macro providing an appropriate failure message. */
    def assert(assertion: Result => Boolean, msg: Result => String)(implicit runner: Runner):
        Test[Result] = {
      val test = Test[Result](this, assertion, msg)
      runner.record(test)
      test
    }
      
     /* if(!runner.skip(hash)) {
        val t0 = System.nanoTime
        val result = Try(action())
        val duration = System.nanoTime - t0
        
        val outcome = result.map { r =>
          Try(if(assertion(r)) Passed else Failed(msg(r), observed.map(_()).toMap)) match {
            case Success(v) => v
            case Failure(e) => ThrewInCheck(e)
          }
        } match {
          case Success(v) => v
          case Failure(DependencyFailureException(dep)) =>
            FailedDependency(dep.take(6))
          case Failure(f) =>
            ThrewInRun(f, observed.map(_()).toMap)
        }
        runner.record(this, outcome, duration)
        val test = new Test(() => result, hash)
        test.result
        test
      } else new Test(() => Try(action()), hash)*/
  }
}

