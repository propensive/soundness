package probation

import scala.language.implicitConversions
import scala.language.experimental.macros

object `package` extends package_1 {
  /** creates a new [[Test.Definition]], upon which an assertion may be made */
  def test[T](name: String)(action: => T): Test.Definition[T] =
    Test.Definition[T](name, () => action, Nil)

  /** captures any standard output produced during the execution of the thunk into a [[String]] */
  def captureStdout(block: => Unit): String = {
    import java.io._
    val out = scala.Console.out
    val baos = new ByteArrayOutputStream()
    scala.Console.withOut(new PrintStream(baos))(block)
    baos.toString
  }
  
  implicit def namedObserved[T: Show](value: (String, T)): Observed[T] =
    new Observed[T](value)

  def compileTimeRestart(): Unit = macro ReportingMacros.restart
  def compileTimeReport(): Unit = macro ReportingMacros.report
}

trait package_1 {
  implicit def unnamedObserved[T: Show](value: T): Observed[T] =
    new Observed(("parameter", value))
}

class Observed[T: Show](value: => (String, T)) {
  def apply(): (String, String) = {
    val (label, v) = value
    (label, implicitly[Show[T]].show(v))
  }
}
