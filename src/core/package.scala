package probably

import scala.language.implicitConversions
import scala.language.experimental.macros

object `package` {
  /** creates a new [[Test.Definition]], upon which an assertion may be made */
  def test[T](name: String)(action: => T): Test.Definition[T] =
    Test.Definition[T](name, () => action)

  /** captures any standard output produced during the execution of the thunk into a [[String]] */
  def captureStdout(block: => Unit): String = {
    import java.io._
    val out = scala.Console.out
    val baos = new ByteArrayOutputStream()
    scala.Console.withOut(new PrintStream(baos))(block)
    baos.toString
  }
}
