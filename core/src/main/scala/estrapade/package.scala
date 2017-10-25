package estrapade

object `package` {
  /** creates a new [[Test.Definition]], upon which an assertion may be made */
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

  /** captures any standard output produced during the execution of the thunk into a [[String]] */
  def captureStdout(block: => Unit): String = {
    import java.io._
    val out = scala.Console.out
    val baos = new ByteArrayOutputStream()
    scala.Console.withOut(new PrintStream(baos))(block)
    baos.toString
  }
}

