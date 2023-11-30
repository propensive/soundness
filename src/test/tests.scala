package feudalism

import probably.*
import gossamer.*

import language.experimental.captureChecking

object Tests extends Suite(t"Feudalism tests"):
  def run(): Unit =
    test(t"read mutex"):
      val mutex = Mutex[String]("Hello")

      val result = mutex.read: ref =>
        println(ref())
        ref.snapshot()
      
      result

    .assert(_ == "Hello")