package testing

import larceny.*
import annotation.*

@main
def run(): Unit =
  val errors = captureCompileErrors:
    val x = 10
    "foo".substring("x")
    "foo".substring("y")
  
  errors.foreach(println(_))
  
  val errors2 = captureCompileErrors:
    val x = 12
    x.whatever("y")
  
  errors2.foreach(println(_))
