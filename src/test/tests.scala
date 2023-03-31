package cardinality

import probably.*
import rudiments.*
import larceny.*
import gossamer.*

object Tests extends Suite(t"Cardinality tests"):
  def run(): Unit =
    suite(t"Compile-time tests"):
      
      test(t"Value is less than lower bound"):
        captureCompileErrors:
          val x: -1.0 ~ 1.0 = -1.01
        .map(_.errorId)
      .assert(_ == List(ErrorId.TypeMismatchID))
      
      test(t"Value is greater than upper bound"):
        captureCompileErrors:
          val x: -1.0 ~ 1.0 = 1.01
        .map(_.errorId)
      .assert(_ == List(ErrorId.TypeMismatchID))
      
      test(t"Doubling a number doubles its range"):
        captureCompileErrors:
          val x: -1.0 ~ 1.0 = 0.0
          val y: -2.0 ~ 2.0 = x*2.0
      .aspire(_ == Nil)