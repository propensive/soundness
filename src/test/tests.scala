package quantify

import probably.*
import rudiments.*
import larceny.*

object Tests extends Suite(Text("Quantify Tests")):
  def run(): Unit =
    test(Text("Add two lengths")):
      Metre + Metre*2
    .assert(_ == Metre*3)
    
    test(Text("Cannot add quantities of different units")):
      captureCompileErrors:
        Metre + 2*Second
      .map(_.errorId)
    .assert(_ == List(ErrorId.TypeMismatchID))
  
    test(Text("Cannot subtract quantities of different units")):
      captureCompileErrors:
        Metre - 2*Second
      .map(_.errorId)
    .assert(_ == List(ErrorId.TypeMismatchID))
    
    test(Text("Multiply two different units")):
      2*Second * 3*Metre
    .assert(_ == 6*Metre)
    
    test(Text("Add two different units")):
      captureCompileErrors:
        Second*2 + Metre*3
      .map(_.errorId)
    .assert(_ == List(ErrorId.TypeMismatchID))

    test(Text("Units cancel out")):
      captureCompileErrors:
        (20*Metre*Second)/(Metre*Second): Double
    .assert(_ == Nil)
    