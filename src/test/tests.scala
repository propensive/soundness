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
  
    test(Text("Principal units are preferred")):
      captureCompileErrors:
        val x = 2*Metre
        val y = 3*Foot
        val z: Quantity[Metre[2]] = x*y
    .assert(_ == Nil)
    
    test(Text("Conversions are applied automatically to RHS in multiplication")):
      val x = 2*Metre
      val y = 3*Foot
      x*y
    .assert(_ == 1.828799941478402*Metre*Metre)
    
    test(Text("Conversions are applied automatically to LHS in multiplication")):
      val x = 2*Metre
      val y = 3*Foot
      y*x
    .assert(_ == 1.828799941478402*Metre*Metre)
    
    test(Text("Conversions are applied automatically in division")):
      val x = 2*Metre*Metre
      val y = 3*Foot
      x/y
    .assert(_ == 2.1872266666666667*Metre)
    
    test(Text("Conversions are applied automatically to LHS in division")):
      val x = 2*Metre
      val y = 3*Foot*Foot
      val z: Quantity[Metre[1]] = y/x
      z
    .assert(_ == 0.1393545510813086*Metre)
    