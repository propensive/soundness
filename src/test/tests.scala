/*
    Quantify, version [unreleased]. Copyright 2023 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

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
  
    test(Text("SI kilo prefix multiplies by 10^3")):
      15.0*Kilo(Metre)
    .assert(_ == 15000.0*Metre)
    
    test(Text("SI mega prefix multiplies by 10^6")):
      15.0*Mega(Metre)
    .assert(_ == 15000000.0*Metre)
    
    test(Text("SI giga prefix multiplies by 10^9")):
      15.0*Giga(Metre)
    .assert(_ == 15000000000.0*Metre)
    
    test(Text("SI kibi prefix multiplies by 2^10")):
      10*Kibi(Metre)
    .assert(_ == 10240*Metre)
    
    test(Text("SI mebi prefix multiplies by 2^20")):
      10*Mebi(Metre)
    .assert(_ == (1024*1024*10)*Metre)
    
    test(Text("SI milli prefix multiplies by 10^-3")):
      1.5*Milli(Metre)
    .assert(_ == 0.0015*Metre)
    
    test(Text("SI micro prefix multiplies by 10^-6")):
      1.5*Micro(Metre)
    .assert(_ == 0.0000015*Metre)
    
    test(Text("SI nano prefix multiplies by 10^-9")):
      2.5*Nano(Metre)
    .assert(_ == 0.0000000025*Metre)
    