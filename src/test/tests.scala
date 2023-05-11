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
import gossamer.{t, Decimalizer}
import larceny.*
import spectacular.*

import language.strictEquality

given decimalizer: Decimalizer = Decimalizer(3)

object Tests extends Suite(Text("Quantify Tests")):
  def run(): Unit =
    suite(Text("Arithmetic tests")):
      test(Text("Add two lengths")):
        Metre + Metre*2
      .assert(_ == Metre*3)
    
      test(Text("Multiply two different units")):
        2*Second * 3*Metre
      .assert(_ == 6*Metre*Second)

      test(Text("Invert a quantity")):
        (2*Metre/Second).invert
      .assert(_ == 0.5*Second/Metre)
      
      test(Text("Divide a double by a quantity")):
        1.0/(2.0*Metre/Second)
      .assert(_ == 0.5*Second/Metre)
    
    suite(t"Compile errors"):
      test(Text("Cannot add quantities of different units")):
        captureCompileErrors:
          Metre + 2*Second
        .map(_.errorId)
      .assert(_.contains(ErrorId.NoExplanationID))
    
      test(Text("Cannot subtract quantities of different units")):
        captureCompileErrors:
          Metre - 2*Second
        .map(_.errorId)
      .assert(_.contains(ErrorId.NoExplanationID))
      
      test(Text("Add two different units")):
        captureCompileErrors:
          Second*2 + Metre*3
        .map(_.errorId)
      .assert(_.contains(ErrorId.NoExplanationID))

      test(Text("Units cancel out")):
        captureCompileErrors:
          (20*Metre*Second)/(Metre*Second): Double
      .assert(_.isEmpty)
    
      test(Text("Principal units are preferred")):
        captureCompileErrors:
          val x = 2*Metre
          val y = 3*Foot
          val z: Quantity[Metres[2]] = x*y
      .assert(_.isEmpty)
    
      test(Text("Units of different dimension cannot be added")):
        captureCompileErrors:
          2*Metre + 2*Joule
        .map(_.message)
      .assert(_ == List("quantify: the left operand represents length, but the right operand represents energy; these are incompatible physical quantities"))
      
      test(Text("Different dimensions are incomparable")):
        captureCompileErrors:
          7*Metre >= 2*Kilo(Gram)
        .map(_.message)
      .assert(_ == List("quantify: the left operand represents length, but the right operand represents mass; these are incompatible physical quantities"))
      
      test(Text("Different powers of the same dimension are incomparable")):
        captureCompileErrors:
          7*Metre >= 2*Metre*Metre
        .map(_.message)
      .assert(_ == List("quantify: the left operand represents length, but the right operand represents area; these are incompatible physical quantities"))
    
    suite(t"Automatic conversions"):
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
        y/x
      .assert(_ == 0.1393545510813086*Metre)
    
      test(Text("Mixed units of the same dimension can be added")):
        2*Metre + 2*Foot
      .assert(_ == 2.6095999804928005*Metre)
      
      test(Text("Mixed units of the same dimension can be subtracted")):
        2*Metre - 2*Foot
      .assert(_ == 1.3904000195071995*Metre)
      
      test(Text("Mixed units of the same type can be added (reverse order)")):
        2*Foot + 2*Metre
      .assert(_ == 2.6095999804928005*Metre)
    
  
    suite(t"Metric prefixes"):
      test(Text("Metric kilo prefix multiplies by 10^3")):
        15.0*Kilo(Metre)
      .assert(_ == 15000.0*Metre)
      
      test(Text("Metric mega prefix multiplies by 10^6")):
        15.0*Mega(Metre)
      .assert(_ == 15000000.0*Metre)
      
      test(Text("Metric giga prefix multiplies by 10^9")):
        15.0*Giga(Metre)
      .assert(_ == 15000000000.0*Metre)
      
      test(Text("Metric kibi prefix multiplies by 2^10")):
        10*Kibi(Metre)
      .assert(_ == 10240*Metre)
      
      test(Text("Metric mebi prefix multiplies by 2^20")):
        10*Mebi(Metre)
      .assert(_ == (1024*1024*10)*Metre)
      
      test(Text("Metric milli prefix multiplies by 10^-3")):
        1.5*Milli(Metre)
      .assert(_ == 0.0015*Metre)
      
      test(Text("Metric micro prefix multiplies by 10^-6")):
        1.5*Micro(Metre)
      .assert(_ == 0.0000015*Metre)
      
      test(Text("Metric nano prefix multiplies by 10^-9")):
        2.5*Nano(Metre)
      .assert(_ == 0.0000000025*Metre)

    suite(t"Explicit conversion tests"):
      test(Text("Convert feet to metres")):
        (3.0*Foot).in[Metres]
      .assert(_ == 0.914399970739201*Metre)
      
      test(Text("Convert metres to feet")):
        (3.0*Metre).in[Feet]
      .assert(_ == 9.84252*Foot)
      
      test(Text("Convert m² to ft²")):
        (Metre*Metre).in[Feet]
      .assert(_ == 10.7639111056*Foot*Foot)
      
      test(Text("Conversion to seconds does nothing")):
        (3.0*Metre).in[Seconds]
      .assert(_ == 3.0*Metre)
    
    suite(t"Inequalities"):
      test(Text("6ft < 2m")):
        6*Foot < 2*Metre
      .assert(_ == true)
      
      test(Text("6ft <= 2m")):
        6*Foot < 2*Metre
      .assert(_ == true)
      
      test(Text("7ft > 2m")):
        7*Foot > 2*Metre
      .assert(_ == true)
      
      test(Text("7ft >= 2m")):
        7*Foot >= 2*Metre
      .assert(_ == true)
      
      test(Text("9ft² < 1m²")):
        9*Foot*Foot < Metre*Metre
      .assert(_ == true)
      
      test(Text("10ft² < 1m²")):
        10*Foot*Foot < Metre*Metre
      .assert(_ == true)
      
      test(Text("11ft² > 1m²")):
        11*Foot*Foot > Metre*Metre
      .assert(_ == true)
    
    suite(Text("Rendering tests")):
      test(Text("Show a value in metres")):
        (7.567*Metre).show
      .assert(_ == Text("7.57 m"))
      
      test(Text("Show a value in square metres")):
        (1.4*Metre*Metre).show
      .assert(_ == Text("1.40 m²"))
      
      test(Text("Show a value in metres per second")):
        (8.54*Metre/Second).show
      .assert(_ == Text("8.54 m·s¯¹"))
      
      test(Text("Show a value in kilometres per second")):
        (8.54*Kilo(Metre)/Second).show
      .assert(_ == Text("8.54×10³ m·s¯¹"))
      
      test(Text("Show a value in kilograms")):
        (10.4*Kilo(Gram)/Second).show
      .assert(_ == Text("10.4 kg·s¯¹"))
      
      test(Text("Show the speed of light")):
        constants.SpeedOfLightInVacuum.show
      .assert(_ == Text("3.00×10⁸ m·s¯¹"))
      
      test(Text("Show Planck's constant")):
        constants.PlanckConstant.show
      .assert(_ == Text("6.63×10¯³⁴ m²·kg·s¯¹"))

    suite(t"Quantity descriptions"):
      test(t"describe a base dimension"):
        Metre.dimension
      .assert(_ == t"length")
      
      test(t"describe a compound dimension"):
        (Metre/Second).dimension
      .assert(_ == t"velocity")
      
      test(t"describe a complex compound dimension"):
        (Foot*Foot*Kilo(Gram)/(Second*Second*Mole)).dimension
      .assert(_ == t"chemical potential")
        
    suite(t"Quantifiability tests"):
      case class Pts(value: Double)
      given Quantifiable[Pts, Inches[1]] = pts => (Inch*pts.value)/72
      
      test(t"quantify a length"):
        Pts(71).quantify < Inch
      .assert(_ == true)
      
      test(t"quantify a length"):
        Pts(73).quantify > Inch
      .assert(_ == true)