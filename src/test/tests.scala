/*
    Baroque, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package baroque

import probably.*
import symbolism.*
import gossamer.*
import spectacular.*
import quantitative.*
import symbolism.*

import scala.compiletime.*

given Decimalizer = Decimalizer(3)

object Tests extends Suite(t"Baroque tests"):
  def run(): Unit =
    test(t"Show a complex number"):
      Complex(1, 3).show
    .assert(_ == t"1 + 3𝒾")

    test(t"Show a quantity complex number"):
      Complex(1*Metre/Second, 9*Metre/Second).show
    .assert(_ == t"1.00 + 9.00𝒾 m·s¯¹")

    test(t"Add two int-complex numbers"):
      Complex(1, 2) + Complex(8, 2)
    .assert(_ == Complex(9, 4))
    
    test(t"Add two double-complex numbers"):
      Complex(0.1, 0.2) + Complex(0.8, 0.2)
    .assert(_ == Complex(0.9, 0.4))

    test(t"Add two quantity-complex numbers"):
      Complex(1*Metre, 3*Metre) + Complex(2*Metre, 8*Metre)
    .assert(_ == Complex(3*Metre, 11*Metre))
    
    test(t"Add heterogeneous quantity complex numbers"):
      Complex(1*Inch, 3*Inch) + Complex(2*Foot, 8*Foot)
    .assert(_ == Complex(0.635*Metre, 2.5146*Metre))
  
    test(t"Multiply complex numbers"):
      Complex(1, 3)*Complex(2, 4)
    .assert(_ == Complex(-10, 10))
    
    test(t"Divide complex numbers"):
      Complex(3.0, 2.0)/Complex(1.0, -4.0)
    .assert { c => c.real == -0.29411764705882354 && c.imaginary == 0.8235294117647058 }
    
    test(t"Divide complex quantities"):
      Complex(3.0*Metre, 2.0*Metre)/Complex(1.0*Foot, -4.0*Foot)
    .assert(_ == Complex(-0.9649529102979775,2.7018681488343375))
    
    test(t"Multiply complex quantity numbers"):
      Complex(18*Foot, 1.4*Foot)*Complex(4*Kilo(Gram), 2*Kilo(Gram))
    .assert(_ == Complex(69.2*Foot*Kilo(Gram), 41.6*Foot*Kilo(Gram)))

