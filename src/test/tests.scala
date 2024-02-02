/*
    Mosquito, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package mosquito

import gossamer.*
import rudiments.*
import probably.*
import spectacular.*
import turbulence.*
import quantitative.*
import hieroglyph.*, textMetrics.uniform
import larceny.*

object Tests extends Suite(t"Mosquito tests"):
  def run(): Unit =
    test(t"Create a Euclidean of Ints"):
      Euclidean(1, 2, 3)
    .assert(_ == Euclidean(1, 2, 3))
    
    test(t"A Euclidean of Ints infers the correct size"):
      demilitarize:
        val vector: Euclidean[Int, 3] = Euclidean(1, 3, 4)
      .map(_.message)
    .assert(_ == Nil)
    
    test(t"Type error if size is incorrect"):
      demilitarize:
        val vector: Euclidean[Int, 2] = Euclidean(1, 3, 4)
      .map(_.errorId)
    .assert(_ == List(ErrorId.TypeMismatchID))
    
    test(t"Type error if type is incorrect"):
      demilitarize:
        val vector: Euclidean[String, 3] = Euclidean(1, 3, 4)
      .map(_.errorId)
    .assert(_ == List(ErrorId.TypeMismatchID))

    test(t"Calculate integer dot-product"):
      Euclidean(1, 2, 3).dot(Euclidean(4, 3, 7))
    .assert(_ == 31)
    
    test(t"Calculate Double dot-product"):
      Euclidean(0.1, 0.2, 0.3).dot(Euclidean(0.4, 0.3, 0.7))
    .assert(_ meets 0.31 +/- 0.000001)
    
    test(t"Calculate integer cross-product"):
      Euclidean(1, 2, 3).cross(Euclidean(4, 3, 7))
    .assert(_ == Euclidean(5, 5, -5))
    
    test(t"Calculate Double cross-product"):
      Euclidean(1.4, 2.4, 3.8).cross(Euclidean(4.9, 3.6, 0.7))
    .assert(_ == Euclidean(-12.0, 17.64, -6.72))
    
    test(t"Show Euclidean 3-vector"):
      Euclidean(1, 3, 6).show
    .assert(_ == t"\u239b 1 \u239e\n\u239c 3 \u239f\n\u239d 6 \u23a0")
    
    test(t"Add two vectors"):
      Euclidean(1, 2, 3) + Euclidean(3, 4, 5)
    .assert(_ == Euclidean(4, 6, 8))
    
    suite(t"Quantity operations"):
      test(t"Add two quantity vectors"):
        Euclidean(1*Metre, 2*Metre, 3*Metre) + Euclidean(3*Metre, 4*Metre, 5*Metre)
      .assert(_ == Euclidean(4*Metre, 6*Metre, 8*Metre))
    
      test(t"Add two mixed-quantity vectors"):
        Euclidean(1*Foot, 1*Foot, 1*Foot) + Euclidean(3*Metre, 4*Metre, 5*Metre)
      .assert(_ == Euclidean(3.3048*Metre, 4.3048*Metre, 5.3048*Metre))

      test(t"Map from m to m²"):
        Euclidean(1*Metre, 2*Metre, 3*Metre, 4*Metre).map(_*Metre)
      .assert(_ == Euclidean(1*Metre*Metre, 2*Metre*Metre, 3*Metre*Metre, 4*Metre*Metre))

    suite(t"Matrix tests"):
      val m1 = Matrix[2, 3][Int](1, 2, 3, 4, 5, 6)
      val m2 = Matrix[3, 2][Int](7, 8, 9, 10, 11, 12)
    
      test(t"Access matrix elements"):
        m1(0, 0)
      .assert(_ == 1)
      
      test(t"Access matrix elements2"):
        m1(1, 1)
      .assert(_ == 5)
      
      test(t"Access matrix elements 3"):
        m1(1, 2)
      .assert(_ == 6)
      
      test(t"Multiply matrices"):
        m1*m2
      .assert(_ == Matrix[2, 2][Int](58, 139, 64, 154))
