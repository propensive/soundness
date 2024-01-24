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
    