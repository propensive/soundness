/*
    Cardinality, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package cardinality

import probably.*
import rudiments.*
import larceny.*
import gossamer.*
import fulminate.*

object Tests extends Suite(t"Cardinality tests"):
  def run(): Unit =
    suite(t"Compile-time tests"):
      
      test(t"Value is less than lower bound"):
        demilitarize:
          val x: -1.0 ~ 1.0 = -1.01
        .map(_.errorId)
      .assert(_ == List(ErrorId.TypeMismatchID))
      
      test(t"Value is greater than upper bound"):
        demilitarize:
          val x: -1.0 ~ 1.0 = 1.01
        .map(_.errorId)
      .assert(_ == List(ErrorId.TypeMismatchID))
      
      test(t"Doubling a number doubles its range"):
        demilitarize:
          val x: -1.0 ~ 1.0 = 0.0
          val y: -2.0 ~ 2.0 = x*2.0
      .aspire(_ == Nil)
