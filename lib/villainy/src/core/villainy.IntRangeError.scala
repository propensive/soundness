/*
    Villainy, version [unreleased]. Copyright 2025 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package villainy

import anticipation.*
import contingency.*
import fulminate.*
import inimitable.*
import jacinta.*
import kaleidoscope.*
import merino.*
import urticose.*
import polyvinyl.*
import rudiments.*
import vacuous.*

import scala.compiletime.*

import strategies.throwUnsafely

object IntRangeError:
  def range(minimum: Optional[Int], maximum: Optional[Int]): Text =
    Text(s"${minimum.let { n => s"$n ≤ " }.or("")}x${minimum.let { n => s" ≤ $n" }.or("")}")

case class IntRangeError(value: Int, minimum: Optional[Int], maximum: Optional[Int])
   (using Diagnostics)
extends Error(m"the integer $value is not in the range ${IntRangeError.range(minimum, maximum)}")
