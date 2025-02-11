/*
    Symbolism, version 0.26.0. Copyright 2025 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package symbolism

import prepositional.*

import scala.annotation.targetName

object Subtractable:
  def apply[MinuendType, SubtrahendType, ResultType]
     (lambda: (MinuendType, SubtrahendType) => ResultType)
  :     MinuendType is Subtractable by SubtrahendType into ResultType = new Subtractable:
    type Self = MinuendType
    type Result = ResultType
    type Operand = SubtrahendType

    def subtract(minuend: MinuendType, subtrahend: SubtrahendType): ResultType =
      lambda(minuend, subtrahend)

  given Double is Subtractable by Double into Double = Subtractable:
    (minuend, subtrahend) => minuend - subtrahend

  given Float is Subtractable by Float into Float = Subtractable:
    (minuend, subtrahend) => minuend - subtrahend

  given Long is Subtractable by Long into Long = Subtractable:
    (minuend, subtrahend) => minuend - subtrahend

  given Int is Subtractable by Int into Int = Subtractable:
    (minuend, subtrahend) => minuend - subtrahend

  given Short is Subtractable by Short into Short = Subtractable:
    (minuend, subtrahend) => (minuend - subtrahend).toShort

  given Byte is Subtractable by Byte into Byte = Subtractable:
    (minuend, subtrahend) => (minuend - subtrahend).toByte

trait Subtractable:
  type Self
  type Result
  type Operand
  type Minuend = Self
  type Subtrahend = Operand

  def subtract(minuend: Minuend, subtrahend: Subtrahend): Result

  extension (minuend: Minuend)
    @targetName("subtract")
    inline infix def - (subtrahend: Subtrahend): Result = subtract(minuend, subtrahend)
