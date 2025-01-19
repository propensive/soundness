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

import language.experimental.captureChecking

import scala.annotation.targetName

object Subtractable:
  class Basic[MinuendType, SubtrahendType, ResultType]
     (lambda: (MinuendType, SubtrahendType) => ResultType)
  extends Subtractable:
    type Self = MinuendType
    type Result = ResultType
    type Operand = SubtrahendType

    def subtract(minuend: MinuendType, subtrahend: SubtrahendType): ResultType =
      lambda(minuend, subtrahend)

  given Double is Subtractable by Double into Double = new Subtractable:
    type Self = Double
    type Result = Double
    type Operand = Double
    def subtract(minuend: Minuend, subtrahend: Subtrahend): Result = minuend - subtrahend

  given Float is Subtractable by Float into Float = new Subtractable:
    type Self = Float
    type Result = Float
    type Operand = Float
    def subtract(minuend: Minuend, subtrahend: Subtrahend): Result = minuend - subtrahend

  given Long is Subtractable by Long into Long = new Subtractable:
    type Self = Long
    type Result = Long
    type Operand = Long
    def subtract(minuend: Minuend, subtrahend: Subtrahend): Result = minuend - subtrahend

  given Int is Subtractable by Int into Int = new Subtractable:
    type Self = Int
    type Result = Int
    type Operand = Int
    def subtract(minuend: Minuend, subtrahend: Subtrahend): Result = minuend - subtrahend

  given Short is Subtractable by Short into Short = new Subtractable:
    type Self = Short
    type Result = Short
    type Operand = Short
    def subtract(minuend: Minuend, subtrahend: Subtrahend): Result = (minuend - subtrahend).toShort

  given Byte is Subtractable by Byte into Byte = new Subtractable:
    type Self = Byte
    type Result = Byte
    type Operand = Byte
    def subtract(minuend: Minuend, subtrahend: Subtrahend): Result = (minuend - subtrahend).toByte

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
