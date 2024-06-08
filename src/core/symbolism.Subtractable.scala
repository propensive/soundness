/*
    Symbolism, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

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

import language.experimental.captureChecking

import scala.annotation.targetName

object Subtractable:
  class Basic[MinuendType, SubtrahendType, ResultType]
      (lambda: (MinuendType, SubtrahendType) => ResultType)
  extends Subtractable[SubtrahendType]:
    type Self = MinuendType
    type Result = ResultType

    def subtract(minuend: MinuendType, subtrahend: SubtrahendType): ResultType =
      lambda(minuend, subtrahend)

  given Double is Subtractable[Double] into Double as double = _ - _
  given Float is Subtractable[Float] into Float as float = _ - _
  given Long is Subtractable[Long] into Long as long = _ - _
  given Int is Subtractable[Int] into Int as int = _ - _
  given Short is Subtractable[Short] into Short as short = (minuend, subtrahend) => (minuend - subtrahend).toShort
  given Byte is Subtractable[Byte] into Byte as byte = (minuend, subtrahend) => (minuend - subtrahend).toByte

trait Subtractable[-SubtrahendType]:
  type Self
  type Result
  type Minuend = Self
  def subtract(minuend: Minuend, subtrahend: SubtrahendType): Result

  extension (minuend: Minuend)
    @targetName("subtract")
    inline infix def - (subtrahend: SubtrahendType): Result = subtract(minuend, subtrahend)
