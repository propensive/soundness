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

object Divisible:
  def apply[DividendType, DivisorType, ResultType]
     (lambda: (DividendType, DivisorType) => ResultType)
  :     DividendType is Divisible by DivisorType into ResultType = new Divisible:
    type Self = DividendType
    type Result = ResultType
    type Operand = DivisorType

    def divide(dividend: DividendType, divisor: DivisorType): ResultType = lambda(dividend, divisor)

  given Double is Divisible by Double into Double = Divisible:
    (dividend, divisor) => dividend/divisor

  given Float is Divisible by Float into Float = Divisible:
    (dividend, divisor) => dividend/divisor

  given Long is Divisible by Long into Long = Divisible:
    (dividend, divisor) => dividend/divisor

  given Int is Divisible by Int into Int = Divisible:
    (dividend, divisor) => dividend/divisor

  given Short is Divisible by Short into Short = Divisible:
    (dividend, divisor) => (dividend/divisor).toShort

  given Byte is Divisible by Byte into Byte = Divisible:
    (dividend, divisor) => (dividend/divisor).toByte


trait Divisible:
  type Self
  type Result
  type Operand
  type Dividend = Self
  type Divisor = Operand

  def divide(dividend: Self, divisor: Divisor): Result

  extension (dividend: Self)
    @targetName("divide")
    inline infix def / (divisor: Divisor): Result = divide(dividend, divisor)
