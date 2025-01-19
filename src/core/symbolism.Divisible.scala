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

object Divisible:
  class Basic[DividendType, DivisorType, ResultType](lambda: (DividendType, DivisorType) => ResultType)
  extends Divisible:
    type Self = DividendType
    type Result = ResultType
    type Operand = DivisorType

    def divide(dividend: DividendType, divisor: DivisorType): ResultType = lambda(dividend, divisor)

  given Double is Divisible by Double into Double = new Divisible:
    type Self = Double
    type Operand = Double
    type Result = Double
    def divide(dividend: Dividend, divisor: Divisor): Result = dividend/divisor

  given Float is Divisible by Float into Float = new Divisible:
    type Self = Float
    type Operand = Float
    type Result = Float
    def divide(dividend: Dividend, divisor: Divisor): Result = dividend/divisor

  given Long is Divisible by Long into Long = new Divisible:
    type Self = Long
    type Operand = Long
    type Result = Long
    def divide(dividend: Dividend, divisor: Divisor): Result = dividend/divisor

  given Int is Divisible by Int into Int = new Divisible:
    type Self = Int
    type Operand = Int
    type Result = Int
    def divide(dividend: Dividend, divisor: Divisor): Result = dividend/divisor

  given Short is Divisible by Short into Short = new Divisible:
    type Self = Short
    type Operand = Short
    type Result = Short
    def divide(dividend: Dividend, divisor: Divisor): Result = (dividend/divisor).toShort

  given Byte is Divisible by Byte into Byte = new Divisible:
    type Self = Byte
    type Operand = Byte
    type Result = Byte
    def divide(dividend: Dividend, divisor: Divisor): Result = (dividend/divisor).toByte


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
