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

object Multiplicable:
  class Basic[MultiplicandType, MultiplierType, ResultType]
     (lambda: (MultiplicandType, MultiplierType) => ResultType)
  extends Multiplicable:
    type Self = MultiplicandType
    type Result = ResultType
    type Operand = MultiplierType

    def multiply(multiplicand: MultiplicandType, multiplier: MultiplierType): ResultType =
      lambda(multiplicand, multiplier)

  given Double is Multiplicable by Double into Double = new Multiplicable:
    type Self = Double
    type Operand = Double
    type Result = Double
    def multiply(multiplicand: Double, multiplier: Double): Double = multiplicand*multiplier

  given Long is Multiplicable by Long into Long = new Multiplicable:
    type Self = Long
    type Operand = Long
    type Result = Long
    def multiply(multiplicand: Long, multiplier: Long): Long = multiplicand*multiplier

  given Int is Multiplicable by Int into Int = new Multiplicable:
    type Self = Int
    type Operand = Int
    type Result = Int
    def multiply(multiplicand: Int, multiplier: Int): Int = multiplicand*multiplier

  given Float is Multiplicable by Float into Float = new Multiplicable:
    type Self = Float
    type Operand = Float
    type Result = Float
    def multiply(multiplicand: Float, multiplier: Float): Float = multiplicand*multiplier

  given Short is Multiplicable by Short into Short = new Multiplicable:
    type Self = Short
    type Operand = Short
    type Result = Short
    def multiply(multiplicand: Short, multiplier: Short): Short = (multiplicand*multiplier).toByte

  given Byte is Multiplicable by Byte into Byte = new Multiplicable:
    type Self = Byte
    type Operand = Byte
    type Result = Byte
    def multiply(multiplicand: Byte, multiplier: Byte): Byte = (multiplicand*multiplier).toByte

trait Multiplicable:
  type Self
  type Multiplicand = Self
  type Result
  type Operand
  type Multiplier = Operand

  def multiply(multiplicand: Multiplicand, multiplier: Multiplier): Result

  extension (multiplicand: Multiplicand)
    @targetName("multiply")
    inline infix def * (multiplier: Multiplier): Result = multiply(multiplicand, multiplier)
