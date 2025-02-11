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

object Multiplicable:
  def apply[MultiplicandType, MultiplierType, ResultType]
     (lambda: (MultiplicandType, MultiplierType) => ResultType)
  :     MultiplicandType is Multiplicable by MultiplierType into ResultType = new Multiplicable:
    type Self = MultiplicandType
    type Result = ResultType
    type Operand = MultiplierType

    def multiply(multiplicand: MultiplicandType, multiplier: MultiplierType): ResultType =
      lambda(multiplicand, multiplier)

  given Double is Multiplicable by Double into Double = Multiplicable:
    (multiplicand, multiplier) => multiplicand*multiplier

  given Long is Multiplicable by Long into Long = Multiplicable:
    (multiplicand, multiplier) => multiplicand*multiplier

  given Int is Multiplicable by Int into Int = Multiplicable:
    (multiplicand, multiplier) => multiplicand*multiplier

  given Float is Multiplicable by Float into Float = Multiplicable:
    (multiplicand, multiplier) => multiplicand*multiplier

  given Short is Multiplicable by Short into Short = Multiplicable:
    (multiplicand, multiplier) => (multiplicand*multiplier).toByte

  given Byte is Multiplicable by Byte into Byte = Multiplicable:
    (multiplicand, multiplier) => (multiplicand*multiplier).toByte

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
