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

object Negatable:
  def apply[OperandType, ResultType](lambda: OperandType => ResultType)
  :     OperandType is Negatable into ResultType = new Negatable:
    type Self = OperandType
    type Result = ResultType

    def negate(operand: Self): Result = lambda(operand)

  given Double is Negatable into Double = Negatable(-_)
  given Float is Negatable into Float = Negatable(-_)
  given Long is Negatable into Long = Negatable(-_)

  given Int is Negatable into Int = Negatable(-_)

  given Short is Negatable into Short = Negatable:
    operand => (-operand).toShort

  given Byte is Negatable into Byte = Negatable:
    operand => (-operand).toByte

trait Negatable:
  type Self
  type Result
  def negate(operand: Self): Result

  extension (operand: Self)
    @targetName("divide")
    inline def `unary_-`: Result = negate(operand)
