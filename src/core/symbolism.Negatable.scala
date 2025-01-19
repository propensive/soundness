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

object Negatable:
  given Double is Negatable into Double = new Negatable:
    type Self = Double
    type Result = Double
    def negate(operand: Self): Result = -operand

  given Float is Negatable into Float = new Negatable:
    type Self = Float
    type Result = Float
    def negate(operand: Self): Result = -operand

  given Long is Negatable into Long = new Negatable:
    type Self = Long
    type Result = Long
    def negate(operand: Self): Result = -operand

  given Int is Negatable into Int = new Negatable:
    type Self = Int
    type Result = Int
    def negate(operand: Self): Result = -operand

  given Short is Negatable into Short = new Negatable:
    type Self = Short
    type Result = Short
    def negate(operand: Self): Result = (-operand).toShort

  given Byte is Negatable into Byte = new Negatable:
    type Self = Byte
    type Result = Byte
    def negate(operand: Self): Result = (-operand).toByte

trait Negatable:
  type Self
  type Result
  def negate(operand: Self): Result

  extension (operand: Self)
    @targetName("divide")
    inline def `unary_-`: Result = negate(operand)
