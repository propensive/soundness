/*
    Symbolism, version [unreleased]. Copyright 2023 Jon Pretty, Propensive OÜ.

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

trait Operator[OperatorType <: String & Singleton, -LeftType, -RightType]:
  type Result
  def apply(left: LeftType, right: RightType): Result

trait ClosedOperator[OperatorType <: String & Singleton, Type]
extends Operator[OperatorType, Type, Type]:
  type Result = Type
  def op(left: Type, right: Type): Type
  def apply(left: Type, right: Type): Type = op(left, right)

trait UnaryOperator[OperatorType <: String & Singleton, Type]:
  type Result
  inline def apply(inline value: Type): Result

trait ClosedUnaryOperator[OperatorType <: String & Singleton, Type]:
  type Result = Type
  def op(value: Type): Type
  inline def apply(inline value: Type): Type = op(value)

object Operator:  
  given plusByte: ClosedOperator["+", Byte] = (left, right) => (left + right).toByte
  given plusShort: ClosedOperator["+", Short] = (left, right) => (left + right).toShort
  given plusInt: ClosedOperator["+", Int] = _ + _
  given plusLong: ClosedOperator["+", Long] = _ + _
  given plusFloat: ClosedOperator["+", Float] = _ + _
  given plusDouble: ClosedOperator["+", Double] = _ + _

  given minusByte: ClosedOperator["-", Byte] = (left, right) => (left - right).toByte
  given minusShort: ClosedOperator["-", Short] = (left, right) => (left - right).toShort
  given minusInt: ClosedOperator["-", Int] = _ - _
  given minusLong: ClosedOperator["-", Long] = _ - _
  given minusFloat: ClosedOperator["-", Float] = _ - _
  given minusDouble: ClosedOperator["-", Double] = _ - _

  given starByte: ClosedOperator["*", Byte] = (left, right) => (left*right).toByte
  given starShort: ClosedOperator["*", Short] = (left, right) => (left*right).toShort
  given starInt: ClosedOperator["*", Int] = _*_
  given starLong: ClosedOperator["*", Long] = _*_
  given starFloat: ClosedOperator["*", Float] = _*_
  given starDouble: ClosedOperator["*", Double] = _*_

  given slashByte: ClosedOperator["/", Byte] = (left, right) => (left/right).toByte
  given slashShort: ClosedOperator["/", Short] = (left, right) => (left/right).toShort
  given slashInt: ClosedOperator["/", Int] = _/_
  given slashLong: ClosedOperator["/", Long] = _/_
  given slashFloat: ClosedOperator["/", Float] = _/_
  given slashDouble: ClosedOperator["/", Double] = _/_

object UnaryOperator:
  given ClosedUnaryOperator["-", Byte] = byte => (-byte).toByte
  given ClosedUnaryOperator["-", Short] = short => (-short).toShort
  given ClosedUnaryOperator["-", Int] = -_
  given ClosedUnaryOperator["-", Long] = -_
  given ClosedUnaryOperator["-", Float] = -_
  given ClosedUnaryOperator["-", Double] = -_
