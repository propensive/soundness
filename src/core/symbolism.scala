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

import scala.annotation.*

import language.experimental.captureChecking

object AddOperator:
  given double: Addition[Double, Double, Double] = Addition(_ + _)
  given float: Addition[Float, Float, Float] = Addition(_ + _)
  given long: Addition[Long, Long, Long] = Addition(_ + _)
  given int: Addition[Int, Int, Int] = Addition(_ + _)
  given short: Addition[Short, Short, Short] = Addition({ (left, right) => (left + right).toShort })
  given byte: Addition[Byte, Byte, Byte] = Addition({ (left, right) => (left + right).toByte })

trait AddOperator[-LeftType, -RightType]:
  type Result
  def add(left: LeftType, right: RightType): Result
  
  extension (left: LeftType)
    @targetName("add")
    inline infix def + (right: RightType): Result = add(left, right)

final class Addition[-LeftType, -RightType, ResultType](lambda: (LeftType, RightType) => ResultType)
extends AddOperator[LeftType, RightType]:
  type Result = ResultType
  inline def add(left: LeftType, right: RightType): Result = lambda(left, right)

object SubOperator:
  given double: Subtraction[Double, Double, Double] = Subtraction(_ - _)
  given float: Subtraction[Float, Float, Float] = Subtraction(_ - _)
  given long: Subtraction[Long, Long, Long] = Subtraction(_ - _)
  given int: Subtraction[Int, Int, Int] = Subtraction(_ - _)
  
  given short: Subtraction[Short, Short, Short] =
    Subtraction({ (left, right) => (left - right).toShort })
  
  given byte: Subtraction[Byte, Byte, Byte] =
    Subtraction({ (left, right) => (left - right).toByte })

trait SubOperator[-LeftType, -RightType]:
  type Result
  def sub(left: LeftType, right: RightType): Result
  
  extension (left: LeftType)
    @targetName("sub")
    inline infix def - (right: RightType): Result = sub(left, right)

final class Subtraction[-LeftType, -RightType, ResultType]
    (lambda: (LeftType, RightType) => ResultType)
extends SubOperator[LeftType, RightType]:

  type Result = ResultType
  inline def sub(left: LeftType, right: RightType): Result = lambda(left, right)

object MulOperator:
  given double: Multiplication[Double, Double, Double] = Multiplication(_*_)
  given float: Multiplication[Float, Float, Float] = Multiplication(_*_)
  given long: Multiplication[Long, Long, Long] = Multiplication(_*_)
  given int: Multiplication[Int, Int, Int] = Multiplication(_*_)
  
  given short: Multiplication[Short, Short, Short] =
    Multiplication({ (left, right) => (left*right).toShort })
  
  given byte: Multiplication[Byte, Byte, Byte] =
    Multiplication({ (left, right) => (left*right).toByte })

trait MulOperator[-LeftType, -RightType]:
  type Result
  def mul(left: LeftType, right: RightType): Result
  
  extension (left: LeftType)
    @targetName("mul")
    inline infix def * (right: RightType): Result = mul(left, right)

final class Multiplication[-LeftType, -RightType, ResultType]
    (lambda: (LeftType, RightType) => ResultType)
extends MulOperator[LeftType, RightType]:

  type Result = ResultType
  inline def mul(left: LeftType, right: RightType): Result = lambda(left, right)

object DivOperator:
  given double: Division[Double, Double, Double] = Division(_/_)
  given float: Division[Float, Float, Float] = Division(_/_)
  given long: Division[Long, Long, Long] = Division(_/_)
  given int: Division[Int, Int, Int] = Division(_/_)
  given short: Division[Short, Short, Short] = Division({ (left, right) => (left/right).toShort })
  given byte: Division[Byte, Byte, Byte] = Division({ (left, right) => (left/right).toByte })

trait DivOperator[-LeftType, -RightType]:
  type Result
  def div(left: LeftType, right: RightType): Result
  
  extension (left: LeftType)
    @targetName("div")
    inline infix def / (right: RightType): Result = div(left, right)

final class Division[-LeftType, -RightType, ResultType](lambda: (LeftType, RightType) => ResultType)
extends DivOperator[LeftType, RightType]:
  type Result = ResultType
  inline def div(left: LeftType, right: RightType): Result = lambda(left, right)

object NegOperator:
  given byte: Negation[Byte, Byte] = Negation({ value => (-value).toByte })
  given short: Negation[Short, Short] = Negation({ value => (-value).toShort })
  given int: Negation[Int, Int] = Negation(-_)
  given long: Negation[Long, Long] = Negation(-_)
  given float: Negation[Float, Float] = Negation(-_)
  given double: Negation[Double, Double] = Negation(-_)

trait NegOperator[-LeftType]:
  type Result
  def neg(left: LeftType): Result
  
  extension (left: LeftType)
    @targetName("neg")
    inline def `unary_-`: Result = neg(left)

class Negation[-ValueType, ResultType](lambda: ValueType => ResultType)
extends NegOperator[ValueType]:

  type Result = ResultType
  def neg(value: ValueType): ResultType = lambda(value)

object RootOperator:
  given double2: Extraction[2, Double, Double] = Extraction(math.sqrt(_))
  given double3: Extraction[3, Double, Double] = Extraction(math.cbrt(_))
  given float2: Extraction[2, Double, Double] = Extraction(math.sqrt(_).toFloat)
  given float3: Extraction[3, Double, Double] = Extraction(math.cbrt(_).toFloat)

trait RootOperator[RootType <: Int & Singleton, -ValueType]:
  type Result
  def root(value: ValueType): Result

class Extraction[RootType <: Int & Singleton, -ValueType, ResultType]
    (lambda: ValueType => ResultType)
extends RootOperator[RootType, ValueType]:

  type Result = ResultType
  def root(value: ValueType): Result = lambda(value)
  
extension [ValueType](value: ValueType)
  def sqrt(using operator: RootOperator[2, ValueType]): operator.Result = operator.root(value)
  def cbrt(using operator: RootOperator[3, ValueType]): operator.Result = operator.root(value)