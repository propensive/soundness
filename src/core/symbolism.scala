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

infix type into[TypeclassType, ResultType] = TypeclassType { type Result = ResultType }

object Addable:
  class Basic[AugendType, AddendType, ResultType]
      (lambda: (AugendType, AddendType) => ResultType)
  extends Addable[AddendType]:
    type Self = AugendType
    type Result = ResultType

    def add(augend: AugendType, addend: AddendType): ResultType = lambda(augend, addend)

  given Double is Addable[Double] into Double as double = _ + _
  given Float is Addable[Float] into Float as float = _ + _
  given Long is Addable[Long] into Long as long = _ + _
  given Int is Addable[Int] into Int as int = _ + _
  given Short is Addable[Short] into Short as short = (augend, addend) => (augend + addend).toShort
  given Byte is Addable[Byte] into Byte as byte = (augend, addend) => (augend + addend).toByte

trait Addable[-AddendType]:
  type Self
  type Augend = Self
  type Result
  def add(augend: Augend, addend: AddendType): Result

  extension (augend: Augend)
    @targetName("add")
    inline infix def + (addend: AddendType): Result = add(augend, addend)

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

object Multiplicable:
  class Basic[MultiplicandType, MultiplierType, ResultType]
      (lambda: (MultiplicandType, MultiplierType) => ResultType)
  extends Multiplicable[MultiplierType]:
    type Self = MultiplicandType
    type Result = ResultType

    def multiply(multiplicand: MultiplicandType, multiplier: MultiplierType): ResultType =
      lambda(multiplicand, multiplier)

  given Double is Multiplicable[Double] into Double as double = _*_
  given Float is Multiplicable[Float] into Float as float = _*_
  given Long is Multiplicable[Long] into Long as long = _*_
  given Int is Multiplicable[Int] into Int as int = _*_

  given Short is Multiplicable[Short] into Short as short =
    (multiplicand, multiplier) => (multiplicand*multiplier).toShort

  given Byte is Multiplicable[Byte] into Byte as byte =
    (multiplicand, multiplier) => (multiplicand*multiplier).toByte

trait Multiplicable[-MultiplierType]:
  type Self
  type Multiplicand = Self
  type Result

  def multiply(multiplicand: Multiplicand, multiplier: MultiplierType): Result

  extension (multiplicand: Multiplicand)
    @targetName("multiply")
    inline infix def * (multiplier: MultiplierType): Result = multiply(multiplicand, multiplier)

object Divisible:
  class Basic[DividendType, DivisorType, ResultType](lambda: (DividendType, DivisorType) => ResultType)
  extends Divisible[DivisorType]:
    type Self = DividendType
    type Result = ResultType

    def divide(dividend: DividendType, divisor: DivisorType): ResultType = lambda(dividend, divisor)

  given Double is Divisible[Double] into Double as double = _/_
  given Float is Divisible[Float] into Float as float = _/_
  given Long is Divisible[Long] into Long as long = _/_
  given Int is Divisible[Int] into Int as int = _/_

  given Short is Divisible[Short] into Short as short =
    (dividend, divisor) => (dividend/divisor).toShort

  given Byte is Divisible[Byte] into Byte as byte =
    (dividend, divisor) => (dividend/divisor).toByte

trait Divisible[-DivisorType]:
  type Self
  type Result
  def divide(dividend: Self, divisor: DivisorType): Result

  extension (dividend: Self)
    @targetName("divide")
    inline infix def / (divisor: DivisorType): Result = divide(dividend, divisor)

object Negatable:
  given Double is Negatable into Double as double = -_
  given Float is Negatable into Float as float = -_
  given Long is Negatable into Long as long = -_
  given Int is Negatable into Int as int = -_
  given Short is Negatable into Short as short = operand => (-operand).toShort
  given Byte is Negatable into Byte as byte = operand => (-operand).toByte

trait Negatable:
  type Self
  type Operand = Self
  type Result
  def negate(operand: Operand): Result

  extension (operand: Operand)
    @targetName("divide")
    inline def `unary_-`: Result = negate(operand)

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
