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
  given byte: AddOperator[Byte, Byte] with
    type Result = Byte
    inline def add(left: Byte, right: Byte): Byte = (left + right).toByte

  given short: AddOperator[Short, Short] with
    type Result = Short
    inline def add(left: Short, right: Short): Short = (left + right).toShort

  given int: AddOperator[Int, Int] with
    type Result = Int
    inline def add(left: Int, right: Int): Int = left + right

  given long: AddOperator[Long, Long] with
    type Result = Long
    inline def add(left: Long, right: Long): Long = left + right

  given float: AddOperator[Float, Float] with
    type Result = Float
    inline def add(left: Float, right: Float): Float = left + right

  given double: AddOperator[Double, Double] with
    type Result = Double
    inline def add(left: Double, right: Double): Double = left + right

trait AddOperator[-LeftType, -RightType]:
  type Result
  def add(left: LeftType, right: RightType): Result
  extension (left: LeftType) inline def + (right: RightType): Result = add(left, right)

object SubOperator:
  given byte: SubOperator[Byte, Byte] with
    type Result = Byte
    inline def sub(left: Byte, right: Byte): Byte = (left - right).toByte

  given short: SubOperator[Short, Short] with
    type Result = Short
    inline def sub(left: Short, right: Short): Short = (left - right).toShort

  given int: SubOperator[Int, Int] with
    type Result = Int
    inline def sub(left: Int, right: Int): Int = left - right

  given long: SubOperator[Long, Long] with
    type Result = Long
    inline def sub(left: Long, right: Long): Long = left - right

  given float: SubOperator[Float, Float] with
    type Result = Float
    inline def sub(left: Float, right: Float): Float = left - right

  given double: SubOperator[Double, Double] with
    type Result = Double
    inline def sub(left: Double, right: Double): Double = left - right

trait SubOperator[-LeftType, -RightType]:
  type Result
  def sub(left: LeftType, right: RightType): Result
  extension (left: LeftType) inline def - (right: RightType): Result = sub(left, right)

object MulOperator:
  given byte: MulOperator[Byte, Byte] with
    type Result = Byte
    inline def mul(left: Byte, right: Byte): Byte = (left*right).toByte

  given short: MulOperator[Short, Short] with
    type Result = Short
    inline def mul(left: Short, right: Short): Short = (left*right).toShort

  given int: MulOperator[Int, Int] with
    type Result = Int
    inline def mul(left: Int, right: Int): Int = left*right

  given long: MulOperator[Long, Long] with
    type Result = Long
    inline def mul(left: Long, right: Long): Long = left*right

  given float: MulOperator[Float, Float] with
    type Result = Float
    inline def mul(left: Float, right: Float): Float = left*right

  given double: MulOperator[Double, Double] with
    type Result = Double
    inline def mul(left: Double, right: Double): Double = left*right

trait MulOperator[-LeftType, -RightType]:
  type Result
  def mul(left: LeftType, right: RightType): Result
  extension (left: LeftType) inline def * (right: RightType): Result = mul(left, right)

object DivOperator:
  given byte: DivOperator[Byte, Byte] with
    type Result = Byte
    inline def div(left: Byte, right: Byte): Byte = (left/right).toByte

  given short: DivOperator[Short, Short] with
    type Result = Short
    inline def div(left: Short, right: Short): Short = (left/right).toShort

  given int: DivOperator[Int, Int] with
    type Result = Int
    inline def div(left: Int, right: Int): Int = left/right

  given long: DivOperator[Long, Long] with
    type Result = Long
    inline def div(left: Long, right: Long): Long = left/right

  given float: DivOperator[Float, Float] with
    type Result = Float
    inline def div(left: Float, right: Float): Float = left/right

  given double: DivOperator[Double, Double] with
    type Result = Double
    inline def div(left: Double, right: Double): Double = left/right

trait DivOperator[-LeftType, -RightType]:
  type Result
  def div(left: LeftType, right: RightType): Result
  extension (left: LeftType) inline def / (right: RightType): Result = div(left, right)

object NegOperator:
  given byte: NegOperator[Byte] with
    type Result = Byte
    inline def neg(left: Byte): Byte = (-left).toByte

  given short: NegOperator[Short] with
    type Result = Short
    inline def neg(left: Short): Short = (-left).toShort

  given int: NegOperator[Int] with
    type Result = Int
    inline def neg(left: Int): Int = -left

  given long: NegOperator[Long] with
    type Result = Long
    inline def neg(left: Long): Long = -left

  given float: NegOperator[Float] with
    type Result = Float
    inline def neg(left: Float): Float = -left

  given double: NegOperator[Double] with
    type Result = Double
    inline def neg(left: Double): Double = -left

trait NegOperator[-LeftType]:
  type Result
  def neg(left: LeftType): Result
  extension (left: LeftType) inline def `unary_-`: Result = neg(left)
