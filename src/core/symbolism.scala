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

import scala.annotation.*
import scala.compiletime.*

object Add:
  given ClosedAdd[Int] with
    inline def apply(inline left: Int, inline right: Int): Int = left + right
  
  given ClosedAdd[Byte] with
    inline def apply(inline left: Byte, inline right: Byte): Byte = (left + right).toByte
  
  given ClosedAdd[Double] with
    inline def apply(inline left: Double, inline right: Double): Double = left + right
  
  given ClosedAdd[Float] with
    inline def apply(inline left: Float, inline right: Float): Float = left + right
  
  given ClosedAdd[Short] with
    inline def apply(inline left: Short, inline right: Short): Short = (left + right).toShort
  
  given ClosedAdd[Long] with
    inline def apply(inline left: Long, inline right: Long): Long = left + right

trait Add[-LeftType, -RightType]:
  type Result
  inline def apply(inline left: LeftType, inline right: RightType): Result

trait ClosedAdd[Type] extends Add[Type, Type]:
  type Result = Type

object Negate:
  given Negate[Int] with
    type Result = Int
    inline def apply(value: Int): Int = -value

  given Negate[Long] with
    type Result = Long
    inline def apply(value: Long): Long = -value

  given Negate[Double] with
    type Result = Double
    inline def apply(value: Double): Double = -value

  given Negate[Float] with
    type Result = Float
    inline def apply(value: Float): Float = -value

trait Negate[ValueType]:
  type Result
  inline def apply(value: ValueType): Result

object Multiply:
  given ClosedMultiply[Int] with
    inline def apply(inline left: Int, inline right: Int): Int = left*right
  
  given ClosedMultiply[Byte] with
    inline def apply(inline left: Byte, inline right: Byte): Byte = (left*right).toByte
  
  given ClosedMultiply[Double] with
    inline def apply(inline left: Double, inline right: Double): Double = left*right
  
  given ClosedMultiply[Float] with
    inline def apply(inline left: Float, inline right: Float): Float = left*right
  
  given ClosedMultiply[Short] with
    inline def apply(inline left: Short, inline right: Short): Short = (left*right).toShort
  
  given ClosedMultiply[Long] with
    inline def apply(inline left: Long, inline right: Long): Long = left*right

trait Multiply[-LeftType, -RightType]:
  type Result
  inline def apply(inline left: LeftType, inline right: RightType): Result

trait ClosedMultiply[Type] extends Multiply[Type, Type]:
  type Result = Type

extension [LeftType](inline left: LeftType)
  @targetName("add")
  transparent inline infix def +
      [RightType]
      (inline right: RightType)
      (using inline add: Add[LeftType, RightType])
      : Any =
    add(left, right)
  
  @targetName("subtract")
  transparent inline infix def -
      [RightType]
      (inline right: RightType)(using inline negate: Negate[RightType])
      : Any =
    val negation = negate(right)
    summonInline[Add[LeftType, negation.type]](left, negation)
  
  @targetName("multiply")
  transparent inline infix def *
      [RightType]
      (inline right: RightType)(using inline multiply: Multiply[LeftType, RightType])
      : Any =
    multiply(left, right)

  @targetName("negate")
  transparent inline def unary_-(using inline negate: Negate[LeftType]): Any = negate(left)
