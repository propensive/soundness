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

object Plus:
  given Plus[Int, Int] with
    type Result = Int
    inline def apply(inline left: Int, inline right: Int): Int = left + right
  
  given Plus[Byte, Byte] with
    type Result = Int
    inline def apply(inline left: Byte, inline right: Byte): Int = left + right
  
  given Plus[Double, Double] with
    type Result = Double
    inline def apply(inline left: Double, inline right: Double): Double = left + right
  
  given Plus[Float, Float] with
    type Result = Float
    inline def apply(inline left: Float, inline right: Float): Float = left + right
  
  given Plus[Short, Short] with
    type Result = Int
    inline def apply(inline left: Short, inline right: Short): Int = left + right
  
  given Plus[Long, Long] with
    type Result = Long
    inline def apply(inline left: Long, inline right: Long): Long = left + right

trait Plus[-LeftType, -RightType]:
  type Result
  inline def apply(inline left: LeftType, inline right: RightType): Result

object Minus:
  given Minus[Int, Int] with
    type Result = Int
    inline def apply(inline left: Int, inline right: Int): Int = left - right
  
  given Minus[Byte, Byte] with
    type Result = Int
    inline def apply(inline left: Byte, inline right: Byte): Int = left - right
  
  given Minus[Double, Double] with
    type Result = Double
    inline def apply(inline left: Double, inline right: Double): Double = left - right
  
  given Minus[Float, Float] with
    type Result = Float
    inline def apply(inline left: Float, inline right: Float): Float = left - right
  
  given Minus[Short, Short] with
    type Result = Int
    inline def apply(inline left: Short, inline right: Short): Int = left - right
  
  given Minus[Long, Long] with
    type Result = Long
    inline def apply(inline left: Long, inline right: Long): Long = left - right

trait Minus[-LeftType, -RightType]:
  type Result
  inline def apply(inline left: LeftType, inline right: RightType): Result

object UnaryMinus:
  given UnaryMinus[Int] with
    type Result = Int
    inline def apply(value: Int): Int = -value

  given UnaryMinus[Long] with
    type Result = Long
    inline def apply(value: Long): Long = -value

  given UnaryMinus[Double] with
    type Result = Double
    inline def apply(value: Double): Double = -value

  given UnaryMinus[Float] with
    type Result = Float
    inline def apply(value: Float): Float = -value

trait UnaryMinus[ValueType]:
  type Result
  inline def apply(value: ValueType): Result

object Star:
  given Star[Int, Int] with
    type Result = Int
    inline def apply(inline left: Int, inline right: Int): Int = left*right
  
  given Star[Byte, Byte] with
    type Result = Int
    inline def apply(inline left: Byte, inline right: Byte): Int = left*right
  
  given Star[Double, Double] with
    type Result = Double
    inline def apply(inline left: Double, inline right: Double): Double = left*right
  
  given Star[Float, Float] with
    type Result = Float
    inline def apply(inline left: Float, inline right: Float): Float = left*right
  
  given Star[Short, Short] with
    type Result = Int
    inline def apply(inline left: Short, inline right: Short): Int = left*right
  
  given Star[Long, Long] with
    type Result = Long
    inline def apply(inline left: Long, inline right: Long): Long = left*right

trait Star[-LeftType, -RightType]:
  type Result
  inline def apply(inline left: LeftType, inline right: RightType): Result

object Slash:
  given Slash[Int, Int] with
    type Result = Int
    inline def apply(inline left: Int, inline right: Int): Int = left/right
  
  given Slash[Byte, Byte] with
    type Result = Int
    inline def apply(inline left: Byte, inline right: Byte): Int = left/right
  
  given Slash[Double, Double] with
    type Result = Double
    inline def apply(inline left: Double, inline right: Double): Double = left/right
  
  given Slash[Float, Float] with
    type Result = Float
    inline def apply(inline left: Float, inline right: Float): Float = left/right
  
  given Slash[Short, Short] with
    type Result = Int
    inline def apply(inline left: Short, inline right: Short): Int = left/right
  
  given Slash[Long, Long] with
    type Result = Long
    inline def apply(inline left: Long, inline right: Long): Long = left/right

trait Slash[-LeftType, -RightType]:
  type Result
  inline def apply(inline left: LeftType, inline right: RightType): Result

extension [LeftType](inline left: LeftType)
  @targetName("plus")
  transparent inline infix def +
      [RightType]
      (inline right: RightType)
      (using inline plus: Plus[LeftType, RightType])
      : Any =
    plus(left, right)
  
  @targetName("minus")
  transparent inline infix def +
      [RightType]
      (inline right: RightType)
      (using inline minus: Minus[LeftType, RightType])
      : Any =
    minus(left, right)
  
  
  @targetName("star")
  transparent inline infix def *
      [RightType]
      (inline right: RightType)(using inline star: Star[LeftType, RightType])
      : Any =
    star(left, right)
  
  @targetName("slash")
  transparent inline infix def /
      [RightType]
      (inline right: RightType)(using inline slash: Slash[LeftType, RightType])
      : Any =
    slash(left, right)

  @targetName("unaryMinus")
  transparent inline def unary_-(using inline unaryMinus: UnaryMinus[LeftType]): Any =
    unaryMinus(left)
