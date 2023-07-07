/*
    Anticipation, version [unreleased]. Copyright 2023 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package anticipation

object Add:
  given Add[Int, Int] with
    type Result = Int
    def apply(left: Int, right: Int, subtract: Boolean): Int =
      if subtract then left - right else left + right
  
  given Add[Byte, Byte] with
    type Result = Int
    def apply(left: Byte, right: Byte, subtract: Boolean): Int =
      if subtract then left - right else left + right
  
  given Add[Double, Double] with
    type Result = Double
    def apply(left: Double, right: Double, subtract: Boolean): Double =
      if subtract then left - right else left + right
  
  given Add[Float, Float] with
    type Result = Float
    def apply(left: Float, right: Float, subtract: Boolean): Float =
      if subtract then left - right else left + right
  
  given Add[Short, Short] with
    type Result = Int
    def apply(left: Short, right: Short, subtract: Boolean): Int =
      if subtract then left - right else left + right
  
  given Add[Long, Long] with
    type Result = Long
    def apply(left: Long, right: Long, subtract: Boolean): Long =
      if subtract then left - right else left + right

trait Add[-LeftType, -RightType]:
  type Result
  def apply(left: LeftType, right: RightType, subtract: Boolean): Result

object Multiply:
  given Multiply[Int, Int] with
    type Result = Int
    def apply(left: Int, right: Int): Int = left*right
  
  given Multiply[Byte, Byte] with
    type Result = Int
    def apply(left: Byte, right: Byte): Int = left*right
  
  given Multiply[Double, Double] with
    type Result = Double
    def apply(left: Double, right: Double): Double = left*right
  
  given Multiply[Float, Float] with
    type Result = Float
    def apply(left: Float, right: Float): Float = left*right
  
  given Multiply[Short, Short] with
    type Result = Int
    def apply(left: Short, right: Short): Int = left*right
  
  given Multiply[Long, Long] with
    type Result = Long
    def apply(left: Long, right: Long): Long = left*right

trait Multiply[-LeftType, -RightType]:
  type Result
  def apply(left: LeftType, right: RightType): Result
