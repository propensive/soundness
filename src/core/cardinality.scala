/*
    Cardinality, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package cardinality

import compiletime.ops.double.*
import scala.util.FromDigits
import scala.reflect.TypeTest
import annotation.*

import language.experimental.genericNumberLiterals

type Asym[ValueType <: Double, TrueValueType <: Double, FalseValueType <: Double] <: Double =
  (ValueType > 0.0) match
    case true  => TrueValueType
    case false => FalseValueType

type Min4[Value1Type <: Double, Value2Type <: Double, Value3Type <: Double, Value4Type <: Double] =
  Min[Min[Value1Type, Value2Type], Min[Value3Type, Value4Type]]

type Max4[Value1Type <: Double, Value2Type <: Double, Value3Type <: Double, Value4Type <: Double] =
  Max[Max[Value1Type, Value2Type], Max[Value3Type, Value4Type]]

extension (value: Double)
  def force[MinValueType <: Double, MaxValueType <: Double]: MinValueType ~ MaxValueType =
    value.asInstanceOf[MinValueType ~ MaxValueType]

object NumericRange:
  @targetName("Range")
  opaque infix type ~[MinValueType <: Double, MaxValueType <: Double] = Double

  def apply
      [MinValueType <: Double, MaxValueType <: Double]
      (value: Double)
      : MinValueType ~ MaxValueType =
    value

  @targetName("Range")
  object `~`:
    given comparable
        [MinValueType <: Double & Singleton, MaxValueType <: Double & Singleton]
        (using min: ValueOf[MinValueType], max: ValueOf[MaxValueType])
        : TypeTest[Double, MinValueType ~ MaxValueType] =
      value =>
        if value >= min.value && value <= max.value
        then Some(value.asInstanceOf[(MinValueType ~ MaxValueType) & value.type])
        else None

    class RangeParser[MinValueType <: Double, MaxValueType <: Double]
    extends FromDigits.Decimal[MinValueType ~ MaxValueType]:
      def fromDigits(digits: String): Double = apply(digits.toDouble)

    given cardinality[MinValueType <: Double, MaxValueType <: Double]
        : RangeParser[MinValueType, MaxValueType] with
      override inline def fromDigits(digits: String): MinValueType ~ MaxValueType =
        ${Cardinality('digits)}
    
    extension [LeftMinType <: Double, LeftMaxType <: Double](left: LeftMinType ~ LeftMaxType)
      def double: Double = left

      @targetName("add")
      infix def + [RightMinType <: Double, RightMaxType <: Double](right: RightMinType ~ RightMaxType)
          : (LeftMinType + RightMinType) ~ (LeftMaxType + RightMaxType) =
        left + right
      
      @targetName("add2")
      infix def + [E <: Double & Singleton](right: E)
          : (LeftMinType + right.type) ~ (LeftMaxType + right.type) =
        left + right

      @targetName("add3")
      infix def + (right: Double): Double = left + right

      @targetName("times")
      infix def * [RightMinType <: Double, RightMaxType <: Double](right: RightMinType ~ RightMaxType)
          : (Min4[LeftMinType*RightMinType, LeftMinType*RightMaxType, LeftMaxType*RightMaxType,
              LeftMaxType*RightMinType]) ~ (Max4[LeftMinType*RightMinType, LeftMinType*RightMaxType,
              LeftMaxType*RightMaxType, LeftMaxType*RightMinType]) =
        left*right
      
      @targetName("times2")
      infix def * [E <: Double & Singleton](right: E)
          : Min[LeftMinType*E, LeftMaxType*E] ~ Max[LeftMinType*E, LeftMaxType*E] =
        left*right
      
      @targetName("times3")
      infix def * (right: Double): Double = left*right

      @targetName("minus")
      infix def - [RightMinType <: Double, RightMaxType <: Double](right: RightMinType ~ RightMaxType)
          : Min[LeftMinType - RightMinType, LeftMinType - RightMaxType] ~ Max[LeftMaxType -
              RightMinType, LeftMaxType - RightMaxType] =
        left - right

      @targetName("minus2")
      infix def - [E <: Double & Singleton](right: E)
          : Min[LeftMinType - E, LeftMaxType - E] ~ Max[LeftMinType - E, LeftMaxType - E] =
        left - right

      @targetName("minus3")
      infix def - (right: Double): Double = left - right

      @targetName("divide")
      infix def / [E <: Double & Singleton](right: E)
          : Min[LeftMinType/E, LeftMaxType/E] ~ Max[LeftMinType/E, LeftMaxType/E] =
        left/right
      
      @targetName("divide2")
      infix def / [RightMinType <: Double, RightMaxType <: Double](right: RightMinType ~ RightMaxType)
          : Asym[RightMinType*RightMaxType, Min4[LeftMinType/RightMinType, LeftMaxType/RightMinType,
              LeftMinType/RightMaxType, LeftMaxType/RightMaxType], -1.0/0.0] ~ Asym[
              RightMinType*RightMaxType, Max4[LeftMinType/RightMinType, LeftMaxType/RightMinType,
              LeftMinType/RightMaxType, LeftMaxType/RightMaxType], 1.0/0.0] =
        left/right
      
      @targetName("divide3")
      infix def / (right: Double): Double = left/right

export NumericRange.`~`

