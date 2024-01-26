/*
    Hypotenuse, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package hypotenuse

import rudiments.*

import scala.annotation.*

import language.experimental.captureChecking

object NumericallyComparable:
  inline given numeric: Inequality[Boolean, Int | Double | Char | Byte | Short | Float | Long] with
    inline def compare
        (inline left: Boolean, inline right: Int | Double | Char | Byte | Short | Float | Long,
            inline strict: Boolean, inline greaterThan: Boolean)
        : Boolean =
      ${Hypotenuse2.inequality('left, 'right, 'strict, 'greaterThan)}

  given inequality: Inequality[ByteSize, ByteSize] with
    inline def compare
        (inline left: ByteSize, inline right: ByteSize, inline strict: Boolean, inline greaterThan: Boolean)
        : Boolean =
      !strict && left.long == right.long || (left.long < right.long) ^ greaterThan

trait NumericallyComparable

trait CompareLess[-LeftType, -RightType, +ResultType] extends NumericallyComparable:
  inline def lessThan(inline left: LeftType, inline right: RightType): ResultType

trait CompareLessEqual[-LeftType, -RightType, +ResultType] extends NumericallyComparable:
  inline def lessThanOrEqual(inline left: LeftType, inline right: RightType): ResultType

trait CompareGreater[-LeftType, -RightType, +ResultType] extends NumericallyComparable:
  inline def greaterThan(inline left: LeftType, inline right: RightType): ResultType

trait CompareGreaterEqual[-LeftType, -RightType, +ResultType] extends NumericallyComparable:
  inline def greaterThanOrEqual(inline left: LeftType, inline right: RightType): ResultType


trait Inequality[-LeftType, -RightType]
extends CompareGreaterEqual[LeftType, RightType, Boolean], CompareLess[LeftType, RightType, Boolean],
    CompareLessEqual[LeftType, RightType, Boolean], CompareGreater[LeftType, RightType, Boolean]:

  inline def compare
      (inline left: LeftType, inline right: RightType, inline strict: Boolean,
          inline greaterThan: Boolean)
      : Boolean

  inline def lessThan(inline left: LeftType, inline right: RightType): Boolean =
    compare(left, right, true, false)
  
  inline def lessThanOrEqual(inline left: LeftType, inline right: RightType): Boolean =
    compare(left, right, false, false)
  
  inline def greaterThanOrEqual(inline left: LeftType, inline right: RightType): Boolean =
    compare(left, right, false, true)
  
  inline def greaterThan(inline left: LeftType, inline right: RightType): Boolean =
    compare(left, right, true, true)

extension [LeftType](inline left: LeftType)
  @targetName("lt")
  inline infix def < [RightType, ResultType](inline right: RightType)
      (using inline compareLess: CompareLess[LeftType, RightType, ResultType])
      : ResultType =
    compareLess.lessThan(left, right)
  
  @targetName("lte")
  inline infix def <= [RightType, ResultType](inline right: RightType)
      (using inline compareLessEqual: CompareLessEqual[LeftType, RightType, ResultType])
      : ResultType =
    compareLessEqual.lessThanOrEqual(left, right)
  
  @targetName("gt")
  inline def > [RightType, ResultType](inline right: RightType)
      (using inline compareGreater: CompareGreater[LeftType, RightType, ResultType])
      : ResultType =
    compareGreater.greaterThan(left, right)
  
  @targetName("gte")
  inline def >= [RightType, ResultType](inline right: RightType)
      (using inline compareGreaterEqual: CompareGreaterEqual[LeftType, RightType, ResultType])
      : ResultType =
    compareGreaterEqual.greaterThanOrEqual(left, right)
  
