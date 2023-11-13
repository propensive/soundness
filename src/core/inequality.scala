/*
    Rudiments, version [unreleased]. Copyright 2023 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package rudiments

import language.experimental.captureChecking

object Inequality:
  inline given numeric: Inequality[Boolean, Int | Double | Char | Byte | Short | Float | Long] with
    inline def compare
        (inline left: Boolean, inline right: Int | Double | Char | Byte | Short | Float | Long,
            inline strict: Boolean, inline greaterThan: Boolean)
        : Boolean =
      ${Rudiments.inequality('left, 'right, 'strict, 'greaterThan)}

trait Inequality[-LeftType, -RightType]:
  inline def compare
      (inline left: LeftType, inline right: RightType, inline strict: Boolean,
          inline greaterThan: Boolean)
      : Boolean

extension [LeftType](inline left: LeftType)
  @targetName("lt")
  inline def <
      [RightType]
      (inline right: RightType)(using inline inequality: Inequality[LeftType, RightType])
      : Boolean =
    inequality.compare(left, right, true, false)
  
  @targetName("lte")
  inline def <=
      [RightType]
      (inline right: RightType)(using inline inequality: Inequality[LeftType, RightType])
      : Boolean =
    inequality.compare(left, right, false, false)
  
  @targetName("gt")
  inline def >
      [RightType]
      (inline right: RightType)(using inline inequality: Inequality[LeftType, RightType])
      : Boolean =
    inequality.compare(left, right, true, true)
  
  @targetName("gte")
  inline def >=
      [RightType]
      (inline right: RightType)(using inline inequality: Inequality[LeftType, RightType])
      : Boolean =
    inequality.compare(left, right, false, true)
  
