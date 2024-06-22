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

import language.experimental.captureChecking

import scala.annotation.*

trait Orderable[-LeftType, -RightType]
extends
    CompareGreaterEqual[LeftType, RightType, Boolean],
    CompareLess[LeftType, RightType, Boolean],
    CompareLessEqual[LeftType, RightType, Boolean],
    CompareGreater[LeftType, RightType, Boolean]:

  inline def compare
      (inline left: LeftType, inline right: RightType, inline strict: Boolean, inline greaterThan: Boolean)
          : Boolean

  inline def lessThan(inline left: LeftType, inline right: RightType): Boolean =
    compare(left, right, true, false)

  inline def lessThanOrEqual(inline left: LeftType, inline right: RightType): Boolean =
    compare(left, right, false, false)

  inline def greaterThanOrEqual(inline left: LeftType, inline right: RightType): Boolean =
    compare(left, right, false, true)

  inline def greaterThan(inline left: LeftType, inline right: RightType): Boolean =
    compare(left, right, true, true)
