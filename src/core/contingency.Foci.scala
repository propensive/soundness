/*
    Contingency, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package contingency

import rudiments.*
import vacuous.*

import scala.collection.mutable as scm

object Foci:
  given [FocusType] => Foci[FocusType] = new:
    def length: Int = 0
    def success: Boolean = false
    def register(error: Exception): Unit = ()

    def fold[AccrualType](initial: AccrualType)
       (lambda: (Optional[FocusType], AccrualType) => PartialFunction[Exception, AccrualType])
            : AccrualType =
      initial

    def supplement(count: Int, transform: Optional[FocusType] => FocusType): Unit = ()

trait Foci[FocusType]:
  def length: Int
  def success: Boolean
  def register(error: Exception): Unit

  def fold[AccrualType](initial: AccrualType)
     (lambda: (Optional[FocusType], AccrualType) => PartialFunction[Exception, AccrualType])
          : AccrualType

  def supplement(count: Int, transform: Optional[FocusType] => FocusType): Unit

class TrackFoci[FocusType]() extends Foci[FocusType]:
  private val errors: scm.ArrayBuffer[Exception] = scm.ArrayBuffer()
  private val focuses: scm.ArrayBuffer[Optional[FocusType]] = scm.ArrayBuffer()

  def length: Int = errors.length
  def success: Boolean = length == 0

  def register(error: Exception): Unit =
    errors.append(error)
    focuses.append(Unset)

  def fold[AccrualType](initial: AccrualType)
     (lambda: (Optional[FocusType], AccrualType) => PartialFunction[Exception, AccrualType])
          : AccrualType =
    (0 until errors.length).foldLeft(initial): (accrual, index) =>
      lambda(focuses(index), accrual)(errors(index))

  def supplement(count: Int, transform: Optional[FocusType] => FocusType): Unit =
    for i <- (errors.length - count) until errors.length
    do focuses(i) = transform(focuses(i))
