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

class Tracing[SupplementType]():
  private val buffer: scm.ArrayBuffer[Exception] = scm.ArrayBuffer()
  private val supplements: scm.ArrayBuffer[Optional[SupplementType]] = scm.ArrayBuffer()

  def length: Int = buffer.length
  def success: Boolean = length == 0
  def register(error: Exception): Unit =
    buffer.append(error)
    supplements.append(Unset)

  def fold[AccrualType](initial: AccrualType)
      (lambda: (Optional[SupplementType], AccrualType) => PartialFunction[Exception, AccrualType])
          : AccrualType =
    (0 until buffer.length).foldLeft(initial): (accrual, index) =>
      lambda(supplements(index), accrual)(buffer(index))

  def supplement(last: Int, transform: Optional[SupplementType] => SupplementType)
          : Unit =

    for i <- (buffer.length - last) until buffer.length do
      supplements(i) = transform(supplements(i))
