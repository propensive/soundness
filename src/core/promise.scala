/*
    Parasite, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package parasite

import anticipation.*
import contingency.*
import vacuous.*

import language.experimental.captureChecking

import AsyncState.*

object Promise:
  sealed trait Special
  case object Cancelled extends Special
  case object Incomplete extends Special

trait Covenant[+ValueType]:
  inline def cancelled: Boolean
  inline def incomplete: Boolean
  inline def ready: Boolean

  def await()(using Raises[CancelError]): ValueType

  def await[DurationType: GenericDuration](duration: DurationType)
      (using Raises[CancelError], Raises[TimeoutError])
          : ValueType

case class Promise[ValueType]() extends Covenant[ValueType]:
  private var value: ValueType | Promise.Special = Promise.Incomplete

  inline def cancelled: Boolean = value == Promise.Cancelled
  inline def incomplete: Boolean = value == Promise.Incomplete
  inline def ready: Boolean = !incomplete

  private def get()(using Raises[CancelError]): ValueType =
    if cancelled then abort(CancelError()) else value.asInstanceOf[ValueType]

  def apply(): Optional[ValueType] = if ready then value.asInstanceOf[ValueType] else Unset

  def fulfill(supplied: -> ValueType)(using complete: Raises[AlreadyCompleteError]): Unit^{complete} =
    synchronized:
      if !incomplete then raise(AlreadyCompleteError())(()) else value = supplied
      notifyAll()
  
  def offer(supplied: -> ValueType): Unit = synchronized:
    if incomplete then
      value = supplied
      notifyAll()

  def await()(using Raises[CancelError]): ValueType = synchronized:
    while !ready do wait()
    get()

  def cancel(): Unit = synchronized:
    if incomplete then
      value = Promise.Cancelled
      notifyAll()

  def await[DurationType: GenericDuration](duration: DurationType)
      (using Raises[CancelError], Raises[TimeoutError])
          : ValueType =
    
    synchronized:
      if ready then get() else
        wait(duration.milliseconds)
        if !ready then abort(TimeoutError()) else get()

case class Trigger():
  private val promise: Promise[Unit] = Promise()
  def apply(): Unit = promise.offer(())
  def pull()(using Raises[AlreadyCompleteError]): Unit = promise.fulfill(())
  def await()(using Raises[CancelError]): Unit = promise.await()
  def cancel(): Unit = promise.cancel()
  def cancelled: Boolean = promise.cancelled
  
  def await[DurationType: GenericDuration](duration: DurationType)
      (using Raises[CancelError], Raises[TimeoutError])
          : Unit =

    promise.await(duration)

