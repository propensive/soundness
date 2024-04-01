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

import Completion.*

object Promise:
  sealed trait Special
  case object Cancelled extends Special
  case object Incomplete extends Special

case class Promise[ValueType]():
  private var value: ValueType | Promise.Special = Promise.Incomplete

  inline def cancelled: Boolean = value == Promise.Cancelled
  inline def incomplete: Boolean = value == Promise.Incomplete
  inline def ready: Boolean = !incomplete

  private def get(): ValueType raises ConcurrencyError =
    if cancelled then abort(ConcurrencyError(ConcurrencyError.Reason.Cancelled))
    else value.asInstanceOf[ValueType]

  def apply(): Optional[ValueType] = if ready then value.asInstanceOf[ValueType] else Unset

  def fulfill(supplied: -> ValueType)(using concurrency: Raises[ConcurrencyError])
          : Unit^{concurrency} =

    synchronized:
      if !incomplete then raise(ConcurrencyError(ConcurrencyError.Reason.AlreadyComplete))(())
      else value = supplied

      notifyAll()
  
  def offer(supplied: -> ValueType): Unit = synchronized:
    if incomplete then
      value = supplied
      notifyAll()

  def await(): ValueType raises ConcurrencyError = synchronized:
    while !ready do wait()
    get()

  def attend(): Unit = synchronized { while !ready do wait() }

  def cancel(): Unit = synchronized:
    if incomplete then
      value = Promise.Cancelled
      notifyAll()

  def await[DurationType: GenericDuration](duration: DurationType)
          : ValueType raises ConcurrencyError =
    
    synchronized:
      if ready then get() else
        wait(duration.milliseconds)
        if ready then get() else abort(ConcurrencyError(ConcurrencyError.Reason.Timeout))

  def attend[DurationType: GenericDuration](duration: DurationType): Unit raises ConcurrencyError =
    synchronized:
      if ready then get() else
        wait(duration.milliseconds)
        if ready then get() else abort(ConcurrencyError(ConcurrencyError.Reason.Timeout))
