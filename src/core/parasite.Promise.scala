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

import language.experimental.pureFunctions

import anticipation.*
import contingency.*
import vacuous.*

import Completion.*

object Promise:
  enum State:
    case Incomplete, Cancelled, Complete

case class Promise[ValueType]():
  private var state: Promise.State = Promise.State.Incomplete
  private var value: Optional[ValueType] = Unset
  private[parasite] def get(): ValueType = value.asInstanceOf[ValueType]

  def cancelled: Boolean = state == Promise.State.Cancelled
  def apply(): Optional[ValueType] = if ready then value else Unset
  def ready: Boolean = state != Promise.State.Incomplete
  def complete: Boolean = state == Promise.State.Complete

  private def set(supplied: ValueType): Unit =
    value = supplied
    state = Promise.State.Complete
    notifyAll()

  def fulfill(supplied: -> ValueType): Unit raises ConcurrencyError = synchronized:
    if ready then raise(ConcurrencyError(ConcurrencyError.Reason.AlreadyComplete), ())
    else set(supplied)

  def offer(supplied: -> ValueType): Unit = synchronized { if !ready then set(supplied) }

  def await(): ValueType = synchronized:
    while !ready do wait()
    get()

  def attend(): Unit = synchronized { while !ready do wait() }

  def cancel(): Unit = synchronized:
    if !ready then
      state = Promise.State.Cancelled
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
