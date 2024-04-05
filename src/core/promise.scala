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
import rudiments.*
import vacuous.*

import language.experimental.pureFunctions

import Completion.*

object Promise:
  enum State:
    case Incomplete, Cancelled, Complete

case class Promise[ValueType]():
  private var state: Promise.State = Promise.State.Incomplete
  private var value: ValueType | Null = null
  private var callbacks: List[() => Unit] = Nil

  def cancelled: Boolean = state == Promise.State.Cancelled

  def onComplete(block: => Unit): Unit =
    val immediate = synchronized:
      if ready then true else
        callbacks = (() => block) :: callbacks
        false
    
    if immediate then block

  private[parasite] def get(): ValueType raises ConcurrencyError =
    if cancelled then abort(ConcurrencyError(ConcurrencyError.Reason.Cancelled))
    else value.asInstanceOf[ValueType]

  def apply(): Optional[ValueType] = if ready then value.nn else Unset
  def ready: Boolean = state != Promise.State.Incomplete
  def complete: Boolean = state == Promise.State.Complete

  private def set(supplied: ValueType): List[() => Unit] =
    value = supplied
    state = Promise.State.Complete
    notifyAll() // FIXME: Check whether notify should be before or after callbacks
    callbacks

  def fulfill(supplied: -> ValueType): Unit raises ConcurrencyError = synchronized:
    if ready then raise(ConcurrencyError(ConcurrencyError.Reason.AlreadyComplete))(())
    else set(supplied).each(_())
  
  def offer(supplied: -> ValueType): Unit = synchronized:
    if !ready then set(supplied).each(_())

  def await(): ValueType raises ConcurrencyError = synchronized:
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
