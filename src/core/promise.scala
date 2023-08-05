/*
    Parasite, version [unreleased]. Copyright 2023 Jon Pretty, Propensive OÜ.

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
import rudiments.*

import language.experimental.captureChecking

object Promise:
  object Cancelled

case class Promise[ValueType]():
  @volatile
  private var value: Maybe[ValueType] | Promise.Cancelled.type = Unset

  def ready: Boolean = !value.unset

  private def get(): ValueType throws CancelError = value match
    case Promise.Cancelled => throw CancelError()
    case Unset             => ???
    case value: ValueType  => value

  def supply
      (supplied: -> ValueType)
      (using alreadyComplete: CanThrow[AlreadyCompleteError])
      : Unit^{alreadyComplete} =
    
    synchronized:
      if value.unset then
        value = supplied
        notifyAll()
      else throw AlreadyCompleteError()

  def await(): ValueType throws CancelError = synchronized:
    while value.unset do wait()
    get()

  def cancel(): Unit = synchronized:
    value = Promise.Cancelled
    notifyAll()

  def await
        [DurationType](duration: DurationType)(using GenericDuration[DurationType])
        : ValueType throws CancelError | TimeoutError =
    synchronized:
      if ready then get() else
        wait(readDuration(duration))
        if !ready then throw TimeoutError() else get()