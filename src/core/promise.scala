/*
    Parasitism, version [unreleased]. Copyright 2023 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package parasitism

import anticipation.*

case class Promise[ValueType]():
  @volatile
  private var value: Option[ValueType throws CancelError] = None

  def ready: Boolean = !value.isEmpty

  def supply(suppliedValue: => ValueType): Unit throws AlreadyCompleteError = synchronized:
    if value.isEmpty then
      value = Some(suppliedValue)
      notifyAll()
    else throw AlreadyCompleteError()

  def await(): ValueType throws CancelError = synchronized:
    while value.isEmpty do wait()
    value.get

  def cancel(): Unit = synchronized:
    value = Some(throw CancelError())
    notifyAll()

  def await
        [DurationType](duration: DurationType)(using GenericDuration[DurationType])
        : ValueType throws CancelError | TimeoutError =
    synchronized:
      if ready then value.get else
        wait(readDuration(duration))
        if !ready then throw TimeoutError() else value.get