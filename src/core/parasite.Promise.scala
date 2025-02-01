/*
    Parasite, version 0.26.0. Copyright 2025 Jon Pretty, Propensive OÜ.

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
import proscenium.*
import rudiments.*
import vacuous.*

import java.util as ju
import ju.concurrent as juc
import juc.atomic as juca
import juc.locks as jucl

import Completion.*

object Promise:
  enum State[+ValueType]:
    case Incomplete(waiting: Set[Thread])
    case Complete(value: ValueType)
    case Cancelled

final case class Promise[ValueType]():
  import Promise.State, State.{Incomplete, Complete, Cancelled}

  private val state: juca.AtomicReference[State[ValueType]] =
    juca.AtomicReference(Incomplete(Set()))

  def cancelled: Boolean = state.get() == Cancelled

  def apply(): Optional[ValueType] = state.get() match
     case Complete(value) => value
     case _               => Unset

  def ready: Boolean = state.get() match
    case Incomplete(_) => false
    case _             => true

  def complete: Boolean = state.get() match
    case Complete(_) => true
    case _           => false

  private def completeIncomplete(supplied: ValueType)(current: State[ValueType] | Null)
  :     State[ValueType] =
    current.nn match
      case Incomplete(waiting) => Complete(supplied.nn).also(waiting.each(jucl.LockSupport.unpark))
      case current             => current

  def fulfill(supplied: => ValueType): Unit raises AsyncError =
    state.updateAndGet(completeIncomplete(supplied)).nn match
      case Cancelled           => raise(AsyncError(AsyncError.Reason.Cancelled))
      case Complete(_)         => raise(AsyncError(AsyncError.Reason.AlreadyComplete))
      case Incomplete(waiting) => ()

  def offer(supplied: => ValueType): Unit = state.updateAndGet(completeIncomplete(supplied))

  private def enqueue(thread: Thread)(current: State[ValueType] | Null): State[ValueType] =
    current.nn match
      case Incomplete(waiting) => Incomplete(waiting + thread)
      case Complete(value)     => Complete(value)
      case _                   => Cancelled

  @tailrec
  def await(): ValueType raises AsyncError =
    state.getAndUpdate(enqueue(Thread.currentThread.nn)).nn match
      case Incomplete(_)   => jucl.LockSupport.park(this) yet await()
      case Complete(value) => value
      case Cancelled       => abort(AsyncError(AsyncError.Reason.Cancelled))

  @tailrec
  def attend(): Unit = state.getAndUpdate(enqueue(Thread.currentThread.nn)) match
    case Incomplete(_) => jucl.LockSupport.park(this) yet attend()
    case _             => ()

  private def cancelIncomplete(current: State[ValueType] | Null): State[ValueType] = current match
    case Incomplete(_) => Cancelled
    case current       => current.nn

  def cancel(): Unit = state.getAndUpdate(cancelIncomplete) match
    case Incomplete(waiting) => waiting.each(jucl.LockSupport.unpark)
    case _                   => ()

  def await[DurationType: GenericDuration](duration: DurationType)
  :     ValueType raises AsyncError =
    val deadline = System.nanoTime() + DurationType.milliseconds(duration)*1000000L

    @tailrec
    def recur(): ValueType =
      if deadline < System.nanoTime then abort(AsyncError(AsyncError.Reason.Timeout))
      else state.getAndUpdate(enqueue(Thread.currentThread.nn)).nn match
        case Incomplete(_)   => jucl.LockSupport.parkUntil(this, deadline - System.nanoTime())
                                recur()
        case Complete(value) => value
        case Cancelled       => abort(AsyncError(AsyncError.Reason.Cancelled))

    recur()

  def attend[DurationType: GenericDuration](duration: DurationType): Unit =
    val deadline = System.nanoTime() + DurationType.milliseconds(duration)*1000000L

    @tailrec
    def recur(): Unit =
      if deadline > System.nanoTime
      then state.getAndUpdate(enqueue(Thread.currentThread.nn)).nn match
        case Incomplete(_) => jucl.LockSupport.parkUntil(this, deadline - System.nanoTime())
                              recur()
        case Cancelled     => ()
        case Complete(_)   => ()

    recur()
