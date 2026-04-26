                                                                                                  /*
┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓
┃                                                                                                  ┃
┃                                                   ╭───╮                                          ┃
┃                                                   │   │                                          ┃
┃                                                   │   │                                          ┃
┃   ╭───────╮╭─────────╮╭───╮ ╭───╮╭───╮╌────╮╭────╌┤   │╭───╮╌────╮╭────────╮╭───────╮╭───────╮   ┃
┃   │   ╭───╯│   ╭─╮   ││   │ │   ││   ╭─╮   ││   ╭─╮   ││   ╭─╮   ││   ╭─╮  ││   ╭───╯│   ╭───╯   ┃
┃   │   ╰───╮│   │ │   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   ╰─╯  ││   ╰───╮│   ╰───╮   ┃
┃   ╰───╮   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   ╭────╯╰───╮   │╰───╮   │   ┃
┃   ╭───╯   ││   ╰─╯   ││   ╰─╯   ││   │ │   ││   ╰─╯   ││   │ │   ││   ╰────╮╭───╯   │╭───╯   │   ┃
┃   ╰───────╯╰─────────╯╰────╌╰───╯╰───╯ ╰───╯╰────╌╰───╯╰───╯ ╰───╯╰────────╯╰───────╯╰───────╯   ┃
┃                                                                                                  ┃
┃    Soundness, version 0.54.0.                                                                    ┃
┃    © Copyright 2021-25 Jon Pretty, Propensive OÜ.                                                ┃
┃                                                                                                  ┃
┃    The primary distribution site is:                                                             ┃
┃                                                                                                  ┃
┃        https://soundness.dev/                                                                    ┃
┃                                                                                                  ┃
┃    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file     ┃
┃    except in compliance with the License. You may obtain a copy of the License at                ┃
┃                                                                                                  ┃
┃        https://www.apache.org/licenses/LICENSE-2.0                                               ┃
┃                                                                                                  ┃
┃    Unless required by applicable law or agreed to in writing,  software distributed under the    ┃
┃    License is distributed on an "AS IS" BASIS,  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,    ┃
┃    either express or implied. See the License for the specific language governing permissions    ┃
┃    and limitations under the License.                                                            ┃
┃                                                                                                  ┃
┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛
                                                                                                  */
package parasite

import language.experimental.pureFunctions

import java.lang as jl
import java.util as ju
import java.util.concurrent as juc
import java.util.concurrent.atomic as juca
import java.util.concurrent.locks as jucl

import anticipation.*
import contingency.*
import prepositional.*
import proscenium.*
import rudiments.*
import vacuous.*

import unsafeExceptions.canThrowAny

object Promise:
  enum State[+value]:
    case Incomplete(waiting: Set[Thread])
    case Complete(value: value)
    case Cancelled

final case class Promise[value]():
  import Promise.State, State.{Incomplete, Complete, Cancelled}

  private val state: juca.AtomicReference[State[value]] =
    juca.AtomicReference(Incomplete(Set()))

  def cancelled: Boolean = state.get() == Cancelled

  def apply(): Optional[value] = state.get() match
    case Complete(value) => value
    case _               => Unset

  def ready: Boolean = state.get() match
    case Incomplete(_) => false
    case _             => true

  def complete: Boolean = state.get() match
    case Complete(_) => true
    case _           => false

  private def completeIncomplete(supplied: value)(current: State[value] | Null): State[value] =
    current.nn match
      case Incomplete(waiting) => Complete(supplied.nn).also(waiting.each(jucl.LockSupport.unpark))
      case current             => current

  def fulfill(supplied: => value): Unit raises AsyncError =
    state.getAndUpdate(completeIncomplete(supplied)).nn match
      case Cancelled           => raise(AsyncError(AsyncError.Reason.Cancelled))
      case Complete(_)         => raise(AsyncError(AsyncError.Reason.AlreadyComplete))
      case Incomplete(waiting) => ()

  def offer(supplied: => value): Unit = state.updateAndGet(completeIncomplete(supplied))

  private def enqueue(thread: Thread)(current: State[value] | Null): State[value] =
    current.nn match
      case Incomplete(waiting) => Incomplete(waiting + thread)
      case Complete(value)     => Complete(value)
      case _                   => Cancelled

  @tailrec
  def await(): value raises AsyncError =
    if Thread.interrupted() then throw new InterruptedException()
    state.getAndUpdate(enqueue(Thread.currentThread.nn)).nn match
      case Incomplete(_)   => jucl.LockSupport.park(this) yet await()
      case Complete(value) => value
      case Cancelled       => abort(AsyncError(AsyncError.Reason.Cancelled))

  @tailrec
  def attend(): Unit =
    if Thread.interrupted() then throw new InterruptedException()
    state.getAndUpdate(enqueue(Thread.currentThread.nn)) match
      case Incomplete(_) => jucl.LockSupport.park(this) yet attend()
      case _             => ()

  private def cancelIncomplete(current: State[value] | Null): State[value] = current match
    case Incomplete(_) => Cancelled
    case current       => current.nn

  def cancel(): Unit = state.getAndUpdate(cancelIncomplete) match
    case Incomplete(waiting) => waiting.each(jucl.LockSupport.unpark)
    case _                   => ()


  def await[generic: Abstractable across Durations to Long](duration: generic)
  :   value raises AsyncError =

    val deadline = jl.System.nanoTime() + duration.generic

    @tailrec
    def recur(): value =
      if Thread.interrupted() then throw new InterruptedException()
      else if deadline < jl.System.nanoTime then abort(AsyncError(AsyncError.Reason.Timeout))
      else state.getAndUpdate(enqueue(Thread.currentThread.nn)).nn match
        case Incomplete(_) =>
          jucl.LockSupport.parkNanos(this, deadline - jl.System.nanoTime())
                                recur()

        case Complete(value) =>
          value

        case Cancelled =>
          abort(AsyncError(AsyncError.Reason.Cancelled))

    recur()


  def attend[generic: Abstractable across Durations to Long](duration: generic): Unit =
    val deadline = jl.System.nanoTime() + duration.generic

    @tailrec
    def recur(): Unit =
      if Thread.interrupted() then throw new InterruptedException()
      else if deadline > jl.System.nanoTime
      then state.getAndUpdate(enqueue(Thread.currentThread.nn)).nn match
        case Incomplete(_) =>
          jucl.LockSupport.parkNanos(this, deadline - jl.System.nanoTime())
                              recur()

        case Cancelled =>
          ()

        case Complete(_) =>
          ()

    recur()
