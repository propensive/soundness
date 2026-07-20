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
┃    Soundness, version 0.63.0.                                                                    ┃
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

import scala.language.experimental.pureFunctions

import java.lang as jl
import java.util.concurrent.atomic as juca
import java.util.function as juf

import anticipation.*
import contingency.*
import digression.*
import prepositional.*
import rudiments.*
import vacuous.*

import unsafeExceptions.canThrowAny

object Promise:
  enum State[+value]:
    case Incomplete(waiting: scala.collection.immutable.Set[Strand])
    case Complete(value: value)
    case Cancelled

// A plain class, not a case class: a zero-field case class would make every promise `==` every
// other, and promises are meaningful only by identity (`Supervisor.park` blocks on `this`).
final class Promise[value]():
  import Promise.State, State.{Incomplete, Complete, Cancelled}

  private val state: juca.AtomicReference[State[value]] =
    juca.AtomicReference(Incomplete(scala.collection.immutable.Set()))

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

  // The promise's completion, reified as a `Task`: the monadic form of `await`, composable with
  // `bind`/`map` without suspending the calling strand.
  def task(using Monitor^, Probate^, Codepoint): (Task[value] emits AsyncError)^ = async(await())

  // The transition is pure — `getAndUpdate` may re-run it under contention — so the waiters are
  // unparked exactly once afterwards, from the returned previous state. No wakeup can be lost:
  // once `Complete` is installed, `enqueue` adds no further waiters.
  private def completeIncomplete(supplied: value)(current: State[value] | Null): State[value] =
    current.nn match
      case Incomplete(_) => Complete(supplied.nn)
      case current       => current

  def fulfill(supplied: => value): Unit raises AsyncError =
    state.getAndUpdate(completeIncomplete(supplied)).nn match
      case Cancelled           => raise(AsyncError(AsyncError.Reason.Cancelled))
      case Complete(_)         => raise(AsyncError(AsyncError.Reason.AlreadyComplete))
      case Incomplete(waiting) => waiting.each(_.unpark())

  def offer(supplied: => value): Unit =
    state.getAndUpdate(completeIncomplete(supplied)).nn match
      case Incomplete(waiting) => waiting.each(_.unpark())
      case _                   => ()

  private def enqueue(strand: Strand)(current: State[value] | Null): State[value] =
    current.nn match
      case Incomplete(waiting) => Incomplete(waiting + strand)
      case Complete(value)     => Complete(value)
      case _                   => Cancelled

  def await()(using monitor: Monitor^): value raises AsyncError =
    if monitor.supervisor.interrupted() then throw new InterruptedException()

    // A settled promise needs no CAS and no waiter-set allocation — the common case when joining
    // an already-finished task. The enqueue operator is allocated once per call, outside the
    // park loop.
    state.get().nn match
      case Complete(value) => value
      case Cancelled       => abort(AsyncError(AsyncError.Reason.Cancelled))
      case Incomplete(_)   =>
        val strand0: Strand = monitor.supervisor.strand()
        val enqueue0: juf.UnaryOperator[State[value]] = enqueue(strand0)(_)

        @tailrec
        def recur(): value =
          if monitor.supervisor.interrupted() then throw new InterruptedException()

          state.getAndUpdate(enqueue0).nn match
            case Incomplete(_)   => monitor.supervisor.park(this) yet recur()
            case Complete(value) => value
            case Cancelled       => abort(AsyncError(AsyncError.Reason.Cancelled))

        recur()

  def attend()(using monitor: Monitor^): Unit =
    if monitor.supervisor.interrupted() then throw new InterruptedException()

    if !ready then
      val strand0: Strand = monitor.supervisor.strand()
      val enqueue0: juf.UnaryOperator[State[value]] = enqueue(strand0)(_)

      @tailrec
      def recur(): Unit =
        if monitor.supervisor.interrupted() then throw new InterruptedException()

        state.getAndUpdate(enqueue0) match
          case Incomplete(_) =>
            monitor.supervisor.park(this)
            recur()

          case _ =>
            ()

      recur()

  private def cancelIncomplete(current: State[value] | Null): State[value] = current match
    case Incomplete(_) => Cancelled
    case current       => current.nn

  def cancel(): Unit = state.getAndUpdate(cancelIncomplete) match
    case Incomplete(waiting) => waiting.each(_.unpark())
    case _                   => ()


  def await[generic: Abstractable across Durations to Long](duration: generic)
    ( using monitor: Monitor^ )
  :   value raises AsyncError =

    if monitor.supervisor.interrupted() then throw new InterruptedException()

    state.get().nn match
      case Complete(value) => value
      case Cancelled       => abort(AsyncError(AsyncError.Reason.Cancelled))
      case Incomplete(_)   =>
        val deadline = jl.System.nanoTime() + duration.generic
        val strand0: Strand = monitor.supervisor.strand()
        val enqueue0: juf.UnaryOperator[State[value]] = enqueue(strand0)(_)

        @tailrec
        def recur(): value =
          if monitor.supervisor.interrupted() then throw new InterruptedException()
          else if deadline < jl.System.nanoTime then abort(AsyncError(AsyncError.Reason.Timeout))
          else state.getAndUpdate(enqueue0).nn match
            case Incomplete(_) =>
              monitor.supervisor.park(this, deadline)
              recur()

            case Complete(value) =>
              value

            case Cancelled =>
              abort(AsyncError(AsyncError.Reason.Cancelled))

        recur()


  def attend[generic: Abstractable across Durations to Long](duration: generic)
    ( using monitor: Monitor^ )
  :   Unit =

    if monitor.supervisor.interrupted() then throw new InterruptedException()

    if !ready then
      val deadline = jl.System.nanoTime() + duration.generic
      val strand0: Strand = monitor.supervisor.strand()
      val enqueue0: juf.UnaryOperator[State[value]] = enqueue(strand0)(_)

      @tailrec
      def recur(): Unit =
        if monitor.supervisor.interrupted() then throw new InterruptedException()
        else if deadline > jl.System.nanoTime
        then state.getAndUpdate(enqueue0).nn match
          case Incomplete(_) =>
            monitor.supervisor.park(this, deadline)
            recur()

          case Cancelled =>
            ()

          case Complete(_) =>
            ()

      recur()
