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
┃    Soundness, version 0.64.0.                                                                    ┃
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

  private val state: Atomic[State[value]] =
    Atomic(Incomplete(scala.collection.immutable.Set()))

  def cancelled: Boolean = state() == Cancelled

  def apply(): Optional[value] = state() match
    case Complete(value) => value
    case _               => Unset

  def ready: Boolean = state() match
    case Incomplete(_) => false
    case _             => true

  def complete: Boolean = state() match
    case Complete(_) => true
    case _           => false

  // The promise's completion, reified as a `Task`: the monadic form of `await`, composable with
  // `bind`/`map` without suspending the calling strand.
  def task(using monitor: Monitor^, probate: Probate^, codepoint: Codepoint)
  :   (Task[value] emits Async.Error)^{monitor, probate} =
    async(await())

  // `ere` may re-run the transition under contention, so `supplied` — a by-name — is forced
  // ONCE here rather than inside it. `ere` yields the state THIS call displaced, so the waiters
  // are unparked exactly once, from that state. No wakeup can be lost: once `Complete` is
  // installed, the enqueue transition adds no further waiters. The settled case returns its
  // argument by reference, so a settled promise takes no compare-and-set at all.
  def fulfill(supplied: => value): (Tactic[Async.Error]^) ?->{supplied} Unit =
    val installed: value = supplied

    val displaced = state.ere:
      case Incomplete(_) => Complete(installed)
      case settled       => settled

    displaced match
      case Cancelled           => raise(Async.Error(Async.Error.Reason.Cancelled))
      case Complete(_)         => raise(Async.Error(Async.Error.Reason.AlreadyComplete))
      case Incomplete(waiting) => waiting.each(_.unpark())

  def offer(supplied: => value): Unit =
    val installed: value = supplied

    val displaced = state.ere:
      case Incomplete(_) => Complete(installed)
      case settled       => settled

    displaced match
      case Incomplete(waiting) => waiting.each(_.unpark())
      case _                   => ()

  def await()(using monitor: Monitor^): (Tactic[Async.Error]^) ?->{monitor} value =
    if monitor.supervisor.interrupted() then throw new InterruptedException()

    // A settled promise needs no CAS and no waiter-set allocation — the common case when joining
    // an already-finished task. Nothing is hoisted out of the park loop any more: `ere` takes an
    // inline transition, so the lambda beta-reduces and no operator is allocated per iteration.
    state() match
      case Complete(value) => value
      case Cancelled       => abort(Async.Error(Async.Error.Reason.Cancelled))
      case Incomplete(_)   =>
        val strand0: Strand = monitor.supervisor.strand()

        @tailrec
        def recur(): value =
          if monitor.supervisor.interrupted() then throw new InterruptedException()

          val displaced = state.ere:
            case Incomplete(waiting) => Incomplete(waiting + strand0)
            case settled             => settled

          displaced match
            case Incomplete(_)   => monitor.supervisor.park(this) yet recur()
            case Complete(value) => value
            case Cancelled       => abort(Async.Error(Async.Error.Reason.Cancelled))

        recur()

  def attend()(using monitor: Monitor^): Unit =
    if monitor.supervisor.interrupted() then throw new InterruptedException()

    if !ready then
      val strand0: Strand = monitor.supervisor.strand()

      @tailrec
      def recur(): Unit =
        if monitor.supervisor.interrupted() then throw new InterruptedException()

        val displaced = state.ere:
          case Incomplete(waiting) => Incomplete(waiting + strand0)
          case settled             => settled

        displaced match
          case Incomplete(_) =>
            monitor.supervisor.park(this)
            recur()

          case _ =>
            ()

      recur()

  def cancel(): Unit =
    val displaced = state.ere:
      case Incomplete(_) => Cancelled
      case settled       => settled

    displaced match
      case Incomplete(waiting) => waiting.each(_.unpark())
      case _                   => ()


  def await[generic: Abstractable across Durations to Long](duration: generic)
    ( using monitor: Monitor^ )
  :   (Tactic[Async.Error]^) ?->{monitor} value =

    if monitor.supervisor.interrupted() then throw new InterruptedException()

    state() match
      case Complete(value) => value
      case Cancelled       => abort(Async.Error(Async.Error.Reason.Cancelled))
      case Incomplete(_)   =>
        val deadline = jl.System.nanoTime() + duration.generic
        val strand0: Strand = monitor.supervisor.strand()

        @tailrec
        def recur(): value =
          if monitor.supervisor.interrupted() then throw new InterruptedException()
          else if deadline < jl.System.nanoTime then abort(Async.Error(Async.Error.Reason.Timeout))
          else
            val displaced = state.ere:
              case Incomplete(waiting) => Incomplete(waiting + strand0)
              case settled             => settled

            displaced match
              case Incomplete(_) =>
                monitor.supervisor.park(this, deadline)
                recur()

              case Complete(value) =>
                value

              case Cancelled =>
                abort(Async.Error(Async.Error.Reason.Cancelled))

        recur()


  def attend[generic: Abstractable across Durations to Long](duration: generic)
    ( using monitor: Monitor^ )
  :   Unit =

    if monitor.supervisor.interrupted() then throw new InterruptedException()

    if !ready then
      val deadline = jl.System.nanoTime() + duration.generic
      val strand0: Strand = monitor.supervisor.strand()

      @tailrec
      def recur(): Unit =
        if monitor.supervisor.interrupted() then throw new InterruptedException()
        else if deadline > jl.System.nanoTime
        then
          val displaced = state.ere:
            case Incomplete(waiting) => Incomplete(waiting + strand0)
            case settled             => settled

          displaced match
            case Incomplete(_) =>
              monitor.supervisor.park(this, deadline)
              recur()

            case Cancelled =>
              ()

            case Complete(_) =>
              ()

      recur()
