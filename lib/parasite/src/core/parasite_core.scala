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

import language.experimental.into
import language.experimental.pureFunctions

import java.lang as jl

import anticipation.*
import contingency.*
import denominative.*
import digression.*
import fulminate.*
import prepositional.*
import symbolism.*
import vacuous.*

import abstractables.durationAbstractable
import abstractables.instantAbstractable

package threading:
  given platformThreading: Threading = () => PlatformSupervisor
  given virtualThreading: Threading = () => VirtualSupervisor
  given adaptiveThreading: Threading = () => AdaptiveSupervisor

package probates:
  given awaitProbate: Probate = _.delegate(_.attend())
  given cancelProbate: Probate = _.delegate(_.cancel())

  given panicProbate: Probate = _.delegate: child =>
    if !child.ready then fulminate.panic(m"asynchronous child task did not complete")

  given failProbate: Tactic[AsyncError] => Probate = _.delegate: child =>
    if !child.ready then raise(AsyncError(AsyncError.Reason.Incomplete))

package supervisors:
  given globalSupervisor: Supervisor = PlatformSupervisor.supervisor

package retryTenacities:
  given exponentialForeverTenacity: Tenacity = Tenacity.exponential(10L, 1.2)
  given exponentialFiveTimesTenacity: Tenacity = Tenacity.exponential(10L, 1.2).limit(5)
  given exponentialTenTimesTenacity: Tenacity = Tenacity.exponential(10L, 1.2).limit(10)
  given fixedNoDelayForeverTenacity: Tenacity = Tenacity.fixed(0L)
  given fixedNoDelayFiveTimesTenacity: Tenacity = Tenacity.fixed(0L).limit(5)
  given fixedNoDelayTenTimesTenacity: Tenacity = Tenacity.fixed(0L).limit(10)

transparent inline def monitor: Monitor = infer[Monitor]

// Like `async`, but fire-and-forget: a daemon is never joined, so a raised error cannot surface at
// a join. The body runs with an `AsyncTactic[error]`; a raised error fails the worker and is routed
// to the nearest `trap` (and escalated if unhandled) rather than tracked in the return type.
def daemon[error <: Exception](using Codepoint)
  ( evaluate: (Worker, Tactic[error]) ?=> Unit )
  ( using Monitor, Probate )
:   Daemon =

  val tactic = AsyncTactic[error]()

  Daemon: worker =>
    evaluate(using worker, tactic)


// Establishes a trap over a region of asynchronous work: `trap { case … => … }.within { … }`. The
// handler maps an escaped error to a `Remedy`; the enclosing `Probate` (the default for unmatched
// or rejected errors) is captured so traps chain outwards to the supervision root.
def trap(handler: PartialFunction[Error, Remedy])(using outer: Probate): Trap = Trap(handler, outer)


// `Task[result] incurs error` = `Task[result] { type Error <: error }`. The bound (not an equality)
// keeps the error covariant: a task that can fail only with `AsyncError` is usable where one that
// `incurs FooError` is expected, and `await`'s `raises (Error | AsyncError)` is dischargeable by a
// `Tactic` for the bound. `task <: Task[?]` keeps the refinement applicable only to actual tasks.
infix type incurs[task <: Task[?], error <: Exception] = task { type Error <: error }


// `error` is the union of error types the body may `raise`, inferred exactly as for synchronous
// `raises`, and carried in the task's `Error` member so it can be delivered, still typed, at the
// join. The body is evaluated with an `AsyncTactic[error]` that records a raised error as the
// worker's `Failed` outcome instead of trying to break a stack-confined `boundary` across threads.
def async[result, error <: Exception](using Codepoint)
  ( evaluate: (Worker, Tactic[error]) ?=> result )
  ( using Monitor, Probate )
:   Task[result] incurs (error | AsyncError) =

  val tactic = AsyncTactic[error]()
  Task[result, error | AsyncError](worker => evaluate(using worker, tactic), name = Unset)


def task[result, error <: Exception](using Codepoint)(name: Text)
  ( evaluate: (Worker, Tactic[error]) ?=> result )
  ( using Monitor, Probate )
:   Task[result] incurs (error | AsyncError) =

  val tactic = AsyncTactic[error]()
  Task[result, error | AsyncError](worker => evaluate(using worker, tactic), name = name)


def relent[result]()(using Worker): Unit = monitor.relent()
def cancel[result]()(using Monitor): Unit = monitor.cancel()


def snooze[duration: Abstractable across Durations to Long](duration: duration)(using Monitor)
:   Unit =

  monitor.snooze(duration)


def delay[generic: Abstractable across Durations to Long](duration: generic)(using Monitor): Unit =
  hibernate(jl.System.currentTimeMillis + duration.generic/1_000_000L)

def sleep[instant: Abstractable across Instants to Long](instant: instant)(using Monitor): Unit =
  monitor.snooze((instant.generic - jl.System.currentTimeMillis)*1_000_000L)


def hibernate[instant: Abstractable across Instants to Long](instant: instant)(using Monitor)
:   Unit =

  while instant.generic > jl.System.currentTimeMillis do sleep(instant.generic)


extension [result](stream: Stream[result])
  def concurrent(using Monitor, Probate): Stream[result] raises AsyncError =
    if async(stream.nil).await() then Stream() else stream.head #:: stream.tail.concurrent


def supervise[result](block: Monitor ?=> result)(using threading: Threading, codepoint: Codepoint)
:   result raises AsyncError =

  block(using threading.supervisor())


def retry[value](evaluate: (surrender: () => Nothing, persevere: () => Nothing) ?=> value)
  ( using Tenacity, Monitor )
:   value raises RetryError =

  @tailrec
  def recur(attempt: Ordinal): value =
    boundary[Perseverance[value]]: label ?=>
      monitor.snooze(summon[Tenacity].delay(attempt).lest(RetryError(attempt.n1)))
      def surrender(): Nothing = boundary.break(Perseverance.Surrender)
      def persevere(): Nothing = boundary.break(Perseverance.Persevere)

      Perseverance.Prevail(evaluate(using surrender, persevere))

    . match
      case Perseverance.Surrender      => abort(RetryError(attempt.n1))
      case Perseverance.Prevail(value) => value
      case Perseverance.Persevere      => recur(attempt + 1)

  recur(Prim)


extension [target](value: target)
  def intercept[event](using interceptable: event is Interceptable onto target)
    ( action: (event: event) ?=> Unit )
  :   Hook =

    Hook(interceptable.register(value, action(using _)))
