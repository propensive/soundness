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
import nomenclature.*
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

  given failProbate: Tactic[AsyncError] => (Probate^) = _.delegate: child =>
    if !child.ready then raise(AsyncError(AsyncError.Reason.Incomplete))

package supervisors:
  given globalSupervisor: Supervisor = PlatformSupervisor

package retryTenacities:
  given exponentialForeverTenacity: Tenacity = Tenacity.exponential(10L, 1.2)
  given exponentialFiveTimesTenacity: Tenacity = Tenacity.exponential(10L, 1.2).limit(5)
  given exponentialTenTimesTenacity: Tenacity = Tenacity.exponential(10L, 1.2).limit(10)
  given fixedNoDelayForeverTenacity: Tenacity = Tenacity.fixed(0L)
  given fixedNoDelayFiveTimesTenacity: Tenacity = Tenacity.fixed(0L).limit(5)
  given fixedNoDelayTenTimesTenacity: Tenacity = Tenacity.fixed(0L).limit(10)

transparent inline def monitor: Monitor = infer[Monitor]

// Like `async`, but fire-and-forget: a daemon is never joined, so an error cannot surface at a join.
// The body runs on a worker thread that outlives the call, so — unlike `async`, which is awaited
// within the capturing scope — it must be *hygienic*: it may not capture any capability from an
// enclosing scope (a `boundary.Label`, a `using`-block file handle, …) whose lifetime could end
// before the worker. This is expressed by the pure context-function arrow `?->{}`: capturing a plain
// value is fine, and the body may freely *open and own* its own capabilities, but it may not close
// over an outer one. So, like `async`, the daemon supplies its own label-free `AsyncTactic` rather
// than letting the body capture an ambient `Emit`; a raised error fails the worker and reaches the
// nearest `contain`/`Probate`.
def daemon[error <: Exception](using Codepoint)
  ( evaluate: (Worker, Tactic[error]) ?->{} Unit )
  ( using Monitor )
:   Daemon =

  val tactic = AsyncTactic[error]()
  Daemon: worker =>
    evaluate(using worker, tactic)


// Contains *thrown* exceptions escaping a region of fire-and-forget work:
// `contain { case … => … }.protect { … }`. The handler maps an escaped exception to a `Remedy`; the
// containment is a child supervision scope of the enclosing `Monitor`, so unmatched or rejected
// errors chain outwards to the parent scope's probate, up to the supervision root. Distinct from the
// typed `trap` (declared emitted errors).
def contain(handler: PartialFunction[Error, Remedy])(using parent: Monitor): Containment^{parent} =
  Containment(handler, parent)


// `X emits error` is the one concept "X can produce these errors as an out-of-band side-channel",
// reified two ways. For a `Task`, it is the bound `Task[result] { type Error <: e }` on its member;
// the bound (not an equality) keeps the error covariant, so a task failing only with `AsyncError`
// is usable where `emits FooError` is expected, and `await`'s `raises (Error | AsyncError)` simply
// converts the emission to a value-replacing exit in the caller's scope. For anything else it is
// the side-effect obligation `Emit[error] ?=> X` — the weaker sibling of `raises error`
// (`Tactic[error] ?=>`). The `Task` branch reduces to a refinement, not a context function, so it
// avoids the match-type-in-return-position erasure crash documented on `raising`.
infix type emits[left, error <: Exception] = left match
  case Task[?] => left { type Error <: error }
  case _       => Emit[error] ?=> left


// `error` is the union of error types the body may `raise`, inferred exactly as for synchronous
// `raises`, and carried in the task's `Error` member so it can be delivered, still typed, at the
// join. The body is evaluated with an `AsyncTactic[error]` that records a raised error as the
// worker's `Failed` outcome instead of trying to break a stack-confined `boundary` across threads.
def async[result, error <: Exception](using Codepoint)
  ( evaluate: (Worker, Tactic[error]) ?=> result )
  ( using monitor: Monitor )
:   (Task[result] emits (error | AsyncError))^{monitor} =

  val tactic = AsyncTactic[error]()
  Task[result, error | AsyncError](worker => evaluate(using worker, tactic), name = Unset)


def task[result, error <: Exception](using Codepoint)(name: Name[Async])
  ( evaluate: (Worker, Tactic[error]) ?=> result )
  ( using monitor: Monitor )
:   (Task[result] emits (error | AsyncError))^{monitor} =

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
  def concurrent(using Monitor): Stream[result] raises AsyncError =
    if async(stream.nil).await() then Stream() else stream.head #:: stream.tail.concurrent


def supervise[result](block: Monitor ?=> result)
  ( using threading: Threading, probate: Probate, codepoint: Codepoint )
:   result raises AsyncError =

  block(using Root(threading.supervisor(), probate))


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
  :   Hook^ =

    Hook(interceptable.register(value, action(using _)))
