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

import language.experimental.into
import language.experimental.pureFunctions

import scala.compiletime.*

import anticipation.*
import contingency.*
import denominative.*
import digression.*
import rudiments.*
import fulminate.*
import vacuous.*

package threadModels:
  given platform: ThreadModel = () => PlatformSupervisor
  given virtual: ThreadModel = () => VirtualSupervisor
  given adaptive: ThreadModel = () => AdaptiveSupervisor

package asyncTermination:
  given await: Codicil = _.delegate(_.attend())
  given cancel: Codicil = _.delegate(_.cancel())

  given panic: Codicil = _.delegate: child =>
    if !child.ready then fulminate.panic(m"asynchronous child task did not complete")

  given fail: Codicil raises AsyncError = _.delegate: child =>
    if !child.ready then raise(AsyncError(AsyncError.Reason.Incomplete))

package supervisors:
  given global: Supervisor = PlatformSupervisor.supervisor

package retryTenacities:
  given exponentialForever: Tenacity = Tenacity.exponential(10L, 1.2)
  given exponentialFiveTimes: Tenacity = Tenacity.exponential(10L, 1.2).limit(5)
  given exponentialTenTimes: Tenacity = Tenacity.exponential(10L, 1.2).limit(10)
  given fixedNoDelayForever: Tenacity = Tenacity.fixed(0L)
  given fixedNoDelayFiveTimes: Tenacity = Tenacity.fixed(0L).limit(5)
  given fixedNoDelayTenTimes: Tenacity = Tenacity.fixed(0L).limit(10)

transparent inline def monitor(using Monitor): Monitor = summonInline[Monitor]

def daemon(using Codepoint)(evaluate: Worker ?=> Unit)(using Monitor, Codicil): Daemon =
  Daemon(evaluate(using _))

def async[ResultType](using Codepoint)(evaluate: Worker ?=> ResultType)(using Monitor, Codicil)
        : Task[ResultType] =

  Task(evaluate(using _), daemon = false, name = Unset)

def task[ResultType](using Codepoint)(name: into Text)(evaluate: Worker ?=> ResultType)
   (using Monitor, Codicil)
        : Task[ResultType] =

  Task(evaluate(using _), daemon = false, name = name)

def trap(lambda: Throwable ~> Transgression)(using monitor: Monitor): Trap = Trap(lambda, monitor)
def relent[ResultType]()(using Worker): Unit = monitor.relent()
def cancel[ResultType]()(using Monitor): Unit = monitor.cancel()

def snooze[DurationType: GenericDuration](duration: DurationType)(using Monitor): Unit =
  monitor.snooze(duration)

def delay[DurationType: GenericDuration](duration: DurationType)(using Monitor): Unit =
  hibernate(System.currentTimeMillis + duration.milliseconds)

def sleep[InstantType: GenericInstant](instant: InstantType)(using Monitor): Unit =
  monitor.snooze(instant.millisecondsSinceEpoch - System.currentTimeMillis)

def hibernate[InstantType: GenericInstant](instant: InstantType)(using Monitor): Unit =
  while instant.millisecondsSinceEpoch > System.currentTimeMillis
  do sleep(instant.millisecondsSinceEpoch)

extension [ResultType](tasks: Seq[Task[ResultType]])
  def sequence(using Monitor, Codicil): Task[Seq[ResultType]] raises AsyncError =
    async(tasks.map(_.await()))

extension [ResultType](tasks: Iterable[Task[ResultType]])
  def race()(using Monitor, Codicil): ResultType raises AsyncError =
    val promise: Promise[ResultType] = Promise()
    tasks.each(_.map(promise.offer(_)))

    promise.await()

extension [ResultType](stream: LazyList[ResultType])
  def concurrent(using Monitor, Codicil): LazyList[ResultType] raises AsyncError =
    if async(stream.isEmpty).await() then LazyList() else stream.head #:: stream.tail.concurrent

def supervise[ResultType](block: Monitor ?=> ResultType)
   (using model: ThreadModel, codepoint: Codepoint)
        : ResultType raises AsyncError =
  block(using model.supervisor())

def retry[ValueType](evaluate: (surrender: () => Nothing, persevere: () => Nothing) ?=> ValueType)
   (using Tenacity, Monitor)
        : ValueType raises RetryError =

  @tailrec
  def recur(attempt: Ordinal): ValueType =
    boundary[Perseverance[ValueType]]: label ?=>
      sleep(summon[Tenacity].delay(attempt).or(abort(RetryError(attempt.n1))))
      def surrender = boundary.break(Perseverance.Surrender)
      def persevere = boundary.break(Perseverance.Persevere)
      Perseverance.Prevail(evaluate(using () => surrender, () => persevere))

    . match
        case Perseverance.Surrender      => abort(RetryError(attempt.n1))
        case Perseverance.Prevail(value) => value
        case Perseverance.Persevere      => recur(attempt + 1)

  recur(Prim)

export Parasite.Stale
