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

import language.experimental.pureFunctions
import language.experimental.into

import scala.compiletime.*

import anticipation.*
import contingency.*
import digression.*
import denominative.*
import rudiments.*
import vacuous.*

package threadModels:
  given ThreadModel as platform = () => PlatformSupervisor
  given ThreadModel as virtual = () => VirtualSupervisor

package orphanDisposal:
  given Codicil as await = _.delegate(_.attend())
  given Codicil as cancel = _.delegate(_.cancel())

  given (using Tactic[ConcurrencyError]) => Codicil as fail = _.delegate: child =>
    if !child.ready then raise(ConcurrencyError(ConcurrencyError.Reason.Incomplete), ())

package retryTenacities:
  given Tenacity as exponentialForever = Tenacity.exponential(10L, 1.2)
  given Tenacity as exponentialFiveTimes = Tenacity.exponential(10L, 1.2).limit(5)

transparent inline def monitor(using Monitor): Monitor = summonInline[Monitor]

def daemon(using Codepoint)(evaluate: Subordinate ?=> Unit)(using Monitor, Codicil): Daemon =
  Daemon(evaluate(using _))

def async[ResultType](using Codepoint)(evaluate: Subordinate ?=> ResultType)(using Monitor, Codicil)
        : Task[ResultType] =

  Task(evaluate(using _), daemon = false, name = Unset)

def task[ResultType](using Codepoint)(name: into Text)(evaluate: Subordinate ?=> ResultType)
    (using Monitor, Codicil)
        : Task[ResultType] =

  Task(evaluate(using _), daemon = false, name = name)

def intercept(lambda: (trace: Trace) ?=> PartialFunction[Throwable, Transgression])
    (using Monitor)
        : Unit =

  monitor.interceptor { trace => lambda(using trace) }

def relent[ResultType]()(using Subordinate): Unit = monitor.relent()
def cancel[ResultType]()(using Monitor): Unit = monitor.cancel()

def sleep[DurationType: GenericDuration](duration: DurationType)(using Monitor): Unit =
  monitor.sleep(duration.milliseconds)

def snooze[InstantType: GenericInstant](instant: InstantType)(using Monitor): Unit =
  monitor.sleep(instant.millisecondsSinceEpoch - System.currentTimeMillis)

extension [ResultType](tasks: Seq[Task[ResultType]])
  def sequence(using Monitor, Codicil): Task[Seq[ResultType]] raises ConcurrencyError =
    async(tasks.map(_.await()))

extension [ResultType](tasks: Iterable[Task[ResultType]])
  def race()(using Monitor, Codicil): ResultType raises ConcurrencyError =
    val promise: Promise[ResultType] = Promise()
    tasks.each(_.map(promise.offer(_)))

    promise.await()

def supervise[ResultType](block: Monitor ?=> ResultType)
    (using model: ThreadModel, codepoint: Codepoint)
        : ResultType raises ConcurrencyError =
  block(using model.supervisor())

def retry[ValueType](evaluate: => Perseverance[ValueType])(using Tenacity, Monitor)
        : ValueType raises RetryError =

  @tailrec
  def recur(attempt: Ordinal): ValueType =
    sleep(summon[Tenacity].delay(attempt).or(abort(RetryError(attempt.n1))))

    evaluate match
      case Perseverance.Surrender      => abort(RetryError(attempt.n1))
      case Perseverance.Persevere      => recur(attempt + 1)
      case Perseverance.Prevail(value) => value

  recur(Prim)

export Perseverance.{Persevere, Prevail, Surrender}
