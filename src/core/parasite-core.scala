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

import anticipation.*
import contingency.*
import digression.*
import rudiments.*
import vacuous.*

package threadModels:
  given platform: ThreadModel = () => PlatformSupervisor
  given virtual: ThreadModel = () => VirtualSupervisor

package asyncOptions:
  given waitForOrphans: Codicil = _.delegate(_.attend())
  given cancelOrphans: Codicil = _.delegate(_.cancel())

  given failIfOrphansExist(using Errant[ConcurrencyError]): Codicil = _.delegate: child =>
    if !child.ready then raise(ConcurrencyError(ConcurrencyError.Reason.Incomplete))(())

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
    (using monitor: Monitor)
        : Unit =

  monitor.interceptor { trace => lambda(using trace) }

def relent[ResultType]()(using monitor: Subordinate): Unit = monitor.relent()
def cancel[ResultType]()(using monitor: Monitor): Unit = monitor.cancel()

def sleep[DurationType: GenericDuration](duration: DurationType)(using monitor: Monitor): Unit =
  monitor.sleep(duration.milliseconds)

def sleepUntil[InstantType: GenericInstant](instant: InstantType)(using monitor: Monitor): Unit =
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
