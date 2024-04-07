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

import anticipation.*
import rudiments.*
import contingency.*
import digression.*
import vacuous.*

import java.util.concurrent as juc

import language.experimental.pureFunctions

enum Completion[+ValueType]:
  case Initializing
  case Active(startTime: Long)
  case Suspended(startTame: Long, count: Int)
  case Completed(duration: Long, value: ValueType)
  case Delivered(duration: Long, value: ValueType)
  case Failed(error: Throwable)

import Completion.*

trait Probate:
  def cleanup(monitor: Monitor): Unit

class Hook(private val thread: Thread):
  def cancel(): Unit = Runtime.getRuntime.nn.removeShutdownHook(thread)

object Hook:
  def onShutdown(block: => Unit): Hook =
    val runnable: Runnable = () => block
    val thread: Thread = Thread(runnable)
    Runtime.getRuntime.nn.addShutdownHook(thread)
    Hook(thread)

trait ThreadModel:
  def supervisor(): Supervisor

package threadModels:
  given platform: ThreadModel = () => PlatformSupervisor
  given virtual: ThreadModel = () => VirtualSupervisor

package asyncOptions:
  given waitForOrphans: Probate = _.delegate(_.attend())
  given cancelOrphans: Probate = _.delegate(_.cancel())
  
  given failIfOrphansExist(using Raises[ConcurrencyError]): Probate = _.delegate: child =>
    if !child.ready then raise(ConcurrencyError(ConcurrencyError.Reason.Incomplete))(())

  given escalateExceptions: Mitigator = (path, error) => Mitigation.Escalate
  given ignoreExceptions: Mitigator = (path, error) => Mitigation.Suppress
    

def daemon(using Codepoint)(evaluate: Subordinate ?=> Unit)
    (using Monitor, Probate, Mitigator)
        : Task[Unit] =

  Task[Unit](evaluate(using _), daemon = true, name = Unset)

def async[ResultType](using Codepoint)(evaluate: Subordinate ?=> ResultType)
    (using Monitor, Probate, Mitigator)
        : Task[ResultType] =

  Task(evaluate(using _), daemon = false, name = Unset)

def task[ResultType](using Codepoint)(name: into Text)
    (evaluate: Subordinate ?=> ResultType)
    (using Monitor, Probate, Mitigator)
        : Task[ResultType] =
  
  Task(evaluate(using _), daemon = false, name = name)

enum Mitigation:
  case Suppress, Escalate, Cancel

trait Mitigator:
  def mitigate(monitor: Monitor, error: Throwable): Mitigation

object Task:
  def apply[ResultType]
      (evaluate: Subordinate => ResultType, daemon: Boolean, name: Optional[Text])
      (using monitor: Monitor, codepoint: Codepoint, probate: Probate, mitigator: Mitigator)
          : Task[ResultType] =
    
    monitor.subordinate(name, daemon, codepoint, mitigator, probate)(evaluate)
    
trait Task[+ResultType]:
  def ready: Boolean
  def await(): ResultType raises ConcurrencyError
  def suspend(): Unit
  def resume(force: Boolean = false): Unit
  def cancel(): Unit
  
  def await[DurationType: GenericDuration](duration: DurationType)
          : ResultType raises ConcurrencyError
  
  def flatMap[ResultType2](lambda: ResultType => Task[ResultType2])
      (using Monitor, Probate, Mitigator)
          : Task[ResultType2] raises ConcurrencyError
  
  def map[ResultType2](lambda: ResultType => ResultType2)(using Monitor, Probate, Mitigator)
          : Task[ResultType2] raises ConcurrencyError

def relent[ResultType]()(using monitor: Subordinate): Unit = monitor.relent()
def cancel[ResultType]()(using monitor: Monitor): Unit = monitor.cancel()

def sleep[DurationType: GenericDuration](duration: DurationType)(using monitor: Monitor): Unit =
  monitor.sleep(duration.milliseconds)

def sleepUntil[InstantType: GenericInstant](instant: InstantType)(using monitor: Monitor): Unit =
  monitor.sleep(instant.millisecondsSinceEpoch - System.currentTimeMillis)

extension [ResultType](tasks: Seq[Task[ResultType]])
  def sequence(using Monitor, Probate, Mitigator): Task[Seq[ResultType]] raises ConcurrencyError =
    async(tasks.map(_.await()))

extension [ResultType](tasks: Iterable[Task[ResultType]])
  def race()(using Monitor, Probate, Mitigator): ResultType raises ConcurrencyError =
    val promise: Promise[ResultType] = Promise()
    tasks.each(_.map(promise.offer(_)))
    
    promise.await()