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
  case Active
  case Suspended(count: Int)
  case Completed(value: ValueType)
  case Delivered(value: ValueType)
  case Failed(error: Throwable)

import Completion.*

trait Probate:
  def cleanup(monitor: Monitor): Unit

class Hook(private val thread: Thread):
  def cancel(): Unit = Runtime.getRuntime.nn.removeShutdownHook(thread)

object Async:
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
    

def daemon(using Codepoint)(evaluate: Submonitor[Unit] ?=> Unit)
    (using Monitor, Probate, Mitigator)
        : Async[Unit] =

  Async[Unit](evaluate(using _), daemon = true, name = Unset)

def async[ResultType](using Codepoint)(evaluate: Submonitor[ResultType] ?=> ResultType)
    (using Monitor, Probate, Mitigator)
        : Async[ResultType] =

  Async(evaluate(using _), daemon = false, name = Unset)

def task[ResultType](using Codepoint)(name: into Text)
    (evaluate: Submonitor[ResultType] ?=> ResultType)
    (using Monitor, Probate, Mitigator)
        : Async[ResultType] =
  
  Async(evaluate(using _), daemon = false, name = name)

enum Mitigation:
  case Suppress, Escalate, Cancel

trait Mitigator:
  def mitigate(monitor: Monitor, error: Throwable): Mitigation

@capability
class Async[+ResultType]
    (evaluate: Submonitor[ResultType] => ResultType, daemon: Boolean, name: Optional[Text])
    (using monitor: Monitor, codepoint: Codepoint, probate: Probate, mitigator: Mitigator):
  
  private val submonitor: Submonitor[ResultType] =
    monitor.child(name, daemon, codepoint, mitigator, evaluate, probate)
  
  def onComplete(callback: => Unit): Unit = submonitor.promise.onComplete(callback)
  
  submonitor.thread

  private[parasite] def get(): ResultType raises ConcurrencyError = submonitor.promise.get()

  def state(): Completion[ResultType] = submonitor.state()
  def ready: Boolean = submonitor.promise.ready

  def await[DurationType: GenericDuration](duration: DurationType)(using Raises[ConcurrencyError])
          : ResultType =
    
    submonitor.await(duration)

  def await()(using cancel: Raises[ConcurrencyError]): ResultType =
    submonitor.await()

  def suspend(): Unit = submonitor.suspend()

  def resume(force: Boolean = false): Unit = submonitor.resume(force)

  def map[ResultType2](lambda: ResultType => ResultType2)
          : Async[ResultType2] raises ConcurrencyError =

    async(lambda(await()))
  
  def foreach[ResultType2](lambda: ResultType => ResultType2): Unit raises ConcurrencyError =
    async(lambda(await()))
  
  def each[ResultType2](lambda: ResultType => ResultType2): Unit raises ConcurrencyError =
    async(lambda(await()))
  
  def flatMap[ResultType2](lambda: ResultType => Async[ResultType2])
          : Async[ResultType2] raises ConcurrencyError =

    async(lambda(await()).await())
  
  def cancel(): Unit = submonitor.cancel()

def relent[ResultType]()(using monitor: Submonitor[ResultType]): Unit = monitor.relent()
def cancel[ResultType]()(using monitor: Submonitor[ResultType]): Unit = monitor.cancel()

// def complete[ResultType](value: ResultType)(using monitor: Submonitor[ResultType]): Nothing =
//   monitor.complete(value)

def sleep[DurationType: GenericDuration](duration: DurationType)(using monitor: Monitor): Unit =
  monitor.sleep(duration.milliseconds)

def sleepUntil[InstantType: GenericInstant](instant: InstantType)(using monitor: Monitor): Unit =
  monitor.sleep(instant.millisecondsSinceEpoch - System.currentTimeMillis)

extension [ResultType](asyncs: Seq[Async[ResultType]])
  def sequence(using Monitor, Probate, Mitigator)
          : Async[Seq[ResultType]] raises ConcurrencyError =
    val count = asyncs.length
    val latch = juc.CountDownLatch(count)
    asyncs.each(_.onComplete(latch.countDown()))
    
    async:
      latch.await()
      asyncs.map(_.get())


extension [ResultType](asyncs: IndexedSeq[Async[ResultType]])
  def race(using Monitor, Probate, Mitigator): Async[ResultType] raises ConcurrencyError =
    async[Int]:
      val promise: Promise[Int] = Promise()
      
      asyncs.zipWithIndex.foreach: (async, index) =>
        async.each: result =>
          promise.offer(index)
      
      promise.await()
    
    .flatMap:
      case -1 => abort(ConcurrencyError(ConcurrencyError.Reason.Cancelled))
      case n  => asyncs(n)
