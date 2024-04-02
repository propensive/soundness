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

import language.experimental.pureFunctions

enum Completion[+ValueType]:
  case Initializing
  case Active
  case Suspended(count: Int)
  case Completed(value: ValueType)
  case Delivered(value: ValueType)
  case Failed(error: Throwable)

import Completion.*

trait OrphanCompletion:
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
  given daemon: ThreadModel = () => DaemonSupervisor

package asyncOptions:
  given waitForOrphans: OrphanCompletion = _.delegate(_.attend())
  given cancelOrphans: OrphanCompletion = _.delegate(_.cancel())
  
  given failIfOrphansExist(using Raises[ConcurrencyError]): OrphanCompletion = _.delegate: child =>
    if !child.ready then raise(ConcurrencyError(ConcurrencyError.Reason.Incomplete))(())

  given escalateExceptions: Mitigator = (path, error) => Mitigation.Escalate
  given ignoreExceptions: Mitigator = (path, error) => Mitigation.Suppress
    

def daemon(evaluate: Submonitor[Unit] ?=> Unit)
    (using Monitor, Codepoint, OrphanCompletion, Mitigator)
        : Async[Unit] =

  Async[Unit](evaluate(using _), daemon = true, name = Unset)

def async[ResultType](evaluate: Submonitor[ResultType] ?=> ResultType)
    (using Monitor, Codepoint, OrphanCompletion, Mitigator)
        : Async[ResultType] =

  Async(evaluate(using _), daemon = false, name = Unset)

def task[ResultType](name: into Text)(evaluate: Submonitor[ResultType] ?=> ResultType)
    (using Monitor, OrphanCompletion, Mitigator)
        : Async[ResultType] =
  
  Async(evaluate(using _), daemon = false, name = name)

enum Mitigation:
  case Suppress, Escalate, Cancel

trait Mitigator:
  def mitigate(path: List[Codepoint], error: Throwable): Mitigation

@capability
class Async[+ResultType]
    (evaluate: Submonitor[ResultType] => ResultType, daemon: Boolean, name: Optional[Text])
    (using monitor: Monitor, codepoint: Codepoint, orphans: OrphanCompletion, mitigator: Mitigator):
  
  private val submonitor: Submonitor[ResultType] = monitor.child(codepoint, mitigator)

  private final val thread: Thread =
    def runnable: Runnable = () => boundary[Unit]:
      try
        submonitor.state() = Active
        evaluate(submonitor).tap: result =>
          submonitor.state() = Completed(result)
      catch
        case error: Throwable =>
          submonitor.mitigate(submonitor.stack, error)
          submonitor.state() = Failed(error)
      finally
        orphans.cleanup(submonitor)
        
        submonitor.state() match
          case Completed(value) => submonitor.promise.offer(value)
          case Active           => submonitor.promise.offer(Promise.Cancelled)
          case Suspended(_)     => submonitor.promise.offer(Promise.Cancelled)
          case Failed(_)        => submonitor.promise.offer(Promise.Incomplete)
        
        monitor.remove(submonitor)
        boundary.break()
  
    monitor.supervisor.fork(runnable)

  def state(): Completion[ResultType] = submonitor.state()
  def ready: Boolean = submonitor.promise.ready

  def await[DurationType: GenericDuration](duration: DurationType)(using Raises[ConcurrencyError])
          : ResultType =

    submonitor.promise.attend(duration)
    thread.join()
    result()

  def await()(using cancel: Raises[ConcurrencyError]): ResultType =
    submonitor.promise.attend()
    thread.join()
    result()

  private def result()(using cancel: Raises[ConcurrencyError]): ResultType =
    submonitor.state() match
      case Initializing      => abort(ConcurrencyError(ConcurrencyError.Reason.Incomplete))
      case Active            => abort(ConcurrencyError(ConcurrencyError.Reason.Incomplete))
      case Suspended(_)      => abort(ConcurrencyError(ConcurrencyError.Reason.Incomplete))
      case Completed(result) => result.also { submonitor.state() = Delivered(result) }
      case Delivered(result) => result
      case Failed(error)     => throw error

  def suspend(): Unit = submonitor.state.replace:
    case Active       => Suspended(1)
    case Suspended(n) => Suspended(n + 1)
    case other        => other

  def resume(force: Boolean = false): Unit = submonitor.state.replace:
    case Suspended(1) => Active.also(monitor.synchronized(monitor.notifyAll()))
    case Suspended(n) => if force then Active else Suspended(n - 1)
    case other        => other

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
  
  def cancel(): Unit =
    thread.interrupt()
    monitor.cancel()

def relent[ResultType]()(using monitor: Submonitor[ResultType]): Unit = monitor.relent()
def cancel[ResultType]()(using monitor: Submonitor[ResultType]): Unit = monitor.cancel()
def terminate()(using monitor: Monitor): Unit = monitor.terminate()

// def complete[ResultType](value: ResultType)(using monitor: Submonitor[ResultType]): Nothing =
//   monitor.complete(value)

def sleep[DurationType: GenericDuration](duration: DurationType)(using monitor: Monitor): Unit =
  monitor.sleep(duration.milliseconds)

def sleepUntil[InstantType: GenericInstant](instant: InstantType)(using monitor: Monitor): Unit =
  monitor.sleep(instant.millisecondsSinceEpoch - System.currentTimeMillis)

extension [ResultType](asyncs: Seq[Async[ResultType]])
  def sequence(using Monitor, OrphanCompletion, Mitigator)
          : Async[Seq[ResultType]] raises ConcurrencyError =

    async(asyncs.map(_.await()))

extension [ResultType](asyncs: IndexedSeq[Async[ResultType]])
  def race(using Monitor, OrphanCompletion, Mitigator): Async[ResultType] raises ConcurrencyError =
    async[Int]:
      val promise: Promise[Int] = Promise()
      
      asyncs.zipWithIndex.foreach: (async, index) =>
        async.each: result =>
          promise.offer(index)
      
      promise.await()
    
    .flatMap:
      case -1 => abort(ConcurrencyError(ConcurrencyError.Reason.Cancelled))
      case n  => asyncs(n)
