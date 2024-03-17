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

import java.util.concurrent.atomic as juca

import language.experimental.pureFunctions

enum AsyncState[+ValueType]:
  case Active
  case Suspended(count: Int)
  case Completed(value: ValueType)
  case Failed(error: Throwable)

import AsyncState.*

class Hook(private val thread: Thread):
  def cancel(): Unit = Runtime.getRuntime.nn.removeShutdownHook(thread)

object Async:
  def onShutdown(block: => Unit): Hook =
    val runnable: Runnable = () => block
    val thread: Thread = Thread(runnable)
    Runtime.getRuntime.nn.addShutdownHook(thread)
    Hook(thread)

  def race[AsyncType](asyncs: Vector[Async[AsyncType]])(using Raises[CancelError], Monitor): Async[AsyncType] =
    async[Int]:
      val promise: Promise[Int] = Promise()
      
      asyncs.zipWithIndex.foreach: (async, index) =>
        async.each: result =>
          promise.offer(index)
      
      promise.await()
    
    .flatMap:
      case -1 => abort(CancelError())
      case n  => asyncs(n)

trait ThreadModel:
  def supervisor(): Supervisor

package threadModels:
  given platform: ThreadModel = () => PlatformSupervisor
  given virtual: ThreadModel = () => VirtualSupervisor
  given daemon: ThreadModel = () => DaemonSupervisor

def daemon(evaluate: Submonitor[Unit] ?=> Unit)(using Monitor, Codepoint): Async[Unit] =
  Async[Unit](evaluate, daemon = true, name = Unset)

def async[ResultType](evaluate: Submonitor[ResultType] ?=> ResultType)(using Monitor, Codepoint)
        : Async[ResultType] =

  Async(evaluate, daemon = false, name = Unset)

def task[ResultType](name: into Text)(evaluate: Submonitor[ResultType] ?=> ResultType)(using Monitor)
        : Async[ResultType] =
  
  Async(evaluate, daemon = false, name = name)

@capability
class Async[+ResultType]
    (evaluate: Submonitor[ResultType] ?=> ResultType, daemon: Boolean, name: Optional[Text])
    (using monitor: Monitor, codepoint: Codepoint):
  
  private final val promise: Promise[ResultType | Promise.Special] = Promise()
  private final val stateRef: juca.AtomicReference[AsyncState[ResultType]] = juca.AtomicReference(Active)

  private final val thread: Thread =
    def runnable: Runnable = () =>
      boundary[Unit]:
        val child = monitor.child[ResultType](identifier, stateRef, promise)
        
        try evaluate(using child).tap { result => stateRef.set(Completed(result)) }
        catch case NonFatal(error) => stateRef.set(Failed(error))
        finally stateRef.get().nn match
          case Completed(value) => promise.offer(value)
          case Active           => promise.offer(Promise.Cancelled)
          case Suspended(_)     => promise.offer(Promise.Cancelled)
          case Failed(_)        => promise.offer(Promise.Incomplete)

          child.cancel()
          boundary.break()
    
    monitor.supervisor.newPlatformThread(name.or("async".tt), runnable)
  
  private def identifier: Text = s"${codepoint.text}".tt

  def id: Text = Text((identifier :: monitor.name).reverse.map(_.s).mkString("// ", " / ", ""))
  def state(): AsyncState[ResultType] = stateRef.get().nn
  def ready: Boolean = promise.ready
  
  def await[DurationType: GenericDuration]
      (duration: DurationType)(using Raises[CancelError], Raises[TimeoutError])
          : ResultType =

    promise.attend(duration)
    thread.join()
    result()
  
  def await()(using cancel: Raises[CancelError]): ResultType =
    promise.attend()
    thread.join()
    result()
  
  private def result()(using cancel: Raises[CancelError]): ResultType = state() match
    case Completed(result) => result
    case Failed(error)     => throw error
    case Active            => abort(CancelError())
    case other             => abort(CancelError())
  
  def suspend(): Unit = stateRef.updateAndGet:
    case Active       => Suspended(1)
    case Suspended(n) => Suspended(n + 1)
    case other        => other

  def resume(force: Boolean = false): Unit = stateRef.updateAndGet:
    case Suspended(1) => Active.also(monitor.synchronized(monitor.notifyAll()))
    case Suspended(n) => if force then Active else Suspended(n - 1)
    case other        => other

  def map[ResultType2](lambda: ResultType => ResultType2)(using Raises[CancelError]): Async[ResultType2] =
    async(lambda(await()))
  
  def foreach[ResultType2](lambda: ResultType => ResultType2)(using Raises[CancelError]): Unit =
    async(lambda(await()))
  
  def each[ResultType2](lambda: ResultType => ResultType2)(using Raises[CancelError]): Unit =
    async(lambda(await()))
  
  def flatMap[ResultType2](lambda: ResultType => Async[ResultType2])(using Raises[CancelError])
          : Async[ResultType2] =

    async(lambda(await()).await())
  
  def cancel(): Unit =
    thread.interrupt()
    monitor.cancel()

def acquiesce[ResultType]()(using monitor: Submonitor[ResultType]): Unit = monitor.acquiesce()
def cancel[ResultType]()(using monitor: Submonitor[ResultType]): Unit = monitor.cancel()
def terminate()(using monitor: Monitor): Unit = monitor.terminate()

def complete[ResultType](value: ResultType)(using monitor: Submonitor[ResultType]): Nothing =
  monitor.complete(value)

def sleep[DurationType: GenericDuration](duration: DurationType)(using monitor: Monitor): Unit =
  monitor.sleep(duration.milliseconds)

def sleepUntil[InstantType: GenericInstant](instant: InstantType)(using monitor: Monitor): Unit =
  monitor.sleep(instant.millisecondsSinceEpoch - System.currentTimeMillis)

extension [ResultType](asyncs: Seq[Async[ResultType]])
  def sequence(using cancel: Raises[CancelError], mon: Monitor): Async[Seq[ResultType]] =
    async(asyncs.map(_.await()))
