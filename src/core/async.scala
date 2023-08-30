/*
    Parasite, version [unreleased]. Copyright 2023 Jon Pretty, Propensive OÜ.

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
import perforate.*
import digression.*

import java.util.concurrent.atomic as juca

import language.experimental.captureChecking

enum AsyncState[+ValueType]:
  case Active
  case Suspended(count: Int)
  case Completed(value: ValueType)
  case Failed(error: Throwable)
  case Cancelled

import AsyncState.*

object Async:
  def race
      [AsyncType]
      (asyncs: Vector[Async[AsyncType]])(using cancel: Raises[CancelError], monitor: Monitor)
      : Async[AsyncType] =

    Async[Int]:
      val promise: Promise[Int] = Promise()
      
      asyncs.zipWithIndex.foreach: (async, index) =>
        async.foreach: result =>
          promise.offer(index)
      
      promise.await()
    
    .flatMap:
      case -1 => abort(CancelError())
      case n  => asyncs(n)

@capability
class Async
    [+ResultType]
    (evaluate: Submonitor[ResultType] ?=> ResultType)
    (using monitor: Monitor, codepoint: Codepoint):
  async =>
  
  private final val trigger: Trigger = Trigger()
  private final val stateRef: juca.AtomicReference[AsyncState[ResultType]] = juca.AtomicReference(Active)

  private final val thread: Thread =
    def runnable: Runnable^{async} = () =>
      boundary[Unit]:
        val child = monitor.child[ResultType](identifier, stateRef, trigger)
        try
          val result = eval(child)
          stateRef.set(Completed(result))
        catch case NonFatal(error) => stateRef.set(Failed(error))
        finally
          trigger()
          child.cancel()
          boundary.break()
      
    Thread(runnable).tap(_.start())
  
  private def identifier: Text = Text(s"${codepoint.text}")
  private def eval(monitor: Submonitor[ResultType]): ResultType = evaluate(using monitor)

  def id: Text = Text((identifier :: monitor.name).reverse.map(_.s).mkString("// ", " / ", ""))
  def state(): AsyncState[ResultType] = stateRef.get().nn
  
  def await
      [DurationType: GenericDuration]
      (duration: DurationType)(using Raises[CancelError], Raises[TimeoutError])
      : ResultType =
    trigger.await(duration).tap(thread.join().waive)
    result()
  
  def await()(using cancel: Raises[CancelError]): ResultType =
    trigger.await().tap(thread.join().waive)
    result()
  
  private def result()(using cancel: Raises[CancelError]): ResultType =
    state() match
      case Completed(result) => result
      case Failed(error)     => throw error
      case Cancelled         => abort(CancelError())
      case other             => abort(CancelError())
  
  def suspend(): Unit =
    stateRef.updateAndGet:
      case Active               => Suspended(1)
      case Suspended(n)         => Suspended(n + 1)
      case other                => other

  def resume(force: Boolean = false): Unit =
    stateRef.updateAndGet:
      case Suspended(1)         => monitor.synchronized(monitor.notifyAll())
                                   Active
      case Suspended(n)         => if force then Active else Suspended(n - 1)
      case other                => other

  def map[ResultType2](fn: ResultType => ResultType2)(using Raises[CancelError]): Async[ResultType2] =
    Async(fn(async.await()))
  
  def foreach[ResultType2](fn: ResultType => ResultType2)(using Raises[CancelError]): Unit =
    Async(fn(async.await()))
  
  def flatMap
      [ResultType2]
      (fn: ResultType => Async[ResultType2])
      (using Raises[CancelError])
      : Async[ResultType2] =
    Async(fn(await()).await())
  
  def cancel(): Unit =
    stateRef.updateAndGet:
      case Active | Suspended(_) => Cancelled
      case other                 => other
    
    monitor.cancel()

def acquiesce[ResultType]()(using monitor: Submonitor[ResultType]): Unit = monitor.acquiesce()
def cancel[ResultType]()(using monitor: Submonitor[ResultType]): Unit = monitor.cancel()
def terminate()(using monitor: Monitor): Unit = monitor.terminate()

def complete[ResultType](value: ResultType)(using monitor: Submonitor[ResultType]): Nothing =
  monitor.complete(value)

def sleep[DurationType: GenericDuration, ResultType](duration: DurationType)(using monitor: Monitor): Unit =
  monitor.sleep(duration.milliseconds)

extension [ResultType](asyncs: Seq[Async[ResultType]]^)
  def sequence(using cancel: Raises[CancelError], mon: Monitor): Async[Seq[ResultType^{}]] = Async:
    asyncs.map(_.await())
