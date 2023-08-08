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
import fulminate.*

import scala.compiletime.*
import scala.annotation.*
import scala.collection.mutable as scm

import java.util.concurrent.atomic as juca

import language.experimental.captureChecking

object Promise:
  object Cancelled

enum TaskState[+ValueType]:
  case Active
  case Suspended(count: Int)
  case Completed(value: ValueType)
  case Failed(error: Throwable)
  case Cancelled

import TaskState.*

case class Promise[ValueType]():
  private val value: juca.AtomicReference[Maybe[ValueType]] = juca.AtomicReference(Unset)
  private val cancelValue: juca.AtomicBoolean = juca.AtomicBoolean(false)

  def cancelled: Boolean = cancelValue.get()
  def ready: Boolean = !value.get.unset || cancelled

  private def get()(using CanThrow[CancelError]): ValueType =
    if cancelled then throw CancelError()
    else value.get.or(throw Mistake(msg"the promise was expected to be completed")).nn

  def fulfill(supplied: -> ValueType)(using complete: CanThrow[AlreadyCompleteError]): Unit^{complete} =
    synchronized:
      if !value.compareAndSet(Unset, supplied) then throw AlreadyCompleteError()
      notifyAll()
  
  def offer(supplied: -> ValueType): Unit = synchronized:
    if value.compareAndSet(Unset, supplied) then notifyAll()

  def await()(using CanThrow[CancelError]): ValueType = synchronized:
    while !ready do wait()
    get()

  def cancel(): Unit = synchronized:
    cancelValue.set(true)
    notifyAll()

  def await
        [DurationType: GenericDuration](duration: DurationType)
        : ValueType throws CancelError | TimeoutError =
    synchronized:
      if ready then get() else
        wait(duration.milliseconds)
        if !ready then throw TimeoutError() else get()

case class Trigger():
  private val promise: Promise[Unit] = Promise()
  def apply(): Unit = promise.offer(())
  def pull(): Unit throws AlreadyCompleteError = promise.fulfill(())
  def await()(using CanThrow[CancelError]): Unit = promise.await()
  def cancel(): Unit = promise.cancel()
  def cancelled: Boolean = promise.cancelled
  
  def await[DurationType: GenericDuration](duration: DurationType): Unit throws CancelError | TimeoutError =
    promise.await(duration)

@capability
sealed trait Monitor(val name: List[Text], trigger: Trigger):
  private val children: scm.HashMap[Text, AnyRef] = scm.HashMap()
  def id: Text = Text(name.reverse.map(_.s).mkString("/"))

  def cancel(): Unit =
    children.foreach: (id, child) =>
      child match
        case child: Monitor => child.cancel()
        case _              => ()

    erased given CanThrow[AlreadyCompleteError] = ###
    trigger.cancel()

  def terminate(): Unit = this match
    case Supervisor                                     => Supervisor.cancel()
    case monitor@Submonitor(id, parent, state, promise) => monitor.terminate()

  def sleep(duration: Long): Unit = Thread.sleep(duration)

  def child
      [ResultType2]
      (id: Text, state: juca.AtomicReference[TaskState[ResultType2]], trigger: Trigger)
      (using label: boundary.Label[Unit])
      : Submonitor[ResultType2] =
    
    val monitor = Submonitor[ResultType2](id, this, state, trigger)
    
    synchronized:
      children(id) = monitor
    
    monitor

case object Supervisor extends Monitor(List(Text("")), Trigger())

def supervise
    [ResultType]
    (fn: Monitor ?=> ResultType)(using cancel: CanThrow[CancelError])
    : ResultType =
  fn(using Supervisor)

@capability
case class Submonitor
    [ResultType]
    (initId: Text, parent: Monitor, stateRef: juca.AtomicReference[TaskState[ResultType]], trigger: Trigger)
    (using label: boundary.Label[Unit])
extends Monitor(initId :: parent.name, trigger):

  def state(): TaskState[ResultType] = stateRef.get().nn
  
  def complete(value: ResultType): Nothing =
    stateRef.set(Completed(value))
    trigger()
    boundary.break()
  
  def acquiesce(): Unit = synchronized:
    stateRef.get().nn match
      case Active            => ()
      case Suspended(_)      => wait()
      case Completed(value)  => trigger()
      case Cancelled         => trigger()
      case Failed(error)     => trigger()
    
    if trigger.cancelled then boundary.break()
  
object Task:
  def race
      [TaskType]
      (tasks: Vector[Task[TaskType]])(using cancel: CanThrow[CancelError], monitor: Monitor)
      : Task[TaskType] =
    
    val race = Task[Int](Text("race")):
      tasks.zipWithIndex.foreach: (task, index) =>
        task.foreach: result =>
          complete(index)
      -1 
    
    tasks(race.await())

@capability
class Task[+ResultType](initId: Text)(evaluate: Submonitor[ResultType] ?=> ResultType)(using monitor: Monitor):
  private val eval = (monitor: Submonitor[ResultType]) => evaluate(using monitor)
  private final val trigger: Trigger = Trigger()
  private val stateRef: juca.AtomicReference[TaskState[ResultType]] = juca.AtomicReference(Active)

  private val thread: Thread =
    def runnable: Runnable^{monitor} = () =>
      boundary[Unit]:
        erased given CanThrow[AlreadyCompleteError] = ###
        
        try
          val result = eval(monitor.child[ResultType](initId, stateRef, trigger))
          stateRef.set(Completed(result))
        catch case NonFatal(error) => stateRef.set(Failed(error))
        
        trigger()
        
        boundary.break()
      
    Thread(runnable).tap(_.start())
  def id: Text = Text((initId :: monitor.name).reverse.map(_.s).mkString("/"))
  def state(): TaskState[ResultType] = stateRef.get().nn
  
  def await
      [DurationType: GenericDuration]
      (duration: DurationType)
      : ResultType throws CancelError | TimeoutError =
    trigger.await(duration).tap(thread.join().waive)
    result()
  
  def await()(using cancel: CanThrow[CancelError]): ResultType =
    trigger.await().tap(thread.join().waive)
    result()
  
  private def result()(using cancel: CanThrow[CancelError]): ResultType = state() match
    case Completed(result) => result
    case Failed(error)     => throw error
    case Cancelled         => throw CancelError()
    case other             => throw CancelError()
  
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

  def wake(): Unit =
    stateRef.updateAndGet:
      case other                => other

  def map[ResultType2](fn: ResultType => ResultType2)(using CanThrow[CancelError]): Task[ResultType2] =
    Task(Text(s"$initId.map"))(fn(await()))
  
  def foreach[ResultType2](fn: ResultType => ResultType2)(using CanThrow[CancelError]): Unit =
    Task(Text(s"$initId.map"))(fn(await()))
  
  def flatMap
      [ResultType2]
      (fn: ResultType => Task[ResultType2])(using CanThrow[CancelError])
      : Task[ResultType2] =
    Task(Text(s"$initId.flatMap"))(fn(await()).await())
  
  def cancel(): Unit = monitor.cancel()

def acquiesce[ResultType]()(using monitor: Submonitor[ResultType]): Unit = monitor.acquiesce()
def cancel[ResultType]()(using monitor: Submonitor[ResultType]): Unit = monitor.cancel()
def terminate()(using monitor: Monitor): Unit = monitor.terminate()

def complete[ResultType](value: ResultType)(using monitor: Submonitor[ResultType]): Nothing =
  monitor.complete(value)

def sleep[DurationType: GenericDuration, ResultType](duration: DurationType)(using monitor: Monitor): Unit =
  monitor.sleep(duration.milliseconds)

extension [ResultType](tasks: Seq[Task[ResultType]]^)
  def sequence(using cancel: CanThrow[CancelError], mon: Monitor): Task[Seq[ResultType^{}]] =
    Task(Text("sequence")):
      tasks.map(_.await())