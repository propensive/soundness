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
import scala.util.{Try, Success, Failure}
import scala.collection.mutable as scm

import java.util.concurrent.atomic as juca

import language.experimental.captureChecking

object Promise:
  object Cancelled

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

  def await()(using CanThrow[CancelError]): ValueType = synchronized:
    while !ready do wait()
    get()

  def cancel(): Unit = synchronized:
    cancelValue.set(true)
    notifyAll()

  def await
        [DurationType](duration: DurationType)(using GenericDuration[DurationType])
        : ValueType throws CancelError | TimeoutError =
    synchronized:
      if ready then get() else
        wait(readDuration(duration))
        if !ready then throw TimeoutError() else get()

@capability
sealed trait Monitor(id: Text, promise: Promise[?]):
  private val children: scm.HashMap[Text, AnyRef] = scm.HashMap()

  override def toString(): String = s"/$id"
  
  def cancel(): Unit =
    children.foreach:
      case (id, child: Monitor) => child.cancel()

    erased given CanThrow[AlreadyCompleteError] = ###
    promise.cancel()

  def child(id: Text, promise: Promise[?])(using label: boundary.Label[Unit]): TaskMonitor =
    val monitor = TaskMonitor(id, this, promise)
    
    synchronized:
      children(id) = monitor
    
    monitor

object Supervisor extends Monitor(Text(""), Promise[Unit]())

def supervise(id: Text)[ResultType](fn: Monitor ?=> ResultType)(using cancel: CanThrow[CancelError]): ResultType =
  fn(using Supervisor)

@capability
class TaskMonitor(id: Text, parent: Monitor, promise: Promise[?])(using label: boundary.Label[Unit]) extends Monitor(id, promise):
  def acquiesce(): Unit = if promise.cancelled then boundary.break()
  override def toString(): String = s"${parent}/$id"

@capability
class Task[ResultType](id: Text)(evaluate: TaskMonitor ?=> ResultType)(using monitor: Monitor):
  def await()(using cancel: CanThrow[CancelError]): ResultType =
    promise.await().tap(thread.join().waive) match
      case Success(result) => result
      case Failure(error)  => throw error
  
  def map[ResultType2](fn: ResultType => ResultType2)(using CanThrow[CancelError]): Task[ResultType2] =
    Task(Text(s"$id.map"))(fn(await()))
  
  def flatMap[ResultType2](fn: ResultType => Task[ResultType2])(using CanThrow[CancelError]): Task[ResultType2] =
    Task(Text(s"$id.flatMap"))(fn(await()).await())
  
  def cancel(): Unit = monitor.cancel()

  private final val promise: Promise[Try[ResultType]] = Promise()
  private val eval = (monitor: TaskMonitor) => evaluate(using monitor)

  private val thread: Thread =
    def runnable: Runnable^{monitor} = () =>
      boundary[Unit]:
        erased given CanThrow[AlreadyCompleteError] = ###
        
        try
          val result = eval(monitor.child(id, promise))
          promise.fulfill(Success(result))
        
        catch case NonFatal(error) => promise.fulfill(Failure(error))
        
        boundary.break()
      
    Thread(runnable).tap(_.start())
  
def acquiesce()(using monitor: TaskMonitor): Unit = monitor.acquiesce()
def cancel()(using monitor: TaskMonitor): Unit = monitor.cancel()
