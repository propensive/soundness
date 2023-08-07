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
  private val cancelled: juca.AtomicBoolean = juca.AtomicBoolean(false)

  def ready: Boolean = !value.get.unset || cancelled.get()

  private def get()(using CanThrow[CancelError]): ValueType =
    if cancelled.get() then throw CancelError()
    else value.get.or(throw Mistake(msg"the promise was expected to be completed")).nn

  def fulfill(supplied: -> ValueType)(using complete: CanThrow[AlreadyCompleteError]): Unit^{complete} =
    synchronized:
      if !value.compareAndSet(Unset, supplied) then throw AlreadyCompleteError()
      notifyAll()

  def await()(using CanThrow[CancelError]): ValueType = synchronized:
    println("Waiting for promise")
    while !ready do wait()
    println("Stopped waiting")
    get()

  def cancel(): Unit = synchronized:
    println("cancelled.set(true)")
    cancelled.set(true)

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
  private val children: scm.HashMap[Text, juca.AtomicBoolean] = scm.HashMap()
  val cancelled: juca.AtomicBoolean = juca.AtomicBoolean(false)

  override def toString(): String = s"/$id"
  
  def cancel(): Unit =
    println("Cancelling "+id)
    erased given CanThrow[AlreadyCompleteError] = ###
    println(cancelled)
    promise.cancel()
    if cancelled.getAndSet(true) then println("Cancelling twice")

  def child(id: Text, promise: Promise[?])(using label: boundary.Label[Unit]): TaskMonitor =
    val monitor = TaskMonitor(id, this, promise)
    
    synchronized:
      children(id) = cancelled
    
    monitor


object Supervisor extends Monitor(Text(""), Promise[Unit]())

def supervise(id: Text)[ResultType](fn: Monitor ?=> ResultType)(using cancel: CanThrow[CancelError]): ResultType =
  fn(using Supervisor)

@capability
class TaskMonitor(id: Text, parent: Monitor, promise: Promise[?])(using label: boundary.Label[Unit]) extends Monitor(id, promise):
  def acquiesce(): Unit =
    if cancelled.get() then
      println("Acquiescing "+id)
      boundary.break()
  
  override def toString(): String = s"${parent}/$id"

@capability
class Task[ResultType](id: Text)(evaluate: TaskMonitor ?=> ResultType)(using monitor: Monitor):
  
  def cancel(): Unit = monitor.cancel()
  
  private final val promise: Promise[Try[ResultType]] = Promise()
  private val eval = (monitor: TaskMonitor) => evaluate(using monitor)

  private val thread: Thread =
    def runnable: Runnable^{monitor} = () =>
      boundary[Unit]:
        erased given CanThrow[AlreadyCompleteError] = ###
        println(s"starting $id")
        
        try
          val result = eval(monitor.child(id, promise))
          println(s"success for $id")
          promise.fulfill(Success(result))
        
        catch case NonFatal(error) =>
          println(s"failure for $id")
          promise.fulfill(Failure(error))
        
        boundary.break()
      
      println("done "+id)
      
    Thread(runnable).tap(_.start())
  
  def await()(using cancel: CanThrow[CancelError]): ResultType =
    println("Awaiting "+id)
    
    val result = promise.await()
    
    println(s"Joining $id...")
    thread.join()
    println(s"Completed $id...")
    
    result match
      case Success(result) => result
      case Failure(error)  => throw error

def acquiesce()(using monitor: TaskMonitor): Unit = monitor.acquiesce()
def cancel()(using monitor: TaskMonitor): Unit = monitor.cancel()

@main
def run(): Unit =
  import unsafeExceptions.canThrowAny
  
  supervise(Text("supervisor")):

    Task(Text("hello")):
      val left = Task(Text("left")):
        for i <- 1 to 20 do
          Thread.sleep(100)
          acquiesce()
        
        println("calculated left")
        3

      val right = Task(Text("right")):
        for i <- 1 to 10 do
          Thread.sleep(100)
          if i == 10 then cancel()
          acquiesce()
        
        println("calculated right")
        5

      println("LEFT:") 
      println(left.await())
      println("RIGHT:") 
      
      println(left.await() + right.await())
    .await()
  