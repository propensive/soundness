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

import language.experimental.captureChecking

object Promise:
  object Cancelled

case class Promise[ValueType]():
  private var value: Maybe[ValueType] = Unset
  private var cancelled: Boolean = false

  def ready: Boolean = !value.unset && !cancelled

  private def get()(using CanThrow[CancelError]): ValueType =
    if cancelled then throw CancelError()
    else value.or(throw Mistake(msg"the promise was expected to be completed"))

  def fulfill(supplied: -> ValueType)(using complete: CanThrow[AlreadyCompleteError]): Unit^{complete} =
    synchronized:
      if value.unset then
        value = supplied
        notifyAll()
      else throw AlreadyCompleteError()

  def await()(using CanThrow[CancelError]): ValueType = synchronized:
    while value.unset do wait()
    get()

  def cancel(): Unit = synchronized:
    cancelled = true
    notifyAll()

  def await
        [DurationType](duration: DurationType)(using GenericDuration[DurationType])
        : ValueType throws CancelError | TimeoutError =
    synchronized:
      if ready then get() else
        wait(readDuration(duration))
        if !ready then throw TimeoutError() else get()


@capability
sealed trait Monitor(id: Text):
  private val children: scm.HashMap[Text, AnyRef] = scm.HashMap()

  override def toString(): String = s"/$id"
  
  def child(id: Text)(using label: boundary.Label[Unit]): TaskMonitor = TaskMonitor(id, this)

object Supervisor extends Monitor(Text(""))

def supervise(id: Text)[ResultType](fn: Monitor ?=> ResultType)(using cancel: CanThrow[CancelError]): ResultType =
  fn(using Supervisor)

@capability
class TaskMonitor(id: Text, parent: Monitor)(using label: boundary.Label[Unit]) extends Monitor(id):
  private var cancelled: Boolean = false
  
  def cancel(): Unit = synchronized:
    cancelled = true
  
  def acquiesce(): Unit = if cancelled then boundary.break()(using label)
  
  override def toString(): String = s"${parent}/$id"

@capability
class Task[ResultType](id: Text)(evaluate: TaskMonitor ?=> ResultType)(using monitor: Monitor):

  private final val promise: Promise[ResultType] = Promise()
  private val eval = (monitor: TaskMonitor) => evaluate(using monitor)
  private def child(using monitor: Monitor, label: boundary.Label[Unit]): TaskMonitor = monitor.child(id)
  
  private val thread: Thread =
    def runnable: Runnable^{monitor} = () =>
      boundary[Unit]:
        println(s"starting $id")
        val result = eval(child)
        erased given CanThrow[AlreadyCompleteError] = ###
        println(s"finishing $id")
        promise.fulfill(result)
        boundary.break()
    
    Thread(runnable).tap(_.start())
  
  def await()(using cancel: CanThrow[CancelError], monitor: Monitor): ResultType = promise.await()

def acquiesce()(using monitor: TaskMonitor): Unit = monitor.acquiesce()

@main
def run(): Unit =
  import unsafeExceptions.canThrowAny
  
  supervise(Text("supervisor")):

    Task(Text("hello")):
      val left = Task(Text("left")):
        Thread.sleep(2000)
        println("calculated left")
        3

      val right = Task(Text("right")):
        Thread.sleep(1000)
        println("calculated right")
        5
      
      left.await() + right.await()
    .await()
  


// @main
// def run(): Unit =
//   val task: Int =
//     supervise:
//       example().await()

// def example[T]()(using monitor: Monitor): (Monitor, Int) =
//   Task(Text("hello")): monitor =>
//     println("hello world")
//     7
