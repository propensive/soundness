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

import scala.compiletime.*
import scala.annotation.*
import scala.collection.mutable as scm

import language.experimental.captureChecking

object Promise:
  object Cancelled

case class Promise[ValueType]():
  @volatile
  private var value: Maybe[ValueType] | Promise.Cancelled.type = Unset

  def ready: Boolean = !value.unset

  private def get()(using boundary.Label[Unit]): ValueType = value.asMatchable match
    case Promise.Cancelled => boundary.break()
    case Unset             => ???
    case value: ValueType  => value

  def fulfill(supplied: -> ValueType)(using complete: CanThrow[AlreadyCompleteError]): Unit^{complete} =
    
    synchronized:
      if value.unset then
        value = supplied
        notifyAll()
      else throw AlreadyCompleteError()

  def await()(using boundary.Label[Unit]): ValueType = synchronized:
    while value.unset do wait()
    get()

  def cancel(): Unit = synchronized:
    value = Promise.Cancelled
    notifyAll()

  def await
        [DurationType](duration: DurationType)(using GenericDuration[DurationType], boundary.Label[Unit])
        : ValueType throws CancelError | TimeoutError =
    synchronized:
      if ready then get() else
        wait(readDuration(duration))
        if !ready then throw TimeoutError() else get()


@capability
sealed trait Monitor(id: Text):
  private val children: scm.HashMap[Text, AnyRef] = scm.HashMap()

  override def toString(): String = s"/$id"

object Supervisor extends Monitor(Text(""))

extension (monitor: Monitor)
  def childMonitor(id: Text)(using label: boundary.Label[Unit]): TaskMonitor = TaskMonitor(id, monitor)

def supervise[ResultType](fn: Monitor ?-> ResultType) = fn(using Supervisor)

@capability
class TaskMonitor(id: Text, parent: Monitor)(using label: boundary.Label[Unit]) extends Monitor(id):
  def boundaryLabel: boundary.Label[Unit] = label
  private var cancelled: Boolean = false
  
  def cancel(): Unit = synchronized:
    cancelled = true
  
  def acquiesce(): Unit = if cancelled then boundary.break()
  
  override def toString(): String = s"${parent}/$id"

object Task:
  transparent inline def apply[ResultType](id: Text)(inline eval: TaskMonitor ?=> ResultType)(using monitor: Monitor)
      : Task[ResultType]^ =
    new Task(id, monitor, eval(using _))

class Task[ResultType](id: Text, monitor: Monitor, eval: TaskMonitor => ResultType):
  private final val promise: Promise[ResultType] = Promise()

  private final def runnable(): Runnable = new Runnable():
    def run(): Unit =
      erased given CanThrow[AlreadyCompleteError] = ###
      
      boundary: bnd ?=>
        val child = monitor.childMonitor(id)(using bnd)
        println(s"start ${child}")
        val result = eval(child).tap(println(s"stop  ${child}").waive)
        promise.fulfill(result)

  private final val thread: Thread = Thread(runnable()).nn.tap(_.start())
  
  def await()(using monitor: TaskMonitor): ResultType = promise.await()(using monitor.boundaryLabel)

def acquiesce()(using monitor: TaskMonitor): Unit = monitor.acquiesce()
@main

def run(): Unit =
  val task: Int =
    supervise:
      example().await()

def example()(using monitor: Monitor): Task[Int]^{monitor} =
  Task(Text("hello")):
    println("hello world")
    7
