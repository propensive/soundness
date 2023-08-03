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

import rudiments.*
import fulminate.*
//import anticipation.*

//import scala.util.*

import language.experimental.captureChecking

// package monitors:
//   given global: Monitor = Supervisor(daemon = true)

case class CancelError() extends Error(msg"the operation was cancelled")
case class IncompleteError() extends Error(msg"the task was not completed")
case class AlreadyCompleteError() extends Error(msg"the promise was already completed")
case class TimeoutError() extends Error(msg"the operation timed out")

// case class Supervisor(baseId: Text = Text("main"), daemon: Boolean = false, virtualThreads: Boolean = false)
// extends Monitor:
//   @volatile
//   private var interrupted: Boolean = false

//   def name: Text = Text("task://"+baseId)
//   def id: Text = name
//   def continue = !interrupted
//   def cancel(): Unit = interrupted = true

//   def makeThread(runnable: {*} Runnable, name: Text): {runnable} Thread =
//     if virtualThreads then Thread.ofVirtual.nn.name(name.s).nn.start(runnable).nn
//     else Thread(runnable, name.s).nn.tap(_.setDaemon(daemon)).tap(_.start())

// def supervise[ResultType](id: Text)(fn: Monitor ?=> ResultType): ResultType = fn(using Supervisor(id))
// def hibernate()(using Monitor): Unit = sleep(using timeApi.long)(Long.MaxValue)

// def sleep(using t: GenericDuration)(time: t.Duration)(using Monitor): Unit =
//   try Thread.sleep(readDuration(time)) catch case err: InterruptedException => unsafely(throw CancelError())

// @implicitNotFound("""|parasite: a contextual Monitor instance is required, for example:
//                      |    import monitors.global  // a top-level supervisor for asynchronous tasks""".stripMargin)
// @capability
// trait Monitor:
//   def id: Text
//   def name: Text
//   def continue: Boolean
//   def cancel(): Unit
//   def makeThread(runnable: {*} Runnable, name: Text): {runnable} Thread
  
//   final def accede(cleanup: => Unit = ()): Unit =
//     import unsafeExceptions.canThrowAny
//     if !continue then cleanup.pipe((throw CancelError()).waive)

//   def child(id: Text, check: => Boolean, abort: => Unit): Monitor =
//     TaskMonitor(id, () => check, () => abort, this)

// case class TaskMonitor(id: Text, interrupted: () => Boolean, stop: () => Unit, parent: Monitor) extends Monitor:
//   def name: Text = Text(parent.name.s+"/"+id)
//   def continue: Boolean = !interrupted() && parent.continue
//   def cancel(): Unit = stop()
//   def makeThread(runnable: {*} Runnable, name: Text): {runnable} Thread = parent.makeThread(runnable, name)

// extension [ResultType](xs: Iterable[Task[ResultType]])
//   transparent inline def sequence(using monitor: Monitor): Task[Iterable[ResultType]] throws CancelError =
//     Task(Text("sequence"))(xs.map(_.await()))

// enum TaskStatus:
//   case New, Running, Completed, Canceled, Failed, Expired

// object Task:
//   def apply[ResultType]
//            (id: Text)(fn: Monitor ?=> ResultType)(using monitor: Monitor, cancel: CanThrow[CancelError])
//            : {cancel, monitor} Task[ResultType] =
//     (new Task(id, (mon: Monitor) => fn(using mon)))

// class Task[ResultType](id: Text, calc: Monitor => ResultType)(using @annotation.constructorOnly monitor: Monitor):
//   private val result: Promise[ResultType] = Promise()
//   private lazy val thread: {calc} Thread = monitor.makeThread(runnable, context.name)
//   private lazy val context = monitor.child(id, thread.isInterrupted, thread.interrupt())
  
//   def name: Text = Text(monitor.name.s+"/"+id)
//   def await()(using cancel: CanThrow[CancelError]): ResultType = result.await().tap(thread.join().waive)
  
//   def await[DurationType](duration: DurationType)
//            (using genericDuration: GenericDuration { type Duration = DurationType },
//                 cancel: CanThrow[CancelError], timeout: CanThrow[TimeoutError])
//            : {cancel, timeout} ResultType =
//     result.await(duration).tap(thread.join().waive)

//   def cancel(): Unit = synchronized:
//     context.cancel()
//     result.cancel()

//   def map[ResultType2](fn: ResultType => ResultType2)(using monitor: Monitor, cancel: CanThrow[CancelError])
//          : {monitor, cancel} Task[ResultType2] =
//     Task(Text(s"${id}.map"))(fn(await()))
  
//   def flatMap[ResultType2](fn: ResultType => Task[ResultType2])
//              (using monitor: Monitor, cancel: CanThrow[CancelError])
//              : {monitor, cancel} Task[ResultType2] =
//     Task(Text(s"${id}.flatMap"))(fn(await()).await())
  
//   private def runnable: {calc} Runnable = () => safely:
//     try result.supply(calc(context))
//     catch
//       case err: TimeoutError => result.supply(throw err)
//       case err: Throwable    => result.supply(throw err)
    
//   override def toString: String = s"Task(${result.toString})"

// def accede(cleanup: => Unit = ())(using mon: Monitor): Unit =
//   import unsafeExceptions.canThrowAny
//   mon.accede(cleanup)
