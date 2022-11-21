/*
    Parasitism, version 0.4.0. Copyright 2022-22 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package parasitism

import rudiments.*
import anticipation.*

import scala.util.*

package monitors:
  given global: Monitor = Supervisor()

case class CancelError() extends Error(err"the operation was cancelled")
case class IncompleteError() extends Error(err"the task was not completed")
case class AlreadyCompleteError() extends Error(err"the promise was already completed")

case class Supervisor(baseId: Text = Text("main")) extends Monitor:
  @volatile
  private var interrupted: Boolean = false

  def name: Text = Text("task://"+baseId)
  def id: Text = name
  def continue = !interrupted
  def cancel(): Unit = interrupted = true

def supervise[T](id: Text)(fn: Monitor ?=> T): T = fn(using Supervisor(id))
def hibernate()(using Monitor): Unit = sleep(using timekeeping.long)(Long.MaxValue)

def sleep(using tk: Timekeeper)(time: tk.Type)(using Monitor): Unit =
  try Thread.sleep(tk.to(time)) catch case err: InterruptedException => unsafely(throw CancelError())

@implicitNotFound("""|parasitism: a contextual Monitor instance is required, for example:
                     |    import monitors.global  // a top-level supervisor for asynchronous tasks""".stripMargin)
trait Monitor:
  def id: Text
  def name: Text
  def continue: Boolean
  def cancel(): Unit
  
  final def bail(cleanup: => Unit = ()): Unit =
    import unsafeExceptions.canThrowAny
    if !continue then cleanup.pipe((throw CancelError()).waive)

  def child(id: Text, check: => Boolean, abort: => Unit): Monitor =
    TaskMonitor(id, () => check, () => abort, this)

case class TaskMonitor(id: Text, interrupted: () => Boolean, stop: () => Unit, parent: Monitor) extends Monitor:
  def name: Text = Text(parent.name.s+"/"+id)
  def continue: Boolean = !interrupted() && parent.continue
  def cancel(): Unit = stop()

case class Promise[T]():
  @volatile
  private var value: Option[T throws CancelError] = None

  @volatile
  private var triggers: List[() => Unit] = Nil
  
  def ready: Boolean = !value.isEmpty
  
  def supply(v: T): Unit throws AlreadyCompleteError = synchronized:
    if value.isEmpty then
      value = Some(v)
      notifyAll()
      triggers.foreach(_())
    else throw AlreadyCompleteError()
  
  def trigger(fn: => Unit): Unit = triggers ::= (() => fn)

  def await(): T throws CancelError = synchronized:
    while value.isEmpty do wait()
    value.get

  def await(using tk: Timekeeper)(time: tk.Type): T throws CancelError | TimeoutError = synchronized:
    if value.isEmpty then
      wait(tk.to(time))
      if value.isEmpty then throw TimeoutError() else value.get
    else value.get

  def cancel(): Unit = synchronized:
    if value.isEmpty then value = Some(throw CancelError())

  def get: T throws IncompleteError | CancelError = value.getOrElse(throw IncompleteError())

  override def toString: String =
    try value.fold("[incomplete]")(_.toString) catch case err: CancelError => "[canceled]"

extension [T](xs: Iterable[Task[T]])
  transparent inline def sequence(using mon: Monitor, threading: Threading): Task[Iterable[T]] =
    Task(Text("sequence"))(xs.map(_.await()))

enum TaskStatus:
  case New, Running, Completed, Canceled, Failed, Expired

package threading:
  // given platform: Threading = (runnable, name) => Thread.ofPlatform.nn.name(name.s).nn.start(runnable).nn
  given virtual: Threading = (runnable, name) => Thread.ofVirtual.nn.name(name.s).nn.start(runnable).nn
  given platform: Threading = (runnable, name) => Thread(runnable, name.s).tap(_.start())

@implicitNotFound("""|parasitism: a contextual Threading instance is required to create new asynchronous tasks, typically
                     |one of:
                     |    import threading.virtual   // use lightweight virtual threads in newer JVMs
                     |    import threading.platform  // use OS threads""".stripMargin)
trait Threading:
  def apply(runnable: Runnable, name: Text): Thread

object Task:
  def apply[T](id: Text)(fn: CanThrow[CancelError] ?=> Monitor ?=> T)
              (using monitor: Monitor, threading: Threading): Task[T] =
    (new Task(id, mon => fn(using unsafeExceptions.canThrowAny)(using mon))(using monitor)).tap(_.start())

class Task[T](id: Text, calc: Monitor => T)(using mon: Monitor, threading: Threading):
  private var startTime: Long = 0L
  private var status: TaskStatus = TaskStatus.New
  def name = Text(mon.name.s+"/"+id)
  //def active: Boolean = !result.ready
  def await(): T throws CancelError = result.await().tap(thread.join().waive)
  
  def await(using tk: Timekeeper)(time: tk.Type): T throws CancelError | TimeoutError =
    result.await(time).tap(thread.join().waive)
  
  private def start(): Promise[T] = //synchronized:
    // if startTime == 0 then
    //   startTime = System.currentTimeMillis
    //   status = TaskStatus.Running
    //   thread.start()
    thread
    result

  def cancel(): Unit = synchronized:
    ctx.cancel()
    result.cancel()
    status = TaskStatus.Canceled

  def map[S](fn: T => S)(using mon: Monitor): Task[S] = Task(Text(s"${id}.map"))(fn(await()))
  
  def flatMap[S](fn: T => Task[S])(using mon: Monitor): Task[S] =
    Task(Text(s"${id}.flatMap"))(fn(await()).await())
  
  private val result: Promise[T] = Promise()
  private lazy val thread: Thread = threading(runnable, ctx.name)
  private lazy val ctx = mon.child(id, thread.isInterrupted, thread.interrupt())
  
  private def runnable: Runnable = () => safely:
    try
      result.supply(calc(ctx))
      status = TaskStatus.Completed
    catch
      case err: TimeoutError =>
        result.supply(throw err)
        status = TaskStatus.Expired

      case err: Throwable =>
        result.supply(throw err)
        status = TaskStatus.Failed
    
  override def toString: String = s"Task(${result.toString})"

def bail(cleanup: => Unit = ())(using mon: Monitor): Unit =
  import unsafeExceptions.canThrowAny
  mon.bail(cleanup)