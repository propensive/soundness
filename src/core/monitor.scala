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
import fulminate.*
import feudalism.*
import vacuous.*
import digression.*

import scala.annotation.*

import language.experimental.pureFunctions

import Completion.*

@capability
sealed trait Monitor:
  type Result
  val promise: Promise[Result]
  protected val semaphore: Semaphore = Semaphore()
  protected var subordinates: Set[Subordinate] = Set()

  def name: Optional[Text]
  def stack: Text
  def daemon: Boolean
  def attend(): Unit = promise.attend()
  def ready: Boolean = promise.ready
  def cancel(): Unit
  def remove(monitor: Subordinate): Unit = semaphore.isolate(subordinates -= monitor)
  def supervisor: Supervisor
  def mitigate(monitor: Monitor, error: Throwable): Unit
  def sleep(duration: Long): Unit = Thread.sleep(duration)
  
  def delegate(lambda: Monitor -> Unit): Unit = semaphore.access:
    subordinates.each { child => if child.daemon then child.cancel() else lambda(child) }

  def subordinate[ResultType]
     (name:      Optional[Text],
      daemon:    Boolean,
      codepoint: Codepoint,
      mitigator: Mitigator,
      probate:   Probate)
     (eval: Subordinate => ResultType)
          : SubordinateTask[ResultType] =

    val submonitor: SubordinateTask[ResultType] =
      SubordinateTask[ResultType](codepoint, name, daemon, this, mitigator, probate, eval)
    
    submonitor.tap: submonitor =>
      semaphore.isolate(subordinates += submonitor)

sealed abstract class Supervisor() extends Monitor:
  type Result = Unit
  val promise: Promise[Unit] = Promise()
  val daemon: Boolean = true
  def name: Text
  def fork(name: Optional[Text])(block: => Unit): Thread
  def supervisor: Supervisor = this
  def stack: Text = name+":".tt
  def cancel(): Unit = ()
  
  def newPlatformThread(name: Text, runnable: Runnable): Thread =
    Thread.ofPlatform().nn.start(runnable).nn.tap(_.setName(name.s))

  def shutdown(): Unit = semaphore.access:
    subordinates.each(_.cancel())

object VirtualSupervisor extends Supervisor():
  def name: Text = "virtual".tt
  def mitigate(monitor: Monitor, error: Throwable): Unit = ()
  
  def fork(name: Optional[Text])(block: => Unit): Thread =
    Thread.ofVirtual().nn.start(() => block).nn
  
object PlatformSupervisor extends Supervisor():
  def name: Text = "platform".tt
  def mitigate(monitor: Monitor, error: Throwable): Unit = ()
  
  def fork(name: Optional[Text])(block: => Unit): Thread =
    Thread.ofPlatform().nn.start(() => block).nn.tap: thread =>
      name.let(_.s).let(thread.setName(_))

def supervise[ResultType](block: Monitor ?=> ResultType)(using model: ThreadModel)
        : ResultType raises ConcurrencyError =

  block(using model.supervisor())

@capability
abstract class Subordinate
    (frame: Codepoint, parent: Monitor, mitigator: Mitigator, probate: Probate)
extends Monitor:
  def evaluate(subordinate: Subordinate): Result
  val promise: Promise[Result] = Promise()
  def supervisor: Supervisor = parent.supervisor
  def apply(): Optional[Result] = promise()

  def stack: Text =
    val ref = name.lay(frame.text.s)(_.s+"@"+frame.text.s)
    
    parent match
      case supervisor: Supervisor  => (supervisor.name.s+"://"+ref).tt
      case submonitor: Subordinate => (submonitor.stack.s+"//"+ref).tt

  def relent(): Unit = state() match
    case Initializing    => ()
    case Active(_)       => ()
    case Suspended(_, _) => synchronized(wait())
    case Completed(_, _) => throw Panic(msg"should not be relenting after completion")
    case Delivered(_, _) => throw Panic(msg"should not be relenting after completion")
    case Failed(_)       => throw Panic(msg"should not be relenting after failure")

  def mitigate(monitor: Monitor, error: Throwable): Unit =
    mitigator.mitigate(monitor, error) match
      case Mitigation.Escalate => parent.mitigate(monitor, error)
      case Mitigation.Cancel   => cancel()
      case Mitigation.Suppress => ()

  def map[ResultType2](lambda: Result => ResultType2)(using Monitor, Probate, Mitigator)
          : Task[ResultType2] raises ConcurrencyError =

    async(lambda(await()))
  
  def flatMap[ResultType2](lambda: Result => Task[ResultType2])
      (using Monitor, Probate, Mitigator)
          : Task[ResultType2] raises ConcurrencyError =

    async(lambda(await()).await())

  def cancel(): Unit =
    thread.interrupt()
    thread.join()
  
  def result()(using cancel: Raises[ConcurrencyError]): Result = state() match
    case Initializing                => abort(ConcurrencyError(ConcurrencyError.Reason.Incomplete))
    case Active(_)                   => abort(ConcurrencyError(ConcurrencyError.Reason.Incomplete))
    case Suspended(_, _)             => abort(ConcurrencyError(ConcurrencyError.Reason.Incomplete))
    case Completed(duration, result) => result.also { state() = Delivered(duration, result) }
    case Delivered(_, result)        => result
    case Failed(error)               => throw error

  def await[DurationType: GenericDuration](duration: DurationType)
          : Result raises ConcurrencyError =

    promise.attend(duration)
    thread.join()
    result()

  def await(): Result raises ConcurrencyError =
    promise.attend()
    thread.join()
    result()

  def suspend(): Unit = state.replace:
    case Active(startTime)       => Suspended(startTime, 1)
    case Suspended(startTime, n) => Suspended(startTime, n + 1)
    case other                   => other

  def resume(force: Boolean = false): Unit = state.replace:
    case Suspended(startTime, 1) => Active(startTime).also(synchronized(notifyAll()))
    case Suspended(startTime, n) => if force then Active(startTime) else Suspended(startTime, n - 1)
    case other                   => other
  
  private val state: Mutex[Completion[Result]] = Mutex(Completion.Initializing)

  private lazy val thread: Thread = parent.supervisor.fork(stack):
    boundary[Unit]:
      val startTime = System.currentTimeMillis
      try
        state() = Active(startTime)

        evaluate(this).tap: result =>
          state() = Completed(System.currentTimeMillis - startTime, result)
      
      catch
        case error: InterruptedException =>
          state() = Failed(error)
          subordinates.each { child => if child.daemon then child.cancel() }
          Thread.interrupted()

        case error: Throwable =>
          mitigate(this, error)
          state() = Failed(error)
          subordinates.each { child => if child.daemon then child.cancel() }
      
      finally
        probate.cleanup(this)
        val runTime = System.currentTimeMillis - startTime
        
        state() match
          case Initializing               => promise.cancel()
          case Active(_)                  => promise.cancel()
          case Completed(duration, value) => promise.offer(value)
          case Delivered(duration, value) => ()
          case Suspended(_, _)            => promise.cancel()
          case Failed(_)                  => promise.cancel()
        
        parent.remove(this)
        boundary.break()
  
  thread

@capability
class SubordinateTask[ResultType]
    (frame: Codepoint,
     val name: Optional[Text],
     val daemon: Boolean,
     parent: Monitor,
     mitigator: Mitigator,
     probate: Probate,
     eval: Subordinate => ResultType)
extends Subordinate(frame, parent, mitigator, probate), Task[ResultType]:
  type Result = ResultType
  def evaluate(subordinate: Subordinate): Result = eval(subordinate)
