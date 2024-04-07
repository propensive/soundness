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
  protected[parasite] var subordinates: Set[Subordinate] = Set()

  def name: Optional[Text]
  def stack: Text
  def daemon: Boolean
  def attend(): Unit = promise.attend()
  def ready: Boolean = promise.ready
  def cancel(): Unit
  def remove(monitor: Subordinate): Unit = subordinates -= monitor
  def supervisor: Supervisor
  def mitigate(monitor: Monitor, error: Throwable): Unit
  def sleep(duration: Long): Unit = Thread.sleep(duration)
  
  def subordinate[ResultType]
     (name:      Optional[Text],
      daemon:    Boolean,
      codepoint: Codepoint,
      mitigator: Mitigator,
      probate:   Probate)
     (eval: Subordinate => ResultType)
          : SubordinateTask[ResultType] =

    SubordinateTask[ResultType](codepoint, name, daemon, this, mitigator, probate, eval)
    
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

  def shutdown(): Unit = subordinates.each(_.cancel())

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
  private val state: Mutex[Completion[Result]] = Mutex(Completion.Initializing)
  def evaluate(subordinate: Subordinate): Result
  val promise: Promise[Result] = Promise()
  def supervisor: Supervisor = parent.supervisor
  def apply(): Optional[Result] = promise()

  def delegate(lambda: Monitor -> Unit): Unit = state.replace: state =>
    subordinates.each { child => if child.daemon then child.cancel() else lambda(child) }
    state

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
    case Cancelled       => throw Panic(msg"should not be relenting after cancellation")

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
    val state2 = state.replace:
      case Initializing | Active(_) | Suspended(_, _) =>
        thread.interrupt()
        promise.cancel()
        Cancelled
      
      case other =>
        other
    
    if state2 == Cancelled then thread.join()
      
  
  def result()(using cancel: Raises[ConcurrencyError]): Result =
    val delivered = state.replace:
      case Initializing =>
        abort(ConcurrencyError(ConcurrencyError.Reason.Incomplete))
  
      case Active(_) =>
        abort(ConcurrencyError(ConcurrencyError.Reason.Incomplete))
  
      case Suspended(_, _) =>
        abort(ConcurrencyError(ConcurrencyError.Reason.Incomplete))
  
      case Completed(duration, result) =>
        Delivered(duration, result)
  
      case Delivered(duration, result) =>
        Delivered(duration, result)
  
      case Failed(error) =>
        throw error
  
      case Cancelled =>
        abort(ConcurrencyError(ConcurrencyError.Reason.Cancelled))
    
    delivered match
      case Delivered(_, result) => result
      case other                => throw Panic(msg"impossible state")
    

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

  private lazy val thread: Thread = parent.supervisor.fork(stack):
    boundary[Unit]:
      try
        state.replace:
          case Initializing =>
            parent.subordinates += this
            Active(System.currentTimeMillis)
          
          case other =>
            throw InterruptedException()

        evaluate(this).tap: result =>
          state.replace:
            case Active(startTime) =>
              Completed(System.currentTimeMillis - startTime, result)
            
            case other =>
              other
      
      catch
        case error: InterruptedException =>
          Thread.interrupted()
          
          val state2 = state.replace:
            case Initializing | Active(_) | Cancelled | Suspended(_, _) => Cancelled
            case state@Completed(_, _)                                  => state
            case state@Delivered(_, _)                                  => state
            case state@Failed(_)                                        => state
          
          state2 match
            case Cancelled => subordinates.each { child => if child.daemon then child.cancel() }
            case _         => ()

        case error: Throwable =>
          mitigate(this, error)
          state() = Failed(error)
          subordinates.each { child => if child.daemon then child.cancel() }
      
      finally
        probate.cleanup(this)
        
        state.replace: state =>
          parent.remove(this)
          state match
            case Initializing                     => Cancelled.also(promise.cancel())
            case Active(_)                        => Cancelled.also(promise.cancel())
            case state@Completed(duration, value) => state.also(promise.offer(value))
            case state@Delivered(duration, value) => state
            case Suspended(_, _)                  => Cancelled.also(promise.cancel())
            case state@Failed(_)                  => state.also(promise.cancel())
            case Cancelled                        => Cancelled

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
