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
import scala.collection.mutable as scm

import language.experimental.pureFunctions

import Completion.*

@capability
sealed trait Monitor(promise: Promise[?]):
  protected val submonitors: scm.HashSet[Monitor] = scm.HashSet()
  def name: Optional[Text]
  def stack: Text
  
  def terminate(): Unit = this match
    case supervisor: Supervisor    => supervisor.cancel()
    case submonitor: Submonitor[?] => submonitor.terminate()
  
  def attend(): Unit = promise.attend()
  def ready: Boolean = promise.ready
  
  def delegate(lambda: Monitor -> Unit): Unit = submonitors.each:
    case submonitor: Submonitor[?] =>
      if submonitor.daemon then submonitor.cancel() else lambda(submonitor)
    
    case _ => ()
  
  def sleep(duration: Long): Unit = Thread.sleep(duration)

  def child[ResultType2]
      (name: Optional[Text],
       daemon: Boolean,
       codepoint: Codepoint,
       mitigator: Mitigator,
       evaluate: Submonitor[ResultType2] => ResultType2,
       orphans: OrphanCompletion)
          : Submonitor[ResultType2] =

    val submonitor =
      Submonitor(codepoint, name, daemon, this, Mutex(Initializing), Promise(), mitigator, evaluate, orphans)
    
    submonitor.tap: submonitor =>
      submonitors += submonitor
  
  def cancel(): Unit
  def remove(monitor: Submonitor[?]): Unit = synchronized:
    submonitors -= monitor

  def supervisor: Supervisor

  def mitigate(monitor: Monitor, error: Throwable): Unit
  
sealed abstract class Supervisor() extends Monitor(Promise()):
  def name: Text
  def fork(runnable: Runnable): Thread
  def supervisor: Supervisor = this
  def stack: Text = name+":".tt
  def cancel(): Unit = ()
  
  def newPlatformThread(name: Text, runnable: Runnable): Thread =
    Thread.ofPlatform().nn.start(runnable).nn.tap(_.setName(name.s))

case object VirtualSupervisor extends Supervisor():
  def name: Text = "virtual".tt
  def fork(runnable: Runnable): Thread = Thread.ofVirtual().nn.start(runnable).nn
  def mitigate(monitor: Monitor, error: Throwable): Unit = ()
  
case object PlatformSupervisor extends Supervisor():
  def name: Text = "platform".tt
  def fork(runnable: Runnable): Thread = Thread.ofPlatform().nn.start(runnable).nn
  def mitigate(monitor: Monitor, error: Throwable): Unit = ()

def supervise[ResultType](block: Monitor ?=> ResultType)(using model: ThreadModel)
        : ResultType raises ConcurrencyError =

  block(using model.supervisor())


@capability
case class Submonitor[ResultType]
    (frame:     Codepoint,
     name:      Optional[Text],
     daemon:    Boolean,
     parent:    Monitor,
     state:     Mutex[Completion[ResultType]],
     promise:   Promise[ResultType],
     mitigator: Mitigator,
     evaluate:  Submonitor[ResultType] => ResultType,
     orphans:   OrphanCompletion)
extends Monitor(promise):

  def supervisor: Supervisor = parent.supervisor

  def stack: Text =
    val prefix = parent match
      case supervisor: Supervisor    => supervisor.name.s+":/"
      case submonitor: Submonitor[?] => submonitor.stack.s
    
    val ref = name.lay(frame.text.s)(_.s+"@"+frame.text.s)
    
    (prefix+"#"+ref).tt
  
  def relent(): Unit = state() match
    case Initializing      => ()
    case Active            => ()
    case Suspended(_)      => wait()
    case Completed(value)  => throw Panic(msg"should not be relenting after completion")
    case Delivered(value)  => throw Panic(msg"should not be relenting after completion")
    case Failed(error)     => throw Panic(msg"should not be relenting after failure")

  def mitigate(monitor: Monitor, error: Throwable): Unit =
    mitigator.mitigate(monitor, error) match
      case Mitigation.Escalate => parent.mitigate(monitor, error)
      case Mitigation.Cancel   => cancel()
      case Mitigation.Suppress => ()

  lazy val thread: Thread =
    val runnable: Runnable = () =>
      boundary[Unit]:
        try
          state() = Active
          evaluate(this).tap: result =>
            state() = Completed(result)
        catch
          case error: Throwable =>
            mitigate(this, error)
            state() = Failed(error)
        finally
          orphans.cleanup(this)
          
          state() match
            case Completed(value) => promise.offer(value)
            case Active           => promise.cancel()
            case Suspended(_)     => promise.cancel()
            case Failed(_)        => promise.cancel()
          
          parent.remove(this)
          boundary.break()
  
    parent.supervisor.fork(runnable)

  def join(): Unit =
    try thread.join() catch case _: InterruptedException => Thread.currentThread.nn.interrupt()

  def cancel(): Unit =
    thread.interrupt()
    promise.cancel()
    submonitors.each(_.cancel())
    join()
  
  def result()(using cancel: Raises[ConcurrencyError]): ResultType = state() match
    case Initializing      => abort(ConcurrencyError(ConcurrencyError.Reason.Incomplete))
    case Active            => abort(ConcurrencyError(ConcurrencyError.Reason.Incomplete))
    case Suspended(_)      => abort(ConcurrencyError(ConcurrencyError.Reason.Incomplete))
    case Completed(result) => result.also { state() = Delivered(result) }
    case Delivered(result) => result
    case Failed(error)     => throw error
