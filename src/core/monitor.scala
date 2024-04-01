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
import digression.*

import scala.annotation.*
import scala.collection.mutable as scm

import language.experimental.captureChecking

import Completion.*

@capability
sealed trait Monitor(val stack: List[Codepoint], promise: Promise[?]):
  private val submonitors: scm.HashSet[AnyRef] = scm.HashSet()
  private def name: String = stack.map(_.text.s).mkString("supervisor://", "/", "")
  override def toString(): String = name

  def cancel(): Unit =
    submonitors.each:
      case child: Monitor => child.cancel()
      case _              => ()
    
    promise.cancel()
  
  def terminate(): Unit = this match
    case supervisor: Supervisor                         => supervisor.cancel()
    case monitor@Submonitor(id, parent, state, promise) => monitor.terminate()
  
  def attend(): Unit = promise.attend()
  def ready: Boolean = promise.ready
  
  def delegate(lambda: Monitor -> Unit): Unit = submonitors.each:
    case submonitor: Monitor => lambda(submonitor)
    case _                   => ()
  
  def sleep(duration: Long): Unit = Thread.sleep(duration)

  def child[ResultType2]
      (codepoint: Codepoint,
       state: Mutex[Completion[ResultType2]],
       promise: Promise[ResultType2 | Promise.Special])
          : Submonitor[ResultType2] =
    
    Submonitor[ResultType2](codepoint, this, state, promise).tap: submonitor =>
      submonitors += submonitor
  
  def remove(monitor: Submonitor[?]): Unit = synchronized:
    submonitors -= monitor

  def supervisor: Supervisor

sealed abstract class Supervisor() extends Monitor(Nil, Promise()):
  def fork(runnable: Runnable^): Thread^{runnable}
  def supervisor: Supervisor = this
  
  def newPlatformThread(name: Text, runnable: Runnable^): Thread^{runnable} =
    Thread.ofPlatform().nn.start(runnable).nn.tap(_.setName(name.s))

case object VirtualSupervisor extends Supervisor():
  def fork(runnable: Runnable^): Thread^{runnable} = Thread.ofVirtual().nn.start(runnable).nn
  
case object PlatformSupervisor extends Supervisor():
  def fork(runnable: Runnable^): Thread^{runnable} = Thread.ofPlatform().nn.start(runnable).nn

case object DaemonSupervisor extends Supervisor():
  def fork(runnable: Runnable^): Thread^{runnable} =
    new Thread(runnable).nn.tap: thread =>
      thread.setDaemon(true)
      thread.start()

def supervise[ResultType]
    (block: Monitor ?=> ResultType)(using model: ThreadModel)
        : ResultType raises ConcurrencyError =

  block(using model.supervisor())

@capability
case class Submonitor[ResultType]
    (frame: Codepoint,
     parent: Monitor,
     stateMutex: Mutex[Completion[ResultType]],
     promise: Promise[ResultType | Promise.Special])
extends Monitor(frame :: parent.stack, promise):

  def state(): Completion[ResultType] = stateMutex()
  def supervisor: Supervisor = parent.supervisor
  
  def relent(): Unit = stateMutex() match
    case Active            => ()
    case Initializing      => ()
    case Suspended(_)      => wait()
    case Completed(value)  => throw Panic(msg"should not be relenting after completion")
    case Delivered(value)  => throw Panic(msg"should not be relenting after completion")
    case Failed(error)     => throw Panic(msg"should not be relenting after failure")
