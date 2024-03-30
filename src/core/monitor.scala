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

import scala.annotation.*
import scala.collection.mutable as scm

import java.util.concurrent.atomic as juca

import language.experimental.captureChecking

import Completion.*

@capability
sealed trait Monitor(val name: List[Text], promise: Promise[?]):
  private val childSet: scm.HashSet[AnyRef] = scm.HashSet()

  def cancel(): Unit =
    childSet.each:
      case child: Monitor => child.cancel()
      case _              => ()
    
    promise.cancel()

  def terminate(): Unit = this match
    case supervisor: Supervisor                         => supervisor.cancel()
    case monitor@Submonitor(id, parent, state, promise) => monitor.terminate()
  
  def attend(): Unit =
    childSet.each:
      case monitor: Monitor => monitor.attend()
  
  def delegate(lambda: Monitor -> Unit): Unit =
    childSet.each:
      case monitor: Monitor => lambda(monitor)
      case _                => ()
  
  def sleep(duration: Long): Unit = Thread.sleep(duration)

  def child[ResultType2]
      (id: Text,
       state: juca.AtomicReference[Completion[ResultType2]],
       promise: Promise[ResultType2 | Promise.Special])
      (using label: boundary.Label[Unit])
          : Submonitor[ResultType2] =
    
    val monitor = Submonitor[ResultType2](id, this, state, promise)
    
    synchronized:
      childSet += monitor
    
    monitor

  def supervisor: Supervisor

sealed abstract class Supervisor() extends Monitor(Nil, Promise()):
  def newThread(runnable: Runnable^): Thread^{runnable}
  def supervisor: Supervisor = this
  
  def newPlatformThread(name: Text, runnable: Runnable^): Thread^{runnable} =
    Thread.ofPlatform().nn.start(runnable).nn.tap(_.setName(name.s))

case object VirtualSupervisor extends Supervisor():
  def newThread(runnable: Runnable^): Thread^{runnable} = Thread.ofVirtual().nn.start(runnable).nn
  
case object PlatformSupervisor extends Supervisor():
  def newThread(runnable: Runnable^): Thread^{runnable} = Thread.ofPlatform().nn.start(runnable).nn

case object DaemonSupervisor extends Supervisor():
  def newThread(runnable: Runnable^): Thread^{runnable} =
    new Thread(runnable).nn.tap: thread =>
      thread.setDaemon(true)
      thread.start()

def supervise[ResultType](block: Monitor ?=> ResultType)(using cancel: Raises[CancelError], model: ThreadModel)
        : ResultType =

  block(using model.supervisor())

@capability
case class Submonitor[ResultType]
    (identifier: Text,
     parent: Monitor,
     stateRef: juca.AtomicReference[Completion[ResultType]],
     promise: Promise[ResultType | Promise.Special])
    (using label: boundary.Label[Unit])
extends Monitor(identifier :: parent.name, promise):

  def state(): Completion[ResultType] = stateRef.get().nn
  def supervisor: Supervisor = parent.supervisor
  
  def complete(value: ResultType): Nothing =
    stateRef.set(Completed(value))
    promise.offer(value)
    boundary.break()
  
  def relent(): Unit = //synchronized:
    stateRef.get().nn match
      case Active            => ()
      case Suspended(_)      => wait()
      case Completed(value)  => throw Panic(msg"should not be acquiescing after completion")
      case Failed(error)     => throw Panic(msg"should not be acquiescing after failure")
