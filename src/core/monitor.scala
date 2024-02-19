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

import AsyncState.*

@capability
sealed trait Monitor(val name: List[Text], promise: Promise[?]):
  private val children: scm.HashMap[Text, AnyRef] = scm.HashMap()
  def id: Text = Text(name.reverse.map(_.s).mkString(" / "))

  def cancel(): Unit =
    promise.cancel()
    children.each: (id, child) =>
      child match
        case child: Monitor => child.cancel()
        case _              => ()

  def terminate(): Unit = this match
    case VirtualSupervisor                              => VirtualSupervisor.cancel()
    case PlatformSupervisor                             => PlatformSupervisor.cancel()
    case monitor@Submonitor(id, parent, state, promise) => monitor.terminate()

  def sleep(duration: Long): Unit = Thread.sleep(duration)

  def child
      [ResultType2]
      (id: Text, state: juca.AtomicReference[AsyncState[ResultType2]], promise: Promise[ResultType2 | Promise.Special])
      (using label: boundary.Label[Unit])
      : Submonitor[ResultType2] =
    
    val monitor = Submonitor[ResultType2](id, this, state, promise)
    
    synchronized:
      children(id) = monitor
    
    monitor

  def supervisor: Supervisor

sealed abstract class Supervisor() extends Monitor(Nil, Promise()):
  def newThread(runnable: Runnable^): Thread^{runnable}

case object VirtualSupervisor extends Supervisor():
  def newThread(runnable: Runnable^): Thread^{runnable} = Thread.ofVirtual().nn.start(runnable).nn
  def supervisor: Supervisor = this

case object PlatformSupervisor extends Supervisor():
  def newThread(runnable: Runnable^): Thread^{runnable} = Thread.ofPlatform().nn.start(runnable).nn
  def supervisor: Supervisor = this

def supervise
    [ResultType]
    (block: Monitor ?=> ResultType)(using cancel: Raises[CancelError], model: ThreadModel)
    : ResultType =
  block(using model.supervisor())

@capability
case class Submonitor
    [ResultType]
    (identifier: Text, parent: Monitor, stateRef: juca.AtomicReference[AsyncState[ResultType]], promise: Promise[ResultType | Promise.Special])
    (using label: boundary.Label[Unit])
extends Monitor(identifier :: parent.name, promise):

  def state(): AsyncState[ResultType] = stateRef.get().nn
  def supervisor: Supervisor = parent.supervisor
  
  def complete(value: ResultType): Nothing =
    stateRef.set(Completed(value))
    promise.offer(value)
    boundary.break()
  
  def acquiesce(): Unit = synchronized:
    stateRef.get().nn match
      case Active            => ()
      case Suspended(_)      => wait()
      case Completed(value)  => throw Panic(msg"should not be acquiescing after completion")
      case Failed(error)     => throw Panic(msg"should not be acquiescing after failure")
