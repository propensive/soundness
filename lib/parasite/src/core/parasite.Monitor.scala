                                                                                                  /*
┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓
┃                                                                                                  ┃
┃                                                   ╭───╮                                          ┃
┃                                                   │   │                                          ┃
┃                                                   │   │                                          ┃
┃   ╭───────╮╭─────────╮╭───╮ ╭───╮╭───╮╌────╮╭────╌┤   │╭───╮╌────╮╭────────╮╭───────╮╭───────╮   ┃
┃   │   ╭───╯│   ╭─╮   ││   │ │   ││   ╭─╮   ││   ╭─╮   ││   ╭─╮   ││   ╭─╮  ││   ╭───╯│   ╭───╯   ┃
┃   │   ╰───╮│   │ │   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   ╰─╯  ││   ╰───╮│   ╰───╮   ┃
┃   ╰───╮   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   ╭────╯╰───╮   │╰───╮   │   ┃
┃   ╭───╯   ││   ╰─╯   ││   ╰─╯   ││   │ │   ││   ╰─╯   ││   │ │   ││   ╰────╮╭───╯   │╭───╯   │   ┃
┃   ╰───────╯╰─────────╯╰────╌╰───╯╰───╯ ╰───╯╰────╌╰───╯╰───╯ ╰───╯╰────────╯╰───────╯╰───────╯   ┃
┃                                                                                                  ┃
┃    Soundness, version 0.54.0.                                                                    ┃
┃    © Copyright 2021-25 Jon Pretty, Propensive OÜ.                                                ┃
┃                                                                                                  ┃
┃    The primary distribution site is:                                                             ┃
┃                                                                                                  ┃
┃        https://soundness.dev/                                                                    ┃
┃                                                                                                  ┃
┃    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file     ┃
┃    except in compliance with the License. You may obtain a copy of the License at                ┃
┃                                                                                                  ┃
┃        https://www.apache.org/licenses/LICENSE-2.0                                               ┃
┃                                                                                                  ┃
┃    Unless required by applicable law or agreed to in writing,  software distributed under the    ┃
┃    License is distributed on an "AS IS" BASIS,  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,    ┃
┃    either express or implied. See the License for the specific language governing permissions    ┃
┃    and limitations under the License.                                                            ┃
┃                                                                                                  ┃
┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛
                                                                                                  */
package parasite

import language.experimental.pureFunctions

import java.lang as jl
import java.util.concurrent.atomic as juca
import java.util.concurrent.locks as jucl

import scala.annotation.*

import anticipation.*
import contingency.*
import digression.*
import fulminate.*
import prepositional.*
import proscenium.*
import rudiments.*
import symbolism.*
import vacuous.*

import AsyncError.Reason
import Fulfillment.*

import unsafeExceptions.canThrowAny

sealed trait Monitor extends Resultant:
  val promise: Promise[Result]

  protected[parasite] val workersRef: juca.AtomicReference[Set[Worker]] =
    juca.AtomicReference(Set())

  protected[parasite] def workers: Set[Worker] = workersRef.get().nn
  protected[parasite] def addWorker(worker: Worker): Unit =
    workersRef.updateAndGet(_.nn + worker)

  def name: Optional[Text]
  def chain: Optional[Chain]
  def stack: Text
  def daemon: Boolean
  def attend(): Unit = promise.attend()
  def ready: Boolean = promise.ready
  def cancel(): Unit
  def remove(monitor: Worker): Unit = workersRef.updateAndGet(_.nn - monitor)
  def supervisor: Supervisor

  def snooze[generic: Abstractable across Durations to Long](duration: generic): Unit =
    jucl.LockSupport.parkNanos(duration.generic)

sealed abstract class Supervisor() extends Monitor:
  type Result = Unit

  def chain: Optional[Chain] = Unset

  val promise: Promise[Unit] = Promise()
  val daemon: Boolean = true

  def name: Text
  def fork(name: Optional[Text])(block: => Unit): Thread
  def supervisor: Supervisor = this
  def stack: Text = name+":".tt
  def cancel(): Unit = ()
  def shutdown(): Unit = workers.each(_.cancel())

object VirtualSupervisor extends Supervisor():
  def name: Text = "virtual".tt

  def fork(name: Optional[Text])(block: => Unit): Thread =
    Thread.ofVirtual().nn.start(() => block).nn

object AdaptiveSupervisor extends Supervisor():
  def name: Text = "adaptive".tt

  def fork(name: Optional[Text])(block: => Unit): Thread =
    try VirtualSupervisor.fork(name)(block) catch case error: Throwable =>
      PlatformSupervisor.fork(name)(block)

object PlatformSupervisor extends Supervisor():
  def name: Text = "platform".tt

  def fork(name: Optional[Text])(block: => Unit): Thread =
    val runnable: Runnable = () => block
    new Thread(runnable).tap: thread =>
      name.let(_.s).let(thread.setName(_))
      thread.start()

abstract class Worker(frame: Codepoint, parent: Monitor, codicil: Codicil) extends Monitor:
  self =>
  private val state: juca.AtomicReference[Fulfillment[Result]] =
    juca.AtomicReference(Fulfillment.Initializing)

  private var relents: Int = 1

  private val startTime: Long = jl.System.currentTimeMillis
  val promise: Promise[Result] = Promise()

  parent.addWorker(this)

  def chain: Chain = Chain(frame, parent.chain)
  def evaluate(worker: Worker): Result
  def supervisor: Supervisor = parent.supervisor
  def apply(): Optional[Result] = promise()
  def relentlessness: Double = (jl.System.currentTimeMillis - startTime).toDouble/relents

  def delegate(lambda: Monitor -> Unit): Unit = state.updateAndGet: state =>
    workers.each { child => if child.daemon then child.cancel() else lambda(child) }
    state

  def stack: Text =
    val ref = name.lay(frame.text.s)(_.s+"@"+frame.text.s)

    parent match
      case supervisor: Supervisor => (supervisor.name.s+"://"+ref).tt
      case submonitor: Worker     => (submonitor.stack.s+"//"+ref).tt

  def relent(): Unit =
    relents += 1
    if Thread.interrupted() then throw new InterruptedException()

    state.get().nn match
      case Initializing    => ()
      case Active(_)       => ()
      case Completed(_, _) => panic(m"should not be relenting after completion")
      case Delivered(_, _) => panic(m"should not be relenting after completion")
      case Failed(_)       => panic(m"should not be relenting after failure")
      case Cancelled       => throw new InterruptedException()

  override def snooze[generic: Abstractable across Durations to Long](duration: generic): Unit =
    if Thread.interrupted() || state.get() == Cancelled then throw new InterruptedException()
    jucl.LockSupport.parkNanos(duration.generic)
    if Thread.interrupted() || state.get() == Cancelled then throw new InterruptedException()


  def map[result2](lambda: Result => result2)(using Monitor, Codicil)
  :   Task[result2] raises AsyncError =

    async(lambda(await()))


  def bind[result2](lambda: Result => Task[result2])(using Monitor, Codicil)
  :   Task[result2] raises AsyncError =

    async(lambda(await()).await())


  def cancel(): Unit =
    val state2 = state.updateAndGet:
      case Initializing | Active(_) =>
        promise.cancel()
        thread.interrupt()
        Cancelled

      case other =>
        other

    if state2 == Cancelled then thread.join()

  def result()(using cancel: Tactic[AsyncError]): Result =
    state.updateAndGet:
      case null                        => abort(AsyncError(Reason.Incomplete))
      case Initializing                => abort(AsyncError(Reason.Incomplete))
      case Active(_)                   => abort(AsyncError(Reason.Incomplete))
      case Completed(duration, result) => Delivered(duration, result)
      case Delivered(duration, result) => Delivered(duration, result)
      case Failed(error)               => throw error
      case Cancelled                   => abort(AsyncError(Reason.Cancelled))

    . match
        case Delivered(_, result) => result
        case other                => panic(m"impossible state")


  def await[abstractable: Abstractable across Durations to Long](duration: abstractable)
  :   Result raises AsyncError =

    promise.attend(duration)
    if !promise.ready then abort(AsyncError(Reason.Timeout))
    thread.join()
    result()


  def await(): Result raises AsyncError =
    promise.attend()
    thread.join()
    result()

  private lazy val thread: Thread = parent.supervisor.fork(stack):
    val started: Boolean = state.updateAndGet:
      case Initializing => Active(jl.System.currentTimeMillis)
      case other        => other
    match
      case Active(_) => true
      case _         => false

    try
      if started then evaluate(this).tap: result =>
        state.updateAndGet:
          case Active(startTime) => Completed(jl.System.currentTimeMillis - startTime, result)
          case other             => other

    catch
      case error: InterruptedException =>
        Thread.interrupted()

        state.updateAndGet:
          case Initializing | Active(_) | Cancelled => Cancelled
          case state                                => state

        . match
            case Cancelled => workers.each { child => if child.daemon then child.cancel() }
            case _         => ()

      case error: Throwable =>
        state.set(Failed(error))

    finally
      try codicil.cleanup(this) catch case error: Throwable => state.set(Failed(error))

      state.updateAndGet: state =>
        parent.remove(this)
        state match
          case null                             => Cancelled.also(promise.cancel())
          case Initializing                     => Cancelled.also(promise.cancel())
          case Active(_)                        => Cancelled.also(promise.cancel())
          case state@Completed(duration, value) => state.also(promise.offer(value))
          case state@Delivered(duration, value) => state
          case state@Failed(_)                  => state.also(promise.cancel())
          case Cancelled                        => Cancelled

  thread
