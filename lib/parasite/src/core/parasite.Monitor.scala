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
import nomenclature.*
import prepositional.*
import rudiments.*
import symbolism.*
import vacuous.*

import Async.nominative
import AsyncError.Reason
import Fulfillment.*
import beneficence.*
import unsafeExceptions.canThrowAny

sealed trait Monitor extends Resultant, Findable, caps.ExclusiveCapability:
  self: Monitor^ =>
  val promise: Promise[Result]

  // The supervision scope owns its child-fate/error-trap policy. Every child `Worker` reaches its
  // `probate` *through* its parent monitor (`Worker#probate = parent.probate`); the capture is tied
  // to `{this}` (not a `fresh` per-call result) so a scope's probate is part of *its* footprint. A
  // `contain` region overrides this by spawning under a child `Subscope` with its own policy.
  def probate: Probate^{this}

  // The live children of this scope. A supervision registry is a *mutable capability collection*
  // whose contents are fresh worker identities created over time; tracking that precisely is the
  // "growing capture set" case that capture checking currently delegates to mutation/separation
  // tracking (still under development), and separation checking itself rejects the aliasing a
  // supervisor needs (a worker is held at once by its thread, its parent's registry, and its
  // caller's handle). So this collection is kept untracked: workers are stored boxed as pure
  // `Worker`, with the `^` dropped at the `addWorker`/`remove` boundary. This is the single capture
  // escape in the supervision core; sound here because the registry is private bookkeeping that
  // never leaks a worker's captures outward, and a worker's lifetime is bounded by this very scope.
  protected[parasite] val workersRef: juca.AtomicReference[Set[Worker^{}]] =
    juca.AtomicReference[Set[Worker^{}]](Set())

  protected[parasite] def workers: Set[Worker^{}] = workersRef.get().nn

  protected[parasite] def addWorker(worker: Worker^): Unit =
    workersRef.updateAndGet(_.nn + caps.unsafe.unsafeAssumePure(worker))

  protected[parasite] def remove(monitor: Worker^): Unit =
    workersRef.updateAndGet(_.nn - caps.unsafe.unsafeAssumePure(monitor))

  def name: Optional[Name[Async]]
  def chain: Optional[Chain]
  def stack: Text
  def daemon: Boolean
  def attend(): Unit = promise.attend()
  def ready: Boolean = promise.ready
  def cancel(): Unit
  def supervisor: Supervisor

  def snooze[generic: Abstractable across Durations to Long](duration: generic): Unit =
    jucl.LockSupport.parkNanos(duration.generic)

// The thread-forking strategy. DECOUPLED from `Monitor` (see capture-checking-capabilities notes):
// the global strategy singletons (`PlatformSupervisor` etc.) are plain values, NOT capabilities, so
// they can be referenced anywhere; the supervision tree (capability-tracked `Monitor`s) is rooted
// locally per `supervise` block by a `Root`.
trait Supervisor:
  def name: Name[Async]
  def fork(name: Optional[Text])(block: => Unit): Thread

// The local root of a supervision tree, created by `supervise`. A `Monitor` (hence a capability),
// but its lifetime is the `supervise` block, so it does not escape as a global capability.
class Root(val supervisor: Supervisor, rootProbate: Probate^) extends Monitor:
  type Result = Unit

  val probate: Probate^{this} = rootProbate
  def chain: Optional[Chain] = Unset
  val promise: Promise[Unit] = Promise()
  val daemon: Boolean = true
  def name: Optional[Name[Async]] = supervisor.name
  def stack: Text = supervisor.name+":".tt
  def cancel(): Unit = ()
  def shutdown(): Unit = workers.each(_.cancel())

// A nested supervision scope that overrides the inherited cleanup/error-trap policy for a region
// (see `Containment`), while delegating naming and thread-forking to its `parent`. Defined here,
// alongside `Root` and `Worker`, because `Monitor` is sealed.
class Subscope(parent: Monitor^, subProbate: Probate^) extends Monitor:
  type Result = Unit

  val probate: Probate^{this} = subProbate
  val promise: Promise[Unit] = Promise()
  def chain: Optional[Chain] = parent.chain
  val daemon: Boolean = true
  def name: Optional[Name[Async]] = parent.name
  def stack: Text = parent.stack
  def supervisor: Supervisor = parent.supervisor
  def cancel(): Unit = workers.each(_.cancel())

object VirtualSupervisor extends Supervisor:
  def name: Name[Async] = n"virtual"

  def fork(name: Optional[Text])(block: => Unit): Thread =
    Thread.ofVirtual().nn.start{ () => block }.nn

object AdaptiveSupervisor extends Supervisor:
  def name: Name[Async] = n"adaptive"

  def fork(name: Optional[Text])(block: => Unit): Thread =
    try VirtualSupervisor.fork(name)(block) catch case error: Throwable =>
      PlatformSupervisor.fork(name)(block)

object PlatformSupervisor extends Supervisor:
  def name: Name[Async] = n"platform"

  def fork(name: Optional[Text])(block: => Unit): Thread =
    val runnable: Runnable^ = () => block

    new Thread(runnable).tap: thread =>
      name.let(_.s).let(thread.setName(_))
      thread.start()

abstract class Worker(frame: Codepoint, parent: Monitor^) extends Monitor:
  self: Worker^ =>
  private val state: juca.AtomicReference[Fulfillment[Result]] =
    juca.AtomicReference(Fulfillment.Initializing)

  private var relents: Int = 1

  private val startTime: Long = jl.System.currentTimeMillis
  val promise: Promise[Result] = Promise()

  // A worker's policy is its parent scope's policy — see the note on `Monitor#probate`.
  def probate: Probate^{this} = parent.probate

  parent.addWorker(this)

  def chain: Chain = Chain(frame, parent.chain)
  def evaluate(worker: Worker): Result
  def supervisor: Supervisor = parent.supervisor
  def apply(): Optional[Result] = promise()
  def relentlessness: Double = (jl.System.currentTimeMillis - startTime).toDouble/relents

  def delegate(lambda: Monitor => Unit): Unit = state.updateAndGet: state =>
    workers.each: child => if child.daemon then child.cancel() else lambda(child)
    state

  def stack: Text =
    val ref = name.lay((frame.text: Text).s)(name => (name: Text).s+"@"+(frame.text: Text).s)

    parent match
      case root: Root         => ((root.supervisor.name: Text).s+"://"+ref).tt
      case submonitor: Worker => ((submonitor.stack: Text).s+"//"+ref).tt
      case _                  => ref.tt

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


  def map[result2](lambda: Result => result2)(using Monitor)
  :   (Task[result2] emits AsyncError)^ =

    async(lambda(join()))


  def bind[result2](lambda: Result => Task[result2])(using Monitor)
  :   (Task[result2] emits AsyncError)^ =

    async(lambda(join()).join())


  def cancel(): Unit =
    val state2 = state.updateAndGet:
      case Initializing | Active(_) =>
        promise.cancel()
        thread.interrupt()
        Cancelled

      case other =>
        other

    if state2 == Cancelled then thread.join()

  def result()(using cancel: Tactic[AsyncError]^): Result =
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


  // The raw, untyped join: the original exception of a `Failed` worker is rethrown verbatim (under
  // `canThrowAny`), so the static error is only `AsyncError`. Used internally (`map`/`bind`/
  // `sequence`/`race`) where the body's error type is not tracked. Public callers go via the typed
  // `Task#await`, which routes through `deliver` instead.
  def join[abstractable: Abstractable across Durations to Long](duration: abstractable)
  :   Result raises AsyncError =

    promise.attend(duration)
    if !promise.ready then abort(AsyncError(Reason.Timeout))
    thread.join()
    result()


  def join(): Result raises AsyncError =
    promise.attend()
    thread.join()
    result()


  // The typed join. A `Failed` worker carries a pure exception; rather than rethrowing it raw
  // (which would bypass a non-throwing `Tactic`), we `abort` it through the caller's in-scope
  // `Tactic[error | AsyncError]`. `error` is reconstructed by an unchecked cast that is sound for
  // any failure raised through the body's `AsyncTactic` (the only typed-error path); a genuinely
  // unchecked throwable from the body flows through as the raw `join` would have rethrown it.
  def deliver[error <: Exception]()(using Tactic[error | AsyncError]^): Result =
    promise.attend()
    thread.join()
    fulfilment()


  def deliver[error <: Exception, abstractable: Abstractable across Durations to Long]
    ( duration: abstractable )
    ( using Tactic[error | AsyncError]^ )
  :   Result =

    promise.attend(duration)
    if !promise.ready then abort(AsyncError(Reason.Timeout))
    thread.join()
    fulfilment()


  private def fulfilment[error <: Exception]()(using Tactic[error | AsyncError]^): Result =
    state.updateAndGet:
      case Completed(duration, result) => Delivered(duration, result)
      case Delivered(duration, result) => Delivered(duration, result)
      case other                       => other

    . match
      case Completed(_, result)        => result
      case Delivered(_, result)        => result
      case Failed(failure: AsyncError) => abort(failure)
      case Failed(failure: Exception)  => abort(failure.asInstanceOf[error])
      case Failed(failure)             => throw failure
      case Cancelled                   => abort(AsyncError(Reason.Cancelled))
      case _                           => abort(AsyncError(Reason.Incomplete))

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
          case Cancelled => workers.each: child => if child.daemon then child.cancel()
          case _         => ()

      case error: Throwable =>
        state.set(Failed(error))

    finally
      try probate.cleanup(this) catch case error: Throwable => state.set(Failed(error))

      // A fire-and-forget worker has no join at which to deliver a failure, so route it to the trap
      // installed nearby. This runs before the promise is settled below, so anything attending the
      // worker observes completion only once the trap has run. An error no trap accepts becomes an
      // `escalation`: rethrown after settling, reaching this thread's uncaught-exception handler
      // (`Fault`, or the JVM default), so that it is never silently dropped.
      def remedy(error: Error): Optional[Throwable] = probate.trap(this, error) match
        case Remedy.Accept          => Unset
        case Remedy.Reject          => error
        case Remedy.Escalate(other) => other

      val escalation: Optional[Throwable] = if !daemon then Unset else state.get().nn match
        case Failed(error: Error) => remedy(error)
        case Failed(error)        => error
        case Initializing         => Unset
        case Cancelled            => Unset
        case Active(_)            => Unset
        case Completed(_, _)      => Unset
        case Delivered(_, _)      => Unset

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

      escalation.let(throw _)

  thread
