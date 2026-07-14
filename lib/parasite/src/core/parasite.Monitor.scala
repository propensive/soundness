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
┃    Soundness, version 0.63.0.                                                                    ┃
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

  // The casts are for the Scala.js pipeline, which infers the updated set's element type with
  // the argument's reach capabilities attached (widening the worker's fields to `any`), where
  // the JVM pipeline accepts the direct form. A cast, not an ascription, because no source-level
  // type spells the pipeline's widened element type. (Compiler divergence.)
  protected[parasite] def addWorker(worker: Worker^): Unit =
    val worker0: Worker^{} = caps.unsafe.unsafeAssumePure(worker)
    workersRef.updateAndGet(_.nn.incl(worker0).asInstanceOf[Set[Worker^{}]])

  protected[parasite] def remove(monitor: Worker^): Unit =
    val monitor0: Worker^{} = caps.unsafe.unsafeAssumePure(monitor)
    workersRef.updateAndGet(_.nn.excl(monitor0).asInstanceOf[Set[Worker^{}]])

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

  // `name` is a thunk: computing a worker's name (`Worker.stack`) walks the whole parent chain
  // building strings, and `VirtualSupervisor` — the default — never evaluates it. (A thunk, not
  // a by-name parameter, because a by-name `Optional[Text]` crashes the capture checker's Setup
  // phase on the union type.)
  def fork(name: () => Optional[Text])(block: => Unit): Thread

// The local root of a supervision tree, created by `supervise`. A `Monitor` (hence a capability),
// but its lifetime is the `supervise` block, so it does not escape as a global capability.
class Root(val supervisor: Supervisor) extends Monitor:
  type Result = Unit

  def chain: Optional[Chain] = Unset
  val promise: Promise[Unit] = Promise()
  val daemon: Boolean = true
  def name: Optional[Name[Async]] = supervisor.name
  def stack: Text = (supervisor.name.s+":").tt
  def cancel(): Unit = ()
  def shutdown(): Unit = workers.each(_.cancel())

object VirtualSupervisor extends Supervisor:
  def name: Name[Async] = n"virtual"

  def fork(name: () => Optional[Text])(block: => Unit): Thread =
    Thread.ofVirtual().nn.start{ () => block }.nn

object AdaptiveSupervisor extends Supervisor:
  def name: Name[Async] = n"adaptive"

  // Virtual-thread support is a deterministic property of the JVM, so it is probed once with a
  // no-op thread rather than trial-forked per task. A dedicated probe means a transient failure
  // (e.g. OutOfMemoryError) on a real fork cannot mis-pin the process to platform threads: only
  // `UnsupportedOperationException` — the deterministic outcome — is cached; anything else
  // propagates and the probe is retried on the next fork.
  private lazy val virtual: Boolean =
    try
      Thread.ofVirtual().nn.start{ () => () }
      true
    catch case error: UnsupportedOperationException => false

  def fork(name: () => Optional[Text])(block: => Unit): Thread =
    if virtual then VirtualSupervisor.fork(name)(block)
    else PlatformSupervisor.fork(name)(block)

object PlatformSupervisor extends Supervisor:
  def name: Name[Async] = n"platform"

  def fork(name: () => Optional[Text])(block: => Unit): Thread =
    val runnable: Runnable^ = () => block

    new Thread(runnable).tap: thread =>
      name().let(_.s).let(thread.setName(_))
      thread.start()

abstract class Worker(frame: Codepoint, parent: Monitor^, probate: Probate^) extends Monitor:
  self: Worker^ =>
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

  def delegate(lambda: Monitor^ => Unit): Unit =
    workers.each: child => if child.daemon then child.cancel() else lambda(child)

  def stack: Text =
    val ref = // The `(x: Text)` ascriptions widen singleton-bounded values (case-2 pure-value box).
      name.lay((frame.text: Text).s)(name => (name: Text).s+"@"+(frame.text: Text).s)

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


  def map[result2](lambda: Result => result2)(using Monitor^, Probate^)
  :   Task[result2] emits AsyncError =

    async(lambda(join()))


  def bind[result2](lambda: Result => Task[result2])(using Monitor^, Probate^)
  :   Task[result2] emits AsyncError =

    async(lambda(join()).join())


  // An explicit CAS loop rather than `updateAndGet`, whose update function may be re-run under
  // contention: the cancellation effects must fire exactly once, and only *after* the `Cancelled`
  // state is visible, so that a joiner woken by `promise.cancel()` cannot observe a stale
  // `Active` state. The final `thread.join()` happens whenever the state is `Cancelled`, even if
  // another cancellation won the race.
  @tailrec
  final def cancel(): Unit =
    val current = state.get().nn

    current match
      case Initializing | Active(_) =>
        if !state.compareAndSet(current, Cancelled) then cancel() else
          promise.cancel()
          thread.interrupt()
          thread.join()

      case Cancelled => thread.join()
      case _         => ()

  def result()(using cancel: Tactic[AsyncError]^): Result =
    state.get() match
      case Delivered(_, result) => result // Repeated joins skip the CAS and allocation below.
      case _ =>
        state.updateAndGet:
          case null                        => abort(AsyncError(Reason.Incomplete))
          case Initializing                => abort(AsyncError(Reason.Incomplete))
          case Active(_)                   => abort(AsyncError(Reason.Incomplete))
          case Completed(duration, result) => Delivered(duration, result)
          case state@Delivered(_, _)       => state
          case Failed(error)               => throw error
          case Cancelled                   => abort(AsyncError(Reason.Cancelled))

        . match
          case Delivered(_, result) => result
          case other                => panic(m"impossible state")


  // The raw, untyped join: the original exception of a `Failed` worker is rethrown verbatim (under
  // `canThrowAny`), so the static error is only `AsyncError`. Used internally (`map`/`bind`/
  // `sequence`/`race`) where the body's error type is not tracked. Public callers go via the typed
  // `Task#await`, which routes through `deliver` instead.
  // No `thread.join()`: the promise is settled in the worker thread's `finally` block, strictly
  // after the state is terminal and probate cleanup has run, so `attend` returning already
  // guarantees everything a join would. (A trailing unbounded `thread.join()` would also defeat
  // the timed variants' deadline.)
  def join[abstractable: Abstractable across Durations to Long](duration: abstractable)
  :   Result raises AsyncError =

    promise.attend(duration)
    if !promise.ready then abort(AsyncError(Reason.Timeout))
    result()


  def join(): Result raises AsyncError =
    promise.attend()
    result()


  // The typed join. A `Failed` worker carries a pure exception; rather than rethrowing it raw
  // (which would bypass a non-throwing `Tactic`), we `abort` it through the caller's in-scope
  // `Tactic[error | AsyncError]`. `error` is reconstructed by an unchecked cast that is sound for
  // any failure raised through the body's `AsyncTactic` (the only typed-error path); a genuinely
  // unchecked throwable from the body flows through as the raw `join` would have rethrown it.
  def deliver[error <: Hazard]()(using Tactic[error | AsyncError]^): Result =
    promise.attend()
    fulfilment()


  def deliver[error <: Hazard, abstractable: Abstractable across Durations to Long]
    ( duration: abstractable )
    ( using Tactic[error | AsyncError]^ )
  :   Result =

    promise.attend(duration)
    if !promise.ready then abort(AsyncError(Reason.Timeout))
    fulfilment()


  private def fulfilment[error <: Hazard]()(using Tactic[error | AsyncError]^): Result =
    state.get() match
      case Delivered(_, result) => result // Repeated joins skip the CAS and allocation below.
      case _ =>
        state.updateAndGet:
          case Completed(duration, result) => Delivered(duration, result)
          case state@Delivered(_, _)       => state
          case other                       => other

        . match
          case Completed(_, result)        => result
          case Delivered(_, result)        => result
          case Failed(failure: AsyncError) => abort(failure)
          case Failed(failure: Exception)  => abort(failure.asInstanceOf[error])
          case Failed(failure)             => throw failure
          case Cancelled                   => abort(AsyncError(Reason.Cancelled))
          case _                           => abort(AsyncError(Reason.Incomplete))

  private lazy val thread: Thread = parent.supervisor.fork(() => stack):
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
      // (`Hazard`, or the JVM default), so that it is never silently dropped.
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

      parent.remove(this)

      // The transition is pure — `updateAndGet` may re-run it under contention — and the promise
      // is settled exactly once afterwards, from the installed state. Ordering matters: the state
      // must be terminal before the promise wakes any joiner.
      state.updateAndGet:
        case null | Initializing | Active(_) => Cancelled
        case state                           => state

      . match
        case Completed(_, value) => promise.offer(value)
        case Delivered(_, _)     => ()
        case _                   => promise.cancel()

      escalation.let(throw _)

  thread
