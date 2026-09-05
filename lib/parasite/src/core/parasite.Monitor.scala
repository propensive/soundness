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
┃    Soundness, version 0.64.0.                                                                    ┃
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

import scala.caps

import scala.language.experimental.pureFunctions

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
import Async.Error.Reason
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
  // The one cell in the collection still held as a raw `AtomicReference`. Its element type
  // carries capabilities (`Worker^{}`), and routing it through `Atomic`'s transition — whose
  // retry loop binds the current value to a local — skolemises those capabilities, so the
  // transition's parameter no longer conforms: "capability `any` cannot flow into capture set
  // {any}". That is the same compiler divergence the casts below already work around, one level
  // deeper, and it is not something the wrapper can launder. Everything else in parasite is
  // migrated; this waits on capture checking, not on this API.
  protected[parasite] val workersRef
  :   juca.AtomicReference[scala.collection.immutable.Set[Worker^{}]] =
    juca.AtomicReference[scala.collection.immutable.Set[Worker^{}]](
      scala.collection.immutable.Set())

  protected[parasite] def workers: scala.collection.immutable.Set[Worker^{}] =
    workersRef.get().nn

  // The casts are for the Scala.js pipeline, which infers the updated set's element type with
  // the argument's reach capabilities attached (widening the worker's fields to `any`), where
  // the JVM pipeline accepts the direct form. A cast, not an ascription, because no source-level
  // type spells the pipeline's widened element type. (Compiler divergence.)
  protected[parasite] def addWorker(worker: Worker^): Unit =
    val worker0: Worker^{} = caps.unsafe.unsafeAssumePure(worker)
    workersRef.updateAndGet(_.nn.incl(worker0).asInstanceOf[scala.collection.immutable.Set[Worker^{}]])

  protected[parasite] def remove(monitor: Worker^): Unit =
    val monitor0: Worker^{} = caps.unsafe.unsafeAssumePure(monitor)
    workersRef.updateAndGet(_.nn.excl(monitor0).asInstanceOf[scala.collection.immutable.Set[Worker^{}]])

  def name: Optional[Name[Async]]
  def chain: List[Codepoint]
  def stack: Text
  def daemon: Boolean
  def attend()(using Monitor^): Unit = promise.attend()
  def ready: Boolean = promise.ready
  def cancel(): Unit
  def supervisor: Supervisor

  def snooze[generic: Abstractable across Durations to Long](duration: generic): Unit =
    supervisor.sleep(duration.generic)

// The execution strategy: forking and — since every way a strand can *suspend* must be routed
// through it — parking, sleeping and cancellation-status too. This is the whole platform seam:
// a backend for a different execution model (an event loop over WASIp3 waitable-sets, say) is a
// `Supervisor` implementation, selected by a `Threading` given; nothing else in parasite touches
// the platform. DECOUPLED from `Monitor` (see capture-checking-capabilities notes): the global
// strategy singletons (`PlatformSupervisor` etc.) are plain values, NOT capabilities, so they can
// be referenced anywhere; the supervision tree (capability-tracked `Monitor`s) is rooted locally
// per `supervise` block by a `Root`. The *license* to suspend is `Monitor^`; the supervisor is
// only the mechanism.
//
// The WASIp3 (Component Model async) mapping, for a future `parasite.wasi` backend:
//   fork                 → allocate a run-queue entry (the strand) and enqueue it
//   park                 → suspend the current task, returning WAIT on its waitable-set;
//                          `Strand.unpark` → resolve its wakeup waitable / mark it runnable
//   park(deadline)/sleep → join a `wasi:clocks/monotonic-clock` timer to the set
//   interrupted          → the strand's cancellation flag, set by `Strand.interrupt`
// On a stackless runtime, a direct-style `park` from arbitrary stack depth only becomes
// implementable once the compiler can CPS-transform suspending code; until then such a backend
// serves the monadic dialect (`Task.bind`/`map`, `Task.sleep`, `Promise#task`), whose
// continuations are already reified.
trait Supervisor:
  def name: Name[Async]

  // `name` is a thunk: computing a worker's name (`Worker.stack`) walks the whole parent chain
  // building strings, and `VirtualSupervisor` — the default — never evaluates it. (A thunk, not
  // a by-name parameter, because a by-name `Optional[Text]` crashes the capture checker's Setup
  // phase on the union type.)
  def fork(name: () => Optional[Text])(block: => Unit): Strand

  // The identity of the calling strand, as a waiter which a later `Strand.unpark` can release.
  def strand(): Strand

  // Suspend the calling strand until it is unparked. Spurious wakeups are permitted — every
  // caller re-checks its condition in a loop, exactly as `LockSupport.park` demands.
  def park(blocker: AnyRef): Unit

  // Suspend the calling strand until the deadline (`System.nanoTime` basis) or an unpark.
  def park(blocker: AnyRef, deadline: Long): Unit

  // Timed suspension with no blocker and no wakeup channel.
  def sleep(nanoseconds: Long): Unit

  // Check-and-clear the calling strand's cancellation-interrupt status.
  def interrupted(): Boolean

// The thread-backed implementation of the suspension primitives, shared by every JVM supervisor.
// This also compiles (and no-ops harmlessly) on the Scala.js/Wasm javalib, which fakes `Thread`
// and `LockSupport`; if the fork's javalib ever drops those fakes, split this out with the
// `jsSources` arrangement used by `pneumatic.flate`.
trait ThreadSupervisor extends Supervisor:
  def strand(): Strand = Strand.Threaded(Thread.currentThread.nn)
  def park(blocker: AnyRef): Unit = jucl.LockSupport.park(blocker)

  def park(blocker: AnyRef, deadline: Long): Unit =
    jucl.LockSupport.parkNanos(blocker, deadline - jl.System.nanoTime())

  def sleep(nanoseconds: Long): Unit = jucl.LockSupport.parkNanos(nanoseconds)
  def interrupted(): Boolean = Thread.interrupted()

// The local root of a supervision tree, created by `supervise`. A `Monitor` (hence a capability),
// but its lifetime is the `supervise` block, so it does not escape as a global capability.
class Root(val supervisor: Supervisor) extends Monitor:
  type Result = Unit

  def chain: List[Codepoint] = List()
  val promise: Promise[Unit] = Promise()
  val daemon: Boolean = true
  def name: Optional[Name[Async]] = supervisor.name
  def stack: Text = (supervisor.name.s+":").tt
  def cancel(): Unit = ()
  def shutdown(): Unit = workers.each(_.cancel())

object PlatformSupervisor extends ThreadSupervisor:
  def name: Name[Async] = n"platform"

  def fork(name: () => Optional[Text])(block: => Unit): Strand =
    val runnable: Runnable^{block} = () => block

    Strand.Threaded:
      new Thread(runnable).tap: thread =>
        name().let(_.s).let(thread.setName(_))
        thread.start()

// The single-threaded, eager supervision model (issue #1450): each forked task runs to
// completion at `fork` time, on the calling strand, so every promise such a task settles is
// already complete before anyone can await it, and parking is never needed. This is the model
// for JavaScript's event loop, which has no block-and-resume primitive — parking would halt
// the loop and deadlock — though it runs identically on any platform, which is how it is
// tested. Its ceiling is that of single-threaded structured concurrency: no parallelism and no
// interleaving, so awaiting a promise that no completed task has settled — a wait which could
// never end — panics rather than deadlocking, and `sleep` completes instantly, since delaying
// without blocking the loop is impossible.
object JavascriptSupervisor extends Supervisor:
  def name: Name[Async] = n"javascript"

  def fork(name: () => Optional[Text])(block: => Unit): Strand =
    block
    Strand.Eager

  def strand(): Strand = Strand.Eager

  def park(blocker: AnyRef): Unit =
    panic(m"a wait can never end under the eager single-threaded model: every forked task has already run to completion, so this promise can no longer be settled")

  def park(blocker: AnyRef, deadline: Long): Unit =
    panic(m"a timed wait can never be settled under the eager single-threaded model, and cannot block without halting the event loop")

  def sleep(nanoseconds: Long): Unit = ()
  def interrupted(): Boolean = false

// The failure path is, in a long-lived process, the code most likely to run for the first time late
// in that process's life — and classloading is not guaranteed to still work by then. A daemon whose
// jar has been replaced underneath it (rebuilt in place while it runs) holds an open `JarFile` whose
// central directory has gone stale, so every class it has not *already* loaded fails from then on;
// and a strand being cancelled through `Thread.interrupt` can fail a classload mid-read, because NIO
// channel reads throw on an interrupted thread. Either way, if recording a failure were the first
// touch of `Fulfillment.Failed`, that classload would throw `NoClassDefFoundError` in place of the
// original error — destroying the only evidence of what actually went wrong — and leave the
// strand's promise unsettled, parking every joiner forever.
//
// So every class the failure and shutdown paths need is loaded and initialized here instead, at the
// birth of the first worker (whose `state` field reads `initial`), while classloading still works.
private object Preload:
  private def touch(value: Any): Unit = ()

  val initial: Fulfillment[Nothing] =
    touch(Fulfillment.Active(0L))
    touch(Fulfillment.Completed(0L, ()))
    touch(Fulfillment.Delivered(0L, ()))
    touch(Fulfillment.Failed(Exception()))
    touch(Fulfillment.Cancelled)
    touch(Remedy.Accept)
    touch(Remedy.Reject)
    touch(Remedy.Escalate(Error(Exception())))
    touch(Async.Error(Reason.Cancelled)(using Diagnostics.omit))

    // Both settlement paths, which reach `Promise.State.Cancelled` — the counterpart inside
    // `Promise`, equally unloaded until the first cancellation, and reached only as a strand dies.
    Promise[Unit]().offer(())
    Promise[Unit]().cancel()

    Fulfillment.Initializing

abstract class Worker(frame: Codepoint, parent: Monitor^, probate: Probate^) extends Monitor:
  self: Worker^ =>
  private val state: Atomic[Fulfillment[Result]] = Atomic(Preload.initial)

  @scala.caps.unsafe.untrackedCaptures
  private var relents: Int = 1

  private val startTime: Long = jl.System.currentTimeMillis
  val promise: Promise[Result] = Promise()

  parent.addWorker(this)

  def chain: List[Codepoint] = frame :: parent.chain
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
    if supervisor.interrupted() then throw new InterruptedException()

    state() match
      case Initializing    => ()
      case Active(_)       => ()
      case Completed(_, _) => panic(m"should not be relenting after completion")
      case Delivered(_, _) => panic(m"should not be relenting after completion")
      case Failed(_)       => panic(m"should not be relenting after failure")
      case Cancelled       => throw new InterruptedException()

  override def snooze[generic: Abstractable across Durations to Long](duration: generic): Unit =
    if supervisor.interrupted() || state() == Cancelled then throw new InterruptedException()
    supervisor.sleep(duration.generic)
    if supervisor.interrupted() || state() == Cancelled then throw new InterruptedException()


  def map[result2](lambda: Result => result2)(using monitor: Monitor^, probate: Probate^)
  :   (Task[result2] emits Async.Error)^{this, lambda, monitor, probate} =

    async(lambda(join()))


  def bind[result2](lambda: Result => Task[result2])(using monitor: Monitor^, probate: Probate^)
  :   (Task[result2] emits Async.Error)^{this, lambda, monitor, probate} =

    async(lambda(join()).join())


  // `ere` yields the state THIS call displaced, which is what makes the cancellation effects
  // fire exactly once and only after `Cancelled` is visible — so a joiner woken by
  // `promise.cancel()` cannot observe a stale `Active`. A caller that loses the race, or arrives
  // late, displaces `Cancelled` and joins; a settled worker returns its argument by reference, so
  // the transition declines and no compare-and-set is issued. This replaces a hand-rolled
  // `@tailrec` retry, which existed because `updateAndGet` returns the NEW state and so cannot
  // say who won.
  final def cancel(): Unit =
    val displaced = state.ere:
      case Initializing | Active(_) => Cancelled
      case settled                  => settled

    displaced match
      case Initializing | Active(_) =>
        promise.cancel()
        strand.interrupt()
        strand.join()

      case Cancelled => strand.join()
      case _         => ()

  def result()(using cancel: Tactic[Async.Error]^): Result =
    state() match
      case Delivered(_, result) => result // Repeated joins skip the CAS and allocation below.
      case _ =>
        state.since:
          case null                        => abort(Async.Error(Reason.Incomplete))
          case Initializing                => abort(Async.Error(Reason.Incomplete))
          case Active(_)                   => abort(Async.Error(Reason.Incomplete))
          case Completed(duration, result) => Delivered(duration, result)
          case state@Delivered(_, _)       => state
          case Failed(error)               => throw error
          case Cancelled                   => abort(Async.Error(Reason.Cancelled))

        . match
          case Delivered(_, result) => result
          case other                => panic(m"impossible state")

  // The raw, untyped join: the original exception of a `Failed` worker is rethrown verbatim (under
  // `canThrowAny`), so the static error is only `Async.Error`. Used internally (`map`/`bind`/
  // `sequence`/`race`) where the body's error type is not tracked. Public callers go via the typed
  // `Task#await`, which routes through `deliver` instead.
  // No `strand.join()`: the promise is settled in the worker strand's `finally` block, strictly
  // after the state is terminal and probate cleanup has run, so `attend` returning already
  // guarantees everything a join would. (A trailing unbounded `strand.join()` would also defeat
  // the timed variants' deadline.)
  def join[abstractable: Abstractable across Durations to Long](duration: abstractable)
    ( using monitor: Monitor^ )
  :   (Tactic[Async.Error]^) ?->{this, monitor} Result =

    promise.attend(duration)
    if !promise.ready then abort(Async.Error(Reason.Timeout))
    result()


  def join()(using monitor: Monitor^): (Tactic[Async.Error]^) ?->{this, monitor} Result =
    promise.attend()
    result()

  // The typed join. A `Failed` worker carries a pure exception; rather than rethrowing it raw
  // (which would bypass a non-throwing `Tactic`), we `abort` it through the caller's in-scope
  // `Tactic[error | Async.Error]`. `error` is reconstructed by an unchecked cast that is sound for
  // any failure raised through the body's `AsyncTactic` (the only typed-error path); a genuinely
  // unchecked throwable from the body flows through as the raw `join` would have rethrown it.
  def deliver[error <: Hazard]()(using Monitor^, Tactic[error | Async.Error]^): Result =
    promise.attend()
    fulfilment()


  def deliver[error <: Hazard, abstractable: Abstractable across Durations to Long]
    ( duration: abstractable )
    ( using Monitor^, Tactic[error | Async.Error]^ )
  :   Result =

    promise.attend(duration)
    if !promise.ready then abort(Async.Error(Reason.Timeout))
    fulfilment()


  private def fulfilment[error <: Hazard]()(using Tactic[error | Async.Error]^): Result =
    state() match
      case Delivered(_, result) => result // Repeated joins skip the CAS and allocation below.
      case _ =>
        state.since:
          case Completed(duration, result) => Delivered(duration, result)
          case state@Delivered(_, _)       => state
          case other                       => other

        . match
          case Completed(_, result)        => result
          case Delivered(_, result)        => result
          case Failed(failure: Async.Error) => abort(failure)
          case Failed(failure: Exception)  => abort(failure.asInstanceOf[error])
          case Failed(failure)             => throw failure
          case Cancelled                   => abort(Async.Error(Reason.Cancelled))
          case _                           => abort(Async.Error(Reason.Incomplete))

  private lazy val strand: Strand = parent.supervisor.fork(() => stack):
    val started: Boolean = state.since:
      case Initializing => Active(jl.System.currentTimeMillis)
      case other        => other
    match
      case Active(_) => true
      case _         => false

    // The body's failure, kept in a local *before* any attempt to record it: recording allocates,
    // and an allocation can fail. Should the `state.set` below not survive, this local is the only
    // remaining evidence of what actually went wrong, and the `finally` block escalates it rather
    // than letting the secondary failure stand in for it.
    var failure: Optional[Throwable] = Unset

    try
      if started then evaluate(this).tap: result =>
        state.since:
          case Active(startTime) => Completed(jl.System.currentTimeMillis - startTime, result)
          case other             => other

    catch
      case error: InterruptedException =>
        supervisor.interrupted()

        state.since:
          case Initializing | Active(_) | Cancelled => Cancelled
          case state                                => state

        . match
          case Cancelled => workers.each: child => if child.daemon then child.cancel()
          case _         => ()

      case error: Throwable =>
        failure = error
        state() = Failed(error)

    finally
      // Nothing in the shutdown path may leave the promise unsettled: a joiner parked on an
      // unsettled promise is parked forever, and *that* — not whatever went wrong here — is what
      // turns a dead strand into a client which never returns. So every step which can throw runs
      // inside a `try` whose own `finally` settles the promise unconditionally.

      // A fire-and-forget worker has no join at which to deliver a failure, so route it to the trap
      // installed nearby. This runs before the promise is settled below, so anything attending the
      // worker observes completion only once the trap has run. An error no trap accepts becomes an
      // `escalation`: rethrown after settling, reaching this thread's uncaught-exception handler
      // (`Hazard`, or the JVM default), so that it is never silently dropped.
      def remedy(error: Error): Optional[Throwable] = probate.trap(this, error) match
        case Remedy.Accept          => Unset
        case Remedy.Reject          => error
        case Remedy.Escalate(other) => other

      var escalation: Optional[Throwable] = failure

      // Fold a shutdown-path failure into the escalation. The body's own failure is the more
      // informative of the two, so it remains the escalation and the newcomer is attached to it;
      // neither is lost, and the one which reaches the uncaught-exception handler still names the
      // original cause.
      def absorb(error: Throwable): Unit =
        escalation = escalation.lay(error): original =>
          if original `ne` error then original.addSuppressed(error)
          original

      try
        try probate.cleanup(this) catch case error: Throwable => state() = Failed(error)

        // The last five cases cover a body which failed without the failure ever reaching the
        // state: no joiner can then be shown it, so escalating is the only way it is not dropped in
        // silence, and that holds whether the worker is a daemon or not.
        escalation = state() match
          case Failed(error: Error) => if daemon then remedy(error) else Unset
          case Failed(error)        => if daemon then error else Unset
          case Initializing         => failure
          case Cancelled            => failure
          case Active(_)            => failure
          case Completed(_, _)      => failure
          case Delivered(_, _)      => failure

      catch case error: Throwable => absorb(error)

      finally
        // Deregistration and settlement both belong here rather than above: a trap which throws
        // must not be able to leave a dead worker in its parent's registry, nor — far worse — leave
        // this promise unsettled.
        try parent.remove(this) catch case error: Throwable => absorb(error)

        // The transition is pure — `updateAndGet` may re-run it under contention — and the promise
        // is settled exactly once afterwards, from the installed state. Ordering matters: the state
        // must be terminal before the promise wakes any joiner. If even that fails, cancelling is
        // the last resort: a joiner woken with an `Async.Error` beats a joiner never woken at all.
        try
          state.since:
            case null | Initializing | Active(_) => Cancelled
            case state                           => state

          . match
            case Completed(_, value) => promise.offer(value)
            case Delivered(_, _)     => ()
            case _                   => promise.cancel()

        catch case error: Throwable => promise.cancel()

      escalation.let(throw _)

  strand
