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
import scala.compiletime.asMatchable

import java.lang as jl
import java.util.concurrent as juc
import java.util.concurrent.atomic as juca
import java.util.concurrent.locks as jucl

import anticipation.*
import vacuous.*

// A supervisor which runs tasks on a pool of reusable carrier threads instead of starting a
// thread per task. `fork` hands the task to an idle carrier when one is waiting, and starts a
// fresh carrier only when none is; a carrier that finishes a task parks itself on the idle
// list for the next. The hand-off to a waiting carrier costs a few hundred nanoseconds where
// a thread start and join cost a few microseconds, which is the whole point: fine-grained
// fan-out — one task per element, a spawn-and-join in a loop — stops being dominated by thread
// creation. The carriers are whatever `spawn` produces: virtual threads on the JVM, platform
// threads on Scala Native, where there is no Loom and a thread start costs tens of
// microseconds, so the saving is larger still.
//
// THE POOL NEVER STARVES. A task that blocks — through `park` or any other way, including
// JDK I/O, `Thread.sleep`, a `synchronized` block or `Handoff`'s own spin-then-park — simply
// keeps its carrier, and the next `fork` finding no idle carrier starts another. So the worst
// case is exactly the cost of a thread per task, and there is no fixed size, no queue that
// tasks wait in, and no compensation logic to get wrong: a task is running from the moment it
// is forked, as with every other supervisor. Idle carriers above `idleLimit` retire instead of
// waiting, which bounds the pool's memory after a burst.
//
// CANCELLATION IS A HANDSHAKE. `Worker.cancel` interrupts a task's strand; on a dedicated
// thread that is the thread's own interrupt status, but a carrier's status must belong only to
// the task mounted on it. Each task entry runs a small state machine — queued, running,
// interrupting, finished — and an interrupter that finds the task running takes the
// `interrupting` state before touching the carrier, while a carrier finishing a task waits
// out any interrupter in flight before clearing the status. So an interrupt aimed at a task
// can neither be lost (it lands while the task is mounted, or is deferred to its mount) nor
// leak to the task that runs next on the same carrier. An entry's `join` waits for the
// finished state, not for the carrier, which lives on.
//
// THE COSTS. A task no longer has a thread of its own: thread-locals persist across the tasks
// a carrier runs (parasite sets none, but a library that keys on the thread may), and a
// thread dump shows carriers rather than tasks — `Worker.stack` and the monitor tree are the
// task-level view. A task's `Thread.currentThread` is stable for its duration, which is all
// the parking and waiter identity below rely on.
//
// A waiting carrier, and a task waiting in `park`, spin briefly before parking: a hand-off to
// a carrier in lockstep, or a join of a task that has all but finished, then completes without
// a park/unpark round trip, as in `Handoff`. The budget is small enough that an idle carrier
// stops burning its core within a microsecond or two.
//
// The pool's state and the classes over it live in the companion, so that none of them closes
// over the supervisor instance: the supervisor is a plain value, not a capability, and its
// inner workings are kept that way.
abstract class PoolingSupervisor extends ThreadSupervisor:
  // Starts a carrier thread running `runnable` and returns it started.
  protected def spawn(runnable: Runnable): Thread

  // The number of idle carriers kept for reuse; one arriving above it retires instead.
  protected def idleLimit: Int = 256

  import PoolingSupervisor.{Carrier, Entry, spins}

  // The idle carriers, most recently idle first: a warm carrier is the better one to reuse.
  // A linked deque (not a hand-rolled stack) so that reused carrier nodes cannot ABA.
  private[parasite] val idle: juc.ConcurrentLinkedDeque[Carrier] = juc.ConcurrentLinkedDeque()
  private[parasite] val idleCount: juca.AtomicInteger = juca.AtomicInteger(0)

  // The carrier the calling thread is, if it is one; the JVM offers no cheaper way to ask.
  private[parasite] val carriers: ThreadLocal[Carrier | Null] = ThreadLocal()

  private[parasite] def limit: Int = idleLimit
  private[parasite] val running: juca.AtomicInteger = juca.AtomicInteger(0)

  // Diagnostics: the carriers running a task, and those waiting for one. A pool at rest has
  // no active carriers; one that does not return to that state has a task that never finished.
  def active: Int = running.get()
  def idling: Int = idleCount.get()

  def fork(name: () => Optional[Text])(block: => Unit): Strand =
    // The entry closes over the task's body, as a dedicated thread's `Runnable` would; it is
    // stored boxed as pure, the same laundering the supervision registry applies to workers.
    val entry: Entry = caps.unsafe.unsafeAssumePure(Entry(() => block))
    val carrier = idle.pollFirst()

    if carrier == null then
      // A supervisor is a plain value, not a capability (see `Supervisor`); capture checking
      // cannot see that from inside its own body, so the reference is laundered here.
      val fresh = Carrier(caps.unsafe.unsafeAssumePure(this))
      fresh.task = entry
      spawn(fresh)
    else
      idleCount.decrementAndGet()
      carrier.task = entry
      val thread = carrier.thread
      if thread != null then jucl.LockSupport.unpark(thread)

    entry

  override def strand(): Strand = carriers.get() match
    case null    => Strand.Threaded(Thread.currentThread.nn)
    case carrier => carrier.strand

  override def park(blocker: AnyRef): Unit = carriers.get() match
    case null => jucl.LockSupport.park(blocker)
    case carrier =>
      var spun = 0
      while !carrier.permit && spun < spins do
        spun += 1
        Thread.onSpinWait()

      if !carrier.permit then jucl.LockSupport.park(blocker)
      carrier.permit = false

  override def park(blocker: AnyRef, deadline: Long): Unit = carriers.get() match
    case null => jucl.LockSupport.parkNanos(blocker, deadline - jl.System.nanoTime())
    case carrier =>
      if !carrier.permit then jucl.LockSupport.parkNanos(blocker, deadline - jl.System.nanoTime())
      carrier.permit = false

object PoolingSupervisor:
  // Spin budget before parking, for a carrier awaiting a task and for a task's `park`: long
  // enough to bridge a counterpart's wake-up latency, short enough not to hold a core.
  private inline val spins = 2048

  private object State:
    inline val Queued = 0
    inline val Running = 1
    inline val Interrupting = 2
    inline val Finished = 3

  // A forked task: its strand handle, and the record a carrier runs. Its mutable fields are
  // published by volatile write, as `Handoff`'s are, so their captures are untracked.
  private[parasite] final class Entry(block: () => Unit) extends Strand:
    private val state: juca.AtomicInteger = juca.AtomicInteger(State.Queued)
    @caps.unsafe.untrackedCaptures @volatile private var carrier: Thread | Null = null
    @caps.unsafe.untrackedCaptures @volatile private var interruptRequested: Boolean = false
    @caps.unsafe.untrackedCaptures @volatile private var joiner: Thread | Null = null

    // Requests cancellation of the task. Queued: remembered, and delivered when it mounts.
    // Running: delivered to the carrier under the `Interrupting` state, so the carrier cannot
    // finish and clear it, nor move on to another task, while it is in flight. Finished: nothing.
    def interrupt(): Unit =
      interruptRequested = true

      if state.compareAndSet(State.Running, State.Interrupting) then
        try
          val thread = carrier
          if thread != null then thread.interrupt()
        finally state.set(State.Running)

    // Blocks until the task has finished; the carrier is not joined, since it lives on. As
    // `Thread.join`, an interrupted joiner throws rather than waiting: a task that cancels
    // itself interrupts its own carrier and then joins its own entry, which can never finish —
    // and `park` returns at once on an interrupted thread, so without this check the join
    // would spin for ever, on a carrier that never returns to the pool.
    def join(): Unit =
      import unsafeExceptions.canThrowAny

      while state.get() != State.Finished do
        if Thread.interrupted() then throw InterruptedException()
        joiner = Thread.currentThread.nn
        if state.get() != State.Finished then jucl.LockSupport.park(this)
        joiner = null

    // A task waiting in `park` is woken through its carrier's strand, not this handle, so this
    // is only reached by a waiter set which recorded the entry itself; wake the carrier anyway.
    def unpark(): Unit =
      val thread = carrier
      if thread != null then jucl.LockSupport.unpark(thread)

    // Runs the task on the calling carrier thread.
    def run(thread: Thread, pool: PoolingSupervisor): Unit =
      pool.running.incrementAndGet()
      state.set(State.Running)
      carrier = thread
      if interruptRequested then thread.interrupt()

      try block()
      catch case error: Throwable =>
        // A worker's body settles its own state and promise; only an escalation reaches here,
        // which goes where an uncaught exception on a dedicated thread would have gone. The
        // carrier itself survives.
        val handler = thread.getUncaughtExceptionHandler
        if handler != null then handler.uncaughtException(thread, error)
      finally
        // The handshake: an interrupter holding `Interrupting` is about to interrupt this
        // thread, so wait for it, then clear the status so nothing leaks to the next task.
        while !state.compareAndSet(State.Running, State.Finished) do Thread.onSpinWait()
        Thread.interrupted()
        carrier = null
        pool.running.decrementAndGet()
        val waiting = joiner
        if waiting != null then jucl.LockSupport.unpark(waiting)

  // A carrier: the loop a pooled thread runs, and the strand identity of whatever task is
  // mounted on it, for waiter sets and parking.
  private[parasite] final class Carrier(pool: PoolingSupervisor) extends Runnable:
    @caps.unsafe.untrackedCaptures @volatile var task: Entry | Null = null
    @caps.unsafe.untrackedCaptures @volatile var thread: Thread | Null = null
    // The park permit `CarrierStrand.unpark` grants, checked by the spinning phase of `park`.
    @caps.unsafe.untrackedCaptures @volatile var permit: Boolean = false

    val strand: Strand = CarrierStrand(this)

    def run(): Unit =
      val self = Thread.currentThread.nn
      thread = self
      pool.carriers.set(this)
      var current: Entry | Null = task

      while current != null do
        current.run(self, pool)
        task = null
        current = next()

    // Parks on the idle list until a task is handed over, or retires when the list is full.
    private def next(): Entry | Null =
      if pool.idleCount.incrementAndGet() > pool.limit then
        pool.idleCount.decrementAndGet()
        null
      else
        pool.idle.addFirst(this)
        var spun = 0

        while task == null do
          if spun < spins then
            spun += 1
            Thread.onSpinWait()
          else jucl.LockSupport.park(this)

        // A hand-off leaves the carrier's own park permit possibly set; nothing else is
        // waiting on it, so a stale permit only makes a later `park` spurious, which is allowed.
        task

  private[parasite] final class CarrierStrand(val carrier: Carrier) extends Strand:
    def interrupt(): Unit =
      val thread = carrier.thread
      if thread != null then thread.interrupt()

    def join(): Unit = ()

    def unpark(): Unit =
      carrier.permit = true
      val thread = carrier.thread
      if thread != null then jucl.LockSupport.unpark(thread)

    override def equals(that: Any): Boolean = that.asMatchable match
      case that: CarrierStrand => that.carrier.eq(carrier)
      case _                   => false

    override def hashCode: Int = carrier.hashCode
