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
package probably

import java.util as ju
import java.util.concurrent as juc

import anticipation.*
import rudiments.*
import symbolism.*
import vacuous.*

import beneficence.*
import denominative.*
import denominative.dysasymptotics.linearSize

object Runner:
  private[probably] val harnessThreadLocal: ThreadLocal[Option[Harness]] = ThreadLocal()

  // One row of a listing: a test the selection admits, its kind, the expected measuring
  // time summed over its admitted cells (absent for untimed kinds), and its axes with the
  // values (or declared bounds) of those cells. The tags are the test's own.
  case class Scheduled
    ( id: Test.Id, kind: Entry.Kind, expected: Optional[Long], axes: List[Axis.Schedule] ):

    def tags: List[Tag] = id.tags

class Runner[report](selection: Selection = Selection.all)(using reporter: Reporter[report])
extends Findable:
  private val mutex: Mutex = Mutex()
  @scala.caps.unsafe.untrackedCaptures
  private var active: List[Test.Id] = Nil
  @scala.caps.unsafe.untrackedCaptures
  private var listed0: List[(Test.Id, Entry.Kind, Optional[Long], List[(Axis.Spec, Value)])] =
    Nil
  @scala.caps.unsafe.untrackedCaptures
  private var declared0: List[(Test.Id, Entry.Kind, Axis.Spec, Optional[Double], Optional[Double])] =
    Nil
  @scala.caps.unsafe.untrackedCaptures
  private var admitted0: Int = 0

  // ---- Queued execution ----
  //
  // With `--workers=<n>`, a pure assertion is not run where the traversal meets it: its
  // thunk is queued, announced (`Reporter.scheduled`), and run by one of `n` workers while the
  // traversal carries on — so a suite's code between tests runs exactly once, and the
  // schedule is known as fast as the suite bodies run. A `SuiteEnded` is held back until the
  // last assertion queued within the suite's block (the traversal's stack of suites at the
  // time, so a nested `Suite` counts too) has completed; `drain` waits for every worker before
  // `complete` decides `passed`. Only assertions the compiler has verified pure are queued
  // (see `Test.assert` and `internal.assert`), so a worker can never reach a resource the
  // traversal has since released; everything else runs inline, as without workers.
  private val stop: Runnable = () => ()

  @scala.caps.unsafe.untrackedCaptures
  private val queue: juc.LinkedBlockingQueue[Runnable] = juc.LinkedBlockingQueue()

  @scala.caps.unsafe.untrackedCaptures
  private var workers: List[Thread] = Nil

  @scala.caps.unsafe.untrackedCaptures
  @volatile private var failure: Optional[Throwable] = Unset

  // The traversal's stack of suites, innermost first (traversal thread only).
  @scala.caps.unsafe.untrackedCaptures
  private var suites: List[Test.Id] = Nil

  // Queued assertions not yet completed, per enclosing suite; and the suites whose block has
  // exited with assertions still pending, whose `ended` the last completion emits.
  private val pending: ju.HashMap[Test.Id, Int] = ju.HashMap()
  private val exited: ju.HashSet[Test.Id] = ju.HashSet()

  def queued: Boolean = selection.workers > 0

  // Queues `thunk` — the rest of an assertion, closing over its pure test — for a worker.
  // The workers start on the first deferral, so a run that queues nothing has no threads.
  def defer(id: Test.Id, thunk: Runnable^): Unit =
    val enclosing: List[Test.Id] = suites

    mutex:
      enclosing.each { suite => pending.put(suite, pending.getOrDefault(suite, 0).nn + 1) }
      if workers.nil then workers = (0 until selection.workers).to(List).map(start(_))

    reporter.scheduled(report, id)

    // The thunk's captures are the assertion's pure test and the runner's own instances —
    // nothing scoped by the traversal — so holding it beyond the block is sound.
    val job: Runnable = scala.caps.unsafe.unsafeAssumePure(thunk)

    queue.put: () =>
      try job.run()
      catch case error: Throwable => mutex { if failure.absent then failure = error }
      finally settle(enclosing)

  private def start(index: Int): Thread =
    val thread = Thread.ofVirtual().nn.name(s"probably-worker-$index").nn.unstarted(() => work()).nn
    thread.start()
    thread

  private def work(): Unit =
    var running = true
    while running do
      val job: Runnable = queue.take().nn
      if job eq stop then running = false else job.run()

  // One queued assertion of each of `enclosing` has completed; a suite whose block has
  // already exited and has nothing left pending ends now, innermost first.
  private def settle(enclosing: List[Test.Id]): Unit =
    val ending: List[Test.Id] = mutex:
      enclosing.filter: suite =>
        val left = pending.getOrDefault(suite, 0).nn - 1
        pending.put(suite, left)
        left == 0 && exited.remove(suite)

    ending.each { suite => reporter.ended(report, suite, true) }

  // Waits for every queued assertion; a worker's escaped `Throwable` (an `Error`, or a
  // failure outside the test's own bracket) is rethrown here, on the traversal's thread.
  def drain(): Unit =
    val running: List[Thread] = mutex { workers.also { workers = Nil } }
    running.each { _ => queue.put(stop) }
    running.each(_.join())
    failure.let { error => throw error }

  // Abandons what is queued and waits for what is in flight, for a run that is terminating.
  private def abort(): Unit =
    queue.clear()
    val running: List[Thread] = mutex { workers.also { workers = Nil } }
    running.each { _ => queue.put(stop) }
    running.each(_.join())

  def skip(id: Test.Id): Boolean = skip(id, Entry.Kind.Check, Nil)

  // The selection's duration multiplier, which the timed kinds apply to their declared
  // targets. It lives on the runner because that is what sedentary already has in hand at
  // the point where a measurement's length is decided.
  def scale: Double = selection.scale

  // Whether a test (or one cell of an axial test) is excluded by the selection. In listing
  // mode every test is skipped, and those the selection admits are noted for enumeration.
  def skip(id: Test.Id, kind: Entry.Kind, coordinates: List[(Axis.Spec, Value)]): Boolean =
    skip(id, kind, coordinates, Unset)

  // As above, but with the caller's estimate of how many NANOSECONDS the test will spend
  // measuring — declared metadata, not a promise — recorded against the schedule so a host
  // can budget a whole run (fume's `--target`) before anything has been staged. The timed
  // kinds supply it; plain checks have no meaningful duration and pass `Unset`.
  def skip
    ( id:          Test.Id,
      kind:        Entry.Kind,
      coordinates: List[(Axis.Spec, Value)],
      expected:    Optional[Long] )
  :   Boolean =

    if !selection.admits(id, kind, coordinates, id.tags) then true
    else if selection.listOnly then
      mutex { listed0 = (id, kind, expected, coordinates) :: listed0 }
      true
    else
      mutex { admitted0 += 1 }
      false

  // Announces, for a listing, an EMERGENT axis of a test whose coordinates only the run will
  // produce (a stress sweep's `N`), with the bounds the producer knows at declaration, so a
  // host can offer a range of it without iterating cells that do not yet exist. A no-op
  // outside listing mode; the declaration is reported only if the test itself is listed.
  def declare
    ( id:    Test.Id,
      kind:  Entry.Kind,
      axis:  Axis.Spec,
      least: Optional[Double],
      most:  Optional[Double] )
  :   Unit =

    if selection.listOnly then mutex { declared0 = (id, kind, axis, least, most) :: declared0 }

  // One schedule row per test, in first-appearance order — an axial spread's cells each call
  // `skip`, but they are one test with coordinates, not many tests — with the row's expected
  // time the SUM over its admitted cells: what the test will spend measuring is the total of
  // the cells the selection admits. Absent estimates stay absent rather than becoming zero.
  // The row's axes are those of its admitted cells, in first-appearance order, each with the
  // distinct values seen, followed by any declared emergent axes not seen among the cells.
  def listed: List[Runner.Scheduled] = mutex:
    val entries = listed0.reverse
    val declarations = declared0.reverse

    entries.map { entry => (entry(0), entry(1)) }.distinct.map: (id, kind) =>
      val cells = entries.filter { entry => entry(0) == id && entry(1) == kind }

      val expected: Optional[Long] =
        cells.fold(Unset: Optional[Long]): (sum, entry) =>
          entry(2).lay(sum) { value => sum.lay(value)(_ + value) }

      val declared = declarations.filter { entry => entry(0) == id && entry(1) == kind }

      val specs: List[Axis.Spec] =
        cells.flatMap { entry => entry(3).map(_(0)) }.distinct

      val seen: List[Axis.Schedule] = specs.map: spec =>
        val values: List[Value] =
          cells.flatMap { entry => entry(3).filter(_(0) == spec).map(_(1)) }.distinct

        val bounds = declared.seek(_(2) == spec)
        Axis.Schedule(spec, values, bounds.let(_(3)), bounds.let(_(4)))

      val emergent: List[Axis.Schedule] =
        declared.filter { entry => !specs.has(entry(2)) }.map: entry =>
          Axis.Schedule(entry(2), Nil, entry(3), entry(4))

      Runner.Scheduled(id, kind, expected, seen + emergent)

  def admitted: Int = mutex(admitted0)

  val report: report = reporter.report()

  // The test's `action` may capture a capability (an error tactic, a decoder, …), so the `Test` is
  // accepted as capturing (`Test[result]^`). Without the `^`, capture checking would box the (often
  // pure) `result` type to reconcile a capturing argument with a non-capturing parameter.
  def maybeRun[result](test: Test[result]^): Optional[Trial[result]] =
    if skip(test.id) then Unset else run[result](test)

  def run[result](test: Test[result]^): Trial[result] =
    mutex { active ::= test.id }

    reporter.started(report, test.id, false)

    val context = Harness()
    Runner.harnessThreadLocal.set(Some(context))
    val ns0 = System.nanoTime

    try
      val result: result = test.action(context)
      val ns: Long = System.nanoTime - ns0
      Trial.Returns(result, ns, context.captured.toMap.to(Map))

    catch case error: Exception =>
      val ns: Long = System.nanoTime - ns0

      def lazyException(): Nothing =
        given canThrow: CanThrow[Exception] = unsafeExceptions.canThrowAny
        throw error

      Trial.Throws(lazyException, ns, context.captured.toMap.to(Map))

    // The bracket closes whatever escaped — an `Error` in the body must not leave the test
    // among the active ones that a termination reports.
    finally
      Runner.harnessThreadLocal.set(None)
      mutex { active = active.filter(_ != test.id) }
      reporter.ended(report, test.id, false)

  // Suites are always entered, whatever the selection: their bodies are cheap, and pruning
  // by name would defeat hash- and moniker-based selection of the tests within them.
  def suite(suite: Testable, block: Testable ?=> Unit): Unit =
    mutex { active ::= suite.id }
    suites ::= suite.id

    reporter.declare(report, suite)
    reporter.started(report, suite.id, true)
    block(using suite)

    suites = suites.tail
    mutex { active = active.filter(_ != suite.id) }

    // With assertions still queued from within the block, the suite ends when the last of
    // them completes (see `settle`).
    val ended: Boolean = mutex:
      if pending.getOrDefault(suite.id, 0).nn == 0 then true
      else
        exited.add(suite.id)
        false

    if ended then reporter.ended(report, suite.id, true)

  def terminate(error: Throwable): Unit =
    abort()

    mutex:
      reporter.fail(report, error, active.to[Set])
      reporter.complete(report)

  def complete(): Unit = reporter.complete(report)
