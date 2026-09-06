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
      val ns0: Long = System.nanoTime
      val result: result = test.action(context)
      val ns: Long = System.nanoTime - ns0

      Trial.Returns(result, ns, context.captured.toMap.to(Map)).also:
        mutex { active = active.filter(_ != test.id) }

        reporter.ended(report, test.id, false)

    catch case error: Exception =>
      val ns: Long = System.nanoTime - ns0

      def lazyException(): Nothing =
        given canThrow: CanThrow[Exception] = unsafeExceptions.canThrowAny
        throw error

      Trial.Throws(lazyException, ns, context.captured.toMap.to(Map)).also:
        mutex { active = active.filter(_ != test.id) }

        reporter.ended(report, test.id, false)

    finally
      Runner.harnessThreadLocal.set(None)

  // Suites are always entered, whatever the selection: their bodies are cheap, and pruning
  // by name would defeat hash- and moniker-based selection of the tests within them.
  def suite(suite: Testable, block: Testable ?=> Unit): Unit =
    mutex { active ::= suite.id }

    reporter.declare(report, suite)
    reporter.started(report, suite.id, true)
    block(using suite)

    mutex { active = active.filter(_ != suite.id) }

    reporter.ended(report, suite.id, true)

  def terminate(error: Throwable): Unit = mutex:
    reporter.fail(report, error, active.to[Set])
    reporter.complete(report)

  def complete(): Unit = reporter.complete(report)
