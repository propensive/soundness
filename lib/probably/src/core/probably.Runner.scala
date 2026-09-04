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

import java.lang as jl


import ambience.{System as _, *}, environments.javaBaseEnvironment
import anticipation.*
import escapade.*
import gossamer.*
import iridescence.*
import rudiments.*
import turbulence.*
import vacuous.*

import stdios.fileDescriptorStdio
import termcaps.environmentTermcap
import beneficence.*
import denominative.*
import denominative.dysasymptotics.linearSize

object Runner:
  private[probably] val harnessThreadLocal: ThreadLocal[Option[Harness]] = ThreadLocal()

class Runner[report](selection: Selection = Selection.all)(using reporter: Reporter[report])
extends Findable:
  private val mutex: Mutex = Mutex()
  @scala.caps.unsafe.untrackedCaptures
  private var active: List[Test.Id] = Nil
  @scala.caps.unsafe.untrackedCaptures
  private var listed0: List[(Test.Id, Entry.Kind, Optional[Long])] = Nil
  @scala.caps.unsafe.untrackedCaptures
  private var admitted0: Int = 0
  private val silent: Boolean = Ci.claudeCode || Ci()

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

    if !selection.admits(id, kind, coordinates) then true
    else if selection.listOnly then
      mutex { listed0 = (id, kind, expected) :: listed0 }
      true
    else
      mutex { admitted0 += 1 }
      false

  // One schedule row per test, in first-appearance order — an axial spread's cells each call
  // `skip`, but they are one test with coordinates, not many tests — with the row's expected
  // time the SUM over its admitted cells: what the test will spend measuring is the total of
  // the cells the selection admits. Absent estimates stay absent rather than becoming zero.
  def listed: List[(Test.Id, Entry.Kind, Optional[Long])] = mutex:
    val entries = listed0.reverse

    entries.map { (id, kind, _) => (id, kind) }.distinct.map: (id, kind) =>
      val expected: Optional[Long] =
        entries.fold(Unset: Optional[Long]): (sum, entry) =>
          if entry(0) == id && entry(1) == kind
          then entry(2).lay(sum) { value => sum.lay(value)(_ + value) }
          else sum

      (id, kind, expected)
  def admitted: Int = mutex(admitted0)

  val report: report = reporter.report()

  // The test's `action` may capture a capability (an error tactic, a decoder, …), so the `Test` is
  // accepted as capturing (`Test[result]^`). Without the `^`, capture checking would box the (often
  // pure) `result` type to reconcile a capturing argument with a non-capturing parameter.
  def maybeRun[result](test: Test[result]^): Optional[Trial[result]] =
    if skip(test.id) then Unset else run[result](test)

  def redraw(size: Int): Unit = if !silent && reporter.live(report) then
    if size > 0 then Out.print(e"\e[${size}A\r\e[2K")

    // Left on `stdlib`: with a native `each`, the `e"…"` interpolation in the lambda body below
    // crashes the compiler's `wildApprox` assertion.
    active.stdlib.reverse.foreach: test =>
      val indent: Text = " ".repeat(test.depth*2).nn.tt
      Out.println(e"> ${WebColors.CadetBlue}(${test.id})$indent${test.name}\e[K")

    Out.print(e"\e[J")

  def run[result](test: Test[result]^): Trial[result] =
    mutex:
      val size = active.size
      active ::= test.id
      redraw(size)

    reporter.started(report, test.id, false)

    val context = Harness()
    Runner.harnessThreadLocal.set(Some(context))
    val ns0 = System.nanoTime

    try
      val ns0: Long = System.nanoTime
      val result: result = test.action(context)
      val ns: Long = System.nanoTime - ns0

      Trial.Returns(result, ns, context.captured.toMap.to(Map)).also:
        mutex:
          val size = active.size
          active = active.filter(_ != test.id)
          redraw(size)

        reporter.ended(report, test.id, false)

    catch case error: Exception =>
      val ns: Long = System.nanoTime - ns0

      def lazyException(): Nothing =
        given canThrow: CanThrow[Exception] = unsafeExceptions.canThrowAny
        throw error

      Trial.Throws(lazyException, ns, context.captured.toMap.to(Map)).also:
        mutex:
          val size = active.size
          active = active.filter(_ != test.id)
          redraw(size)

        reporter.ended(report, test.id, false)

    finally
      Runner.harnessThreadLocal.set(None)

  // Suites are always entered, whatever the selection: their bodies are cheap, and pruning
  // by name would defeat hash- and moniker-based selection of the tests within them.
  def suite(suite: Testable, block: Testable ?=> Unit): Unit =
    mutex:
      val size = active.size
      active ::= suite.id
      redraw(size)

    reporter.declare(report, suite)
    reporter.started(report, suite.id, true)
    block(using suite)

    mutex:
      val size = active.size
      active = active.filter(_ != suite.id)
      redraw(size)

    reporter.ended(report, suite.id, true)

  def terminate(error: Throwable): Unit = mutex:
    reporter.fail(report, error, active.to[Set])
    reporter.complete(report)

  def complete(): Unit =
    redraw(0)
    reporter.complete(report)
