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

import ambience.{System as _, *}, environments.javaEnvironment
import escapade.*
import iridescence.*
import rudiments.*
import turbulence.*
import vacuous.*

import stdios.virtualMachineStdio
import termcaps.environmentTermcap
import beneficence.*

object Runner:
  private[probably] val harnessThreadLocal: ThreadLocal[Option[Harness]] = ThreadLocal()

class Runner[report](selection: Selection = Selection.all)(using reporter: Reporter[report])
extends Findable:
  private val mutex: Mutex = Mutex()
  private var active: List[TestId] = Nil
  private var listed0: List[(TestId, Entry.Kind)] = Nil
  private var admitted0: Int = 0
  private val silent: Boolean = Ci.claudeCode || Ci()

  def skip(id: TestId): Boolean = skip(id, Entry.Kind.Check, Nil)

  // Whether a test (or one cell of an axial test) is excluded by the selection. In listing
  // mode every test is skipped, and those the selection admits are noted for enumeration.
  def skip(id: TestId, kind: Entry.Kind, coordinates: List[(Axis.Spec, Value)]): Boolean =
    if !selection.admits(id, kind, coordinates) then true
    else if selection.listOnly then
      mutex { listed0 = (id, kind) :: listed0 }
      true
    else
      mutex { admitted0 += 1 }
      false

  def listed: List[(TestId, Entry.Kind)] = mutex(listed0.reverse.distinct)
  def admitted: Int = mutex(admitted0)

  val report: report = reporter.report()

  // The test's `action` may capture a capability (an error tactic, a decoder, …), so the `Test` is
  // accepted as capturing (`Test[result]^`). Without the `^`, capture checking would box the (often
  // pure) `result` type to reconcile a capturing argument with a non-capturing parameter.
  def maybeRun[result](test: Test[result]^): Optional[Trial[result]] =
    if skip(test.id) then Unset else run[result](test)

  def redraw(size: Int): Unit = if !silent then
    if size > 0 then Out.print(e"\e[${size}A\r\e[2K")

    active.reverse.each: test =>
      Out.println(e"> ${WebColors.CadetBlue}(${test.id})${" "*(test.depth*2)}${test.name}\e[K")

    Out.print(e"\e[J")


  def run[result](test: Test[result]^): Trial[result] =
    mutex:
      val size = active.size
      active ::= test.id
      redraw(size)

    val context = Harness()
    Runner.harnessThreadLocal.set(Some(context))
    val ns0 = System.nanoTime

    try
      val ns0: Long = System.nanoTime
      val result: result = test.action(context)
      val ns: Long = System.nanoTime - ns0

      Trial.Returns(result, ns, context.captured.to(Map)).also:
        mutex:
          val size = active.size
          active = active.filter(_ != test.id)
          redraw(size)

    catch case error: Exception =>
      val ns: Long = System.nanoTime - ns0

      def lazyException(): Nothing =
        given canThrow: CanThrow[Exception] = unsafeExceptions.canThrowAny
        throw error

      Trial.Throws(lazyException, ns, context.captured.to(Map)).also:
        mutex:
          val size = active.size
          active = active.filter(_ != test.id)
          redraw(size)

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
    block(using suite)

    mutex:
      val size = active.size
      active = active.filter(_ != suite.id)
      redraw(size)

  def terminate(error: Throwable): Unit = mutex:
    reporter.fail(report, error, active.to(Set))
    reporter.complete(report)

  def complete(): Unit =
    redraw(0)
    reporter.complete(report)
