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
package probably

import java.lang as jl

import ambience.{System as _, *}, environments.java
import escapade.*
import iridescence.*
import rudiments.*
import turbulence.*
import vacuous.*

import termcaps.environment
import stdios.virtualMachine

object Runner:
  private[probably] val harnessThreadLocal: ThreadLocal[Option[Harness]] = ThreadLocal()

class Runner[report]()(using reporter: Reporter[report]):
  private var active: List[TestId] = Nil

  def skip(id: TestId): Boolean = false

  val report: report = reporter.report()

  def maybeRun[result](test: Test[result]): Optional[Trial[result]] =
    if skip(test.id) then Unset else run[result](test)

  def redraw(size: Int): Unit =
    if size > 0 then Out.print(e"\e[${size}A\r\e[2K")

    active.reverse.each: test =>
      Out.println(e"> ${WebColors.CadetBlue}(${test.id})${" "*(test.depth*2)}${test.name}\e[K")

    Out.print(e"\e[J")


  def run[result](test: Test[result]): Trial[result] =
    synchronized:
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
        synchronized:
          val size = active.size
          active = active.filter(_ != test.id)
          redraw(size)

    catch case error: Exception =>
      val ns: Long = System.nanoTime - ns0

      def lazyException(): Nothing =
        given canThrow: CanThrow[Exception] = unsafeExceptions.canThrowAny
        throw error

      Trial.Throws(lazyException, ns, context.captured.to(Map)).also:
        synchronized:
          val size = active.size
          active = active.filter(_ != test.id)
          redraw(size)

    finally
      Runner.harnessThreadLocal.set(None)

  def suite(suite: Testable, block: Testable ?=> Unit): Unit =
    if !skip(suite.id) then
      synchronized:
        val size = active.size
        active ::= suite.id
        redraw(size)

      reporter.declare(report, suite)
      block(using suite)

      synchronized:
        val size = active.size
        active = active.filter(_ != suite.id)
        redraw(size)

  def terminate(error: Throwable): Unit = synchronized:
    reporter.fail(report, error, active.to(Set))
    reporter.complete(report)

  def complete(): Unit =
    redraw(0)
    reporter.complete(report)
