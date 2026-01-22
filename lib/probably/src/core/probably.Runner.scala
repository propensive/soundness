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
┃    Soundness, version 0.53.0.                                                                    ┃
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

import rudiments.*
import vacuous.*

object Runner:
  private[probably] val harnessThreadLocal: ThreadLocal[Option[Harness]] = ThreadLocal()

class Runner[report]()(using reporter: Reporter[report]):
  private var active: Set[TestId] = Set()
  def skip(id: TestId): Boolean = false
  val report: report = reporter.make()

  def maybeRun[result](test: Test[result]): Optional[Trial[result]] =
    if skip(test.id) then Unset else run[result](test)

  def run[result](test: Test[result]): Trial[result] =
    synchronized { active += test.id }
    val ctx = Harness()
    Runner.harnessThreadLocal.set(Some(ctx))
    val ns0 = System.nanoTime

    try
      val ns0: Long = System.nanoTime
      val result: result = test.action(ctx)
      val ns: Long = System.nanoTime - ns0
      Trial.Returns(result, ns, ctx.captured.to(Map)).also:
        synchronized { active -= test.id }

    catch case err: Exception =>
      val ns: Long = System.nanoTime - ns0

      def lazyException(): Nothing =
        given canThrow: CanThrow[Exception] = unsafeExceptions.canThrowAny
        throw err

      Trial.Throws(lazyException, ns, ctx.captured.to(Map)).also:
        synchronized { active -= test.id }

    finally
      Runner.harnessThreadLocal.set(None)

  def suite(suite: Testable, block: Testable ?=> Unit): Unit =
    if !skip(suite.id) then
      reporter.declare(report, suite)
      block(using suite)

  def terminate(error: Throwable): Unit = synchronized:
    reporter.fail(report, error, active)
    reporter.complete(report)

  def complete(): Unit = reporter.complete(report)
