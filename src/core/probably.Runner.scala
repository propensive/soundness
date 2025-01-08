/*
    Probably, version [unreleased]. Copyright 2025 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package probably

import rudiments.*
import vacuous.*

object Runner:
  private[probably] val testContextThreadLocal: ThreadLocal[Option[TestContext]] = ThreadLocal()

class Runner[ReportType]()(using reporter: TestReporter[ReportType]):
  private var active: Set[TestId] = Set()
  def skip(id: TestId): Boolean = false
  val report: ReportType = reporter.make()

  def maybeRun[T, S](test: Test[T]): Optional[Trial[T]] = if skip(test.id) then Unset else run[T, S](test)

  def run[T, S](test: Test[T]): Trial[T] =
    synchronized { active += test.id }
    val ctx = TestContext()
    Runner.testContextThreadLocal.set(Some(ctx))
    val ns0 = System.nanoTime

    try
      val ns0: Long = System.nanoTime
      val result: T = test.action(ctx)
      val ns: Long = System.nanoTime - ns0
      Trial.Returns(result, ns, ctx.captured.to(Map)).also:
        synchronized { active -= test.id }

    catch case err: Exception =>
      val ns: Long = System.nanoTime - ns0

      def lazyException(): Nothing =
        given CanThrow[Exception] = unsafeExceptions.canThrowAny
        throw err

      Trial.Throws(lazyException, ns, ctx.captured.to(Map)).also:
        synchronized { active -= test.id }

    finally
      Runner.testContextThreadLocal.set(None)

  def suite(suite: TestSuite, block: TestSuite ?=> Unit): Unit =
    if !skip(suite.id) then
      reporter.declareSuite(report, suite)
      block(using suite)

  def terminate(error: Throwable): Unit = synchronized:
    reporter.fail(report, error, active)

  def complete(): Unit = reporter.complete(report)
