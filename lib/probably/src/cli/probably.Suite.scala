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

import anticipation.*
import fulminate.*
import gossamer.*
import rudiments.*
import vacuous.*

// A suite is not a main class. The beneficence plugin lists every `Suite` object in the
// `META-INF/services/probably.Suite` index of the jar it compiles, and a host — the fume test
// runner — discovers suites from that index and drives each one through `invoke`, receiving
// its results as a stream of `TestEvent`s (`probably.Streamer` in the `events` module carries
// them across a classloader boundary as BinTEL frames). Nothing is rendered here.
abstract class Suite(suiteName: Message) extends Testable(suiteName):
  // A `Reporter[Report]` whose report emits every datum as a `TestEvent` through the sink,
  // with execution brackets becoming progress events — the consumer owns all presentation.
  private def eventReporter(sink: TestEvent -> Unit): Reporter[Report] =
    new Reporter[Report]:
      def report(): Report =
        val report = Report()
        report.stream(sink)
        report

      def declare(report: Report, suite: Testable): Unit = report.declare(suite)

      def fail(report: Report, error: Throwable, active: Set[Test.Id]): Unit =
        report.fail(error, active)

      def complete(report: Report): Unit = report.complete()

      override def started(report: Report, id: Test.Id, suite: Boolean): Unit =
        if !suite then
          report.emit:
            TestEvent.TestStarted(TestEvent.Ref.of(id), jl.System.currentTimeMillis)

      override def ended(report: Report, id: Test.Id, suite: Boolean): Unit =
        report.emit:
          if suite then TestEvent.SuiteEnded(TestEvent.Ref.of(id), jl.System.currentTimeMillis)
          else TestEvent.TestEnded(TestEvent.Ref.of(id), jl.System.currentTimeMillis)

  private def makeRunner(selection: Selection, sink: Optional[TestEvent -> Unit])
  :   Runner[Report] =

    Runner(selection)(using sink.lay(Reporter.report)(eventReporter(_)))

  @scala.caps.unsafe.untrackedCaptures
  var runner0: Runner[Report] = makeRunner(Selection.all, Unset)

  // An alias given is memoized on first use, which is safe here only because `invoke`
  // replaces `runner0` with a selection-aware runner before anything summons it.
  given runner: Runner[Report] = runner0

  // A pure `Testable` view of this suite rather than `this`: the suite itself captures its
  // runner and deferred test blocks, so it is a capability, which `Testable`'s pure type
  // (rightly) forbids. `Testable` equality is structural (name and parent), so the view is
  // interchangeable with the suite wherever tests are grouped or reported.
  private val testableView: Testable = Testable(suiteName)
  given testable: Testable = testableView

  def run(): Unit

  def apply()(using runner: Runner[Report]): Unit =
    runner0 = runner
    runner.suite(testableView, run())

  // Runs the suite with an EVENT SINK and RETURNS the exit status — 0 = passed, 1 = failures,
  // 2 = the suite threw — instead of terminating the JVM, so a host can invoke suites
  // in-process without each one bringing the process down. The host is expected to load each
  // suite in a FRESH classloader per invocation (the suite object holds per-run state in
  // `runner0`). The arguments are selection terms, newline-separated in one `Text` (a term can
  // never contain a newline), the empty `Text` meaning none. The report still accumulates
  // (for `passed` and selection accounting), but every result leaves as a `TestEvent`
  // through `sink`, ending with `RunCompleted`. A `--list` selection emits one
  // `TestScheduled` per admitted test — the run's schedule, with each test's REAL `Ref`
  // (full path segments, file and line), which no text format could carry unambiguously
  // (test and suite names may contain any character).
  final def invoke(arguments: Text, sink: TestEvent -> Unit): Int =
    val selection = Selection.parse(arguments.cut(t"\n").filter(_ != t""))

    if selection.listOnly then
      runner0 = makeRunner(selection, sink)

      try
        runner.suite(testableView, run())

        runner.listed.each: (id, kind, expected) =>
          sink(TestEvent.TestScheduled(TestEvent.Ref.of(id), TestEvent.kindName(kind), expected))

        0
      catch case error: Throwable => 2
    else
      runner0 = makeRunner(selection, sink)

      try
        runner.suite(testableView, run())
        if runner.admitted == 0 && !selection.trivial then sink(TestEvent.NothingMatched(0))
        runner.complete()
        if runner.report.passed then 0 else 1

      catch case error: Throwable =>
        runner.terminate(error)
        2
