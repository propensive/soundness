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

import ambience.*, environments.javaEnvironment
import anticipation.*
import contingency.*
import denominative.*
import digression.*
// The stack-trace renderers live in `digression.ansi`, not in `StackTrace`'s companion, so they
// are not in implicit scope; without this import `.teletype` on a stack trace falls back to
// escapade's generic `Showable` renderer.
import digression.teletypeables.{exceptionTeletype, frameTeletype, methodTeletype, stackTraceTeletype}
import escapade.*
import fulminate.*
import gossamer.*
import iridescence.*
import prepositional.*
import rudiments.*
import turbulence.*
import vacuous.*

import luminosity.darkBrightness
import termcaps.environmentTermcap
import themes.solarizedTheme
import denominative.dysasymptotics.linearSize

abstract class Suite(suiteName: Message) extends Testable(suiteName):
  // The CURRENT `System.out`/`err`/`in` (`systemStdio`), read afresh at each use, rather than
  // the process's file descriptors (`virtualMachineStdio`): an in-process host invoking the
  // suite through `invoke` redirects `System.out` to its own stream for the duration, and the
  // FD-backed streams would bypass that redirection entirely (the report would go to the host
  // process's terminal, not the host's client). In a conventional `java -cp … <Suite>` run the
  // two are indistinguishable, since `System.out` IS the process's stdout.
  // A host may also have NULLED `System.out` outright — superlunary's `Executor` does, so
  // that stray prints from staged code cannot corrupt its protocol stream — and a staged
  // expression which touches a suite object's statics initializes the whole suite there. In
  // that case the FD-backed streams are the only ones left to fall back to.
  def suiteIo: Stdio =
    safely(stdios.systemStdio).or(stdios.virtualMachineStdio)

  private def makeRunner(selection: Selection): Runner[Report] = makeRunner(selection, Unset)

  // A `Reporter[Report]` which renders nothing: the report accumulates as usual (so `passed`
  // and selection accounting still work), but every datum leaves as a `TestEvent` through the
  // sink, execution brackets become progress events, and the runner's own ANSI drawing is
  // suppressed (`live = false`) — the consumer owns all presentation.
  private def eventReporter(sink: TestEvent -> Unit)(using Environment, TestPalette, Stdio)
  :   Reporter[Report] =

    new Reporter[Report]:
      def report(): Report =
        val report = Report()
        report.stream(sink)
        report

      def declare(report: Report, suite: Testable): Unit = report.declare(suite)

      def fail(report: Report, error: Throwable, active: Set[Test.Id]): Unit =
        report.fail(error, active)

      def complete(report: Report): Unit = report.complete(None)

      override def started(report: Report, id: Test.Id, suite: Boolean): Unit =
        if !suite then
          report.emit:
            TestEvent.TestStarted(TestEvent.Ref.of(id), jl.System.currentTimeMillis)

      override def ended(report: Report, id: Test.Id, suite: Boolean): Unit =
        report.emit:
          if suite then TestEvent.SuiteEnded(TestEvent.Ref.of(id), jl.System.currentTimeMillis)
          else TestEvent.TestEnded(TestEvent.Ref.of(id), jl.System.currentTimeMillis)

      override def live(report: Report): Boolean = false

  private def makeRunner(selection: Selection, sink: Optional[TestEvent -> Unit])
  :   Runner[Report] =

    given stdio: Stdio = suiteIo

    given palette: (theme: Theme) => TestPalette = new TestPalette:
      val yellow:      Color in Srgb = theme.spectrum.yellow.to[Srgb]
      val red:         Color in Srgb = theme.spectrum.red.to[Srgb]
      val blue:        Color in Srgb = theme.spectrum.blue.to[Srgb]

      def warning:     Color in Srgb = yellow
      def critical:    Color in Srgb = theme.spectrum.magenta.to[Srgb]
      def benchmark:   Color in Srgb = theme.spectrum.cyan.to[Srgb]
      def mixed:       Color in Srgb = blue
      def informative: Color in Srgb = blue
      def cold:        Color in Srgb = mix(yellow, red, 0.2)
      def warm:        Color in Srgb = mix(yellow, red, 0.5)
      def hot:         Color in Srgb = mix(yellow, red, 0.8)
      def accented:    Color in Srgb = theme.spectrum.cyan.to[Srgb]
      def highlight:   Color in Srgb = accent(yellow)
      def pass:        Color in Srgb = theme.spectrum.green.to[Srgb]
      def fail:        Color in Srgb = red

      def aspirePass:  Color in Srgb =
        mix(theme.spectrum.green.to[Srgb], theme.spectrum.cyan.to[Srgb], 0.5)

      def aspireFail:  Color in Srgb = subdue(yellow, 0.5)
      def detail:      Color in Srgb = blue
      def background:  Color in Srgb = theme.background.to[Srgb]
      def foreground:  Color in Srgb = theme.foreground.to[Srgb]
      def subdued:     Color in Srgb = subdue(theme.foreground.to[Srgb], 0.5)
      def unaccented:  Color in Srgb = subdued
      def positive:    Color in Srgb = pass
      def negative:    Color in Srgb = fail

    val reporter: Reporter[Report] = sink.lay(Reporter.report)(eventReporter(_))

    try Runner(selection)(using reporter) catch case error: Environment.Error =>
      jl.System.out.nn.println(StackTrace(error).teletype.render)
      ???

  @scala.caps.unsafe.untrackedCaptures
  var runner0: Runner[Report] = makeRunner(Selection.all)

  // An alias given is memoized on first use, which is safe here only because `main`
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

  // Runs the suite as `main` does, but RETURNS the exit status (0 = passed, 1 = failures,
  // 2 = the suite threw, 3 = environment error during reporting) instead of terminating the
  // JVM, so a host process — a test-running tool such as fume — can invoke suites in-process
  // without each one bringing the process down. The host is expected to load each suite in a
  // FRESH classloader per invocation (the suite object holds per-run state in `runner0`), and
  // may call this reflectively across an isolating classloader boundary: as a public method of
  // the suite object it gains a static forwarder in the mirror class, and its signature erases
  // to `invoke(String[]): int` — JDK types only, so no Soundness class need be shared between
  // the host's world and the suite's.
  final def invoke(arguments: Array[Text]^{}): Int = invoke(arguments.to[List])

  // A single-argument form for reflective and structural-type callers, taking the arguments
  // newline-separated in one `Text` (a selection term can never contain a newline), the empty
  // `Text` meaning none: its signature erases to `invoke(String): int`, which a host can
  // describe as `{ def invoke(arguments: Text): Int }` and call through
  // `reflectiveSelectable` — whereas a structural type over the array form is currently
  // uncompilable under capture checking (`classOf` of an array type fails pickling).
  final def invoke(arguments: Text): Int =
    invoke(arguments.cut(t"\n").filter(_ != t""))

  final def invoke(arguments: List[Text]): Int =
    val selection = Selection.parse(arguments)
    if !arguments.nil then runner0 = makeRunner(selection)

    if selection.listOnly then
      given stdio: Stdio = suiteIo

      try
        runner.suite(testableView, run())

        runner.listed.each: (id, kind) =>
          val path =
            def names(id: Test.Id): List[Text] =
              id.suite.let { suite => names(suite.id) }.or(Nil) :+ id.moniker.or(id.name.text)
            names(id).join(t"/")

          Out.println(t"${id.id}  ${kindName(kind)}  $path")

        0
      catch case error: Throwable =>
        jl.System.out.nn.println(StackTrace(error).teletype.render)
        2
    else
      try
        runner.suite(testableView, run())

        try
          if runner.admitted == 0 && !selection.trivial then
            given stdio: Stdio = suiteIo
            Out.println(t"No tests matched the selection.")

          runner.complete()
          if runner.report.passed then 0 else 1
        catch case error: Environment.Error =>
          jl.System.out.nn.println(StackTrace(error).teletype)
          3
      catch case error: Throwable =>
        runner.terminate(error)
        2

  // Runs the suite with an EVENT SINK: no rendering happens in this process — the report
  // still accumulates (for `passed` and selection accounting), but every result leaves as a
  // `TestEvent` through `sink`, ending with `RunCompleted`. Exit codes as `invoke`:
  // 0 = passed, 1 = failures, 2 = the suite threw, 3 = environment error. A `--list`
  // selection emits one `TestScheduled` per admitted test — the run's schedule, with each
  // test's REAL `Ref` (full path segments, file and line), which no text format could carry
  // unambiguously (test and suite names may contain any character).
  final def invoke(arguments: Text, sink: TestEvent -> Unit): Int =
    val selection = Selection.parse(arguments.cut(t"\n").filter(_ != t""))

    if selection.listOnly then
      runner0 = makeRunner(selection, sink)

      try
        runner.suite(testableView, run())

        runner.listed.each: (id, kind) =>
          sink(TestEvent.TestScheduled(TestEvent.Ref.of(id), TestEvent.kindName(kind)))

        0
      catch case error: Throwable => 2
    else
      runner0 = makeRunner(selection, sink)

      try
        runner.suite(testableView, run())

        try
          if runner.admitted == 0 && !selection.trivial then sink(TestEvent.NothingMatched(0))
          runner.complete()
          if runner.report.passed then 0 else 1
        catch case error: Environment.Error => 3

      catch case error: Throwable =>
        runner.terminate(error)
        2

  final def main(arguments: Array[Text]^{}): Unit = jl.System.exit(invoke(arguments))

  private def kindName(kind: Entry.Kind): Text = kind match
    case Entry.Kind.Check   => t"test"
    case Entry.Kind.Bench   => t"bench"
    case Entry.Kind.Stress  => t"stress"
    case Entry.Kind.Profile => t"profile"
