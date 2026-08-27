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

import scala.collection.mutable as scm


import ambience.*
import anticipation.*
import digression.*
import escapade.*
import iridescence.*
import rudiments.*
import turbulence.*
import vacuous.*

object Report:
  given verdict: Inclusion[Report, Verdict]:
    def include
      ( report:      Report,
        testId:      Test.Id,
        coordinates: List[(Axis.Spec, Value)],
        verdict:     Verdict )
    :   Report =

      val metrics = Ledger(Metric.Duration -> verdict.duration.toDouble)
      val report2 = report.record(testId, Entry.Kind.Check, coordinates, Run(verdict, metrics))

      report.emit:
        TestEvent.TestCompleted
          ( TestEvent.Ref.of(testId),
            TestEvent.kindName(Entry.Kind.Check),
            TestEvent.Coordinate.of(coordinates),
            TestEvent.Outcome.of(verdict),
            TestEvent.MetricValue.of(metrics),
            java.lang.System.currentTimeMillis )

      verdict match
        case Verdict.Pass(_)       => report2
        case Verdict.Fail(_)       => report2
        case Verdict.AspirePass(_) => report2
        case Verdict.AspireFail(_) => report2

        case Verdict.Throws(error, _) =>
          val stack = StackTrace(error)

          report.emit:
            TestEvent.DetailThrows(TestEvent.Ref.of(testId), false, TestEvent.Trace.of(stack))

          report2.addDetail(testId, Verdict.Detail.Throws(stack))

        case Verdict.CheckThrows(error, _) =>
          val stack = StackTrace(error)

          report.emit:
            TestEvent.DetailThrows(TestEvent.Ref.of(testId), true, TestEvent.Trace.of(stack))

          report2.addDetail(testId, Verdict.Detail.CheckThrows(stack))

  given anchor: Inclusion[Report, Anchor]:
    def include
      ( report:      Report,
        testId:      Test.Id,
        coordinates: List[(Axis.Spec, Value)],
        anchor:      Anchor )
    :   Report =

      report.emit:
        val coordinate = TestEvent.Coordinate.of(anchor.axis, anchor.value)

        TestEvent.AnchorRecorded
          ( TestEvent.Ref.of(testId),
            anchor.axis.label,
            coordinate.discrete,
            coordinate.integral,
            coordinate.decimal,
            anchor.baseline.compare.toString.tt,
            anchor.baseline.metric.toString.tt,
            anchor.baseline.mode.toString.tt )

      report.anchor(testId, anchor)

  given detail: Inclusion[Report, Verdict.Detail]:
    def include
      ( report:      Report,
        testId:      Test.Id,
        coordinates: List[(Axis.Spec, Value)],
        detail:      Verdict.Detail )
    :   Report =

      report.emit:
        val ref = TestEvent.Ref.of(testId)

        detail match
          case Verdict.Detail.Throws(stack) =>
            TestEvent.DetailThrows(ref, false, TestEvent.Trace.of(stack))

          case Verdict.Detail.CheckThrows(stack) =>
            TestEvent.DetailThrows(ref, true, TestEvent.Trace.of(stack))

          case Verdict.Detail.Captures(values) =>
            TestEvent.DetailCaptures(ref, values)

          case Verdict.Detail.Compare(expected, found, juxtaposition) =>
            val rows = TestEvent.CompareRow.flatten(juxtaposition)
            TestEvent.DetailCompare(ref, expected, found, rows)

          case Verdict.Detail.Message(message) =>
            TestEvent.DetailMessage(ref, message)

      report.addDetail(testId, detail)

  enum Status:
    case Pass, Fail, Throws, CheckThrows, Mixed, Suite, Bench, Stress, Profile, AspirePass,
      AspireFail

    private val nbsp = '\u00a0'

    def symbol(using palette: TestPalette): Teletype = this match
      case Pass        => e"${Bg(palette.pass)}($Bold(${Fg(palette.black)}( ✓ )))"
      case Fail        => e"${Bg(palette.fail)}($Bold(${Fg(palette.black)}( ✗ )))"
      case Throws      => e"${Bg(palette.warning)}($Bold(${Fg(palette.black)}( ! )))"
      case CheckThrows => e"${Bg(palette.critical)}($Bold(${Fg(palette.black)}( ‼ )))"
      case Mixed       => e"${Bg(palette.mixed)}($Bold(${Fg(palette.black)}( ? )))"
      case Suite       => e"   "
      case Bench       => e"${Bg(palette.benchmark)}($Bold(${Fg(palette.black)}($nbsp*$nbsp)))"
      case Stress      => e"${Bg(palette.benchmark)}($Bold(${Fg(palette.black)}($nbsp≈$nbsp)))"
      case Profile     => e"${Bg(palette.benchmark)}($Bold(${Fg(palette.black)}($nbsp%$nbsp)))"
      case AspirePass  => e"${Bg(palette.aspirePass)}($Bold(${Fg(palette.black)}( ↑ )))"
      case AspireFail  => e"${Bg(palette.aspireFail)}($Bold(${Fg(palette.black)}( ↓ )))"

    def describe: Teletype = this match
      case Pass        => e"Pass"
      case Fail        => e"Fail"
      case Throws      => e"Throws exception"
      case CheckThrows => e"Exception in check"
      case Mixed       => e"Mixed"
      case Suite       => e"Suite"
      case Bench       => e"Benchmark"
      case Stress      => e"Stress"
      case Profile     => e"Profile"
      case AspirePass  => e"Aspire passed"
      case AspireFail  => e"Aspire failed"

// The insertion-ordered, mutex-guarded map of report lines within one suite node.
class TestsMap():
  private val mutex: Mutex = Mutex()
  @scala.caps.unsafe.untrackedCaptures
  private var tests: Ledger[Test.Id, ReportLine] = Ledger()

  def list: List[(Test.Id, ReportLine)] = mutex(tests.to[List])
  def apply(testId: Test.Id): Optional[ReportLine] = mutex(tests(testId))

  def update(testId: Test.Id, reportLine: ReportLine) = mutex:
    tests = tests.define(testId, reportLine)

  def getOrElseUpdate(testId: Test.Id, reportLine: => ReportLine): ReportLine = mutex:
    tests(testId).or:
      val line = reportLine
      tests = tests.define(testId, line)
      line

// The report's intermediate representation: a tree of suites (namespacing only), whose
// leaves are uniform `Entry` values — one per named test, of any kind, holding that test's
// axes and cells. All rendering is performed by `Documenting` and the two renderers.
enum ReportLine:
  case Suite(suite: Optional[Testable], tests: TestsMap = TestsMap())
  case Item(entry: Entry)

// `final` so the capture checker infers a precise self-type rather than the universal capture an
// extensible class would get.
final class Report(using environment: Environment)(using palette: TestPalette):
  // Event emission: when a sink has been installed (`stream`), every recorded datum also
  // leaves as a `TestEvent`, and `complete` emits `RunCompleted` INSTEAD of rendering — the
  // consumer owns presentation. With no sink (the default), behavior is exactly as before.
  // The sink is a PURE function (`->`): `Report` appears as a pure type throughout the
  // `Inclusion` machinery, so it may not retain a capability — and a real sink (writing
  // through a host's untracked `OutputStream` under a `Mutex`) satisfies purity naturally.
  @scala.caps.unsafe.untrackedCaptures
  private var sink: Optional[TestEvent -> Unit] = Unset

  private[probably] def stream(sink1: TestEvent -> Unit): Unit = sink = sink1
  private[probably] def streaming: Boolean = sink != Unset

  private[probably] def emit(event: => TestEvent): Unit = sink.let(_(event))

  @scala.caps.unsafe.untrackedCaptures
  private var failure0: Optional[(Throwable, Set[Test.Id])] = Unset
  @scala.caps.unsafe.untrackedCaptures
  private var pass: Boolean = false

  private[probably] val lines: ReportLine.Suite = ReportLine.Suite(Unset)

  private[probably] val details: scm.SortedMap[Test.Id, scm.ArrayBuffer[Verdict.Detail]] =
    scm.TreeMap[Test.Id, scm.ArrayBuffer[Verdict.Detail]]()
    . withDefault(_ => scm.ArrayBuffer[Verdict.Detail]())

  private[probably] def failure: Optional[(Throwable, Set[Test.Id])] = failure0

  def passed: Boolean = failure0.absent && pass

  def resolve(suite: Optional[Testable]): ReportLine.Suite =
    suite.option.map: suite =>
      resolve(suite.parent).tests(suite.id).absolve match
        case suite@ReportLine.Suite(_, _) => suite

    . getOrElse(lines)

  // Non-destructive: a suite is declared once per run, but two distinct suites can share a
  // `Test.Id` — `Testable`'s identity is its name and parent, and `Suite`'s own `Testable` is
  // built on a single source line, so every top-level suite of the same name has the same id.
  // An unconditional update would replace the earlier suite's whole subtree, silently
  // discarding everything it had recorded; merging into the existing node keeps both.
  def declare(suite: Testable): Report = this.also:
    emit(TestEvent.SuiteStarted(TestEvent.Ref.of(suite.id), java.lang.System.currentTimeMillis))
    resolve(suite.parent).tests.getOrElseUpdate(suite.id, ReportLine.Suite(suite))

  def fail(error: Throwable, active: Set[Test.Id]): Unit =
    emit:
      TestEvent.RunTerminated
        ( TestEvent.Trace.of(StackTrace(error)),
          active.to[List].map(TestEvent.Ref.of(_)),
          java.lang.System.currentTimeMillis )

    failure0 = (error, active)

  // Records one run at one coordinate of one test, creating the test's entry on first
  // sight. Repeated runs of the same coordinates accumulate in that cell; `headline`, when
  // given, designates the entry's headline metric.
  def record
    ( testId:      Test.Id,
      kind:        Entry.Kind,
      coordinates: List[(Axis.Spec, Value)],
      run:         Run,
      headline:    Optional[Metric] = Unset )
  :   Report =

    this.also:
      val nodes = resolve(testId.suite).tests

      nodes.getOrElseUpdate(testId, ReportLine.Item(Entry(testId, kind))).absolve match
        case ReportLine.Item(entry) =>
          entry.cell(coordinates).record(run)

          headline.let: metric => entry.headline = metric

  def addDetail(testId: Test.Id, info: Verdict.Detail): Report =
    this.also(details(testId) = details(testId).append(info))

  // Sets the comparison anchor of a test's entry: the axis value against which its other
  // cells are compared. A no-op if the test recorded no cells at all.
  def anchor(testId: Test.Id, anchor: Anchor): Report = this.also:
    resolve(testId.suite).tests.list.seek(_(0) == testId).let(_(1)).let:
      case ReportLine.Item(entry) => entry.anchor = anchor
      case _: ReportLine.Suite    => ()

  def complete(coverage: Option[Coverage])(using Stdio): Unit =
    val document = Documenting.document(this)
    pass = document.totals.failed == 0 && failure0.absent && document.totals.total > 0

    if streaming then emit(TestEvent.RunCompleted(passed, java.lang.System.currentTimeMillis))
    else if Ci.claudeCode then TerseRenderer.render(document)
    else AnsiRenderer.render(document, coverage)(using summon[Stdio], environment, palette)
