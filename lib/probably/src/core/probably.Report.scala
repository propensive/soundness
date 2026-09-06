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

import scala.collection.immutable as sci
import scala.collection.mutable as scm

import anticipation.*
import digression.*
import scala.math.Ordering

import rudiments.*
import vacuous.*
import symbolism.*

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
// axes and cells. Rendering is the consumer's business: everything recorded here also leaves as
// a `TestEvent` through the installed sink.
enum ReportLine:
  case Suite(suite: Optional[Testable], tests: TestsMap = TestsMap())
  case Item(entry: Entry)

// `final` so the capture checker infers a precise self-type rather than the universal capture an
// extensible class would get.
final class Report():
  // Event emission: when a sink has been installed (`stream`), every recorded datum also
  // leaves as a `TestEvent`, and `complete` emits `RunCompleted` INSTEAD of rendering — the
  // consumer owns presentation. With no sink (the default), behavior is exactly as before.
  // The sink is a PURE function (`->`): `Report` appears as a pure type throughout the
  // `Inclusion` machinery, so it may not retain a capability — and a real sink (writing
  // through a host's untracked `OutputStream` under a `Mutex`) satisfies purity naturally.
  @scala.caps.unsafe.untrackedCaptures
  private var sink: Optional[TestEvent -> Unit] = Unset

  private[probably] def stream(sink1: TestEvent -> Unit): Unit = sink = sink1

  private[probably] def emit(event: => TestEvent): Unit = sink.let(_(event))

  @scala.caps.unsafe.untrackedCaptures
  private var failure0: Optional[(Throwable, Set[Test.Id])] = Unset
  @scala.caps.unsafe.untrackedCaptures
  private var pass: Boolean = false

  private[probably] val lines: ReportLine.Suite = ReportLine.Suite(Unset)

  // A `TreeMap` orders its keys with a stdlib `Ordering`, so `Test.Id`'s `Comparable` is
  // adapted here rather than being one.
  private given ordering: Ordering[Test.Id] = Test.Id.comparable.ordering

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

  // Every entry beneath a suite node, in report order. Plain stdlib collections throughout
  // this settlement: lambdas over the opaque collections' higher-kinded methods are where the
  // compiler's `wildApprox` assertion trips (see `Documenting` before its removal).
  private def entries(line: ReportLine): sci.List[Entry] = line match
    case ReportLine.Suite(_, tests) => tests.list.stdlib.flatMap { pair => entries(pair(1)) }
    case ReportLine.Item(entry)     => sci.List(entry)

  private def verdicts(entry: Entry): sci.List[Verdict] =
    entry.cells.stdlib.flatMap { cell => cell(1).runs.stdlib }.flatMap { run => run.verdict.option }

  // A check counts only once it has a verdict; a measurement (a benchmark, stress test or
  // profile) counts, and passes, by having run at all. A check passes when its verdicts, over
  // all its cells, are uniformly `Pass`, uniformly `AspirePass` or uniformly `AspireFail`: a
  // failure, an exception, or a mixture of outcomes fails it.
  private def counted(entry: Entry): Boolean =
    entry.kind != Entry.Kind.Check || verdicts(entry).nonEmpty

  private def failing(entry: Entry): Boolean =
    entry.kind == Entry.Kind.Check && {
      val all = verdicts(entry)
      val passes = all.forall { case Verdict.Pass(_) => true; case _ => false }
      val aspirePasses = all.forall { case Verdict.AspirePass(_) => true; case _ => false }
      val aspireFails = all.forall { case Verdict.AspireFail(_) => true; case _ => false }
      !(passes || aspirePasses || aspireFails)
    }

  // Settles the run: `passed` is decided here, and `RunCompleted` leaves through the sink (a
  // no-op without one). A run in which nothing was counted has not passed.
  def complete(): Unit =
    val all = entries(lines).filter(counted)
    pass = failure0.absent && all.nonEmpty && !all.exists(failing)
    emit(TestEvent.RunCompleted(passed, java.lang.System.currentTimeMillis))
