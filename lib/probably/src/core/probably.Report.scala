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
        testId:      TestId,
        coordinates: List[(Axis.Spec, Value)],
        verdict:     Verdict )
    :   Report =

      val metrics = ListMap(Metric.Duration -> verdict.duration.toDouble)
      val report2 = report.record(testId, Entry.Kind.Check, coordinates, Run(verdict, metrics))

      verdict match
        case Verdict.Pass(_)       => report2
        case Verdict.Fail(_)       => report2
        case Verdict.AspirePass(_) => report2
        case Verdict.AspireFail(_) => report2

        case Verdict.Throws(error, _) =>
          report2.addDetail(testId, Verdict.Detail.Throws(StackTrace(error)))

        case Verdict.CheckThrows(error, _) =>
          report2.addDetail(testId, Verdict.Detail.CheckThrows(StackTrace(error)))

  given detail: Inclusion[Report, Verdict.Detail]:
    def include
      ( report:      Report,
        testId:      TestId,
        coordinates: List[(Axis.Spec, Value)],
        detail:      Verdict.Detail )
    :   Report =

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
  private var tests: ListMap[TestId, ReportLine] = ListMap()

  def list: List[(TestId, ReportLine)] = mutex(tests.to(List))
  def apply(testId: TestId): ReportLine = mutex(tests(testId))

  def update(testId: TestId, reportLine: ReportLine) = mutex:
    tests = tests.updated(testId, reportLine)

  def getOrElseUpdate(testId: TestId, reportLine: => ReportLine): ReportLine = mutex:
    if !tests.defines(testId) then tests = tests.updated(testId, reportLine)
    tests(testId)

// The report's intermediate representation: a tree of suites (namespacing only), whose
// leaves are uniform `Entry` values — one per named test, of any kind, holding that test's
// axes and cells. All rendering is performed by `Documenting` and the two renderers.
enum ReportLine:
  case Suite(suite: Optional[Testable], tests: TestsMap = TestsMap())
  case Item(entry: Entry)

// `final` so the capture checker infers a precise self-type rather than the universal capture an
// extensible class would get.
final class Report(using environment: Environment)(using palette: TestPalette):
  private var failure0: Optional[(Throwable, Set[TestId])] = Unset
  private var pass: Boolean = false

  private[probably] val lines: ReportLine.Suite = ReportLine.Suite(Unset)

  private[probably] val details: scm.SortedMap[TestId, scm.ArrayBuffer[Verdict.Detail]] =
    scm.TreeMap[TestId, scm.ArrayBuffer[Verdict.Detail]]()
    . withDefault(_ => scm.ArrayBuffer[Verdict.Detail]())

  private[probably] def failure: Optional[(Throwable, Set[TestId])] = failure0

  def passed: Boolean = failure0.absent && pass

  def resolve(suite: Optional[Testable]): ReportLine.Suite =
    suite.option.map: suite =>
      resolve(suite.parent).tests(suite.id).absolve match
        case suite@ReportLine.Suite(_, _) => suite

    . getOrElse(lines)

  def declare(suite: Testable): Report = this.also:
    resolve(suite.parent).tests(suite.id) = ReportLine.Suite(suite)

  def fail(error: Throwable, active: Set[TestId]): Unit = failure0 = (error, active)

  // Records one run at one coordinate of one test, creating the test's entry on first
  // sight. Repeated runs of the same coordinates accumulate in that cell; `headline`, when
  // given, designates the entry's headline metric.
  def record
    ( testId:      TestId,
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

          headline.let: metric =>
            entry.headline = metric

  def addDetail(testId: TestId, info: Verdict.Detail): Report =
    this.also(details(testId) = details(testId).append(info))

  def complete(coverage: Option[Coverage])(using Stdio): Unit =
    val document = Documenting.document(this)
    pass = document.totals.failed == 0 && failure0.absent && document.totals.total > 0

    if Ci.claudeCode then TerseRenderer.render(document)
    else AnsiRenderer.render(document, coverage)(using summon[Stdio], environment, palette)
