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
┃    Soundness, version 0.63.0.                                                                    ┃
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
import contingency.*
import dendrology.*
import denominative.*
import digression.*
import distillate.*
import escapade.*
import escritoire.*, columnAttenuation.ignoreAttenuation
import fulminate.*
import gossamer.*
import hieroglyph.*
import iridescence.*
import prepositional.*
import rudiments.*
import spectacular.*
import symbolism.*
import turbulence.*
import vacuous.*

import tableStyles.defaultTableStyle

object Report:
  given verdict: Inclusion[Report, Verdict]:
    def include(report: Report, testId: TestId, verdict: Verdict): Report =
      val report2 = report.addVerdict(testId, verdict)

      verdict match
        case Verdict.Pass(_)       => report2
        case Verdict.Fail(_)       => report2
        case Verdict.AspirePass(_) => report2
        case Verdict.AspireFail(_) => report2

        case Verdict.Throws(error, _) =>
          report2.addDetail(testId, Verdict.Detail.Throws(StackTrace(error)))

        case Verdict.CheckThrows(error, _) =>
          report2.addDetail(testId, Verdict.Detail.CheckThrows(StackTrace(error)))

  given detail: Inclusion[Report, Verdict.Detail] = _.addDetail(_, _)

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

// `final` so the capture checker infers a precise self-type rather than the universal capture an
// extensible class would get — otherwise the nested `Status`/`Summary` types become `^{Report.this}`
// and leak `any` through the status-legend iteration.
final class Report(using Environment)(using palette: TestPalette):
  import Report.Status
  val metrics = textMetrics.wideCharacterWidthMetric

  given measurable: Char is Measurable:
    def width(char: Char): Int = char match
      case '✓' | '✗' | '⎇' | '↑' | '↓'  => 1
      case _                            => metrics.width(char)

  private var failure: Optional[(Throwable, Set[TestId])] = Unset
  private var pass: Boolean = false

  private val lines: ReportLine.Suite = ReportLine.Suite(Unset)

  private val details: scm.SortedMap[TestId, scm.ArrayBuffer[Verdict.Detail]] =
    scm.TreeMap[TestId, scm.ArrayBuffer[Verdict.Detail]]()
    . withDefault(_ => scm.ArrayBuffer[Verdict.Detail]())

  def passed: Boolean = failure.absent && pass

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

  enum ReportLine:
    case Suite(suite: Optional[Testable], tests: TestsMap = TestsMap())
    case Test(test: TestId, verdicts: scm.ArrayBuffer[Verdict] = scm.ArrayBuffer())
    case Bench(test: TestId, benchmark: Benchmark)
    case Strain(test: TestId, strain: probably.Strain)
    case Hotspots(test: TestId, hotspots: probably.Hotspots)

    def summaries: List[Summary] = this match
      case Suite(suite, tests) =>
        val rest = tests.list.sortBy(_(0).timestamp).flatMap(_(1).summaries)

        if suite.absent then rest
        else Summary(Status.Suite, suite.option.get.id, 0, 0, 0, 0) :: rest

      case Bench(testId, bench@Benchmark(_, _, _, _, _, _, _, _, _, _, _)) =>
        List(Summary(Status.Bench, testId, 0, 0, 0, 0))

      case Strain(testId, _) =>
        List(Summary(Status.Stress, testId, 0, 0, 0, 0))

      case Hotspots(testId, _) =>
        List(Summary(Status.Profile, testId, 0, 0, 0, 0))

      case Test(testId, buffer) =>
        val status =
          if buffer.all(_.typed[Verdict.Pass]) then Status.Pass
          else if buffer.all(_.typed[Verdict.Fail]) then Status.Fail
          else if buffer.all(_.typed[Verdict.Throws]) then Status.Throws
          else if buffer.all(_.typed[Verdict.CheckThrows]) then Status.CheckThrows
          else if buffer.all(_.typed[Verdict.AspirePass]) then Status.AspirePass
          else if buffer.all(_.typed[Verdict.AspireFail]) then Status.AspireFail
          else Status.Mixed

        val min: Long = buffer.map(_.duration).min
        val max: Long = buffer.map(_.duration).max
        val avg: Long = buffer.fuse(0L)(state + next.duration)/buffer.length

        List(Summary(status, testId, buffer.length, min, max, avg))

  def resolve(suite: Optional[Testable]): ReportLine.Suite =
    suite.option.map: suite =>
      resolve(suite.parent).tests(suite.id).absolve match
        case suite@ReportLine.Suite(_, _) => suite

    . getOrElse(lines)

  def declare(suite: Testable): Report = this.also:
    resolve(suite.parent).tests(suite.id) = ReportLine.Suite(suite)

  def fail(error: Throwable, active: Set[TestId]): Unit = failure = (error, active)

  def addBenchmark(testId: TestId, benchmark: Benchmark): Report = this.also:
    val benchmarks = resolve(testId.suite).tests
    benchmarks.getOrElseUpdate(testId, ReportLine.Bench(testId, benchmark))

  def addStrain(testId: TestId, strain: probably.Strain): Report = this.also:
    val strains = resolve(testId.suite).tests
    strains.getOrElseUpdate(testId, ReportLine.Strain(testId, strain))

  def addHotspots(testId: TestId, hotspots: probably.Hotspots): Report = this.also:
    val profiles = resolve(testId.suite).tests
    profiles.getOrElseUpdate(testId, ReportLine.Hotspots(testId, hotspots))

  def addVerdict(testId: TestId, verdict: Verdict): Report = this.also:
    val tests = resolve(testId.suite).tests

    tests.getOrElseUpdate(testId, ReportLine.Test(testId, scm.ArrayBuffer[Verdict]())).absolve match
      case ReportLine.Test(_, buffer) => buffer.append(verdict)

  def addDetail(testId: TestId, info: Verdict.Detail): Report =
    this.also(details(testId) = details(testId).append(info))

  private def benches(line: ReportLine): Iterable[ReportLine.Bench] = line match
    case bench@ReportLine.Bench(_, _) => Iterable(bench)
    case ReportLine.Suite(_, tests)   => tests.list.flatMap: (_, line) => benches(line)
    case _                            => Nil

  private def strains(line: ReportLine): Iterable[ReportLine.Strain] = line match
    case strain@ReportLine.Strain(_, _) => Iterable(strain)
    case ReportLine.Suite(_, tests)     => tests.list.flatMap: (_, line) => strains(line)
    case _                              => Nil

  private def profiles(line: ReportLine): Iterable[ReportLine.Hotspots] = line match
    case profile@ReportLine.Hotspots(_, _) => Iterable(profile)
    case ReportLine.Suite(_, tests)        => tests.list.flatMap: (_, line) => profiles(line)
    case _                                 => Nil

  // A histogram bar of `samples` scaled against `max` over a 40-cell span: full blocks
  // with a final fractional character from the eighth-block series. Any nonzero count
  // shows at least the thinnest bar.
  private def bar(samples: Long, max: Long): Text =
    val eighths = (if max == 0L then 0L else samples*320L/max).max(if samples > 0L then 1L else 0L)
    val partial = List(t"", t"▏", t"▎", t"▍", t"▌", t"▋", t"▊", t"▉")
    t"█"*(eighths/8L).toInt + partial((eighths%8L).toInt)

  val unitsSeq: List[Teletype] =
    List(e"${Fg(palette.cold)}(µs)", e"${Fg(palette.warm)}(ms)", e"${Fg(palette.hot)}(s) ")

  def showTime(n: Long, units: List[Teletype] = unitsSeq): Teletype = units match
    case Nil => n.show.teletype

    case unit :: rest =>
      if n > 100000L then showTime(n/1000L, rest) else
        val sig = (n/1000L).show
        val frac = (n%1000).show.pad(3, Rtl, '0')
        e"${Fg(palette.foreground)}(${sig}.$frac) ${unit}"

  val memoryUnitsSeq: List[Teletype] =
    List(e"${Fg(palette.cold)}(kB)", e"${Fg(palette.warm)}(MB)", e"${Fg(palette.hot)}(GB)")

  def showMemory(n: Long, units: List[Teletype] = memoryUnitsSeq): Teletype = units match
    case Nil => n.show.teletype

    case unit :: rest =>
      if n > 100000L then showMemory(n/1000L, rest) else
        val sig = (n/1000L).show
        val frac = (n%1000).show.pad(3, Rtl, '0')
        e"${Fg(palette.foreground)}(${sig}.$frac) ${unit}"

  case class Summary(status: Status, id: TestId, count: Int, min: Long, max: Long, avg: Long):
    def indentedName: Teletype =
      val depth = id.suite.let(_.id.depth).or(0) + 1

      val title =
        if status == Status.Suite then e"${Fg(palette.foreground)}($Bold(${id.name}))"
        else e"${id.name}"

      e"${t"  "*(depth - 1)}$title"

    def minTime: Teletype = if min == 0L then e"" else showTime(min)
    def maxTime: Teletype = if max == 0L then e"" else showTime(max)
    def avgTime: Teletype = if avg == 0L then e"" else showTime(avg)
    def iterations: Teletype = if count == 0 then e"" else count.teletype

  def complete(coverage: Option[Coverage])(using Stdio): Unit =
    if Ci.claudeCode then terseComplete() else humanComplete(coverage)

  private def terseComplete()(using Stdio): Unit =
    import tableStyles.minimalTableStyle

    val columns: Int = safely(Environment.columns.as[Int]).or(120)
    val summaryLines = lines.summaries
    val totals = summaryLines.groupBy(_.status).view.mapValues(_.size).to(Map) - Status.Suite
    val passed: Int =
      totals.getOrElse(Status.Pass, 0)
      + totals.getOrElse(Status.Bench, 0)
      + totals.getOrElse(Status.Stress, 0)
      + totals.getOrElse(Status.Profile, 0)
    val aspirePassed: Int = totals.getOrElse(Status.AspirePass, 0)
    val aspireFailed: Int = totals.getOrElse(Status.AspireFail, 0)
    val total: Int = totals.values.sum
    val failed: Int = total - passed - aspirePassed - aspireFailed
    if failed == 0 && failure.absent then pass = true

    if total == 0 then Out.println(t"No tests were run.")
    else
      val summary = t"$passed passed, $failed failed, $aspirePassed aspire-passed, "
      Out.println(t"$summary$aspireFailed aspire-failed, $total total")

    def truncate(text: Text, max: Int = 800): Text =
      if text.length <= max then text else t"${text.keep(max)}…"

    def statusWord(status: Status): Text = status match
      case Status.Pass        => t"pass"
      case Status.Fail        => t"fail"
      case Status.Throws      => t"throws"
      case Status.CheckThrows => t"check-throws"
      case Status.Mixed       => t"mixed"
      case Status.Suite       => t"suite"
      case Status.Bench       => t"bench"
      case Status.Stress      => t"stress"
      case Status.Profile     => t"profile"
      case Status.AspirePass  => t"aspire-pass"
      case Status.AspireFail  => t"aspire-fail"

    def formatFrame(frame: StackTrace.Frame): Text =
      val ln = frame.line.let(_.toString.tt).or(t"?")
      t"  at ${frame.method.cls}.${frame.method.method} (${frame.file}:$ln)"

    val bySuite = benches(lines).to(List).groupBy(_.test.suite).to(List)
      . sortBy(_(1).iterator.map(_.test.timestamp).min)

    bySuite.each: (suite, benchmarks) =>
      val suiteName = suite.let(_.name.text).or(t"")
      Out.println(t"")
      if suiteName.length > 0 then Out.println(suiteName)

      Scaffold[ReportLine.Bench]
        ( Column(e"Hash"): b =>
            e"${b.test.id}",
          Column(e"Benchmark"): b =>
            e"${b.test.name}",
          Column(e"n", textAlign = TextAlignment.Right): b =>
            e"${b.benchmark.iterations.toLong}",
          Column(e"Mean", textAlign = TextAlignment.Right): b =>
            showTime(b.benchmark.mean.toLong),
          Column(e"SD", textAlign = TextAlignment.Right): b =>
            showTime(b.benchmark.sd.toLong),
          Column(e"Throughput", textAlign = TextAlignment.Right): b =>
            val tp = b.benchmark.throughput
            if tp == 0 then e"" else e"$tp op/s" )

      . tabulate(benchmarks.sortBy(_.test.timestamp)).grid(columns).render
      . each(Out.println(_))

    val strainBySuite = strains(lines).to(List).groupBy(_.test.suite).to(List)
      . sortBy(_(1).iterator.map(_.test.timestamp).min)

    strainBySuite.each: (suite, rows) =>
      val suiteName = suite.let(_.name.text).or(t"")
      Out.println(t"")
      if suiteName.length > 0 then Out.println(suiteName)

      // The same comparison panel as the human report, uncoloured: one throughput
      // sparkline per test across the probed concurrencies.
      val series: List[(Text, Map[Int, probably.Strain], Optional[probably.Strain])] =
        rows.groupBy(_.test.codepoint).to(List)
        . sortBy(_(1).map(_.test.timestamp).min)
        . map: (_, group) =>
            val name = group.head.test.name.text
            val cut1 = name.s.indexOf(" (N = ")
            val cut2 = name.s.indexOf(" (sustained, N = ")
            val cut = if cut1 < 0 then cut2 else if cut2 < 0 then cut1 else cut1.min(cut2)
            val label = if cut < 0 then name else name.keep(cut)
            val curve = group.map { row => row.strain.concurrency -> row.strain }.to(Map)
            val sustained: Optional[probably.Strain] = group.find(_.strain.sustained).map(_.strain).getOrElse(Unset)
            (label, curve, sustained)

      val steps: List[Int] =
        val all = series.flatMap(_(1).keys)

        val shared =
          if series.length < 2 then all.distinct
          else all.groupBy(identity).filter(_(1).length > 1).keys.to(List)

        (if shared.length > 1 then shared else all.distinct).sorted

      if steps.length > 1 then
        val labelWidth = series.map(_(0).length).max
        val peak = series.flatMap(_(1).values).map(_.throughput).max.max(1L)
        val blocks = List(t"▁", t"▂", t"▃", t"▄", t"▅", t"▆", t"▇", t"█")
        val stepWidth = steps.map(_.show.length).max + 2
        Out.println(t"  ${t"N".pad(labelWidth)}${steps.map(_.show.pad(stepWidth, Rtl)).join}")

        series.each: (label, curve, sustained) =>
          val cells: Text =
            steps.map: step =>
              curve.at(step).lay(t"·".pad(stepWidth, Rtl)): strain =>
                val level = ((strain.throughput*8L + peak - 1L)/peak).toInt.min(8).max(1)
                blocks(level - 1).pad(stepWidth, Rtl)

            . join

          val summary: Text = sustained.lay(t""): strain =>
            t"  sustained ${strain.concurrency} @ ${strain.throughput} op/s"

          Out.println(t"  ${label.pad(labelWidth)}$cells$summary")

        Out.println(t"")

      Scaffold[ReportLine.Strain]
        ( Column(e"Hash"): s =>
            e"${s.test.id}",
          Column(e"Stress"): s =>
            e"${s.test.name}",
          Column(e"N", textAlign = TextAlignment.Right): s =>
            e"${s.strain.concurrency}",
          Column(e"Ops", textAlign = TextAlignment.Right): s =>
            e"${s.strain.operations}",
          Column(e"Throughput", textAlign = TextAlignment.Right): s =>
            val tp = s.strain.throughput
            if tp == 0 then e"" else e"$tp op/s",
          Column(e"p99", textAlign = TextAlignment.Right): s =>
            s.strain.p99.lay(e"")(showTime(_)),
          Column(e"SLO", textAlign = TextAlignment.Right): s =>
            s.strain.compliance.lay(e""): compliance =>
              e"${compliance*100}%",
          Column(e"Alloc/op", textAlign = TextAlignment.Right): s =>
            showMemory(s.strain.allocationRate.toLong),
          Column(e"Peak", textAlign = TextAlignment.Right): s =>
            showMemory(s.strain.peakHeap),
          Column(e"Retained", textAlign = TextAlignment.Right): s =>
            showMemory(s.strain.retained),
          Column(e"GC", textAlign = TextAlignment.Right): s =>
            e"${s.strain.gcCount}" )

      . tabulate(rows.sortBy(_.test.timestamp)).grid(columns).render
      . each(Out.println(_))

    val profileBySuite = profiles(lines).to(List).groupBy(_.test.suite).to(List)
      . sortBy(_(1).iterator.map(_.test.timestamp).min)

    profileBySuite.each: (suite, rows) =>
      val suiteName = suite.let(_.name.text).or(t"")
      Out.println(t"")
      if suiteName.length > 0 then Out.println(suiteName)

      rows.sortBy(_.test.timestamp).each: row =>
        Out.println(t"${row.test.id}  ${row.test.name.text}")
        val max = row.hotspots.frames.map(_.samples).maxOption.getOrElse(0L)

        def name(frame: probably.Hotspots.Frame): Text =
          val method = StackTrace.Method(frame.className, frame.method)
          val cls = if method.cls.starts(t"Ξ") then method.cls.skip(1) else method.cls
          t"${method.prefix}.$cls#${frame.method}"

        val width = row.hotspots.frames.map(name(_).length).maxOption.getOrElse(0)

        row.hotspots.frames.each: frame =>
          val basisPoints =
            if row.hotspots.total == 0L then 0L else frame.samples*10000L/row.hotspots.total

          val percent = t"${basisPoints/100}.${(basisPoints%100).show.pad(2, Rtl, '0')}"

          Out.println:
            t"  ${name(frame).pad(width, Rtl)} ${percent.pad(6, Rtl)}% ${bar(frame.samples, max)}"

    val failureStatuses: Set[Status] =
      Set(Status.Fail, Status.Throws, Status.CheckThrows, Status.Mixed)

    val failures = summaryLines.filter: s => failureStatuses.has(s.status)

    if failures.nonEmpty then
      Out.println(t"")

      Scaffold[Summary]
        ( Column(e"Hash"): s =>
            e"${s.id.id}",
          Column(e"Test"): s =>
            val depth = s.id.suite.let(_.id.depth).or(0)
            e"${t"  "*depth}${s.id.name}",
          Column(e"Status"): s =>
            e"${statusWord(s.status)}" )

      . tabulate(failures).grid(columns).render.each(Out.println(_))

      Out.println(t"")

      failures.each: summary =>
        val location = t"${summary.id.codepoint.source}:${summary.id.codepoint.line}"
        Out.println(t"${summary.id.id}  ${summary.id.name.text} @ $location")

        details(summary.id).each: detail =>
          detail match
            case Verdict.Detail.Throws(err) =>
              Out.println:
                t"  threw ${err.component}.${err.className}: ${truncate(err.message.text)}"

              err.crop(t"probably.Runner", t"run()").frames.take(3).each: frame =>
                Out.println(formatFrame(frame))

            case Verdict.Detail.CheckThrows(err) =>
              Out.println:
                t"  check threw ${err.component}.${err.className}: ${truncate(err.message.text)}"

              err.crop(t"probably.Verdict#", t"apply()").frames.take(3).each: frame =>
                Out.println(formatFrame(frame))

            case Verdict.Detail.Compare(expected, observed, _) =>
              Out.println(t"  expected: ${truncate(expected.sub(t"\n", t" "))}")
              Out.println(t"  observed: ${truncate(observed.sub(t"\n", t" "))}")

            case Verdict.Detail.Message(text) =>
              Out.println(t"  ${truncate(text)}")

            case Verdict.Detail.Captures(captures) =>
              captures.each: (expr, value) =>
                Out.println(t"  $expr = ${truncate(value)}")

        Out.println(t"")

    failure.let: (error, active) =>
      val activeNames = active.to(List).map(_.name.text).join(t", ")
      val errorClass = Option(error.getClass.getName).map(_.nn.tt).getOrElse(t"")
      val msg = Option(error.getMessage).map(_.nn.tt).getOrElse(t"")

      if active.nil then Out.println(t"FATAL: $errorClass: $msg")
      else Out.println(t"FATAL in $activeNames: $errorClass: $msg")

      StackTrace(error).frames.take(3).each: frame => Out.println(formatFrame(frame))

  private def humanComplete(coverage: Option[Coverage])(using Stdio): Unit =
    val table =
      val showStats = !lines.summaries.all(_.count < 2)
      val timeTitle = if showStats then t"Avg" else t"Time"

      Scaffold[Summary]
        ( Column(e"")(_.status.symbol),
          Column(e"$Bold(Hash)"): s =>
            e"${Fg(palette.informative)}(${s.id.id})",
          Column(e"$Bold(Test)")(_.indentedName),
          Column(e"$Bold(Count)", textAlign = TextAlignment.Right): s =>
            e"${Fg(palette.informative)}(${s.iterations})",
          Column(e"$Bold(Min)", textAlign = TextAlignment.Right): s =>
            if s.count < 2 then e"" else s.minTime,

          Column(e"$Bold($timeTitle)", textAlign = TextAlignment.Right)(_.avgTime),

          Column(e"$Bold(Max)", textAlign = TextAlignment.Right): s =>
            if s.count < 2 then e"" else s.maxTime )

    val columns: Int = safely(Environment.columns.as[Int]).or(120)
    val summaryLines = lines.summaries
    val githubActions = Ci.githubActions

    val ghDetected: Text = if githubActions then t"true" else t"false"
    val ghEnv: Text = safely(Environment.githubActions[Text]).or(t"<unset>")

    def truncate(text: Text, max: Int = 800): Text =
      if text.length <= max then text else t"${text.keep(max)}…"

    def describeFailure(info: Optional[Verdict.Detail]): Text = (info: @unchecked) match
      case Verdict.Detail.Throws(err) =>
        truncate(t"Threw ${err.component}.${err.className}: ${err.message.text}")

      case Verdict.Detail.CheckThrows(err) =>
        truncate(t"Check threw ${err.component}.${err.className}: ${err.message.text}")

      case Verdict.Detail.Compare(expected, observed, _) =>
        truncate(t"Expected: ${expected.sub(t"\n", t" ")}; observed: ${observed.sub(t"\n", t" ")}")

      case Verdict.Detail.Message(text) =>
        truncate(text)

      case Verdict.Detail.Captures(_) | Unset =>
        t"Test failed"

    coverage.each: coverage =>
      Out.println(e"$Bold($Underline(Test coverage))")

      case class CoverageData(path: Text, branches: Int, hits: Int, oldHits: Int):
        def hitsText: Teletype =
          val main = e"${if hits == 0 then palette.subdued else palette.detail}($hits)"

          if oldHits == 0 then main
          else e"${palette.detail}(${oldHits.show.subscripts}) $main"

      val data = coverage.spec.groupBy(_.path).to(List).map: (path, branches) =>
        val hitCount: Int =
          branches.to(List).map(_.id).map(coverage.hits.has).count(identity(_))

        val oldHitCount: Int =
          branches.to(List).map(_.id).map(coverage.oldHits.has).count(identity(_))

        CoverageData(path, branches.size, hitCount, oldHitCount)

      val maxHits = data.map(_.branches).maxOption

      import treeStyles.defaultTreeStyle

      def describe(surface: Surface): Teletype =
        if surface.juncture.treeName == t"DefDef" then e"• ${surface.juncture.method.teletype}"
        else e"• ${surface.juncture.shortCode}"

      def render(junctures: List[Surface]): LazyList[(Surface, Teletype)] =
        val diagram = TreeDiagram.by[Surface](_.children)(junctures*)
        diagram.nodes.zip(diagram.render(describe))

      val allHits = coverage.hits ++ coverage.oldHits

      val junctures2 =
        coverage.structure.values.flatten
        . to(List)
        . filter(!_.covered(allHits))
        . map(_.copy(children = Nil))

      Scaffold[(Surface, Teletype)]
        ( Column(e""): row =>
            if row(0).juncture.branch then e"⎇" else e"",

          Column(e""): row =>
            if coverage.hits.has(row(0).juncture.id) then e"${Bg(palette.detail)}(  )"
            else if coverage.oldHits.has(row(0).juncture.id) then e"${Bg(palette.detail)}(  )"
            else e"${Bg(palette.highlight)}(  )",

          Column(e"Juncture")(_(1)),

          Column(e"Line"): row =>
            val path = e"${Fg(palette.pass)}(${row(0).juncture.path})"
            val line = e"${Fg(palette.accented)}(${row(0).juncture.lineNo})"
            e"$path${Fg(palette.subdued)}(:)$line",

          Column(e"Symbol")(_(0).juncture.symbolName) )

      . tabulate(render(junctures2))
      . grid(columns)(using tableStyles.horizontalTableStyle)
      . render
      . each(Out.println(_))

      Out.println(e"")

      Scaffold[CoverageData]
        ( Column(e"Source file", textAlign = TextAlignment.Left): data =>
            data.path,
          Column(e"Hits", textAlign = TextAlignment.Right)(_.hitsText),
          Column(e"Size", textAlign = TextAlignment.Right)(_.branches),
          Column(e"Coverage", textAlign = TextAlignment.Right): data =>
            e"${(100*(data.hits + data.oldHits)/data.branches.toDouble)}%",
          Column(e""): data =>
            def width(n: Double): Text = if n == 0 then t"" else t"━"*(1 + (70*n).toInt)
            val covered: Text = width(maxHits.map(data.hits.toDouble/_).getOrElse(0))
            val oldCovered: Text = width(maxHits.map(data.oldHits.toDouble/_).getOrElse(0))

            val notCovered: Text =
              width(maxHits.map((data.branches.toDouble - data.hits - data.oldHits)/_).getOrElse(0))

            val bars = List
                        ( palette.accented  -> covered,
                          palette.detail    -> oldCovered,
                          palette.highlight -> notCovered )

            bars.filter(_(1).length > 0).map { (color, bar) => e"$color($bar)" }.join )

      . tabulate(data).grid(columns).render.each(Out.println(_))

      Out.println(e"")

    def totals(tabulation: Boolean): Unit =
      if summaryLines.exists(_.count > 0) then
        val totals = summaryLines.groupBy(_.status).view.mapValues(_.size).to(Map) - Status.Suite
        val passed: Int =
          totals.getOrElse(Status.Pass, 0)
          + totals.getOrElse(Status.Bench, 0)
          + totals.getOrElse(Status.Stress, 0)
          + totals.getOrElse(Status.Profile, 0)
        val aspirePassed: Int = totals.getOrElse(Status.AspirePass, 0)
        val aspireFailed: Int = totals.getOrElse(Status.AspireFail, 0)
        val total: Int = totals.values.sum
        val failed: Int = total - passed - aspirePassed - aspireFailed
        if failed == 0 && failure.absent then pass = true

        if !tabulation then
          Out.print(e"${escapes.Reset}")
          Out.println(e"$Bold($Underline(Test results))")

          table.tabulate(summaryLines).grid(columns).render.each(Out.println(_))

        else
          Out.println(t"─"*72)

        val color = if pass then palette.pass else palette.fail

        val text1 =
          if !pass then t"┳┳━━━┓ ┏┳━━━┳┓   ┳┳   ┳┳    "
          else t"┳┳━━━┳┓  ┏┳━━━┳┓  ┏┳━━━┓  ┏┳━━━┓"

        val text2 =
          if !pass then t"┃┃     ┃┃   ┃┃   ┃┃   ┃┃    "
          else t"┃┃   ┃┃  ┃┃   ┃┃  ┃┃      ┃┃    "

        val text3 =
          if !pass then t"┃┣━━   ┃┣━━━┫┃   ┃┃   ┃┃    "
          else t"┃┣━━━┻┛  ┃┣━━━┫┃  ┗┻━━┳┓  ┗┻━━┳┓"

        val text4 =
          if !pass then t"┃┃     ┃┃   ┃┃   ┃┃   ┃┃    "
          else t"┃┃       ┃┃   ┃┃      ┃┃      ┃┃"

        val text5 =
          if !pass then t"┻┻     ┻┻   ┻┻   ┻┻   ┻┻━━━┛"
          else t"┻┻       ┻┻   ┻┻  ┗━━━┻┛  ┗━━━┻┛"

        val width = if pass then 38 else 34

        if !pass || !tabulation then
          Out.println(e"$color(╭${t"─"*width}╮)")

          def banner(text: Text): Teletype =
            e"$color(│) $Bold(${Bg(color)}(${Fg(palette.black)}(  $text  ))) $color(│)"

          Out.println(banner(text1))
          Out.println(banner(text2))
          Out.println(banner(text3))
          Out.println(banner(text4))
          Out.println(banner(text5))
          Out.println(e"$color(╰${t"─"*width}╯)")

          given decimalizer: Decimalizer = Decimalizer(decimalPlaces = 1)
          val fg = Fg(palette.foreground)
          val passText = e"$Bold($fg($passed)) passed (${100.0*passed/total}%)"
          val failText = e"$Bold($fg($failed)) failed (${100.0*failed/total}%)"

          val aspirePassText =
            e"$Bold($fg($aspirePassed)) aspire-passed (${100.0*aspirePassed/total}%)"

          val aspireFailText =
            e"$Bold($fg($aspireFailed)) aspire-failed (${100.0*aspireFailed/total}%)"

          val allText = e"$Bold(${Fg(palette.foreground)}($total)) total"

          if aspirePassed + aspireFailed == 0
          then Out.println(e" $passText, $failText, $allText")
          else Out.println(e" $passText, $failText, $aspirePassText, $aspireFailText, $allText")

          Out.println(t"─"*72)

        import Status.*

        val allStatuses =
          List(Pass, Bench, Stress, Profile, AspirePass, Throws, Fail, AspireFail, Mixed,
              CheckThrows)

        // Printed with index loops rather than `grouped(_).each`/`map` closures: a lambda whose
        // parameter is a `List` and whose body uses the `Stdio` capability leaks the list's reach
        // capability into the surrounding `(using Stdio)` scope under capture checking.
        val cells: IndexedSeq[Teletype] =
          allStatuses.map: status =>
            gossamer.pad[Teletype](e"  ${status.symbol} ${status.describe}")(20)
          . to(IndexedSeq)

        var cell = 0
        while cell < cells.length do
          Out.println(cells.slice(cell, cell + 4).join(e" "))
          cell += 4

        Out.println(t"─"*72)

      else
        Out.println(e"$Italic(No tests were run.)")

    totals(true)

    benches(lines).groupBy(_.test.suite).each: (suite, benchmarks) =>
      val ribbon =
        Ribbon
          ( palette.subdue(palette.detail, 0.3),
            palette.subdue(palette.detail, 0.6),
            palette.subdue(palette.detail, 0.9) )

      val suiteName = suite.let(_.name.teletype).or(e"")

      if githubActions then
        GithubActions.group(t"Benchmarks: ${suite.let(_.name.text).or(t"")}")

      Out.println:
        ribbon.fill(e"${suite.lay(t"")(_.id.id)}", e"Benchmarks", suiteName)

      val comparisons: List[ReportLine.Bench] =
        benchmarks.filter(!_.benchmark.baseline.absent).to(List)

      def confInt(b: Benchmark): Teletype =
        if b.confidenceInterval == 0 || b.mean == 0.0 then e""
        else
          val basisPoints = (b.confidenceInterval*10000.0/b.mean).toLong
          val sig = (basisPoints/100).show
          val frac = (basisPoints%100).show.pad(2, Rtl, '0')
          e"${Fg(palette.accented)}(±)${Fg(palette.foreground)}($sig.$frac)%"

      def frequency(benchmark: Benchmark): Teletype =
        if benchmark.throughput == 0 then e""
        else
          val tp = e"${Fg(palette.foreground)}(${benchmark.throughput})"
          e"$tp ${Fg(palette.accented)}(op${Fg(palette.subdued)}(·)s¯¹)"

      val bench: Scaffold[ReportLine.Bench, Teletype] = Scaffold[ReportLine.Bench](
        (List(
          Column(e"$Bold(Hash)"): s =>
            e"${Fg(palette.informative)}(${s.test.id})",
          Column(e"$Bold(Test)"): s =>
            e"${s.test.name}",
          Column(e"$Bold(n)", textAlign = TextAlignment.Right): s =>
            s.benchmark.iterations.toLong,
          Column(e"$Bold(μ)", textAlign = TextAlignment.Right): s =>
            showTime(s.benchmark.mean.toLong),
          Column(e"$Bold(σ)", textAlign = TextAlignment.Right): s =>
            showTime(s.benchmark.sd.toLong),
          Column(e"$Bold(Confidence)", textAlign = TextAlignment.Right): s =>
            e"P${s.benchmark.confidence: Int} ${confInt(s.benchmark)}",

          Column(e"$Bold(Throughput)", textAlign = TextAlignment.Right): s =>
            e"${frequency(s.benchmark)}") :::
          (
          if benchmarks.exists(_.benchmark.operationSize.present) then List(
            Column(e"$Bold(Size)", textAlign = TextAlignment.Right):
              (s: ReportLine.Bench) => s.benchmark.operationSize.lay(e""){ t => e"$t" },
            Column(e"$Bold(Rate)", textAlign = TextAlignment.Right):
              (s: ReportLine.Bench) => s.benchmark.operationRate.lay(e""){ t => e"$t" })
          else Nil) :::
          comparisons.map: comparison =>
          import Baseline.*
          val baseline = comparison.benchmark.baseline.vouch

          val title = e"$Bold(${Fg(palette.informative)}(${comparison.test.id}))"

          Column(title, textAlign = TextAlignment.Right):
            (bench: ReportLine.Bench) =>
              def operations(left: Double, right: Double): Double = baseline.mode match
                case Arithmetic => left - right
                case Geometric  => left/right

              def metric(value: Double) = if baseline.metric == Temporal then value else 1/value

              val value = baseline.compare match
                case Compare.Min =>
                  operations(metric(bench.benchmark.min), metric(comparison.benchmark.min))

                case Compare.Max =>
                  operations(metric(bench.benchmark.max), metric(comparison.benchmark.max))

                case Compare.Mean =>
                  operations(metric(bench.benchmark.mean), metric(comparison.benchmark.mean))

              val valueWithUnits = baseline.metric match
                case Temporal =>
                  showTime(value.toLong)

                case Cadential =>
                  val v = e"${Fg(palette.foreground)}(${value})"
                  e"$v ${Fg(palette.accented)}(op${Fg(palette.subdued)}(·)s¯¹)"

              baseline.mode match
                case Arithmetic =>
                  if value == 0 then e"★"
                  else if value < 0
                  then e"${Fg(palette.accented)}(-)${valueWithUnits.dropChars(1)}"
                  else e"${Fg(palette.accented)}(+)$valueWithUnits"

                case Geometric =>
                  if value == 1 then e"★" else e"${Fg(palette.foreground)}($value)"
        )*
      )

      bench.tabulate(benchmarks.to(List).sortBy(-_.benchmark.throughput))
      . grid(columns).render.each(Out.println(_))

      if githubActions then GithubActions.endGroup()

    strains(lines).groupBy(_.test.suite).each: (suite, rows) =>
      val ribbon =
        Ribbon
          ( palette.subdue(palette.detail, 0.3),
            palette.subdue(palette.detail, 0.6),
            palette.subdue(palette.detail, 0.9) )

      val suiteName = suite.let(_.name.teletype).or(e"")

      if githubActions then
        GithubActions.group(t"Stress: ${suite.let(_.name.text).or(t"")}")

      Out.println:
        ribbon.fill(e"${suite.lay(t"")(_.id.id)}", e"Stress", suiteName)

      // Comparison panel: when the suite has a scaling dimension, one sparkline per test —
      // throughput across N, scaled against the suite's fastest window — makes the tests
      // comparable at a glance ahead of the detailed table. Probes of one test share their
      // call site, so grouping is by codepoint; `·` marks an unprobed N, and cells beyond a
      // test's sustained concurrency are subdued.
      val series: List[(Text, Map[Int, probably.Strain], Optional[probably.Strain])] =
        rows.to(List).groupBy(_.test.codepoint).to(List)
        . sortBy(_(1).map(_.test.timestamp).min)
        . map: (_, group) =>
            val name = group.head.test.name.text
            val cut1 = name.s.indexOf(" (N = ")
            val cut2 = name.s.indexOf(" (sustained, N = ")
            val cut = if cut1 < 0 then cut2 else if cut2 < 0 then cut1 else cut1.min(cut2)
            val label = if cut < 0 then name else name.keep(cut)
            val curve = group.map { row => row.strain.concurrency -> row.strain }.to(Map)
            val sustained: Optional[probably.Strain] = group.find(_.strain.sustained).map(_.strain).getOrElse(Unset)
            (label, curve, sustained)

      val steps: List[Int] =
        val all = series.flatMap(_(1).keys)

        val shared =
          if series.length < 2 then all.distinct
          else all.groupBy(identity).filter(_(1).length > 1).keys.to(List)

        (if shared.length > 1 then shared else all.distinct).sorted

      if steps.length > 1 then
        val stackPalette = summon[StackTrace.Palette]

        def accent(level: Int): Color in Srgb = (level%5) + 1 match
          case 1 => stackPalette.accent1
          case 2 => stackPalette.accent2
          case 3 => stackPalette.accent3
          case 4 => stackPalette.accent4
          case _ => stackPalette.accent5

        val labelWidth = series.map(_(0).length).max
        val peak = series.flatMap(_(1).values).map(_.throughput).max.max(1L)
        val blocks = List(t"▁", t"▂", t"▃", t"▄", t"▅", t"▆", t"▇", t"█")
        val stepWidth = steps.map(_.show.length).max + 2

        Out.println:
          val headings = steps.map(_.show.pad(stepWidth, Rtl)).join
          e"  ${Fg(palette.subdued)}(${t"N".pad(labelWidth)}$headings)"

        series.zipWithIndex.each: (data, index) =>
          val (label, curve, sustained) = data
          val color = accent(index)
          val limit = sustained.lay(Int.MaxValue)(_.concurrency)

          val cells: Teletype =
            steps.map: step =>
              curve.at(step).lay(e"${Fg(palette.subdued)}(${t"·".pad(stepWidth, Rtl)})"): strain =>
                val level = ((strain.throughput*8L + peak - 1L)/peak).toInt.min(8).max(1)
                val cell = blocks(level - 1).pad(stepWidth, Rtl)
                if step <= limit then e"$color($cell)"
                else e"${Fg(palette.subdued)}($cell)"

            . join

          val summary: Teletype = sustained.lay(e""): strain =>
            val rate = e"${Fg(palette.foreground)}(${strain.throughput})"
            e"  sustained $Bold(${Fg(palette.foreground)}(${strain.concurrency})) @ $rate ${Fg(palette.accented)}(op${Fg(palette.subdued)}(·)s¯¹)"

          Out.println(e"  $color(${label.pad(labelWidth)})$cells$summary")

        Out.println(e"")

      val comparisons: List[ReportLine.Strain] =
        rows.filter(!_.strain.baseline.absent).to(List)

      def frequency(strain: probably.Strain): Teletype =
        if strain.throughput == 0 then e""
        else
          val tp = e"${Fg(palette.foreground)}(${strain.throughput})"
          e"$tp ${Fg(palette.accented)}(op${Fg(palette.subdued)}(·)s¯¹)"

      val strain: Scaffold[ReportLine.Strain, Teletype] = Scaffold[ReportLine.Strain](
        (List(
          Column(e"$Bold(Hash)"): s =>
            e"${Fg(palette.informative)}(${s.test.id})",
          Column(e"$Bold(Test)"): s =>
            e"${s.test.name}",
          Column(e"$Bold(N)", textAlign = TextAlignment.Right): s =>
            s.strain.concurrency,
          Column(e"$Bold(Ops)", textAlign = TextAlignment.Right): s =>
            s.strain.operations,
          Column(e"$Bold(Throughput)", textAlign = TextAlignment.Right): s =>
            frequency(s.strain),
          Column(e"$Bold(Alloc·op¯¹)", textAlign = TextAlignment.Right): s =>
            showMemory(s.strain.allocationRate.toLong)) :::
          (
          if rows.exists(_.strain.p50.present) then List(
            Column(e"$Bold(p50)", textAlign = TextAlignment.Right):
              (s: ReportLine.Strain) => s.strain.p50.lay(e"")(showTime(_)),
            Column(e"$Bold(p99)", textAlign = TextAlignment.Right):
              (s: ReportLine.Strain) => s.strain.p99.lay(e"")(showTime(_)),
            Column(e"$Bold(p999)", textAlign = TextAlignment.Right):
              (s: ReportLine.Strain) => s.strain.p999.lay(e"")(showTime(_)))
          else Nil) :::
          (
          if rows.exists(_.strain.compliance.present) then List(
            Column(e"$Bold(SLO)", textAlign = TextAlignment.Right):
              (s: ReportLine.Strain) => s.strain.compliance.lay(e""): compliance =>
                e"${Fg(palette.foreground)}(${compliance*100})%")
          else Nil) :::
          List(
          Column(e"$Bold(Peak)", textAlign = TextAlignment.Right):
            (s: ReportLine.Strain) => showMemory(s.strain.peakHeap),
          Column(e"$Bold(Retained)", textAlign = TextAlignment.Right):
            (s: ReportLine.Strain) => showMemory(s.strain.retained),
          Column(e"$Bold(GC n)", textAlign = TextAlignment.Right):
            (s: ReportLine.Strain) => e"${s.strain.gcCount}",
          Column(e"$Bold(GC t)", textAlign = TextAlignment.Right):
            (s: ReportLine.Strain) => showTime(s.strain.gcTime*1000000L)) :::
          comparisons.map: comparison =>
          import Baseline.*
          val baseline = comparison.strain.baseline.vouch

          val title = e"$Bold(${Fg(palette.informative)}(${comparison.test.id}))"

          // The relative column compares allocation per operation: the `Compare`/`Metric`
          // axes of `Baseline` describe timing distributions and don't apply to memory.
          Column(title, textAlign = TextAlignment.Right):
            (row: ReportLine.Strain) =>
              val value = baseline.mode match
                case Arithmetic =>
                  row.strain.allocationRate - comparison.strain.allocationRate

                case Geometric =>
                  if comparison.strain.allocationRate == 0.0 then 0.0
                  else row.strain.allocationRate/comparison.strain.allocationRate

              baseline.mode match
                case Arithmetic =>
                  if value == 0 then e"★"
                  else if value < 0
                  then e"${Fg(palette.accented)}(-)${showMemory((-value).toLong)}"
                  else e"${Fg(palette.accented)}(+)${showMemory(value.toLong)}"

                case Geometric =>
                  if value == 1 then e"★" else e"${Fg(palette.foreground)}($value)"
        )*
      )

      strain.tabulate(rows.to(List).sortBy(_.test.timestamp))
      . grid(columns).render.each(Out.println(_))

      if githubActions then GithubActions.endGroup()

    profiles(lines).groupBy(_.test.suite).each: (suite, rows) =>
      val ribbon =
        Ribbon
          ( palette.subdue(palette.detail, 0.3),
            palette.subdue(palette.detail, 0.6),
            palette.subdue(palette.detail, 0.9) )

      val suiteName = suite.let(_.name.teletype).or(e"")

      if githubActions then
        GithubActions.group(t"Profile: ${suite.let(_.name.text).or(t"")}")

      Out.println:
        ribbon.fill(e"${suite.lay(t"")(_.id.id)}", e"Profile", suiteName)

      val stackPalette = summon[StackTrace.Palette]

      // The digression convention: packages in first-appearance order take the accent
      // colours cyclically, exactly as coloured stack traces do.
      def accent(level: Int): Color in Srgb = (level%5) + 1 match
        case 1 => stackPalette.accent1
        case 2 => stackPalette.accent2
        case 3 => stackPalette.accent3
        case 4 => stackPalette.accent4
        case _ => stackPalette.accent5

      rows.to(List).sortBy(_.test.timestamp).each: row =>
        Out.println(e"$Bold(${Fg(palette.foreground)}(${row.test.name}))")
        val max = row.hotspots.frames.map(_.samples).maxOption.getOrElse(0L)

        val packages: Map[Text, Color in Srgb] =
          row.hotspots.frames.map: frame =>
            StackTrace.Method(frame.className, frame.method).prefix

          . distinct.zipWithIndex.map: (prefix, index) =>
              prefix -> accent(index)

          . to(Map)

        // The method column is leftmost and right-aligned against the bars, so the names
        // read into their histogram rows; the plain width is measured before styling.
        val width: Int =
          row.hotspots.frames.map: frame =>
            val method = StackTrace.Method(frame.className, frame.method)
            val cls = if method.cls.starts(t"Ξ") then method.cls.skip(1) else method.cls
            method.prefix.length + 1 + cls.length + 1 + frame.method.length

          . maxOption.getOrElse(0)

        row.hotspots.frames.each: frame =>
          val method = StackTrace.Method(frame.className, frame.method)
          val color = packages(method.prefix)

          val basisPoints =
            if row.hotspots.total == 0L then 0L else frame.samples*10000L/row.hotspots.total

          val percent = t"${basisPoints/100}.${(basisPoints%100).show.pad(2, Rtl, '0')}"
          val obj = method.cls.starts(t"Ξ")
          val cls = if obj then method.cls.skip(1) else method.cls
          val separator = if obj then t"." else t"⌗"
          val length = method.prefix.length + 1 + cls.length + 1 + frame.method.length
          val margin = t" "*(width - length)

          Out.println:
            e"  $margin${stackPalette.subdue(color, 0.5)}(${method.prefix}.$Bold($color($cls)))${stackPalette.separator}($separator)${stackPalette.method}(${frame.method}) ${Fg(palette.foreground)}(${percent.pad(6, Rtl)}%) $color(${bar(frame.samples, max)})"

        Out.println(e"")

      if githubActions then GithubActions.endGroup()

    def showLegend(): Unit =
      Out.println(t"─"*74)

      Out.println:
        StackTrace.legend.to(List).map: (symbol, description) =>
          e"$Bold(${Fg(palette.foreground)}(${symbol.pad(3, Rtl)}))  ${description.pad(20)}"

        . grouped(3).to(List).map(_.to(List).join).join(e"${t"\n"}")

      Out.println(t"─"*74)

    if githubActions then summaryLines.each: summary =>
      summary.status match
        case Status.Fail | Status.Throws | Status.CheckThrows | Status.Mixed =>
          val firstDetail: Optional[Verdict.Detail] =
            details.get(summary.id).flatMap(_.headOption).getOrElse(Unset)

          GithubActions.error
            ( message = describeFailure(firstDetail),
              file    = summary.id.codepoint.source,
              line    = summary.id.codepoint.line,
              title   = summary.id.name.text )

        case _ => ()

    details.to(List).sortBy(_(0).timestamp).each: (id, info) =>
      val ribbon =
        Ribbon(palette.fail, palette.subdue(palette.fail, 0.5), palette.subdue(palette.fail, 0.75))

      if githubActions then GithubActions.group(t"Failure: ${id.name.text} (${id.id})")

      Out.println(ribbon.fill(e"$Bold(${id.id})", id.codepoint.text.teletype, id.name.teletype))

      info.each: details =>
        Out.println(t"")

        details match
          case Verdict.Detail.Throws(err) =>
            val name = e"$Italic(${Fg(palette.foreground)}(${err.component}.${err.className}))"
            Out.println(e"${Fg(palette.foreground)}(An exception was thrown while running test:)")
            Out.println(err.crop(t"probably.Runner", t"run()").teletype)
            showLegend()

          case Verdict.Detail.CheckThrows(err) =>
            val name = e"$Italic(${Fg(palette.foreground)}(${err.component}.${err.className}))"
            val msg = e"An exception was thrown while checking the test predicate:"
            Out.println(e"${Fg(palette.foreground)}($msg)")
            Out.println(err.crop(t"probably.Verdict#", t"apply()").dropRight(1).teletype)
            showLegend()

          case Verdict.Detail.Compare(expected, observed, cmp) =>
            def indent(text: Text): Text =
              if text.contains(t"\n") then text.trim.cut(t"\n").join(t"\n│ ", t"\n│ ", t"\n")
              else text

            val expected2: Teletype = e"$Italic(${Fg(palette.foreground)}(${indent(expected)}))"
            val observed2: Teletype = e"$Italic(${Fg(palette.foreground)}(${indent(observed)}))"

            Out.println(e"${Fg(palette.foreground)}(Expected:) $expected2")
            Out.println(e"${Fg(palette.foreground)}(Observed:) $observed2")

            Out.println(cmp.teletype)

          case Verdict.Detail.Captures(map) =>
            Scaffold[(Text, Text), Teletype]
              ( Column(e"Expression", textAlign = TextAlignment.Right)(_(0)),
                Column(e"Value")(_(1)) )

            . tabulate(map.to(List)).grid(140).render.each(Out.println(_))

          case Verdict.Detail.Message(text) =>
            Out.println(text)

      Out.println()
      if githubActions then GithubActions.endGroup()

    failure.let: (error, active) =>
      val explanation = active.to(List) match
        case Nil => e"No tests were active when a fatal error occurred."

        case _ =>
          val were = if active.size == 1 then e"was" else e"were"

          val tests =
            active.to(List).map: test => e"$Bold(${test.name})"
            . join(e"", e", ", e" and ", e"")

          e"A fatal error occurred while $tests $were running."

      if githubActions then
        val activeNames = active.to(List).map(_.name.text).join(t", ")
        val cause = Option(error.getMessage).map(_.nn.tt).getOrElse(t"")
        val errorClass = Option(error.getClass.getName).map(_.nn.tt).getOrElse(t"")

        val message =
          if active.nil
          then truncate(t"Fatal error: $errorClass: $cause")
          else truncate(t"Fatal error in $activeNames: $errorClass: $cause")

        GithubActions.error(message = message, title = t"Fatal error")
        GithubActions.group(t"Fatal error stack trace")

      Out.println()

      val fatalRibbon = Ribbon(palette.pass, palette.subdue(palette.pass, 0.5))
      Out.println(fatalRibbon.fill(e"$Bold(FATAL)", explanation))
      Out.println(StackTrace(error).teletype)

      if githubActions then GithubActions.endGroup()

    totals(false)
