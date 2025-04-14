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
┃    Soundness, version 0.27.0.                                                                    ┃
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

import ambience.*
import anticipation.*
import chiaroscuro.*
import contingency.*
import dendrology.*
import digression.*
import distillate.*
import escapade.*
import escritoire.*, tableStyles.default, columnAttenuation.ignore
import fulminate.*
import gossamer.*
import hieroglyph.*
import iridescence.*, webColors.*
import proscenium.*
import rudiments.*
import spectacular.*
import symbolism.*
import turbulence.*
import vacuous.*

import scala.collection.mutable as scm

object Report:
  given verdict: Inclusion[Report, Verdict]:
    def include(report: Report, testId: TestId, verdict: Verdict): Report =
      val report2 = report.addVerdict(testId, verdict)
      verdict match
        case Verdict.Pass(_) =>
          report2
        case Verdict.Fail(_) =>
          report2

        case Verdict.Throws(error, _) =>
          report2.addDetail(testId, Verdict.Detail.Throws(StackTrace(error)))

        case Verdict.CheckThrows(error, _) =>
          report2.addDetail(testId, Verdict.Detail.CheckThrows(StackTrace(error)))

  given detail: Inclusion[Report, Verdict.Detail] = _.addDetail(_, _)

class Report(using Environment):
  private val githubCi: Boolean = safely(Environment.githubActions[Text]).present

  val metrics = textMetrics.eastAsianScripts
  given measurable: Char is Measurable:
    def width(char: Char): Int = char match
      case '✓' | '✗' | '⎇' => 1
      case _                => metrics.width(char)

  var failure: Optional[(Throwable, Set[TestId])] = Unset
  var pass: Boolean = false

  class TestsMap():
    private var tests: ListMap[TestId, ReportLine] = ListMap()
    def list: List[(TestId, ReportLine)] = synchronized(tests.to(List))
    def apply(testId: TestId): ReportLine = synchronized(tests(testId))

    def update(testId: TestId, reportLine: ReportLine) = synchronized:
      tests = tests.updated(testId, reportLine)

    def getOrElseUpdate(testId: TestId, reportLine: => ReportLine): ReportLine = synchronized:
      if !tests.contains(testId) then tests = tests.updated(testId, reportLine)
      tests(testId)

  enum ReportLine:
    case Suite(suite: Optional[Testable], tests: TestsMap = TestsMap())
    case Test(test: TestId, verdicts: scm.ArrayBuffer[Verdict] = scm.ArrayBuffer())
    case Bench(test: TestId, benchmark: Benchmark)

    def summaries: List[Summary] = this match
      case Suite(suite, tests)  =>
        val rest = tests.list.sortBy(_(0).timestamp).flatMap(_(1).summaries)
        if suite.absent then rest
        else Summary(Status.Suite, suite.option.get.id, 0, 0, 0, 0) :: rest

      case Bench(testId, bench@Benchmark(_, _, _, _, _, _, _, _)) =>
        List(Summary(Status.Bench, testId, 0, 0, 0, 0))

      case Test(testId, buf) =>
        val status =
          if buf.all(_.is[Verdict.Pass]) then Status.Pass
          else if buf.all(_.is[Verdict.Fail]) then Status.Fail
          else if buf.all(_.is[Verdict.Throws]) then Status.Throws
          else if buf.all(_.is[Verdict.CheckThrows]) then Status.CheckThrows
          else Status.Mixed

        val min: Long = buf.map(_.duration).min
        val max: Long = buf.map(_.duration).max
        val avg: Long = buf.fuse(0L)(state + next.duration)/buf.length

        List(Summary(status, testId, buf.length, min, max, avg))

  private val lines: ReportLine.Suite = ReportLine.Suite(Unset)

  def resolve(suite: Optional[Testable]): ReportLine.Suite =
    suite.option.map: suite =>
      resolve(suite.parent).tests(suite.id).absolve match
        case suite@ReportLine.Suite(_, _) => suite

    . getOrElse(lines)

  private var coverage: Option[Coverage] = None

  private val details: scm.SortedMap[TestId, scm.ArrayBuffer[Verdict.Detail]] =
    scm.TreeMap[TestId, scm.ArrayBuffer[Verdict.Detail]]()
    . withDefault(_ => scm.ArrayBuffer[Verdict.Detail]())

  def declareSuite(suite: Testable): Report = this.also:
    resolve(suite.parent).tests(suite.id) = ReportLine.Suite(suite)

  def fail(error: Throwable, active: Set[TestId]): Unit = failure = (error, active)

  def addBenchmark(testId: TestId, benchmark: Benchmark): Report = this.also:
    val benchmarks = resolve(testId.suite).tests
    benchmarks.getOrElseUpdate(testId, ReportLine.Bench(testId, benchmark))

  def addVerdict(testId: TestId, verdict: Verdict): Report = this.also:
    val tests = resolve(testId.suite).tests

    tests.getOrElseUpdate(testId, ReportLine.Test(testId, scm.ArrayBuffer[Verdict]())).absolve match
      case ReportLine.Test(_, buf) => buf.append(verdict)

  def addDetail(testId: TestId, info: Verdict.Detail): Report =
    this.also(details(testId) = details(testId).append(info))

  enum Status:
    case Pass, Fail, Throws, CheckThrows, Mixed, Suite, Bench
    
    val nbsp = '\u00a0'

    def color: Rgb24 = this match
      case Pass        => rgb"#8abd00"
      case Fail        => rgb"#cc3333"
      case Throws      => DarkOrange
      case CheckThrows => rgb"#cc0099"
      case Mixed       => rgb"#ffd700"
      case Suite       => SlateBlue
      case Bench       => CadetBlue

    def symbol: Teletype = this match
      case Pass        => e"${Bg(color)}($nbsp$Bold($Black(✓))$nbsp)"
      case Fail        => e"${Bg(color)}($nbsp$Bold($Black(✗))$nbsp)"
      case Throws      => e"${Bg(color)}($nbsp$Bold($Black(!))$nbsp)"
      case CheckThrows => e"${Bg(color)}($nbsp$Bold($Black(‼))$nbsp)"
      case Mixed       => e"${Bg(color)}($nbsp$Bold($Black(?))$nbsp)"
      case Suite       => e"   "
      case Bench       => e"${Bg(color)}($nbsp$Bold($Black(*))$nbsp)"

    def describe: Teletype = this match
      case Pass        => e"Pass"
      case Fail        => e"Fail"
      case Throws      => e"Throws exception"
      case CheckThrows => e"Exception in check"
      case Mixed       => e"Mixed"
      case Suite       => e"Suite"
      case Bench       => e"Benchmark"

  val unitsSeq: List[Teletype] = List(
    e"$BurlyWood(µs)",
    e"$Goldenrod(ms)",
    e"$Sienna(s) "
  )

  def showTime(n: Long, units: List[Teletype] = unitsSeq): Teletype = units match
    case Nil =>
      n.show.teletype

    case unit :: rest =>
      if n > 100000L then showTime(n/1000L, rest) else
        val sig = (n/1000L).show
        val frac = (n%1000).show.pad(3, Rtl, '0')
        e"$Silver(${sig}.$frac) ${unit}"

  case class Summary(status: Status, id: TestId, count: Int, min: Long, max: Long, avg: Long):
    def indentedName: Teletype =
      val depth = id.suite.let(_.id.depth).or(0) + 1

      val title =
        if status == Status.Suite then e"$Silver($Bold(${id.name}))"
        else e"${id.name}"

      e"${t"  "*(depth - 1)}$title"

    def minTime: Teletype = if min == 0L then e"" else showTime(min)
    def maxTime: Teletype = if max == 0L then e"" else showTime(max)
    def avgTime: Teletype = if avg == 0L then e"" else showTime(avg)
    def iterations: Teletype = if count == 0 then e"" else count.teletype

  def complete(coverage: Option[Coverage])(using Stdio): Unit =
    val table =
      val showStats = !lines.summaries.all(_.count < 2)
      val timeTitle = if showStats then t"Avg" else t"Time"

      Table[Summary](
        Column(e"")(_.status.symbol),

        Column(e"$Bold(Hash)"): s =>
          e"$CadetBlue(${s.id.id})",

        Column(e"$Bold(Test)")(_.indentedName),

        Column(e"$Bold(Count)", textAlign = TextAlignment.Right): s =>
          e"$SteelBlue(${s.iterations})",

        Column(e"$Bold(Min)", textAlign = TextAlignment.Right): s =>
          if s.count < 2 then e"" else s.minTime,

        Column(e"$Bold($timeTitle)", textAlign = TextAlignment.Right)(_.avgTime),

        Column(e"$Bold(Max)", textAlign = TextAlignment.Right): s =>
          if s.count < 2 then e"" else s.maxTime
      )

    val columns: Int = safely(Environment.columns.decode[Int]).or(120)

    val summaryLines = lines.summaries

    coverage.each: coverage =>
      Out.println(e"$Bold($Underline(Test coverage))")
      case class CoverageData(path: Text, branches: Int, hits: Int, oldHits: Int):
        def hitsText: Teletype =
          val main = e"${if hits == 0 then Gray else ForestGreen}($hits)"

          if oldHits == 0 then main
          else e"$Goldenrod(${oldHits.show.subscripts}) $main"

      val data = coverage.spec.groupBy(_.path).to(List).map: (path, branches) =>
        val hitCount: Int =
          branches.to(List).map(_.id).map(coverage.hits.contains).count(identity(_))

        val oldHitCount: Int =
          branches.to(List).map(_.id).map(coverage.oldHits.contains).count(identity(_))

        CoverageData(path, branches.size, hitCount, oldHitCount)

      val maxHits = data.map(_.branches).maxOption

      import treeStyles.default

      def describe(surface: Surface): Teletype =
        if surface.juncture.treeName == t"DefDef" then e"• ${surface.juncture.method.teletype}"
         else e"• ${surface.juncture.shortCode}"

      def render(junctures: List[Surface]): Stream[(Surface, Teletype)] =
        val diagram = TreeDiagram.by[Surface](_.children)(junctures*)
        diagram.nodes.zip(diagram.render(describe))

      val allHits = coverage.hits ++ coverage.oldHits

      val junctures2 =
        coverage.structure.values.flatten
        . to(List)
        . filter(!_.covered(allHits))
        . map(_.copy(children = Nil))

      Table[(Surface, Teletype)]
       (Column(e""): row =>
          if row(0).juncture.branch then e"⎇" else e"",
        Column(e""): row =>
          if coverage.hits.contains(row(0).juncture.id) then e"${Bg(ForestGreen)}(  )"
          else if coverage.oldHits.contains(row(0).juncture.id) then e"${Bg(Goldenrod)}(  )"
          else e"${Bg(Brown)}(  )",
        Column(e"Juncture")(_(1)),
        Column(e"Line"): row =>
          e"$GreenYellow(${row(0).juncture.path})$Gray(:)$Gold(${row(0).juncture.lineNo})",
        Column(e"Symbol")(_(0).juncture.symbolName))

      . tabulate(render(junctures2))
      . grid(columns)(using tableStyles.horizontal)
      . render
      . each(Out.println(_))

      Out.println(e"")

      Table[CoverageData]
       (Column(e"Source file", textAlign = TextAlignment.Left): data =>
          data.path,
        Column(e"Hits", textAlign = TextAlignment.Right)(_.hitsText),
        Column(e"Size", textAlign = TextAlignment.Right)(_.branches),
        Column(e"Coverage", textAlign = TextAlignment.Right): data =>
          e"${(100*(data.hits + data.oldHits)/data.branches.toDouble)}%",
        Column(e""): data =>
          def width(n: Double): Text = if n == 0 then t"" else t"━"*(1 + (70*n).toInt)
          val covered: Text = width(maxHits.map(data.hits.toDouble/_).getOrElse(0))
          val oldCovered: Text = width(maxHits.map(data.oldHits.toDouble/_).getOrElse(0))

          val notCovered: Text = width(maxHits.map((data.branches.toDouble - data.hits -
              data.oldHits)/_).getOrElse(0))

          val bars = List(ForestGreen -> covered, Goldenrod -> oldCovered, Brown -> notCovered)

          bars.filter(_(1).length > 0).map { (color, bar) => e"$color($bar)" }.join)
      . tabulate(data).grid(columns).render.each(Out.println(_))

      Out.println(e"")

    if summaryLines.exists(_.count > 0) then
      val totals = summaryLines.groupBy(_.status).view.mapValues(_.size).to(Map) - Status.Suite
      val passed: Int = totals.getOrElse(Status.Pass, 0) + totals.getOrElse(Status.Bench, 0)
      val total: Int = totals.values.sum
      val failed: Int = total - passed
      if total == passed then pass = true
      Out.println(e"${escapes.Reset}")
      Out.println(e"$Bold($Underline(Test results))")

      table.tabulate(summaryLines).grid(columns).render.each(Out.println(_))
      given decimalizer: Decimalizer = Decimalizer(decimalPlaces = 1)
      val passText = e"$Bold($White($passed)) passed (${100.0*passed/total}%)"
      val failText = e"$Bold($White($failed)) failed (${100.0*failed/total}%)"
      val allText = e"$Bold($White(${passed + failed})) total"
      Out.println(e" $passText, $failText, $allText")
      Out.println(t"─"*72)

      import Status.*
      List(Pass, Bench, Throws, Fail, Mixed, CheckThrows).grouped(3).each: statuses =>
        Out.println:
          statuses.map[Teletype]: status =>
            gossamer.pad[Teletype](e"  ${status.symbol} ${status.describe}")(20)

          . join(e" ")

      Out.println(t"─"*72)

    else
      Out.println(e"$Italic(No tests were run.)")

    def benches(line: ReportLine): Iterable[ReportLine.Bench] =
      line match
        case bench@ReportLine.Bench(_, _) => Iterable(bench)
        case ReportLine.Suite(_, tests)   => tests.list.map(_(1)).flatMap(benches(_))
        case _                            => Nil

    benches(lines).groupBy(_.test.suite).each: (suite, benchmarks) =>
      val ribbon =
        Ribbon(DarkGreen.srgb, MediumSeaGreen.srgb, PaleGreen.srgb)

      Out.println:
        val suiteName = suite.let(_.name.teletype).or(e"")
        ribbon.fill(e"${suite.lay(t"")(_.id.id)}", e"Benchmarks", suiteName)

      val comparisons: List[ReportLine.Bench] =
        benchmarks.filter(!_.benchmark.baseline.absent).to(List)

      def confInt(b: Benchmark): Teletype =
        if b.confidenceInterval == 0 then e""
        else e"$Thistle(±)${showTime(b.confidenceInterval)}"

      def opsPerS(b: Benchmark): Teletype =
        if b.throughput == 0 then e""
        else e"$Silver(${b.throughput}) $Turquoise(op$Gray(·)s¯¹)"

      val bench: Table[ReportLine.Bench, Teletype] = Table[ReportLine.Bench](
        (List(
          Column(e"$Bold(Hash)"): s =>
            e"$CadetBlue(${s.test.id})",
          Column(e"$Bold(Test)"): s =>
            e"${s.test.name}",

          Column(e"$Bold(Min)", textAlign = TextAlignment.Right): s =>
            showTime(s.benchmark.min.toLong),

          Column(e"$Bold(Mean)", textAlign = TextAlignment.Right): s =>
            showTime(s.benchmark.mean.toLong),

          Column(e"$Bold(Confidence)", textAlign = TextAlignment.Right): s =>
            e"P${s.benchmark.confidence: Int} ${confInt(s.benchmark)}",

          Column(e"$Bold(Throughput)", textAlign = TextAlignment.Right): s =>
            e"${opsPerS(s.benchmark)}")
       ::: comparisons.map: c =>
          import Baseline.*
          val baseline = c.benchmark.baseline.vouch
          Column(e"$Bold($CadetBlue(${c.test.id}))", textAlign = TextAlignment.Right):
            (bench: ReportLine.Bench) =>
              def op(left: Double, right: Double): Double = baseline.mode match
                case Arithmetic => left - right
                case Geometric  => left/right

              def metric(value: Double) = if baseline.metric == ByTime then value else 1/value

              val value = baseline.compare match
                case Compare.Min  => op(metric(bench.benchmark.min), metric(c.benchmark.min))
                case Compare.Mean => op(metric(bench.benchmark.mean), metric(c.benchmark.mean))
                case Compare.Max  => op(metric(bench.benchmark.max), metric(c.benchmark.max))

              val valueWithUnits = baseline.metric match
                case ByTime  => showTime(value.toLong)
                case BySpeed => e"$Silver(${value}) $Turquoise(op$Gray(·)s¯¹)"

              baseline.mode match
                case Arithmetic => if value == 0 then e"★"
                                   else if value < 0
                                   then e"$Thistle(-)${valueWithUnits.dropChars(1)}"
                                   else e"$Thistle(+)$valueWithUnits"

                case Geometric  => if value == 1 then e"★" else e"$Silver($value)"
        )*
      )

      bench.tabulate(benchmarks.to(List).sortBy(-_.benchmark.throughput))
      . grid(columns).render.each(Out.println(_))

    def showLegend(): Unit =
      Out.println(t"─"*74)
      Out.println:
        StackTrace.legend.to(List).map: (symbol, description) =>
          e"$Bold($White(${symbol.pad(3, Rtl)}))  ${description.pad(20)}"

        . grouped(3).to(List).map(_.to(List).join).join(e"${t"\n"}")

      Out.println(t"─"*74)

    details.to(List).sortBy(_(0).timestamp).each: (id, info) =>
      val ribbon = Ribbon(DarkRed.srgb, FireBrick.srgb, Tomato.srgb)
      Out.println(ribbon.fill(e"$Bold(${id.id})", id.codepoint.text.teletype, id.name.teletype))

      info.each: details =>
        Out.println(t"")
        details match
          case Verdict.Detail.Throws(err) =>
            val name = e"$Italic($White(${err.component}.${err.className}))"
            Out.println(e"$Silver(An exception was thrown while running test:)")
            Out.println(err.crop(t"probably.Runner", t"run()").teletype)
            showLegend()

          case Verdict.Detail.CheckThrows(err) =>
            val name = e"$Italic($White(${err.component}.${err.className}))"
            Out.println(e"$Silver(An exception was thrown while checking the test predicate:)")
            Out.println(err.crop(t"probably.Verdict#", t"apply()").dropRight(1).teletype)
            showLegend()

          case Verdict.Detail.Compare(expected, observed, cmp) =>
            def indent(text: Text): Text =
              if text.contains(t"\n") then text.trim.cut(t"\n").join(t"\n│ ", t"\n│ ", t"\n")
              else text

            val expected2: Teletype = e"$Italic($Silver(${indent(expected)}))"
            val observed2: Teletype = e"$Italic($Silver(${indent(observed)}))"

            Out.println(e"$White(Expected:) $expected2")
            Out.println(e"$White(Observed:) $observed2")

            Out.println(cmp.teletype)

          case Verdict.Detail.Captures(map) =>
            Table[(Text, Text), Teletype]
             (Column(e"Expression", textAlign = TextAlignment.Right)(_(0)),
              Column(e"Value")(_(1)))

            . tabulate(map.to(List)).grid(140).render.each(Out.println(_))

          case Verdict.Detail.Message(text) =>
            Out.println(text)

      Out.println()

    failure.let: (error, active) =>
      val explanation = active.to(List) match
        case Nil =>
          e"No tests were active when a fatal error occurred."

        case _ =>
          val were = if active.size == 1 then e"was" else e"were"
          val tests =
            active.to(List).map { test => e"$Bold(${test.name})" }
            . join(e"", e", ", e" and ", e"")

          e"A fatal error occurred while $tests $were running."

      Out.println()

      Out.println(Ribbon(Crimson.srgb, LightSalmon.srgb).fill(e"$Bold(FATAL)", explanation))
      Out.println(StackTrace(error).teletype)
