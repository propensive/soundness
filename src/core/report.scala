/*
    Probably, version [unreleased]. Copyright 2023 Jon Pretty, Propensive OÜ.

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
import digression.*
import anticipation.*
import gossamer.*
import chiaroscuro.*
import ambience.*
import escritoire.*, tableStyles.rounded
import dendrology.*
import escapade.*
import turbulence.*
import iridescence.*
import spectacular.*
import hieroglyph.*

given Decimalizer = Decimalizer(3)

import scala.collection.mutable as scm

object Baseline:
  enum Compare:
    case Min, Mean, Max
  
  enum Metric:
    case BySpeed, ByTime
  
  enum Calc:
    case Ratio, Difference

export Baseline.Compare.{Min, Mean, Max}
export Baseline.Metric.{BySpeed, ByTime}
export Baseline.Calc.{Ratio, Difference}

case class Baseline
    (compare: Baseline.Compare = Mean, metric: Baseline.Metric = BySpeed, calc: Baseline.Calc = Ratio)

object Benchmark:
  given Inclusion[TestReport, Benchmark] with
    def include(report: TestReport, testId: TestId, benchmark: Benchmark): TestReport =
      report.addBenchmark(testId, benchmark)
  type Percentiles = 80 | 85 | 90 | 95 | 96 | 97 | 98 | 99

case class Benchmark
    (total: Long, count: Int, min: Double, mean: Double, max: Double, sd: Double,
        confidence: Benchmark.Percentiles, baseline: Maybe[Baseline]):
  
  def zScore(percentile: Benchmark.Percentiles): Double = percentile match
    case 80 => 0.842
    case 85 => 1.036
    case 90 => 1.282
    case 95 => 1.645
    case 96 => 1.751
    case 97 => 1.881
    case 98 => 2.054
    case 99 => 2.326
    
  def confidenceInterval: Long = (zScore(confidence)*sd/math.sqrt(count.toDouble)).toLong
  def throughput: Long = (1000000000.0/mean).toLong

enum DebugInfo:
  case Throws(stack: StackTrace)
  case CheckThrows(stack: StackTrace)
  case Captures(values: Map[Text, Text])
  case Compare(expected: Text, found: Text, semblance: Semblance)
  case Message(message: Text)

trait Inclusion[ReportType, DataType]:
  def include(report: ReportType, testId: TestId, data: DataType): ReportType

trait TestReporter[ReportType]:
  def make(): ReportType
  def declareSuite(report: ReportType, suite: TestSuite): Unit
  def complete(report: ReportType): Unit

object TestReporter:
  given (using Io, Environment): TestReporter[TestReport] with
    def make(): TestReport = TestReport()
    def declareSuite(report: TestReport, suite: TestSuite): Unit = report.declareSuite(suite)
    def complete(report: TestReport): Unit =
      report.complete(Coverage())

object TestReport:
  given Inclusion[TestReport, Outcome] with
    def include(report: TestReport, testId: TestId, outcome: Outcome): TestReport = 
      val report2 = report.addOutcome(testId, outcome)
      outcome match
        case Outcome.Pass(_) =>
          report2
        case Outcome.Fail(_) =>
          report2
        case Outcome.Throws(error, _) =>
          report2.addDebugInfo(testId, DebugInfo.Throws(StackTrace(error)))
        case Outcome.CheckThrows(error, _) =>
          report2.addDebugInfo(testId, DebugInfo.CheckThrows(StackTrace(error)))
  
  given Inclusion[TestReport, DebugInfo] = _.addDebugInfo(_, _)


class TestReport(using env: Environment):
  
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
    case Suite(suite: Maybe[TestSuite], tests: TestsMap = TestsMap())
    case Test(test: TestId, outcomes: scm.ArrayBuffer[Outcome] = scm.ArrayBuffer())
    case Bench(test: TestId, benchmark: Benchmark)

    def summaries: List[Summary] = this match
      case Suite(suite, tests)  =>
        val rest = tests.list.sortBy(_(0).timestamp).flatMap(_(1).summaries)
        if suite.unset then rest else Summary(Status.Suite, suite.option.get.id, 0, 0, 0, 0) :: rest
      
      case Bench(testId, bench@Benchmark(_, _, _, _, _, _, _, _)) =>
        List(Summary(Status.Bench, testId, 0, 0, 0, 0))

      case Test(testId, buf) =>
        val status =
          if buf.forall(_.is[Outcome.Pass]) then Status.Pass
          else if buf.forall(_.is[Outcome.Fail]) then Status.Fail
          else if buf.forall(_.is[Outcome.Throws]) then Status.Throws
          else if buf.forall(_.is[Outcome.CheckThrows]) then Status.CheckThrows
          else Status.Mixed
      
        val min: Long = buf.map(_.duration).min
        val max: Long = buf.map(_.duration).max
        val avg: Long = buf.foldLeft(0L)(_ + _.duration)/buf.length
          
        List(Summary(status, testId, buf.length, min, max, avg))
    
  private val lines: ReportLine.Suite = ReportLine.Suite(Unset)
  
  def resolve(suite: Maybe[TestSuite]): ReportLine.Suite =
    suite.option.map: suite =>
      (resolve(suite.parent).tests(suite.id): @unchecked) match
        case suite@ReportLine.Suite(_, _) => suite
    .getOrElse(lines)

  private var coverage: Option[CoverageResults] = None

  private val details: scm.SortedMap[TestId, scm.ArrayBuffer[DebugInfo]] =
    scm.TreeMap[TestId, scm.ArrayBuffer[DebugInfo]]().withDefault(_ => scm.ArrayBuffer[DebugInfo]())

  def declareSuite(suite: TestSuite): TestReport = this.tap: _ =>
    resolve(suite.parent).tests(suite.id) = ReportLine.Suite(suite)

  def addBenchmark(testId: TestId, benchmark: Benchmark): TestReport = this.tap: _ =>
    val benchmarks = resolve(testId.suite).tests
    benchmarks.getOrElseUpdate(testId, ReportLine.Bench(testId, benchmark))
  
  def addOutcome(testId: TestId, outcome: Outcome): TestReport = this.tap: _ =>
    val tests = resolve(testId.suite).tests
    
    (tests.getOrElseUpdate(testId, ReportLine.Test(testId, scm.ArrayBuffer[Outcome]())): @unchecked) match
      case ReportLine.Test(_, buf) => buf.append(outcome)
  
  def addDebugInfo(testId: TestId, info: DebugInfo): TestReport =
    this.tap: _ =>
      details(testId) = details(testId).append(info)

  enum Status:
    case Pass, Fail, Throws, CheckThrows, Mixed, Suite, Bench

    def color: Rgb24 = this match
      case Pass        => rgb"#8abd00"
      case Fail        => colors.Tomato
      case Throws      => colors.DarkOrange
      case CheckThrows => rgb"#dd40a0"
      case Mixed       => rgb"#ddd700"
      case Suite       => colors.SlateBlue
      case Bench       => colors.CadetBlue

    def symbol: Output = this match
      case Pass        => out"${Bg(rgb"#8abd00")}( $Bold(${colors.Black}(✓)) )"
      case Fail        => out"${Bg(colors.Tomato)}( $Bold(${colors.Black}(✗)) )"
      case Throws      => out"${Bg(colors.DarkOrange)}( $Bold(${colors.Black}(!)) )"
      case CheckThrows => out"${Bg(rgb"#dd40a0")}( $Bold(${colors.Black}(‼)) )"
      case Mixed       => out"${Bg(rgb"#ddd700")}( $Bold(${colors.Black}(?)) )"
      case Suite       => out"   "
      case Bench       => out"${Bg(colors.CadetBlue)}( $Bold(${colors.Black}(*)) )"
    
    def describe: Output = this match
      case Pass        => out"Pass"
      case Fail        => out"Fail"
      case Throws      => out"Throws exception"
      case CheckThrows => out"Exception in check"
      case Mixed       => out"Mixed"
      case Suite       => out"Suite"
      case Bench       => out"Benchmark"

  val unitsSeq: List[Output] = List(
    out"${colors.BurlyWood}(µs)",
    out"${colors.Goldenrod}(ms)",
    out"${colors.Sienna}(s) "
  )
    
  def showTime(n: Long, units: List[Output] = unitsSeq): Output = units match
    case Nil =>
      n.show.out
    
    case unit :: rest =>
      if n > 100000L then showTime(n/1000L, rest) else
        val sig = (n/1000L).show
        val frac = (n%1000).show.pad(3, Rtl, '0')(using textWidthCalculation.uniform)
        out"${colors.Silver}(${sig}.$frac) ${unit}"
    
  case class Summary(status: Status, id: TestId, count: Int, min: Long, max: Long, avg: Long):
    def indentedName: Output =
      val depth = id.suite.mm(_.id.depth).or(0) + 1
      
      val title =
        if status == Status.Suite then out"${colors.Silver}($Bold(${id.name}))"
        else out"${id.name}"
      
      out"${t"  "*(depth - 1)}$title"

    def minTime: Output = if min == 0L then out"" else showTime(min)
    def maxTime: Output = if max == 0L then out"" else showTime(max)
    def avgTime: Output = if avg == 0L then out"" else showTime(avg)
    def iterations: Output = if count == 0 then out"" else count.out

  

  def complete(coverage: Option[CoverageResults])(using Io): Unit =
    given TextWidthCalculator with
      private val eastAsian = textWidthCalculation.eastAsianScripts
      def width(text: Text): Int = text.s.foldLeft(0)(_ + width(_))
      def width(char: Char): Int = char match
        case '✓' | '✗' | '⎇' => 1
        case _                => char.displayWidth
    
    val table =
      val showStats = !lines.summaries.forall(_.count < 2)
      val timeTitle = if showStats then t"Avg" else t"Time"
      
      Table[Summary](
        Column(out"")(_.status.symbol),
        
        Column(out"$Bold(Hash)"): s =>
          out"${colors.CadetBlue}(${s.id.id})",
        
        Column(out"$Bold(Test)")(_.indentedName),
        
        Column(out"$Bold(Count)", align = Alignment.Right, hide = !showStats): s =>
          out"${colors.SteelBlue}(${s.iterations})",
        
        Column(out"$Bold(Min)", align = Alignment.Right, hide = !showStats): s =>
          if s.count < 2 then out"" else s.minTime,
        
        Column(out"$Bold($timeTitle)", align = Alignment.Right)(_.avgTime),
        
        Column(out"$Bold(Max)", align = Alignment.Right, hide = !showStats): s =>
          if s.count < 2 then out"" else s.maxTime
      )
      
    val columns = env(t"COLUMNS") match
      case As[Int](cols) => cols
      case _             => 120

    val summaryLines = lines.summaries

    coverage.foreach: coverage =>
      Io.println(out"$Bold($Underline(Test coverage))")
      case class CoverageData(path: Text, branches: Int, hits: Int, oldHits: Int):
        def hitsText: Output =
          val main = out"${if hits == 0 then colors.Gray else colors.ForestGreen}($hits)"
          if oldHits == 0 then main else out"${colors.Goldenrod}(${oldHits.show.subscript}) $main"
      
      val data = coverage.spec.groupBy(_.path).to(List).map: (path, branches) =>
        val hitCount = branches.map(_.id).map(coverage.hits.contains).count(identity(_))
        val oldHitCount = branches.map(_.id).map(coverage.oldHits.contains).count(identity(_))
        CoverageData(path, branches.size, hitCount, oldHitCount)

      val maxHits = data.map(_.branches).maxOption

      def line(tiles: List[TreeTile], surface: Surface): (Output, Juncture) =
        import treeStyles.default
        import surface.juncture.*
        
        val description: Output =
          if surface.juncture.treeName == t"DefDef" then surface.juncture.method.out
          else out"$shortCode"
        
        out"${tiles.map(_.text).join}• $description" -> surface.juncture

      def render(junctures: List[Surface]): LazyList[(Output, Juncture)] =
        drawTree[Surface, (Output, Juncture)](_.children, line)(junctures)
      
      import colors.*
      
      val allHits = coverage.hits ++ coverage.oldHits
      
      val junctures2 = coverage.structure.values.flatten
          .to(List)
          .filter(!_.covered(allHits))
          .map(_.copy(children = Nil))

      Table[(Output, Juncture)](
        Column(out"") { row => if row(1).branch then out"⎇" else out"" },
        Column(out"") { row =>
          if coverage.hits.contains(row(1).id) then out"${Bg(ForestGreen)}(  )"
          else if coverage.oldHits.contains(row(1).id) then out"${Bg(Goldenrod)}(  )"
          else out"${Bg(Brown)}(  )"
        },
        Column(out"Juncture")(_(0)),
        Column(out"Line") { row => out"$GreenYellow(${row(1).path})$Gray(:)$Gold(${row(1).lineNo})" },
        Column(out"Symbol")(_(1).symbolName)
      ).tabulate(render(junctures2), columns)(using tableStyles.horizontal).foreach(Io.println)
      
      Io.println(out"")
    
      Table[CoverageData](
        Column(out"Source file", align = Alignment.Left): data =>
          data.path,
        Column(out"Hits", align = Alignment.Right)(_.hitsText),
        Column(out"Size", align = Alignment.Right)(_.branches),
        Column(out"Coverage", align = Alignment.Right): data =>
          out"${(100*(data.hits + data.oldHits)/data.branches.toDouble)}%",
        Column(out""): data =>
          def width(n: Double): Text = if n == 0 then t"" else t"━"*(1 + (70*n).toInt)
          val covered: Text = width(maxHits.map(data.hits.toDouble/_).getOrElse(0))
          val oldCovered: Text = width(maxHits.map(data.oldHits.toDouble/_).getOrElse(0))
          
          val notCovered: Text = width(maxHits.map((data.branches.toDouble - data.hits -
              data.oldHits)/_).getOrElse(0))
          
          val bars = List(colors.ForestGreen -> covered, colors.Goldenrod -> oldCovered,
              colors.Brown -> notCovered)
          
          bars.filter(_(1).length > 0).map { (color, bar) => out"$color($bar)" }.join
      ).tabulate(data, columns).foreach(Io.println(_))
      
      Io.println(out"")
    
    if summaryLines.exists(_.count > 0) then
      val totals = summaryLines.groupBy(_.status).view.mapValues(_.size).to(Map) - Status.Suite
      val passed: Int = totals.getOrElse(Status.Pass, 0) + totals.getOrElse(Status.Bench, 0)
      val total: Int = totals.values.sum
      val failed: Int = total - passed
      Io.println(out"${escapes.Reset}")
      Io.println(out"$Bold($Underline(Test results))")

      table.tabulate(summaryLines, columns, delimitRows = DelimitRows.SpaceIfMultiline).foreach(Io.println(_))
      given Decimalizer = Decimalizer(decimalPlaces = 1)
      Io.println(out" $Bold(${colors.White}($passed)) passed (${100.0*passed/total}%), $Bold(${colors.White}($failed)) failed (${100.0*failed/total}%), $Bold(${colors.White}(${passed + failed})) total")
      Io.println(t"─"*72)
      List(Status.Pass, Status.Bench, Status.Throws, Status.Fail, Status.Mixed, Status.CheckThrows).grouped(3).foreach: statuses =>
        Io.println:
          statuses.map[Output]: status =>
            gossamer.pad[Output](out"  ${status.symbol} ${status.describe}")(20)
          .join(out" ")
      Io.println(t"─"*72)

    def benches(line: ReportLine): Iterable[ReportLine.Bench] =
      line match
        case bench@ReportLine.Bench(_, _) => Iterable(bench)
        case ReportLine.Suite(_, tests)   => tests.list.map(_(1)).flatMap(benches(_))
        case _                            => Nil
    
    benches(lines).groupBy(_.test.suite).foreach: (suite, benchmarks) =>
      val ribbon = Ribbon(colors.DarkGreen.srgb, colors.MediumSeaGreen.srgb, colors.PaleGreen.srgb)
      Io.println(ribbon.fill(out"${suite.mm(_.id.id).or(t"")}", out"Benchmarks", out"${suite.mm(_.name).or(t"")}"))
      
      val comparisons: List[ReportLine.Bench] =
        benchmarks.filter(!_.benchmark.baseline.unset).to(List)
      
      def confInt(b: Benchmark): Output =
        if b.confidenceInterval == 0 then out"" else out"${colors.Thistle}(±)${showTime(b.confidenceInterval)}"
      
      def opsPerS(b: Benchmark): Output =
        if b.throughput == 0 then out""
        else out"${colors.Silver}(${b.throughput}) ${colors.Turquoise}(op${colors.Gray}(·)s¯¹)"

      val bench: Table[ReportLine.Bench, Output] = Table[ReportLine.Bench](
        (List(
          Column(out"$Bold(Hash)"): s =>
            out"${colors.CadetBlue}(${s.test.id})",
          Column(out"$Bold(Test)"): s =>
            out"${s.test.name}",

          Column(out"$Bold(Min)", align = Alignment.Right): s =>
            showTime(s.benchmark.min.toLong),
        
          Column(out"$Bold(Mean)", align = Alignment.Right): s =>
            showTime(s.benchmark.mean.toLong),
          
          Column(out"$Bold(Confidence)", align = Alignment.Right): s =>
            out"P${s.benchmark.confidence} ${confInt(s.benchmark)}",
          
          Column(out"$Bold(Throughput)", align = Alignment.Right): s =>
            out"${opsPerS(s.benchmark)}"
        ) ::: (
          comparisons.map: c =>
            import Baseline.*
            val baseline = c.benchmark.baseline.avow(using Unsafe)
            Column(out"$Bold(${colors.CadetBlue}(${c.test.id}))", align = Alignment.Right): (bench: ReportLine.Bench) =>
              def op(left: Double, right: Double): Double = baseline.calc match
                case Difference => left - right
                case Ratio      => left/right
              
              def metric(value: Double) = if baseline.metric == ByTime then value else 1/value
              
              val value = baseline.compare match
                case Compare.Min  => op(metric(bench.benchmark.min), metric(c.benchmark.min))
                case Compare.Mean => op(metric(bench.benchmark.mean), metric(c.benchmark.mean))
                case Compare.Max  => op(metric(bench.benchmark.max), metric(c.benchmark.max))
              
              val valueWithUnits = baseline.metric match
                case ByTime =>
                  showTime(value.toLong)
                
                case BySpeed =>
                  out"${colors.Silver}(${value}) ${colors.Turquoise}(op${colors.Gray}(·)s¯¹)"

              baseline.calc match
                case Difference => if value == 0 then out"★"
                                   else if value < 0
                                   then out"${colors.Thistle}(-)${valueWithUnits.dropChars(1)}"
                                   else out"${colors.Thistle}(+)$valueWithUnits"
                
                case Ratio      => if value == 1 then out"★" else out"${colors.Silver}($value)"
        ))*
      )

      bench.tabulate(benchmarks.to(List).sortBy(-_.benchmark.throughput), columns).foreach(Io.println(_))

    def showLegend(): Unit =
      Io.println(t"─"*74)
      Io.println:
        StackTrace.legend.to(List).map: (symbol, description) =>
          out"$Bold(${colors.White}(${symbol.pad(3, Rtl)}))  ${description.pad(20)}"
        .grouped(3).to(List).map(_.to(List).join).join(out"${t"\n"}")
      Io.println(t"─"*74)

    details.to(List).sortBy(_(0).timestamp).foreach: (id, info) =>
      val ribbon = Ribbon(colors.DarkRed.srgb, colors.FireBrick.srgb, colors.Tomato.srgb)
      Io.println(ribbon.fill(out"$Bold(${id.id})", id.codepoint.text.out, id.name.out))
      
      info.foreach: debugInfo =>
        Io.println(t"")
        debugInfo match
          case DebugInfo.Throws(err) =>
            val name = out"$Italic(${colors.White}(${err.component}.${err.className}))"
            Io.println(out"${colors.Silver}(An exception was thrown while running test:)")
            Io.println(err.crop(t"probably.Runner", t"run()").out)
            showLegend()
          
          case DebugInfo.CheckThrows(err) =>
            val name = out"$Italic(${colors.White}(${err.component}.${err.className}))"
            Io.println(out"${colors.Silver}(An exception was thrown while checking the test predicate:)")
            Io.println(err.crop(t"probably.Outcome#", t"apply()").dropRight(1).out)
            showLegend()
          
          case DebugInfo.Compare(expected, found, cmp) =>
            val expected2: Output = out"$Italic(${colors.White}($expected))"
            val found2: Output = out"$Italic(${colors.White}($found))"
            val nl = if expected.contains(t"\n") || found.contains(t"\n") then '\n' else ' '
            val instead = out"but instead it returned$nl$found2$nl"
            Io.println(out"${colors.Silver}(The test was expected to return$nl$expected2$nl$instead)")
            Io.println(cmp.out)
          
          case DebugInfo.Captures(map) =>
            Table[(Text, Text), Output](
              Column(out"Expression", align = Alignment.Right)(_(0)),
              Column(out"Value")(_(1)),
            ).tabulate(map.to(List), 140).foreach(Io.println(_))
          
          case DebugInfo.Message(text) =>
            Io.println(text)
      
      Io.println()
