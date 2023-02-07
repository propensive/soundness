package probably

import rudiments.*
import deviation.*
import gossamer.*
import chiaroscuro.*
import escritoire.*
import escapade.*
import iridescence.*
import dendrology.*

import scala.collection.mutable as scm

enum DebugInfo:
  case Throws(stack: StackTrace)
  case CheckThrows(stack: StackTrace)
  case Captures(values: Map[Text, Text])
  case Compare(expected: Text, found: Text, comparison: Comparison)

trait Inclusion[ReportType, DataType]:
  def include(report: ReportType, testId: TestId, data: DataType): ReportType

trait TestReporter[ReportType]:
  def make(): ReportType
  def declareSuite(report: ReportType, suite: TestSuite): Unit
  def complete(report: ReportType): Unit

object TestReporter:
  given TestReporter[TestReport] with
    def make(): TestReport = TestReport()
    def declareSuite(report: TestReport, suite: TestSuite): Unit = report.declareSuite(suite)
    def complete(report: TestReport): Unit = report.complete()

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

class TestReport():
  private val tests: scm.SortedMap[TestId, scm.ArrayBuffer[Outcome]] =
    scm.TreeMap[TestId, scm.ArrayBuffer[Outcome]]().withDefault { _ => scm.ArrayBuffer[Outcome]() }
  
  private val details: scm.SortedMap[TestId, scm.ArrayBuffer[DebugInfo]] =
    scm.TreeMap[TestId, scm.ArrayBuffer[DebugInfo]]().withDefault { _ => scm.ArrayBuffer[DebugInfo]() }

  def declareSuite(suite: TestSuite): TestReport =
    this.tap { _ => tests(suite.id) = scm.ArrayBuffer[Outcome]() }

  def addOutcome(testId: TestId, outcome: Outcome): TestReport =
    this.tap { _ => tests(testId) = tests(testId).append(outcome) }
  
  def addDebugInfo(testId: TestId, info: DebugInfo): TestReport =
    this.tap { _ => details(testId) = details(testId).append(info) }

  enum Status:
    case Pass, Fail, Throws, CheckThrows, Mixed, Suite

    def color: Rgb24 = this match
      case Pass        => rgb"#8abd00"
      case Fail        => colors.Tomato
      case Throws      => colors.DarkOrange
      case CheckThrows => rgb"#dd40a0"
      case Mixed       => rgb"#ddd700"
      case Suite       => colors.SlateBlue

    def symbol: AnsiText = this match
      case Pass        => ansi"${Bg(rgb"#8abd00")}( $Bold(${colors.Black}(✓)) )"
      case Fail        => ansi"${Bg(colors.Tomato)}( $Bold(${colors.Black}(✗)) )"
      case Throws      => ansi"${Bg(colors.DarkOrange)}( $Bold(${colors.Black}(!)) )"
      case CheckThrows => ansi"${Bg(rgb"#dd40a0")}( $Bold(${colors.Black}(‼)) )"
      case Mixed       => ansi"${Bg(rgb"#ddd700")}( $Bold(${colors.Black}(?)) )"
      case Suite       => ansi"   "

  case class Summary(status: Status, id: TestId, count: Int, min: Long, max: Long, avg: Long):
    def indentedName: AnsiText =
      val depth = id.suite.mm(_.id.depth).or(0) + 1
      val title = if status == Status.Suite then ansi"${colors.Silver}($Bold(${id.name}))" else ansi"${id.name}"
      ansi"${t"  "*(depth - 2)}$title"

    val unitsSeq: List[AnsiText] = List(
      ansi"${colors.BurlyWood}(µs)",
      ansi"${colors.Goldenrod}(ms)",
      ansi"${colors.Sienna}(s) "
    )
    
    protected def time(n: Long, units: List[AnsiText] = unitsSeq): AnsiText = units match
      case Nil =>
        n.show.ansi
      
      case unit :: rest =>
        if n > 100000L then time(n/1000L, rest) else
          val sig = (n/1000L).show
          val frac = (n%1000).show.pad(3, Rtl, '0')(using textWidthCalculation.uniform)
          ansi"${colors.Silver}(${sig}.$frac) ${unit}"

    def minTime: AnsiText = if min == 0L then ansi"" else time(min)
    def maxTime: AnsiText = if max == 0L then ansi"" else time(max)
    def avgTime: AnsiText = if avg == 0L then ansi"" else time(avg)
    def iterations: AnsiText = if count == 0 then ansi"" else count.ansi

  def complete(): Unit =
    import hieronymus.textWidthCalculation.eastAsianScripts
    val summaries: List[Summary] = tests.to(List).map: (id, buf) =>
      val status =
        if buf.isEmpty then Status.Suite
        else if buf.forall(_.is[Outcome.Pass]) then Status.Pass
        else if buf.forall(_.is[Outcome.Fail]) then Status.Fail
        else if buf.forall(_.is[Outcome.Throws]) then Status.Throws
        else if buf.forall(_.is[Outcome.CheckThrows]) then Status.CheckThrows
        else Status.Mixed
      
      if buf.length == 0 then Summary(status, id, 0, 0, 0, 0) else
        val avg: Long = buf.foldLeft(0L)(_ + _.duration)/buf.length
        Summary(status, id, buf.length, buf.map(_.duration).min, buf.map(_.duration).max, avg)
    
    val table: Table[Summary] =
      val stats = !summaries.forall(_.count < 2)
      Table(
        Column(ansi"")(_.status.symbol),
        Column(ansi"$Bold(Hash)"): s =>
          ansi"${colors.CadetBlue}(${s.id.id})",
        Column(ansi"$Bold(Test)")(_.indentedName),
        Column(ansi"$Bold(Count)", align = Alignment.Right, hide = !stats): s =>
          ansi"${colors.SteelBlue}(${s.iterations})",
        Column(ansi"$Bold(Min)", align = Alignment.Right, hide = !stats): s =>
          if s.count < 2 then ansi"" else s.minTime,
        Column(ansi"$Bold(${if stats then t"Avg" else t"Time"})", align = Alignment.Right) { s => s.avgTime },
        Column(ansi"$Bold(Max)", align = Alignment.Right, hide = !stats): s =>
          if s.count < 2 then ansi"" else s.maxTime
      )
      
    import tableStyles.rounded, treeStyles.default
    table.tabulate(summaries, 120).map(_.render).foreach(println(_))

    details.foreach: (id, info) =>
      val color = tests(id)
      val ribbon = Ribbon(colors.DarkRed.srgb, colors.FireBrick.srgb, colors.Tomato.srgb)
      println(ribbon.fill(ansi"$Bold(${id.id})", id.codepoint.text.ansi, id.name.ansi).render)
      
      info.foreach: debugInfo =>
        println()
        debugInfo match
          case DebugInfo.Throws(err) =>
            val name = ansi"$Italic(${colors.White}(${err.component}.${err.className}))"
            println(ansi"${colors.Silver}(An exception was thrown while running test:)".render)
            println(err.crop(t"probably.Runner", t"run()").ansi.render)
          
          case DebugInfo.CheckThrows(err) =>
            val name = ansi"$Italic(${colors.White}(${err.component}.${err.className}))"
            println(ansi"${colors.Silver}(An exception was thrown while checking the test predicate:)".render)
            println(err.crop(t"probably.Outcome#", t"apply()").dropRight(1).ansi.render)
          
          case DebugInfo.Compare(expected, found, cmp) =>
            val expected2: AnsiText = ansi"$Italic(${colors.White}($expected))"
            val found2: AnsiText = ansi"$Italic(${colors.White}($found))"
            val nl = if expected.contains(t"\n") || found.contains(t"\n") then '\n' else ' '
            println(ansi"${colors.Silver}(The test was expected to return$nl$expected2${nl}but instead it returned$nl$found2${nl})".render)
            println(cmp.ansi.render)
          
          case DebugInfo.Captures(map) =>
            Table[(Text, Text)](
              Column(ansi"Expression", align = Alignment.Right)(_(0)),
              Column(ansi"Value")(_(1)),
            ).tabulate(map.to(List), 140).map(_.render).foreach(println(_))
      
      println()