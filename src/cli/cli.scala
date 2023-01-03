/*
    Probably, version 0.4.0. Copyright 2017-23 Jon Pretty, Propensive OÜ.

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

import escritoire.*
import rudiments.*
import gossamer.*
import escapade.*
import iridescence.*
import tableStyles.rounded

import Runner.*

object Suite:
  val statuses@List(pass, fail, checkThrows, throws, tailFail, mixed) = List(
    '✓' -> colors.YellowGreen,
    '✗' -> colors.Red,
    '‼' -> colors.LightSeaGreen,
    '!' -> colors.OrangeRed,
    '±' -> colors.CornflowerBlue,
    '∂' -> colors.Gold
  ).map { (ch, color) => ansi"${Bg(color)}( ${colors.Black}($Bold(${ch.show})) )" }
  
  private val legend: List[AnsiText] =
    statuses.zip(List(t"Pass", t"Fail", t"Assertion throws exception", t"Test throws exception",
        t"Inconsistent test results", t"Suite partially fails")).map:
      (status, desc) => ansi"$status ${desc.show.fit(32)}"
    .to(List)

  val footer: AnsiText = legend.grouped(2).map(_.join(ansi"  ")).to(Seq).join(AnsiText(t"\n"),
      AnsiText(t"\n"), AnsiText(t"\n"))
  
  def show(report: Report): LazyList[AnsiText] =
    val simple = report.results.forall(_.count == 1)

    given AnsiShow[Outcome] =
      case Outcome.Passed                                         => pass
      case Outcome.FailsAt(Datapoint.Fail(map, _), 1)             => fail
      case Outcome.FailsAt(Datapoint.PredicateThrows(_, _, _), n) => checkThrows
      case Outcome.FailsAt(Datapoint.Throws(exception, map), 1)   => throws
      case Outcome.FailsAt(_, n)                                  => tailFail
      case Outcome.Mixed                                          => mixed

    val status: Column[Summary] = Column(ansi"", width = 3)(v => v.outcome)
    val hash: Column[Summary] = Column(ansi"Hash", width = 6)(v => Runner.shortDigest(v.name))
    val name: Column[Summary] = Column(ansi"Test")(s => t"${t"  "*s.indent}${s.name}")
    val count: Column[Summary] = Column(ansi"Count")(_.count)
    val min: Column[Summary] = Column(ansi"Min")(_.min)
    val avg: Column[Summary] = Column(if simple then "Time" else "Avg")(_.avg)
    val max: Column[Summary] = Column(ansi"Max")(_.max)
    
    val table =
      if simple then Table[Summary](status, hash, name, avg)
      else Table[Summary](status, hash, name, count, min, avg, max)

    val resultsTable: LazyList[AnsiText] =
      table.tabulate(report.results, 120, DelimitRows.SpaceIfMultiline)

    val failures: AnsiText =
      report.results.filter: result =>
        result.outcome != Outcome.Passed && result.outcome != Outcome.Mixed
      .flatMap: result =>
        List(
          ansi"${result.outcome} $Bold($Underline(${result.name})): ${colors.SkyBlue}(${result.outcome.filename}):${colors.Goldenrod}(${result.outcome.line})",
          ansi"${(result.outcome.inspect.cut(t"\n"): List[Text]).join(t"      ", t"\n      ", t"")}",
          ansi""
        )
      .join(AnsiText(t"\n"))

    val summary: AnsiText = Map(
      "Passed" -> report.passed,
      "Failed" -> report.failed,
      "Total" -> report.total
    ).map { case (key, value) => ansi"$Bold($key): ${value.show}" }.join(ansi"   ")

    resultsTable #::: LazyList(failures, summary, Suite.footer)

trait Suite(val name: Text) extends TestSuite:
  def run(using Runner): Unit
  
  final def main(args: IArray[Text]): Unit =
    val runner = Runner(args.map(TestId(_)).to(Set))
    
    val succeeded = try
      run(using runner)
      val report = runner.report()
      
      Suite.show(report).foreach: line =>
        System.out.nn.println(line.render)

      report.total == report.passed
    catch case error: Throwable =>
      val report = runner.report()
      Suite.show(report).foreach: line =>
        System.out.nn.println(line.render)

      System.out.nn.println(StackTrace(error).ansi.render)
      false
    
    terminate(succeeded)
  
  def terminate(success: Boolean): Unit = System.exit(if success then 0 else 1)
