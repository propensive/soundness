/*
    Probably, version 0.18.0. Copyright 2017-21 Jon Pretty, Propensive OÜ.

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

import scala.collection.mutable

import Runner.*

object Suite:
  val statuses@List(pass, fail, checkThrows, throws, tailFail, mixed) = List(
    '✓' -> colors.YellowGreen,
    '✗' -> colors.Crimson,
    '?' -> colors.LightSeaGreen,
    '!' -> colors.PaleVioletRed,
    '±' -> colors.DodgerBlue,
    '#' -> colors.Gold
  ).map { (ch, color) => ansi"${Bg(color)}( ${colors.Black}($Bold(${ch.show})) )" }
  
  private val legend: List[AnsiString] = statuses.zip(List("Pass", "Fail", "Assertion throws",
      "Throws an exception", "Inconsistent", "Suite partially fails")).map { (status, desc) =>
    ansi"$status ${desc.padTo(32, ' ')}"
  }.to(List)

  val footer: AnsiString = legend.grouped(2).map(_.join(ansi"  ")).to(Seq).join(AnsiString(str"\n"),
      AnsiString(str"\n"), AnsiString(str"\n"))
  
  def show(report: Report): AnsiString =
    val simple = report.results.forall(_.count == 1)

    given AnsiShow[Outcome] =
      case Outcome.Passed                                         => pass
      case Outcome.FailsAt(Datapoint.Fail(map, _), 1)             => fail
      case Outcome.FailsAt(Datapoint.PredicateThrows(_, _, _), n) => checkThrows
      case Outcome.FailsAt(Datapoint.Throws(exception, map), 1)   => throws
      case Outcome.FailsAt(_, n)                                  => tailFail
      case Outcome.Mixed                                          => mixed

    val status = Column[Summary, String, Outcome]("", _.outcome)
    val hash = Column[Summary, String, Txt]("Hash", v => Runner.shortDigest(v.name))
    val name = Column[Summary, String, String]("Test", s => s"${"  "*s.indent}${s.name}")
    val count = Column[Summary, String, Int]("Count", _.count)
    val min = Column[Summary, String, Double]("Min", _.min)
    val avg = Column[Summary, String, Double](if simple then "Time" else "Avg", _.avg)
    val max = Column[Summary, String, Double]("Max", _.max)
    
    val table =
      if simple then Tabulation[Summary](status, hash, name, avg)
      else Tabulation[Summary](status, hash, name, count, min, avg, max)

    val resultsTable: AnsiString = table.tabulate(100, report.results).join(str"\n").ansi

    val failures: AnsiString = report.results.filter { result =>
      result.outcome != Outcome.Passed && result.outcome != Outcome.Mixed
    }.flatMap { result =>
      List(
        ansi"${result.outcome} $Bold($Underline(${result.name})): ${colors.SkyBlue}(${result.outcome.filename}):${colors.Goldenrod}(${result.outcome.line})",
        ansi"${(result.outcome.debug.cut(str"\n"): List[Txt]).join(str"      ", str"\n      ", str"")}",
        ansi""
      )
    }.join(AnsiString(str"\n"))

    val summary: AnsiString = Map(
      "Passed" -> report.passed,
      "Failed" -> report.failed,
      "Total" -> report.total
    ).map { (key, value) => ansi"$Bold($key): ${value.show}" }.join(ansi"   ")

    List(resultsTable, failures, summary, Suite.footer).join(AnsiString(str"\n"))

trait Suite(val name: Txt) extends TestSuite:
  def run(using Runner): Unit
  
  final def main(args: IArray[Txt]): Unit =
    val runner = Runner(args.map(TestId(_)).to(Set))
    run(using runner)
    val report = runner.report()
    println(Suite.show(report).render)

    terminate(report.total == report.passed)
  
  def terminate(success: Boolean): Unit = System.exit(if success then 0 else 1)
