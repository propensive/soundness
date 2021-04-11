/*

    Probably, version 0.8.0. Copyright 2017-20 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
    compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the License is
    distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and limitations under the License.

*/
package probably

import escritoire.*

import scala.collection.mutable

import Runner.*

object Suite:
  import Ansi.Color.*
  def ansi(symbol: Char, code: Ansi.Color) = code(Ansi.bold(Ansi.reverse(s" ${symbol} ")))

  val statuses@List(pass, fail, checkThrows, throws, tailFail, mixed) =
    List('✓' -> green, '✗' -> red, '?' -> cyan, '!' -> magenta, '±' -> blue, '#' -> yellow).map(ansi)
  
  private val legend: List[String] = statuses.zip(List("Pass", "Fail", "Throws in check",
      "Throws in body", "Fails sometimes", "Suite partially fails")).map { (status, description) =>
    s"${status} ${description.padTo(32, ' ')}"
  }.to(List)

  val footer: String = legend.grouped(2).map(_.mkString("  ")).mkString("\n", "\n", "\n")
  
  def show(report: Report): String =
    val simple = report.results.forall(_.count == 1)

    given AnsiShow[Outcome] =
      case Outcome.Passed                                       => pass
      case Outcome.FailsAt(Datapoint.Fail(map, _), 1)           => fail
      case Outcome.FailsAt(Datapoint.ThrowsInCheck(_, _, _), n) => checkThrows
      case Outcome.FailsAt(Datapoint.Throws(exception, map), 1) => throws
      case Outcome.FailsAt(_, n)                                => tailFail
      case Outcome.Mixed                                        => mixed

    val status = Heading[Summary, Outcome]("", _.outcome)
    val hash = Heading[Summary, String]("Hash", v => Runner.shortDigest(v.name))
    val name = Heading[Summary, String]("Test", s => s"${"  "*s.indent}${s.name}")
    val count = Heading[Summary, Int]("Count", _.count)
    val min = Heading[Summary, Double]("Min", _.min)
    val avg = Heading[Summary, Double](if(simple) "Time" else "Avg", _.avg)
    val max = Heading[Summary, Double]("Max", _.max)
    val debug = Heading[Summary, String]("Debug", _.outcome.debug)
    
    val table =
      if(simple) Tabulation[Summary](status, hash, name, avg, debug)
      else Tabulation[Summary](status, hash, name, count, min, avg, max, debug)

    val resultsTable = table.tabulate(100, report.results).mkString("\n")
    
    val summary = Map("Passed" -> report.passed, "Failed" -> report.failed, "Total" -> report.total).map {
      (key, value) => s"${Ansi.bold(key)}: $value"
    }.mkString("   ")

    List(resultsTable, summary, Suite.footer).mkString("\n")

trait Suite(val name: String) extends TestSuite:
  def run(using Runner): Unit
  
  final def main(args: Array[String]): Unit =
    val runner = Runner(args.map(TestId(_)).to(Set))
    run(using runner)
    val report = runner.report()
    println(Suite.show(report))

    terminate(report.total == report.passed)
  
  def terminate(success: Boolean): Unit = System.exit(if success then 0 else 1)
