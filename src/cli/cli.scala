/*

    Probably, version 0.1.0. Copyright 2017-20 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
    compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the License is
    distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and limitations under the License.

*/
package probably

import escritoire._
import gastronomy._

object Tests:
  import Ansi.Color._
  def ansi(symbol: Char, code: Ansi.Color) = code(Ansi.bold(Ansi.reverse(s" ${symbol} ")))

  val statuses@List(pass, fail, checkThrows, throws, tailFail, mixed) =
    List('✓' -> green, '✗' -> red, '?' -> cyan, '!' -> magenta, '±' -> blue, '#' -> yellow).map(ansi)

trait Tests:
  def run(test: Runner): Unit
  
  final def main(args: Array[String]): Unit =
    val test = Runner(args.map(TestId(_)).to(Set))
    run(test)
    val report = test.report()
    val simple = report.results.forall(_.count == 1)

    given AnsiShow[Outcome] =
      case Passed                                    => Tests.pass
      case FailsAt(Fail(map), 1)                     => Tests.fail
      case FailsAt(ThrowsInCheck(exception, map), n) => Tests.checkThrows
      case FailsAt(Throws(exception, map), 0)        => Tests.throws
      case FailsAt(_, n)                             => Tests.tailFail
      case Mixed                                     => Tests.mixed

    val status = Heading[Summary, Outcome]("", _.outcome)
    val hash = Heading[Summary, String]("Hash", _.name.digest[Sha256].encoded[Hex].take(6).toLowerCase)
    val name = Heading[Summary, String]("Test", s => s"${"  "*s.indent}${s.name}")
    val count = Heading[Summary, Int]("Count", _.count)
    val min = Heading[Summary, Double]("Min", _.min)
    val avg = Heading[Summary, Double](if simple then "Time" else "Avg", _.avg)
    val max = Heading[Summary, Double]("Max", _.max)
    val debug = Heading[Summary, String]("Debug", _.outcome.debug)
    
    val table =
      if simple then Tabulation[Summary](status, hash, name, avg, debug)
      else Tabulation[Summary](status, hash, name, count, min, avg, max, debug)

    table.tabulate(100, report.results).foreach(println)
    val passed = report.results.count(_.outcome == Passed)
    val total = report.results.size
    val failed = total - passed
    
    println(Ansi.bold(s"Pass: ${passed}   Fail: ${failed}   Total: ${total}"))
    println()

    val legend = Tests.statuses.zip(List("Pass", "Fail", "Throws exception during check", "Throws exception",
        "Fails sometimes", "Test suite partially fails")).map { (status, description) =>
      s"${status} ${description.padTo(32, ' ')}"
    }.to(List)
    
    legend.take(3).zip(legend.drop(3)).map { (left, right) => println((" "*2)+left+right) }
    
    println()

    System.exit(if(total == passed) 0 else 1)