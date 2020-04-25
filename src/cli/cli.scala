package probably

import escritoire._
import gastronomy._

abstract class Tests() {
  def run(): Unit
  
  final def main(args: Array[String]): Unit = {
    test.setOnly(args.to[Set])
    run()
    val report = test.report()
    val simple = report.results.forall(_.count == 1)

    val status = Heading[Summary, String]("", _.outcome match {
      case Passed =>
        Ansi.Color.green(Ansi.bold(Ansi.reverse(" ✓ ")))
      case FailsAt(Fail(map), n) =>
        Ansi.Color.red(Ansi.bold(Ansi.reverse(" ✗ ")))
      case FailsAt(ThrowsInCheck(exception, map), n) =>
        Ansi.Color.cyan(Ansi.bold(Ansi.reverse(" ? ")))
      case FailsAt(Throws(exception, map), 0) =>
        Ansi.Color.magenta(Ansi.bold(Ansi.reverse(" ! ")))
      case FailsAt(_, n) =>
        Ansi.Color.yellow(Ansi.bold(Ansi.reverse(" ± ")))
    })

    val hash = Heading[Summary, String]("Hash", _.name.digest[Sha256].encoded[Hex].take(6).toLowerCase)
    val name = Heading[Summary, String]("Test", _.name)
    val count = Heading[Summary, Int]("Count", _.count)
    val min = Heading[Summary, Double]("Min", _.min)
    val avg = Heading[Summary, Double](if(simple) "Time" else "Avg", _.avg)
    val max = Heading[Summary, Double]("Max", _.max)
    val debug = Heading[Summary, String]("Debug", _.outcome.debug)
    
    val table =
      if(simple) Tabulation[Summary](status, hash, name, avg, debug)
      else Tabulation[Summary](status, hash, name, count, min, avg, max, debug)

    table.tabulate(100, report.results).foreach(println)
    val passed = report.results.count(_.outcome == Passed)
    val total = report.results.size
    val failed = total - passed
    println(Ansi.bold(s"Pass: ${passed}   Fail: ${failed}   Total: ${total}"))
    System.exit(if(total == passed) 0 else 1)
  }
}

object test extends Runner {

  def digest(test: Test): String = test.name.digest[Sha256].encoded[Hex].take(6).toLowerCase

  private var testsToRun: Set[String] = Set()
  def setOnly(only: Set[String]) = testsToRun = only

  override def only(test: Test): Boolean = testsToRun.isEmpty || testsToRun.contains(digest(test))
}
