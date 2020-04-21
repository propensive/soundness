package probably

import escritoire._
import gastronomy._

abstract class Tests() {
  def tests(): Unit
  
  final def main(args: Array[String]): Unit = {
    tests()
    val report = test.report()
    val table = Tabulation[TestSummary](
      Heading("", _.result match {
        case Pass =>
          Ansi.Color.green(Ansi.bold(Ansi.reverse(" ✓ ")))
        case FailsAt(Fail, n, m) =>
          Ansi.Color.red(Ansi.bold(Ansi.reverse(" ✗ ")))
        case FailsAt(ThrowsInCheck(e), n, m) =>
          Ansi.Color.cyan(Ansi.bold(Ansi.reverse(" ? ")))
        case FailsAt(Throws(e), 0, m) =>
          Ansi.Color.magenta(Ansi.bold(Ansi.reverse(" ! ")))
        case FailsAt(_, n, m) =>
          Ansi.Color.yellow(Ansi.bold(Ansi.reverse(" ± ")))
      }),
      Heading("Hash", _.name.digest[Sha256].encoded[Hex].take(6).toLowerCase),
      Heading("Test", _.name),
      Heading("Count", _.count),
      Heading("Min", _.min),
      Heading("Avg", _.avg),
      Heading("Max", _.max)
    )
    table.tabulate(100, report.results).foreach(println)
    val passed = report.results.count(_.result == Pass)
    val total = report.results.size
    val failed = total - passed
    List(s"Pass: ${passed}   Fail: ${failed}   Total: ${total}")
    System.exit(if(total == passed) 1 else 0)
  }
}

object test extends Runner