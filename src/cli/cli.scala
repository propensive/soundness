package probably

import escritoire._
import gastronomy._

abstract class Tests() {
  def run(): Unit
  
  final def main(args: Array[String]): Unit = {
    test.setOnly(args.to[Set])
    run()
    val report = test.report()
    val table = Tabulation[TestSummary](
      Heading("", _.result match {
        case Pass =>
          Ansi.Color.green(Ansi.bold(Ansi.reverse(" ✓ ")))
        case FailsAt(Fail(map), n) =>
          Ansi.Color.red(Ansi.bold(Ansi.reverse(" ✗ ")))
        case FailsAt(ThrowsInCheck(exception, map), n) =>
          Ansi.Color.cyan(Ansi.bold(Ansi.reverse(" ? ")))
        case FailsAt(Throws(exception, map), 0) =>
          Ansi.Color.magenta(Ansi.bold(Ansi.reverse(" ! ")))
        case FailsAt(_, n) =>
          Ansi.Color.yellow(Ansi.bold(Ansi.reverse(" ± ")))
      }),
      Heading("Hash", _.name.digest[Sha256].encoded[Hex].take(6).toLowerCase),
      Heading("Test", _.name),
      Heading("Count", _.count),
      Heading("Min", _.min),
      Heading("Avg", _.avg),
      Heading("Max", _.max),
      Heading("Debug", _.result.debug)
    )
    table.tabulate(100, report.results).foreach(println)
    val passed = report.results.count(_.result == Pass)
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
