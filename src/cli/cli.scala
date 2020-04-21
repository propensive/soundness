package probably

import escritoire._
import gastronomy._

abstract class Tests() {
  def tests(): Unit
  
  final def main(args: Array[String]): Unit = {
    import Status._
    tests()
    val report = test.report()
    val table = Tabulation[TestResult](
      Heading("", _.status match {
        case Pass       => Ansi.Color.green(Ansi.bold(Ansi.reverse(" ✓ ")))
        case Fail       => Ansi.Color.red(Ansi.bold(Ansi.reverse(" ✗ ")))
        case Threw      => Ansi.Color.magenta(Ansi.bold(Ansi.reverse(" ! ")))
        case CheckThrew => Ansi.Color.cyan(Ansi.bold(Ansi.reverse(" ? ")))
        case Fickle     => Ansi.Color.yellow(Ansi.bold(Ansi.reverse(" ± ")))
      }),
      Heading("Hash", _.name.digest[Sha256].encoded[Hex].take(6).toLowerCase),
      Heading("Test", _.name),
      Heading("Count", _.count),
      Heading("Min", _.min),
      Heading("Avg", _.avg),
      Heading("Max", _.max)
    )
    table.tabulate(100, report.results).foreach(println)
    System.exit(if(report.results.exists(_.status != Status.Pass)) 1 else 0)
  }
}

object test extends Runner