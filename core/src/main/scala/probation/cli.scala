package probation

import scala.collection.mutable.ListBuffer
import scala.collection.immutable.ListMap

/** companion object for [[CliRunner]] */
object CliRunner {
  
  case class Config(
    columns: Int = 100,
    color: Boolean = true,
    runLazily: Boolean = false,
    testOnly: Option[Set[String]] = None,
    quiet: Boolean = false,
    outputDir: Option[String] = None
  )

  object Ansi {
    val esc = 27.toChar
    def rgb(red: Int, green: Int, blue: Int) = s"$esc[38;2;$red;$green;${blue}m"
    val reset: String = s"$esc[39;49m"
    
    // Colors are taken from the solarized palette
    val base03: String = rgb(0, 43, 54)
    val base02: String = rgb(7, 54, 66)
    val base01: String = rgb(88, 110, 117)
    val base00: String = rgb(101, 123, 131)
    val base0: String = rgb(131, 148, 150)
    val base1: String = rgb(147, 161, 161)
    val base2: String = rgb(238, 232, 213)
    val base3: String = rgb(253, 246, 227)
    val yellow: String = rgb(181, 137, 0)
    val orange: String = rgb(203, 75, 22)
    val red: String = rgb(220, 50, 47)
    val magenta: String = rgb(211, 54, 130)
    val violet: String = rgb(108, 113, 196)
    val blue: String = rgb(38, 139, 210)
    val cyan: String = rgb(42, 161, 152)
    val green: String = rgb(133, 153, 0)
  }
  
  private val decimalFormat = {
    val df = new java.text.DecimalFormat()
    df.setMinimumFractionDigits(3)
    df.setMaximumFractionDigits(3)
    df
  }

  private val timeUnits: List[String] = List("ns", "μs", "ms", "s", "ks")
  
  sealed abstract class Outcome(val string: String, val diagnosis: List[String],
      val success: Option[Boolean])

  private def showCaptures(values: Map[String, String]) =
    values.map { case (label, value) => s"$label=$value" }.mkString(",")

  case object Passed extends Outcome("pass", Nil, Some(true))
  case class Failed(reason: String, captures: Map[String, String]) extends Outcome("fail",
      (if(captures.isEmpty) Nil else List(showCaptures(captures))) :::
      List(reason), Some(false))
  
  case class ThrewInCheck(throwable: Throwable) extends
      Outcome("fail", List(s"exception thrown during assertion: ${throwable}"), None)
  
  case class ThrewInRun(throwable: Throwable, captures: Map[String, String]) extends
      Outcome("fail", (if(captures.isEmpty) Nil else List(captures.mkString(", "))) ::: List(s"exception thrown during run: ${throwable}"), Some(false))
  
  case class FailedDependency(dep: String) extends
      Outcome("skip", List(s"dependency $dep failed"), None)
  
  case object Unstable extends Outcome("vari", Nil, Some(false))
  
  case class Result(definition: Test.Definition[_], outcome: Outcome, duration: Long)
}

/** a general-purpose instance of a [[Runner]] which reports by printing results to standard output
 */
class CliRunner(config: CliRunner.Config = CliRunner.Config()) extends Runner {
  import CliRunner._, Ansi._
  import scala.util._

  private[this] val results: ListBuffer[CliRunner.Result] = ListBuffer()

  type Return = Unit

  private def check[T](test: Test[T]): Outcome = test.result._1.map { r =>
    Try {
      if(test.assertion(test())) Passed
      else Failed(test.failure(r), test.definition.observed.map(_()).toMap)
    } match {
        case Success(v) => v
        case Failure(e) => ThrewInCheck(e)
      }
    } match {
      case Success(v) => v
      case Failure(Test.DependencyFailureException(dep)) =>
        FailedDependency(dep.take(6))
      case Failure(f) =>
        ThrewInRun(f, test.definition.observed.map(_()).toMap)
    }

  def exec[T](test: Test[T]): T = {
    if(config.runLazily) record(test)
    test.result._1.getOrElse { throw Test.DependencyFailureException(test.definition.hash) }
  }

  def record[T](test: Test[T]): Unit = {
    val run = config.testOnly.map(_.exists(test.definition.hash startsWith _)).getOrElse(true)
    if(run) {
      val outcome: Outcome = check(test)
      synchronized { results += Result(test.definition, outcome, test.result._2) }
      ()
    }
  }

  private[this] def stripColor(string: String): String = string.replaceAll("""\e\[?.*?[\@-~]""", "")

  private[this] def tabulate(
    titles: Vector[Either[String, String]],
    rows: Vector[Either[Vector[String], List[String]]]
  ): Unit = {
    // organize the rows into columns
    val colRows = rows.collect { case Left(xs) => xs }
    val cols: Vector[Vector[String]] = (titles.map(_.merge) +: colRows).transpose
    val naturalColWidths: Vector[Int] = cols.map(_.map(stripColor(_).size).max)
    val naturalWidth: Int = naturalColWidths.sum + (naturalColWidths.size - 1)*2
    val widestColIdx: Int = naturalColWidths.zipWithIndex.maxBy(_._1)._2
    val widestColWidth: Int = naturalColWidths(widestColIdx) - naturalWidth + config.columns
    val colWidths: Vector[Int] = naturalColWidths.updated(widestColIdx, widestColWidth)

    def trim(string: String, width: Int): String =
      if(stripColor(string).length > width) trim(string.take(string.length - 1), width) else string

    def pad(string: String, width: Int): String =
      string+" "*(width - (stripColor(string).length))

    def leftAlign(value: String, width: Int): String =
      pad(if(stripColor(value).length <= width) value else trim(value, width - 3)+"...", width)
    
    def rightAlign(value: String, width: Int): String =
      leftAlign(value.reverse, width).reverse

    def row(values: Vector[String]): String = values.indices.map { idx =>
      if(titles(idx).isRight) rightAlign(values(idx), colWidths(idx))
      else leftAlign(values(idx), colWidths(idx))
    }.mkString("  ")

    val underlines: String = row(titles.map(_.merge.map { case ' ' => ' ' case _ => '-' }))

    println(row(titles.map(_.merge)))
    println(underlines)
    rows.foreach {
      case Left(cells) =>
        println(row(cells))
      case Right(lines) =>
        val indent = 16
        
        val indented =
          if(lines.forall(_.size <= config.columns - indent)) lines.map((" "*indent)+_)
          else lines
        
        indented.foreach { line => println(base1+stripColor(line)+reset) }
    }
  }

  private[this] def outcomeColor(outcome: Outcome): String =
    outcome.success.map(if(_) green else red).getOrElse(yellow)

  private[this] def formatTime(time: Double, suffix: List[String] = CliRunner.timeUnits): String =
    if(time > 1e3) formatTime(time/1000L, suffix.tail)
    else CliRunner.decimalFormat.format(time)+suffix.head

  def report(): Unit = {
    val testResults = results.foldLeft(ListMap[Test.Definition[_], List[CliRunner.Result]]()) {
      case (results, next) =>
        results.updated(next.definition, next :: results.get(next.definition).getOrElse(Nil))
    }
    
    case class Result(outcome: Outcome, cells: Vector[String], diagnosis: List[String]) {
      def tabulation = if(diagnosis.isEmpty) Vector(Left(cells))
          else Vector(Left(cells), Right(diagnosis))
    }

    val rows: Vector[Result] = testResults.map {
      case (test, many) => 
        val durations = many.map(_.duration)
        val avg = durations.sum/many.size
        
        // FIXME: Better check for consistency
        val outcome = many.map(_.outcome).groupBy(identity).to[List] match {
          case (o, xs) :: Nil => o
          case _ => Unstable
        }
        
        val outcomeTxt = s"$base00[${outcomeColor(outcome)}${outcome.string}$base00]$reset"
        val countTxt = if(durations.length == 1) "" else durations.length.toString
        val minTxt = if(durations.length == 1) "" else formatTime(durations.min.toDouble)
        val maxTxt = if(durations.length == 1) "" else formatTime(durations.max.toDouble)
        
        Result(outcome, Vector(outcomeTxt, cyan+test.hash+reset, test.name, countTxt,
            minTxt, formatTime(avg.toDouble), maxTxt), outcome.diagnosis)
    }.to[Vector]

    val List(skip, fail, pass) = List(None, Some(false), Some(true)).map { r =>
      rows.count(_.outcome.success == r)
    }
    
    val total = pass + fail + skip
    val percent = (100*pass/total.toDouble + 0.5).toInt

    if(!config.quiet) {
      tabulate(Vector(Left("RESULT"), Left("HASH"), Left("TEST"), Right("COUNT"), Right("MIN"),
          Right("AVG"), Right("MAX")), rows.flatMap(_.tabulation))
      println(s"\n${base3}Passed: $pass/$total   Failed: $fail/$total   Skipped: $skip/$total")
      println(s"$base3$percent% of the tests passed\n")
    }
  }
}
