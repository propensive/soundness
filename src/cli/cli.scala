package probation

import scala.collection.mutable.ListBuffer
import scala.collection.immutable.ListMap

import escritoire._

/** companion object for [[CliRunner]] */
object CliRunner {
  
  case class Config(
    columns: Int = 95,
    color: Boolean = true,
    runLazily: Boolean = false,
    testOnly: Option[Set[String]] = None,
    quiet: Boolean = false,
    outputDir: Option[String] = None
  )

  private[this] val decimalFormat = {
    val df = new java.text.DecimalFormat()
    df.setMinimumFractionDigits(3)
    df.setMaximumFractionDigits(3)
    df
  }

  private[this] val timeUnits: List[String] = List("ns", "μs", "ms", "s", "ks")
 
  final case class Time(value: Double) extends AnyVal
  final case class Hash(value: String) extends AnyVal
  final case class Count(value: Int) extends AnyVal

  def formatTime(time: Double, suffix: List[String] = timeUnits): String =
    if(time > 1e3) formatTime(time/1000L, suffix.tail)
    else decimalFormat.format(time)+suffix.head

  
  sealed abstract class Outcome(val string: String, val diagnosis: List[String],
      val success: Option[Boolean])

  case object Passed extends Outcome("pass", Nil, Some(true))
  case class Failed(reason: String) extends Outcome("fail", List(reason), Some(false))
  
  case class ThrewInCheck(throwable: Throwable) extends
      Outcome("fail", List(s"exception thrown during assertion: ${throwable}"), None)
  
  case class ThrewInRun(throwable: Throwable) extends
      Outcome("fail", List(s"exception thrown during run: ${throwable}"), Some(false))
  
  case class FailedDependency(dep: String) extends
      Outcome("skip", List(s"dependency $dep failed"), None)
  
  case object Mixed extends Outcome("vari", Nil, Some(false))
  
  case class RunResult(definition: Test.Definition[_], outcome: Outcome, duration: Long)
}

/** a general-purpose instance of a [[Runner]] which reports by printing results to standard output
 */
class CliRunner(config: CliRunner.Config = CliRunner.Config()) extends Runner {
  import CliRunner._, Ansi._, Color._
  import scala.util._

  private[this] val results: ListBuffer[CliRunner.RunResult] = ListBuffer()

  type Return = Nothing

  private def check[T](test: Test[T]): Outcome = test.result._1.map { r =>
    Try {
      if(test.assertion(test())) Passed
      else Failed(test.failure(r))
    } match {
        case Success(v) => v
        case Failure(e) => ThrewInCheck(e)
      }
    } match {
      case Success(v) => v
      case Failure(Test.DependencyFailureException(dep)) =>
        FailedDependency(dep.take(6))
      case Failure(f) =>
        ThrewInRun(f)
    }

  def exec[T](test: Test[T]): T = {
    if(config.runLazily) record(test)
    test.result._1.getOrElse { throw Test.DependencyFailureException(test.definition.hash) }
  }

  def record[T](test: Test[T]): Unit = {
    val run = config.testOnly.map(_.exists(test.definition.hash startsWith _)).getOrElse(true)
    if(run) {
      val outcome: Outcome = check(test)
      synchronized { results += RunResult(test.definition, outcome, test.result._2) }
      ()
    }
  }

  case class Result(outcome: Outcome, name: String, hash: Hash, count: Count, min: Time, mean: Time, max: Time, diagnosis: List[String])
  
  implicit val showOutcome: AnsiShow[Outcome] = { outcome =>
    val color = outcome.success.map(if(_) green else red).getOrElse(yellow)
    s"${base00("[")}${color(outcome.string)}${base00("]")}$reset"
  }
  
  implicit val showHash: AnsiShow[Hash] = hash => s"${cyan(hash.value)}"
  implicit val showCount: AnsiShow[Count] = count => if(count.value == 1) "" else count.value.toString
  implicit val showTime: AnsiShow[Time] = time => if(time.value < 0) "" else CliRunner.formatTime(time.value)

  val tabulation = Tabulation[Result](
    Heading("RESULT", _.outcome),
    Heading("HASH", _.hash),
    Heading("TEST", _.name),
    Heading("COUNT", _.count),
    Heading("MIN", _.min),
    Heading("MEAN", _.mean),
    Heading("MAX", _.max)
  )

  def report(): Nothing = {
    val testResults = results.foldLeft(ListMap[Test.Definition[_], List[CliRunner.RunResult]]()) {
      case (results, next) =>
        results.updated(next.definition, next :: results.get(next.definition).getOrElse(Nil))
    }
    

    val rows: Seq[Result] = testResults.map {
      case (test, many) => 
        val durations = many.map(_.duration)
        val meanDuration: Time = Time(durations.sum/many.size.toDouble)
        
        // FIXME: Better check for consistency
        val outcome = many.map(_.outcome).groupBy(identity).to[List] match {
          case (o, xs) :: Nil => o
          case _ => Mixed
        }

        Result(
          outcome,
          test.name,
          Hash(test.hash),
          Count(durations.length),
          if(durations.length == 1) Time(-1) else Time(durations.min.toDouble),
          meanDuration,
          if(durations.length == 1) Time(-1) else Time(durations.max.toDouble),
          outcome.diagnosis)
    }.to[Seq]

    val List(skip, fail, pass) = List(None, Some(false), Some(true)).map { r =>
      rows.count(_.outcome.success == r)
    }
    
    val total = pass + fail + skip
    val percent = (100*pass/total.toDouble + 0.5).toInt

    if(!config.quiet) {
      tabulation.tabulate(config.columns, rows).foreach(println)
      println()
      println(bold(s"Passed: $pass/$total   Failed: $fail/$total   Skipped: $skip/$total"))
      println(bold(s"$percent% of the tests passed"))
      println()
    }
    
    sys.exit(if(fail == 0) 0 else 1)
  }
}
