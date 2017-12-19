package probation

import scala.reflect._
import macros._

object ReportingMacros {
  def report(c: blackbox.Context)(): c.Tree = {
    import c.universe._
    compileTimeTesting.report()
    q"()"
  }

  def restart(c: blackbox.Context)(): c.Tree = {
    import c.universe._
    compileTimeTesting.report()
    q"()"
  }
}

object compileTimeTesting {
  private[this] var compileTimeRunner: Runner = new CliRunner(CliRunner.Config())
  implicit val runner = compileTimeRunner
  private[probation] def restart() = compileTimeRunner = new CliRunner(CliRunner.Config())
  private[probation] def report() = compileTimeRunner.report()
}

