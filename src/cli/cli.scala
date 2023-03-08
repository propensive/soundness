package probably

import rudiments.*
import deviation.*
import escapade.*

import language.adhocExtensions

abstract class Suite[ReportType](name: Text)(using reporter: TestReporter[ReportType])
extends TestSuite(name):
  given runner: Runner[ReportType] = Runner()
  given TestSuite = this
  def run(): Unit
  
  final def main(args: IArray[Text]): Unit =
    try run()
    catch case err: Throwable =>
      println(StackTrace(err).ansi.render)
    finally runner.complete()
