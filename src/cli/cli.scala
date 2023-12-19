/*
    Probably, version [unreleased]. Copyright 2023 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package probably

import escapade.*
import turbulence.*
import perforate.*
import rudiments.*
import vacuous.*
import anticipation.*
import ambience.*, environments.jvm
import hieroglyph.*, textWidthCalculation.uniform

import language.adhocExtensions

abstract class Suite(suiteName: Text) extends TestSuite(suiteName):
  val suiteIo = safely(stdioSources.jvm).vouch(using Unsafe)

  // FIXME: This seems to introduce a ghost ambiguity on the previous line
  // It should be moved back to the top-level as soon as it works
  import digression.*

  given runner: Runner[TestReport] =
    given Stdio = suiteIo
    try Runner() catch case err: EnvironmentError =>
      println(StackTrace(err).display.render)
      ???

  given TestSuite = this
  
  def run(): Unit
  
  final def main(args: IArray[Text]): Unit =
    try runner.suite(this, run())
    catch
      case err: EnvironmentError => println(StackTrace(err).display.render)
      case err: Throwable => println(StackTrace(err).display.render)
    finally
      try runner.complete()
      catch case err: EnvironmentError => println(StackTrace(err).display.render)
