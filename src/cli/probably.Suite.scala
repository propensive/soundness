/*
    Probably, version [unreleased]. Copyright 2025 Jon Pretty, Propensive OÜ.

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

import ambience.*, environments.virtualMachine
import anticipation.*
import contingency.*
import digression.*
import escapade.*
import hieroglyph.*, textMetrics.uniform
import turbulence.*
import vacuous.*

import language.adhocExtensions

abstract class Suite(suiteName: Text) extends Testable(suiteName):
  val suiteIo = safely(stdioSources.virtualMachine.ansi).vouch(using Unsafe)

  given runner: Runner[Report] =
    given Stdio = suiteIo
    try Runner() catch case err: EnvironmentError =>
      println(StackTrace(err).teletype.render)
      ???

  given Testable = this

  def run(): Unit

  final def main(args: IArray[Text]): Unit =
    try runner.suite(this, run())
    catch case err: Throwable => runner.terminate(err)
    finally try runner.complete() catch case err: EnvironmentError =>
      println(StackTrace(err).teletype.render)
