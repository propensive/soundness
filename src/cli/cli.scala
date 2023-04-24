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

import rudiments.*
import digression.*
import escapade.*
import turbulence.*

import language.adhocExtensions

abstract class Suite(name: Text)
extends TestSuite(name):
  val io = unsafely(basicIo.jvm)
  given runner: Runner[TestReport] =
    given Stdio = io
    Runner()

  given TestSuite = this
  def run(): Unit
  
  final def main(args: IArray[Text]): Unit =
    try runner.suite(this, run())
    catch case err: Throwable => println(StackTrace(err).ansi.render)
    finally runner.complete()
