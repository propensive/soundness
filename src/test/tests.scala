/*

    Exoskeleton, version 2.0.0. Copyright 2017-20 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
    compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the License is
    distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and limitations under the License.

*/
package exoskeleton.tests

import exoskeleton.*
import guillotine.{Exit => _, _}

import environments.enclosing
import probably.*

object Main extends Tests:

  def simulate(shell: Shell, keys: String): String =
    sh"etc/capture ${shell.shell} ${keys}".exec[String]

  def tests() =
    println(simulate(Bash, "cmd f"))
    test("Complete") {
      val out = simulate(Bash, "cmd on")
      println(out)
      out
    }.assert(_ == "")

object Counter extends Application:
  def complete(cli: Cli): Completions =
    Completions(List(
      CompDef("one", Some("first option")),
      CompDef("two", Some("second option")),
      CompDef("three", Some("third option")),
      CompDef("four", Some("fourth option")),
      CompDef("five", Some("fifth option"))
    ))

  def main(ctx: Context): Io[Exit] = Io(Exit(0))