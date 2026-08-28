                                                                                                  /*
┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓
┃                                                                                                  ┃
┃                                                   ╭───╮                                          ┃
┃                                                   │   │                                          ┃
┃                                                   │   │                                          ┃
┃   ╭───────╮╭─────────╮╭───╮ ╭───╮╭───╮╌────╮╭────╌┤   │╭───╮╌────╮╭────────╮╭───────╮╭───────╮   ┃
┃   │   ╭───╯│   ╭─╮   ││   │ │   ││   ╭─╮   ││   ╭─╮   ││   ╭─╮   ││   ╭─╮  ││   ╭───╯│   ╭───╯   ┃
┃   │   ╰───╮│   │ │   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   ╰─╯  ││   ╰───╮│   ╰───╮   ┃
┃   ╰───╮   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   ╭────╯╰───╮   │╰───╮   │   ┃
┃   ╭───╯   ││   ╰─╯   ││   ╰─╯   ││   │ │   ││   ╰─╯   ││   │ │   ││   ╰────╮╭───╯   │╭───╯   │   ┃
┃   ╰───────╯╰─────────╯╰────╌╰───╯╰───╯ ╰───╯╰────╌╰───╯╰───╯ ╰───╯╰────────╯╰───────╯╰───────╯   ┃
┃                                                                                                  ┃
┃    Soundness, version 0.64.0.                                                                    ┃
┃    © Copyright 2021-25 Jon Pretty, Propensive OÜ.                                                ┃
┃                                                                                                  ┃
┃    The primary distribution site is:                                                             ┃
┃                                                                                                  ┃
┃        https://soundness.dev/                                                                    ┃
┃                                                                                                  ┃
┃    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file     ┃
┃    except in compliance with the License. You may obtain a copy of the License at                ┃
┃                                                                                                  ┃
┃        https://www.apache.org/licenses/LICENSE-2.0                                               ┃
┃                                                                                                  ┃
┃    Unless required by applicable law or agreed to in writing,  software distributed under the    ┃
┃    License is distributed on an "AS IS" BASIS,  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,    ┃
┃    either express or implied. See the License for the specific language governing permissions    ┃
┃    and limitations under the License.                                                            ┃
┃                                                                                                  ┃
┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛
                                                                                                  */
package vivisection

import gossamer.*
import spectacular.*

// A debuggee for the live JDWP suite: launched under the agent, suspended at start, and driven by
// `vivisection_test`. `marker` is where the tests break; its parameters (a primitive, a string, an
// array, an opaque type) exercise local-slot recovery, and the enclosing `Specimen` gives a `this`
// whose fields exercise field recovery and an unforced lazy val. Kept close to plain idiom so its
// compiled shape is what a debugger meets in ordinary code; the one exception is the opaque `Port`
// and its `Inspectable`, which exist so a test can show static-type-driven rendering.
object Fixture:
  // An opaque type erasing to `Int`, with its own `Inspectable`: a debugger that types a binding by
  // its *runtime* class would render a `Port` as a bare `Int`, while one that recovers the static
  // type renders it through this instance.
  opaque type Port = Int

  object Port:
    def apply(number: Int): Port = number
    given (Port is Inspectable) = port => t"⟨port ${port: Int}⟩"

  def main(args: Array[String]): Unit =
    // Construct first so `Specimen` is loaded, then pause to give the debugger a window to install
    // its breakpoint before `marker` runs.
    val specimen = Specimen(7)
    Thread.sleep(1500)
    specimen.compute(35)

  class Specimen(seed: Int):
    lazy val squared: Int = seed*seed

    def compute(base: Int): Unit =
      val label = "answer"
      val numbers = Array(base, base + 1, base + 2)
      marker(base + seed, label, numbers, Port(8080))

    def marker(total: Int, tag: String, values: Array[Int], port: Port): Unit =
      val gateway: Port = Port(443)
      System.out.nn.println(gateway.toString)
