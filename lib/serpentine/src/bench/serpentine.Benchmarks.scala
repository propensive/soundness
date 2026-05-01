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
┃    Soundness, version 0.54.0.                                                                    ┃
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
package serpentine

import scala.quoted.*

import ambience.*, environments.java, systems.java
import contingency.*, strategies.throwUnsafely
import fulminate.*
import hellenism.*, classloaders.threadContext
import probably.*
import quantitative.*
import sedentary.*
import symbolism.*
import temporaryDirectories.system

object Benchmarks extends Suite(m"Serpentine benchmarks"):
  given device: BenchmarkDevice = LocalhostDevice

  def run(): Unit =
    val bench = Bench()

    suite(m"Conjunctions"):
      bench(m"Find conjunction of 2-element paths")
        (target = 500*Milli(Second), baseline = Baseline(compare = Min)):
        '{
            val p1 = % / "foo" / "bar"
            val p2 = % / "foo" / "baz"
            p1.conjunction(p2)
          }

      bench(m"Find conjunction of 3-element paths")(target = 500*Milli(Second)):
        '{
            val p1 = % / "foo" / "bar" / "quux"
            val p2 = % / "foo" / "baz" / "quux"
            p1.conjunction(p2)
          }

      bench(m"Find conjunction of 4-element paths")(target = 500*Milli(Second)):
        '{
            val p1 = % / "foo" / "bar" / "quux" / "bippy"
            val p2 = % / "foo" / "baz" / "quux" / "bop"
            p1.conjunction(p2)
          }

      bench(m"Find conjunction of 5-element paths")(target = 500*Milli(Second)):
        '{
            val p1 = % / "foo" / "bar" / "quux" / "bippy" / "abc"
            val p2 = % / "foo" / "baz" / "quux" / "bop" / "def"
            p1.conjunction(p2)
          }

      bench(m"Find conjunction of 6-element paths")(target = 500*Milli(Second)):
        '{
            val p1 = % / "foo" / "bar" / "quux" / "bippy" / "abc" / "ghi"
            val p2 = % / "foo" / "baz" / "quux" / "bop" / "def" / "jkl"
            p1.conjunction(p2)
          }

      bench(m"Find conjunction of 7-element paths")(target = 500*Milli(Second)):
        '{
            val p1 = % / "foo" / "bar" / "quux" / "bippy" / "abc" / "ghi" / "mno"
            val p2 = % / "foo" / "baz" / "quux" / "bop" / "def" / "jkl" / "pqr"
            p1.conjunction(p2)
          }
