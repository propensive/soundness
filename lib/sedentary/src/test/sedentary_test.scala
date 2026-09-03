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
package sedentary

import soundness.*

import classloaders.threadContextClassloader
import environments.javaEnvironment
import strategies.throwUnsafely
import superlunary.embeddings.automatic
import systems.javaSystem
import temporaryDirectories.systemTemporaryDirectory

given BenchmarkDevice = LocalhostDevice

enum Summation:
  case Loop, Formula

object Tests extends Suite(m"Sedentary Tests"):
  def run(): Unit =
    val bench = Bench()

    // The run-length multiplier a host passes as `--scale=<factor>`, applied to a declared
    // target. Checked directly rather than through a measurement: what a scaled benchmark
    // does is take proportionally longer, which is not a thing a test can assert cheaply.
    test(m"a duration multiplier scales the declared target"):
      List(Bench.scaled(2_000_000_000L, 0.5), Bench.scaled(2_000_000_000L, 4.0))
    . assert(_ == List(1_000_000_000L, 8_000_000_000L))

    test(m"an unscaled target is left exactly as declared"):
      Bench.scaled(50_000_000L, 1.0)
    . assert(_ == 50_000_000L)

    test(m"a scaled target never falls below a microsecond"):
      Bench.scaled(1000L, 0.000001)
    . assert(_ == 1000L)

    // Two implementations on one axis: distinct staged trees, so each compiles once, and
    // the anchor produces a comparison column against `Formula`.
    bench(m"sum of the first thousand integers")
      ( target = 50*Milli(Second), iterations = 2, warmups = 1,
        baseline = Summation.Formula )

    . over(Summation):
        case Summation.Loop =>
          ' {
              var i = 1L
              var sum = 0L

              while i <= 1000L do
                sum += i
                i += 1L

              sum
            }

        case Summation.Formula =>
          '{1000L*1001L/2L}

    // One implementation over a data axis: the limit rides `References`, so both cells
    // share a single compilation and differ only in transported data; extraction is
    // memoized per slot, so the splice costs a cached read per iteration, not a decode.
    bench(m"count up to a limit")(target = 50*Milli(Second), iterations = 2, warmups = 1)
    . over(Axis(t"limit")(1000, 4000)): limit =>
        ' {
            var i = 0
            var count = 0

            while i < $limit do
              count += 1
              i += 1

            count
          }
