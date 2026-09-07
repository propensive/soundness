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

// A wildcard import brings no givens: the `n"…"` literal's plane inference needs the
// moniker and tag planes' `Nominative`s in lexical scope, by name.
import soundness.{nominative, taggingNominative}

import classloaders.threadContextClassloader
import environments.javaBaseEnvironment
import strategies.throwUnsafely
import superlunary.embeddings.automaticEmbedding
import systems.javaBaseSystem
import temporaryDirectories.systemTemporaryDirectory
import threading.platformThreading

given BenchmarkDevice = LocalhostDevice

enum Summation:
  case Loop, Formula

// A runner in LISTING mode: `skip` records rather than runs, so no measurement JVM is ever
// staged, and nothing is reported.
def listing(): Runner[Unit] =
  given reporter: Reporter[Unit] = new Reporter[Unit]:
    def report(): Unit = ()
    def fail(report: Unit, error: Throwable, active: Set[Test.Id]): Unit = ()
    def declare(report: Unit, suite: Testable): Unit = ()
    def complete(report: Unit): Unit = ()

  Runner(probably.Selection.parse(List(t"--list")))

object Tests extends Suite(m"Sedentary Tests"):
  def run(): Unit =
    val bench = Bench()

    test(m"a listing reports a biaxial benchmark's tags and axis values"):
      given runner: Runner[Unit] = listing()
      given benchmarks: Inclusion[Unit, Benchmark] = (_, _, _, _) => ()
      given anchors: Inclusion[Unit, Anchor] = (_, _, _, _) => ()

      bench(m"grid", n"slow")(target = 50*Milli(Second))
      . over(Axis(t"x")(1, 2), Axis(t"y")(10, 20)):
          case (x, y) => '{$x + $y}

      runner.listed.map: row =>
        ( row.kind,
          row.tags.map(_.text),
          row.axes.map { axis => (axis.spec.label, axis.values.map(_.text)) } )
    . assert:
        _ == List
          ( ( probably.Entry.Kind.Bench,
              List(t"slow"),
              List((t"x", List(t"1", t"2")), (t"y", List(t"10", t"20"))) ) )

    test(m"a listing reports a stress sweep's emergent axis with its bounds"):
      given runner: Runner[Unit] = listing()
      given strains: Inclusion[Unit, Strain] = (_, _, _, _) => ()

      Stress()(m"sweep", n"heavy")(target = 50*Milli(Second), concurrency = 2, sweep = 16):
        '{1 + 1}

      runner.listed.map: row =>
        ( row.kind,
          row.tags.map(_.text),
          row.axes.map { axis => (axis.spec.label, axis.spec.emergent, axis.least, axis.most) } )
    . assert(_ == List((probably.Entry.Kind.Stress, List(t"heavy"), List((t"N", true, 2.0, 16.0)))))

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

    // The schedule's budgeting estimate: warmups and iterations each cost one batch of
    // `target/iterations`, so the default (warmups == iterations) expects double the target.
    test(m"a cell's expected time counts warmup and measured batches"):
      List(Bench.expected(1_000_000L, 5, 5), Bench.expected(1_000_000L, 2, 1))
    . assert(_ == List(2_000_000L, 1_500_000L))

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
