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
import denominative.dysasymptotics.linearSize
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

// One window of a simulated `StressSearch`: the worker count it ran at, and whether it was an
// extended (confirmation) window.
case class Window(count: Int, extended: Boolean)

// What a simulated window measured: its throughput, whether it was feasible, and whether it
// exhausted memory.
case class Outcome(throughput: Double, ok: Boolean = true, exhausted: Boolean = false)

// Drives `search` to completion against a simulated workload, returning every window it ran
// and the winning one, if any. The cap on windows stops a broken search from looping forever.
def drive(search: StressSearch)(workload: Window => Outcome): (List[Window], Optional[Window]) =
  var windows: List[Window] = Nil
  val byOrdinal = scala.collection.mutable.ArrayBuffer[Window]()

  while !search.done && byOrdinal.length < 200 do
    val window = Window(search.current, search.extended)
    windows = windows :+ window
    byOrdinal += window
    val outcome = workload(window)
    search.record(outcome.throughput, outcome.ok, outcome.exhausted)

  (windows, if search.winner < 0 then Unset else byOrdinal(search.winner))

// A throughput curve with a single sharp peak at `peak` workers: every other count is more
// than 5% slower, so the peak is also the optimum.
def humped(peak: Int)(window: Window): Outcome =
  val distance = (window.count - peak).toDouble
  Outcome(10000.0/(1.0 + distance*distance/4.0))

// A throughput curve which rises linearly to `knee` workers and is flat beyond it, with a
// little jitter on the plateau, as a server saturating its cores would show.
def plateau(knee: Int)(window: Window): Outcome =
  Outcome(100.0*window.count.min(knee) + (if window.count > knee then window.count%7 else 0))

object Tests extends Suite(m"Sedentary Tests"):
  def run(): Unit =
    val bench = Bench()

    // `check`s, not assertions: a staged benchmark compiles through a compiler context that
    // is not thread-safe, so a runner must not defer these beside the benchmarks below.
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
    . check:
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
    . check(_ == List((probably.Entry.Kind.Stress, List(t"heavy"), List((t"N", true, 2.0, 16.0)))))

    suite(m"Choosing each window's worker count"):
      def counts(windows: List[Window]): List[Int] = windows.map(_.count)

      test(m"a plain sweep doubles up to its cap"):
        counts(drive(StressSearch(1, 64, false, false))(humped(20))(0))
      . assert(_ == List(1, 2, 4, 8, 16, 32, 64))

      test(m"a plain sweep lands exactly on a cap which is not a power of two"):
        counts(drive(StressSearch(1, 100, false, false))(humped(20))(0))
      . assert(_ == List(1, 2, 4, 8, 16, 32, 64, 100))

      test(m"a plain sweep stops at a window which exhausted memory"):
        val (windows, _) = drive(StressSearch(1, 256, false, false)): window =>
          Outcome(1.0, ok = window.count < 32, exhausted = window.count >= 32)

        counts(windows)
      . assert(_ == List(1, 2, 4, 8, 16, 32))

      test(m"a plain sweep has no winner"):
        drive(StressSearch(1, 64, false, false))(humped(20))(1)
      . assert(_ == Unset)

      // Compliant up to 50 workers: the ascent brackets the boundary between 32 and 64, and
      // the boundary search narrows it to within 12% before confirming.
      test(m"a capacity search confirms the largest compliant count"):
        val (windows, winner) = drive(StressSearch(1, 1024, true, false)): window =>
          Outcome(window.count.toDouble, ok = window.count <= 50)

        (winner.let(_.count).or(-1), winner.let(_.extended).or(false), windows.last == winner)
      . assert: (count, extended, last) =>
          extended && last && count <= 50 && count >= 50 - 50/8

      test(m"a capacity search with nothing compliant stops after one window"):
        drive(StressSearch(1, 1024, true, false))(window => Outcome(1.0, ok = false))
      . assert(_ == (List(Window(1, false)), Unset))

      test(m"a capacity search steps down when its confirmation fails"):
        val (windows, winner) = drive(StressSearch(1, 1024, true, false)): window =>
          Outcome(window.count.toDouble, ok = window.count <= 50 && !window.extended)

        (windows.filter(_.extended).size, winner)
      . assert(_ == (3, Unset))

      test(m"a refined sweep finds a peak between two powers of two"):
        drive(StressSearch(1, 256, false, true))(humped(37))(1).let(_.count).or(-1)
      . assert(count => Math.abs(count - 37) <= 3)

      test(m"a refined sweep's winner is a confirmation window"):
        drive(StressSearch(1, 256, false, true))(humped(37))(1).let(_.extended).or(false)
      . assert(_ == true)

      test(m"a refined sweep probes counts which are not powers of two"):
        counts(drive(StressSearch(1, 256, false, true))(humped(37))(0))
        . exists(count => Integer.bitCount(count) > 1)
      . assert(_ == true)

      // With throughput proportional to N there is no plateau, so the optimum is the smallest
      // count within 5% of the cap's throughput: about 244, to the search's 6% resolution.
      test(m"a refined sweep with rising throughput chooses a count near its cap"):
        drive(StressSearch(1, 256, false, true))(window => Outcome(window.count.toDouble))(1)
        . let(_.count).or(-1)
      . assert(count => count >= 243 - 16 && count <= 256)

      test(m"a refined sweep reports the knee of a plateau"):
        drive(StressSearch(1, 256, false, true))(plateau(12))(1).let(_.count).or(-1)
      . assert(_ == 12)

      test(m"a refined sweep finds a knee between two powers of two"):
        drive(StressSearch(1, 256, false, true))(plateau(100))(1).let(_.count).or(-1)
      . assert(count => count >= 95 - 6 && count <= 100)

      test(m"a plateau reaching the first window chooses one worker"):
        drive(StressSearch(1, 256, false, true))(window => Outcome(1000.0))(1)
        . let(_.count).or(-1)
      . assert(_ == 1)

      test(m"a refined capacity search reports the knee below its SLO boundary"):
        drive(StressSearch(1, 1024, true, true)): window =>
          plateau(12)(window).copy(ok = window.count <= 50)

        . apply(1).let(_.count).or(-1)
      . assert(_ == 12)

      test(m"a refined sweep with falling throughput chooses one worker"):
        drive(StressSearch(1, 256, false, true))(window => Outcome(1000.0 - window.count))(1)
        . let(_.count).or(-1)
      . assert(_ == 1)

      test(m"a refined sweep never probes beyond its cap"):
        counts(drive(StressSearch(1, 256, false, true))(window => Outcome(window.count.toDouble))(0))
        . all(_ <= 256)
      . assert(_ == true)

      test(m"a refined sweep never probes a count twice before confirming"):
        val probes = drive(StressSearch(1, 256, false, true))(humped(37))(0).filter(!_.extended)
        probes.map(_.count).to[Set].size
      . assert(_ == drive(StressSearch(1, 256, false, true))(humped(37))(0).filter(!_.extended).size)

      test(m"a refined sweep stays within its window budget"):
        drive(StressSearch(1, 256, false, true))(humped(37))(0).size
      . assert(_ <= 9 + 2*StressSearch.MaxProbes + 3)

      test(m"a refined sweep treats exhausted windows as a bound, not a candidate"):
        val (windows, winner) = drive(StressSearch(1, 256, false, true)): window =>
          if window.count >= 48 then Outcome(99999.0, ok = false, exhausted = true)
          else Outcome(window.count.toDouble)

        winner.let(_.count).or(-1)
      . assert(count => count < 48 && count >= 42)

      // Throughput peaks at 20 workers, but latency stays compliant up to 50: the best
      // compliant throughput is at 20, not at the largest compliant count.
      test(m"a refined capacity search prefers throughput to concurrency"):
        drive(StressSearch(1, 1024, true, true)): window =>
          humped(20)(window).copy(ok = window.count <= 50)

        . apply(1).let(_.count).or(-1)
      . assert(count => Math.abs(count - 20) <= 2)

      test(m"a refined capacity search never chooses a non-compliant count"):
        drive(StressSearch(1, 1024, true, true)): window =>
          Outcome(window.count.toDouble, ok = window.count <= 50)

        . apply(1).let(_.count).or(-1)
      . assert(count => count <= 50 && count >= 43)

      // One window at 64 workers reports ten times its real throughput. The search chases it,
      // but the extended re-measurement sees the truth, so the winner is chosen by what the
      // confirmation windows measured.
      test(m"a lucky window does not decide the winner"):
        val (windows, winner) = drive(StressSearch(1, 256, false, true)): window =>
          if window.count == 64 && !window.extended then Outcome(100000.0)
          else humped(37)(window)

        val confirmed = windows.filter(_.extended).map(_.count)
        val best = confirmed.stdlib.maxBy(count => humped(37)(Window(count, true)).throughput)
        (winner.let(_.count).or(-1), best)
      . assert(_ == _)

      // The same lucky window on a plateau steers the knee search towards 64; the extended
      // re-measurement of 64 exposes it, and the search starts again from the truth.
      test(m"a lucky window on a plateau does not move the knee"):
        drive(StressSearch(1, 256, false, true)): window =>
          if window.count == 64 && !window.extended then Outcome(100000.0)
          else plateau(12)(window)

        . apply(1).let(_.count).or(-1)
      . assert(_ == 12)

      test(m"a peak which reproduces does not restart the search"):
        drive(StressSearch(1, 256, false, true))(plateau(12))(0).filter(_.extended).size
      . assert(_ == 3)

      // Probes are compliant up to 50 workers, but extended windows only up to 45: none of the
      // confirmations near 50 holds, and the first ~12% step down does.
      test(m"a refined search falls back to stepping down when no confirmation holds"):
        val (windows, winner) = drive(StressSearch(1, 1024, true, true)): window =>
          val ok = if window.extended then window.count <= 45 else window.count <= 50
          Outcome(window.count.toDouble, ok = ok)

        winner.let { window => (window.count <= 45, window.count >= 40, window.extended) }
        . or((false, false, false))
      . assert(_ == (true, true, true))

      // Extended windows only hold up to 30 workers, well below where the probes put the knee:
      // the failed confirmations are discarded and the search starts again lower down.
      test(m"a refined search starts again when its confirmations all fail"):
        drive(StressSearch(1, 1024, true, true)): window =>
          val ok = if window.extended then window.count <= 30 else window.count <= 50
          Outcome(window.count.toDouble, ok = ok)

        . apply(1).let { window => (window.count <= 30, window.extended) }.or((false, false))
      . assert(_ == (true, true))

      test(m"a refined search gives up when no extended window ever holds"):
        drive(StressSearch(1, 1024, true, true)): window =>
          Outcome(window.count.toDouble, ok = !window.extended && window.count <= 50)

        . apply(1)
      . assert(_ == Unset)

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
