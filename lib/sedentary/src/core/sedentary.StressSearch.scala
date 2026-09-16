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

import scala.collection.mutable as scm

object StressSearch:
  // At most this many windows are spent probing around the peak, and as many again looking
  // for the knee; halving a factor-of-two bracket down to the 6% tolerance takes four.
  val MaxProbes: Int = 12

  // A count whose throughput is within this fraction of the best is as good as the best: on
  // a curve which flattens out, the search reports the smallest such count, the knee.
  val Plateau: Double = 0.05

  // How many times the search starts again from its re-measurements, when they show that the
  // peak it found was a lucky window.
  val Restarts: Int = 2

  enum Phase:
    case Ascent, Boundary, Peak, Knee, Confirmation, StepDown, Done

// Chooses the worker count for each window of a `Stress` measurement, from the outcome of the
// windows before it. It runs inside the measurement JVM, called from the staged loop, and is
// kept out of the quote so that the search can be tested without staging anything.
//
// Every search begins with an ascent, doubling the worker count from `start`:
//
//  - Without an SLO, the ascent is the whole of a plain sweep. It stops at `cap`, or at a
//    window which ran out of memory or spent more than half its time collecting garbage.
//  - With an SLO, the ascent stops at the first window that misses the compliance target,
//    and the boundary between the largest compliant and the smallest non-compliant count is
//    then binary-searched to about 12%. The largest compliant count is confirmed over an
//    extended window, stepping down about 12% at a time, up to three times, if it fails.
//
// With `refine`, the search looks for the optimum count: the smallest one whose throughput
// is within `Plateau` (5%) of the highest. On a curve with a sharp peak, that is the peak; on
// one which flattens out, it is the knee, where adding workers stops paying. After the ascent
// (and, with an SLO, the boundary search), it works in two stages, each probing halfway
// across a gap until the gap is within about 6%:
//
//  1. The peak: between the fastest feasible count and its nearest measured neighbours.
//  2. The knee: between the smallest count within 5% of the fastest and the measured count
//     just below it.
//
// A window which failed, or missed the SLO, bounds the probing but is never a candidate. The
// knee, the count just below it and the peak are then re-measured over extended windows, which
// guards against a lucky or unlucky window, and the smallest of them within 5% of the fastest
// re-measurement wins. If the peak's re-measurement falls more than 5% short of what it first
// measured, the peak was a lucky window, and it steered the knee search: the re-measurements
// replace the first measurements and the search starts again from the peak, up to `Restarts`
// times. If none of the candidates is feasible, the search falls back to stepping down from
// the knee.
//
// The winning window, if there is one, is reported as `winner`: an ordinal counting every
// window recorded, from zero.
final class StressSearch(start: Int, cap: Int, slo: Boolean, refine: Boolean):
  import StressSearch.{MaxProbes, Phase, Plateau, Restarts}

  private var phase: Phase = Phase.Ascent
  private var count: Int = start
  private var low: Int = 0
  private var high: Int = 0
  private var stepDowns: Int = 0
  private var probes: Int = 0
  private var windows: Int = 0
  private var winner0: Int = -1
  private var knee: Int = 0
  private var peakCount: Int = 0
  private var peakThroughput: Double = 0.0
  private var restarts: Int = 0

  // Throughput of every feasible window measured before the confirmation, by worker count;
  // every worker count measured at all, feasible or not, which bound the probing; the counts
  // still to confirm; and each confirmation's ordinal, count, throughput and feasibility.
  private val feasible: scm.HashMap[Int, Double] = scm.HashMap()
  private val measured: scm.TreeSet[Int] = scm.TreeSet()
  private val confirmations: scm.Queue[Int] = scm.Queue()
  private val confirmed: scm.ArrayBuffer[(Int, Int, Double, Boolean)] = scm.ArrayBuffer()

  def done: Boolean = phase == Phase.Done

  // The worker count the next window should use.
  def current: Int = count

  // Whether the next window is a confirmation, which runs three times longer.
  def extended: Boolean = phase == Phase.Confirmation || phase == Phase.StepDown

  def winner: Int = winner0

  // Records the outcome of the window just run at `current` workers: its throughput in
  // operations per nanosecond, whether it was feasible (no failure, no memory exhaustion,
  // no thrashing, and within the SLO if there is one), and whether it ran out of memory or
  // thrashed, which ends a plain sweep's ascent.
  def record(throughput: Double, ok: Boolean, exhausted: Boolean): Unit =
    val window = windows
    windows += 1

    if refine && !extended then
      measured += count
      if ok then feasible(count) = throughput

    phase match
      case Phase.Ascent =>
        if !slo then
          if exhausted || count >= cap then finishAscent() else double()
        else if ok then
          low = count
          if count >= cap then finishBoundary() else double()
        else if low == 0 then
          phase = Phase.Done
        else
          high = count
          phase = Phase.Boundary
          count = low + (high - low)/2

      case Phase.Boundary =>
        if ok then low = count else high = count

        if high - low <= (if low > 8 then low/8 else 1) then finishBoundary()
        else count = low + (high - low)/2

      case Phase.Peak =>
        peak()

      case Phase.Knee =>
        descend()

      case Phase.Confirmation =>
        confirmed += ((window, count, throughput, ok))
        if !confirmations.isEmpty then count = confirmations.dequeue() else choose()

      case Phase.StepDown =>
        if ok then
          winner0 = window
          phase = Phase.Done
        else
          stepDowns += 1
          count = count - (if count > 8 then count/8 else 1)
          if stepDowns >= 3 || count < start then phase = Phase.Done

      case Phase.Done =>
        ()

  private def double(): Unit = count = if count >= cap/2 then cap else count*2

  private def tolerance(count: Int): Int = if count > 16 then count/16 else 1

  private def fastest: (Int, Double) = feasible.maxBy(_(1))

  // The least throughput counted as being on the plateau below `best`, which it always meets.
  private def near(best: Double): Double = best - Math.abs(best)*Plateau

  // A plain sweep's ascent is over.
  private def finishAscent(): Unit =
    if refine && !feasible.isEmpty then
      phase = Phase.Peak
      peak()
    else
      phase = Phase.Done

  // An SLO search has found the largest compliant count, `low`.
  private def finishBoundary(): Unit =
    if refine then
      phase = Phase.Peak
      peak()
    else
      count = low
      phase = Phase.StepDown

  // Probes around the fastest feasible count until its measured neighbours are close.
  private def peak(): Unit =
    val (best, _) = fastest
    val below = measured.maxBefore(best).fold(0)(best - _)
    val above = measured.minAfter(best + 1).fold(0)(_ - best)
    val close = tolerance(best)

    if probes >= MaxProbes || below <= close && above <= close then
      phase = Phase.Knee
      probes = 0
      descend()
    else
      probes += 1
      count = if above >= below then best + above/2 else best - below/2

  // Probes below the smallest count within `Plateau` of the fastest, until the measured count
  // beneath it is close.
  private def descend(): Unit =
    val (best, throughput) = fastest
    knee = feasible.filter(_(1) >= near(throughput)).keySet.min
    val below = measured.maxBefore(knee).fold(0)(knee - _)

    if probes >= MaxProbes || below <= tolerance(knee) then
      peakCount = best
      peakThroughput = throughput
      confirm(best)
    else
      probes += 1
      count = knee - below/2

  // Re-measures the knee, the feasible count just below it, and the peak.
  private def confirm(best: Int): Unit =
    val counts = feasible.keySet.to(scm.TreeSet)
    confirmations += knee
    counts.maxBefore(knee).foreach(confirmations += _)

    if best != knee then confirmations += best
    else counts.minAfter(knee + 1).foreach(confirmations += _)

    phase = Phase.Confirmation
    count = confirmations.dequeue()

  // Chooses the smallest confirmed count within `Plateau` of the fastest confirmation, unless
  // the confirmations show the peak to have been a lucky window.
  private def choose(): Unit =
    val passed = confirmed.filter(_(3))
    val peaks = passed.filter(_(1) == peakCount)
    val reproduced = !peaks.isEmpty && peaks.map(_(2)).max >= near(peakThroughput)

    if !reproduced && restarts < Restarts then
      restarts += 1

      confirmed.foreach: (_, count, throughput, ok) =>
        if ok then feasible(count) = throughput else feasible -= count

      confirmed.clear()
      probes = 0

      if feasible.isEmpty then stepDown(knee)
      else
        phase = Phase.Peak
        peak()
    else if passed.isEmpty then
      stepDown(knee)
    else
      val threshold = near(passed.map(_(2)).max)
      val (window, _, _, _) = passed.filter(_(2) >= threshold).minBy(_(1))
      winner0 = window
      phase = Phase.Done

  private def stepDown(from: Int): Unit =
    phase = Phase.StepDown
    stepDowns = 0
    count = from - (if from > 8 then from/8 else 1)
    if count < start then phase = Phase.Done
