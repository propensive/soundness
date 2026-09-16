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
  // At most this many windows are spent probing around the peak; halving a factor-of-two
  // bracket down to the 6% tolerance takes four on each side.
  val MaxProbes: Int = 12

  enum Phase:
    case Ascent, Boundary, Peak, Confirmation, StepDown, Done

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
// With `refine`, the search looks for the count with the highest throughput instead of the
// largest one. After the ascent (and, with an SLO, the boundary search), it repeatedly
// probes halfway between the best feasible count so far and its nearest measured neighbour
// on either side, until both neighbours lie within about 6% of it; a window which failed,
// or missed the SLO, bounds the search but is never a candidate. The best count and its
// nearest feasible neighbours are then each re-measured over an extended window, and the
// fastest of those which is still feasible wins; this guards against one lucky window. If
// none of them is feasible, the search falls back to stepping down from the best count.
//
// The winning window, if there is one, is reported as `winner`: an ordinal counting every
// window recorded, from zero.
final class StressSearch(start: Int, cap: Int, slo: Boolean, refine: Boolean):
  import StressSearch.Phase

  private var phase: Phase = Phase.Ascent
  private var count: Int = start
  private var low: Int = 0
  private var high: Int = 0
  private var stepDowns: Int = 0
  private var probes: Int = 0
  private var windows: Int = 0
  private var winner0: Int = -1
  private var best: Int = 0
  private var fastest: Double = -1.0
  private var fastestWindow: Int = -1

  // Throughput of every feasible window measured before the confirmation, by worker count;
  // and every worker count measured at all, feasible or not, which bound the probing.
  private val feasible: scm.HashMap[Int, Double] = scm.HashMap()
  private val measured: scm.TreeSet[Int] = scm.TreeSet()
  private val confirmations: scm.Queue[Int] = scm.Queue()

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

    if refine && (phase == Phase.Ascent || phase == Phase.Boundary || phase == Phase.Peak)
    then
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
        probe()

      case Phase.Confirmation =>
        if ok && throughput > fastest then
          fastest = throughput
          fastestWindow = window

        if !confirmations.isEmpty then count = confirmations.dequeue()
        else if fastestWindow >= 0 then
          winner0 = fastestWindow
          phase = Phase.Done
        else
          stepDown(best)

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

  // A plain sweep's ascent is over.
  private def finishAscent(): Unit =
    if refine && !feasible.isEmpty then
      phase = Phase.Peak
      probe()
    else
      phase = Phase.Done

  // An SLO search has found the largest compliant count, `low`.
  private def finishBoundary(): Unit =
    if refine then
      phase = Phase.Peak
      probe()
    else
      count = low
      phase = Phase.StepDown

  // Chooses the next worker count to probe around the fastest feasible count, or moves on to
  // confirmation once its neighbours are close enough.
  private def probe(): Unit =
    val (peak, _) = feasible.maxBy(_(1))
    val tolerance = if peak > 16 then peak/16 else 1
    val below = measured.maxBefore(peak).fold(0)(peak - _)
    val above = measured.minAfter(peak + 1).fold(0)(_ - peak)

    if probes >= StressSearch.MaxProbes || below <= tolerance && above <= tolerance
    then confirm(peak)
    else
      probes += 1
      count = if above >= below then peak + above/2 else peak - below/2

  // Re-measures `candidate` and its nearest feasible neighbours over extended windows.
  private def confirm(candidate: Int): Unit =
    best = candidate
    val counts = feasible.keySet.to(scm.TreeSet)
    confirmations += candidate
    counts.maxBefore(candidate).foreach(confirmations += _)
    counts.minAfter(candidate + 1).foreach(confirmations += _)
    phase = Phase.Confirmation
    count = confirmations.dequeue()

  private def stepDown(from: Int): Unit =
    phase = Phase.StepDown
    stepDowns = 0
    count = from - (if from > 8 then from/8 else 1)
    if count < start then phase = Phase.Done
