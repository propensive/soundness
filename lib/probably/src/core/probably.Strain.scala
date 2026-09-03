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
package probably


import anticipation.*
import rudiments.*
import gossamer.*
import symbolism.*
import vacuous.*
import denominative.dysasymptotics.linearSize

object Strain:
  given inclusion: Inclusion[Report, Strain]:
    def include
      ( report:      Report,
        testId:      Test.Id,
        coordinates: List[(Axis.Spec, Value)],
        strain:      Strain )
    :   Report =

      val quantiles: List[(Metric, Optional[Long])] =
        List
          ( Metric.P50  -> strain.p50,
            Metric.P90  -> strain.p90,
            Metric.P99  -> strain.p99,
            Metric.P999 -> strain.p999 )

      val latencies: List[(Metric, Double)] = quantiles.flatMap: pair =>
        // The `Optional` is bound to a typed local as the lambda's first statement: reading it
        // directly inside a combinator lambda trips the compiler's `wildApprox` assertion.
        val value: Optional[Long] = pair(1)

        value.lay(List[(Metric, Double)]()): nanos => List(pair(0) -> nanos.toDouble)

      val slo: List[(Metric, Double)] =
        strain.compliance.option.map(Metric.Compliance -> _).to(List)

      val counters: List[(Metric, Double)] =
        List
          ( Metric.Operations -> strain.operations.toDouble,
            Metric.Throughput -> strain.throughput.toDouble,
            Metric.Allocation -> strain.allocationRate,
            Metric.PeakHeap   -> strain.peakHeap.toDouble,
            Metric.Retained   -> strain.retained.toDouble,
            Metric.GcCount    -> strain.gcCount.toDouble,
            Metric.GcTime     -> strain.gcTime.toDouble*1000000.0 )

      val metrics = (counters + latencies + slo).to[Ledger]

      // Concurrency is a coordinate, not a metric: every strain lands on the emergent `N`
      // axis, so a sweep's steps accumulate as cells of one entry. If the producer already
      // supplied an `N` coordinate, it is respected.
      val coordinates2 =
        if coordinates.exists(_(0).label == t"N") then coordinates else
          val axis = Axis.Spec(t"N", Axis.Domain.Integral, emergent = true)
          coordinates :+ (axis -> Value.Integral(strain.concurrency))

      report.emit:
        TestEvent.StrainRecorded
          ( TestEvent.Ref.of(testId),
            TestEvent.Coordinate.of(coordinates2),
            strain.concurrency,
            strain.operations,
            strain.nanoseconds,
            strain.allocation,
            strain.peakHeap,
            strain.retained,
            strain.gcCount,
            strain.gcTime,
            strain.p50,
            strain.p90,
            strain.p99,
            strain.p999,
            strain.compliance,
            strain.sustained,
            java.lang.System.currentTimeMillis )

      report.record
        ( testId,
          Entry.Kind.Stress,
          coordinates2,
          Run(metrics = metrics, sustained = strain.sustained),
          Metric.Throughput )

// The measured response to a stress test — the memory/scaling counterpart of `Benchmark`.
// `concurrency` workers ran a body repeatedly
// for a fixed wall-clock window of `nanoseconds`, completing `operations` operations in total.
// `allocation` is the total heap allocation over the window; `peakHeap` the high-water mark of
// the heap pools; `retained` the live set remaining after a post-run GC (bounded-memory designs
// show a flat, small value here); `gcCount`/`gcTime` are the collector deltas over the window
// (time in milliseconds). The optional `p50`/`p90`/`p99`/`p999` fields are per-operation
// latency percentiles in nanoseconds, taken from a histogram accumulated across all workers.
// In a capacity search, `compliance` is the measured fraction of operations completing
// within the latency threshold, and `sustained` marks the winning row: the highest
// concurrency whose (extended) window still met the compliance target.
case class Strain
  ( concurrency: Int,
    operations:  Long,
    nanoseconds: Long,
    allocation:  Long,
    peakHeap:    Long,
    retained:    Long,
    gcCount:     Long,
    gcTime:      Long,
    p50:         Optional[Long]   = Unset,
    p90:         Optional[Long]   = Unset,
    p99:         Optional[Long]   = Unset,
    p999:        Optional[Long]   = Unset,
    compliance:  Optional[Double] = Unset,
    sustained:   Boolean          = false ):

  def throughput: Long = if nanoseconds == 0L then 0L else (operations*1e9/nanoseconds).toLong

  def allocationRate: Double =
    if operations == 0L then 0.0 else allocation.toDouble/operations
