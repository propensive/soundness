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
import vacuous.*

object Benchmark:
  given inclusion: Inclusion[Report, Benchmark]:
    def include
      ( report:      Report,
        testId:      TestId,
        coordinates: List[(Axis.Spec, Value)],
        benchmark:   Benchmark )
    :   Report =

      val metrics =
        ListMap
          ( Metric.Iterations -> benchmark.iterations.toDouble,
            Metric.Mean       -> benchmark.mean,
            Metric.Least      -> benchmark.min,
            Metric.Most       -> benchmark.max,
            Metric.Deviation  -> benchmark.sd,
            Metric.Percentile -> (benchmark.confidence: Int).toDouble,
            Metric.Confidence -> (if benchmark.mean == 0.0 then 0.0
                                  else benchmark.confidenceInterval/benchmark.mean),
            Metric.Throughput -> benchmark.throughput.toDouble )

      val payload: Optional[Run.Payload] =
        if benchmark.operationSize.absent && benchmark.operationRate.absent then Unset
        else Run.Payload.Sizing(benchmark.operationSize, benchmark.operationRate)

      val headline = if benchmark.operationSize.present then Metric.Throughput else Metric.Mean

      report.record
        ( testId,
          Entry.Kind.Bench,
          coordinates,
          Run(metrics = metrics, payload = payload),
          headline )

  type Percentiles = 80 | 85 | 90 | 95 | 96 | 97 | 98 | 99

case class Benchmark
  ( nanoseconds:    Long,
    iterations:     Long,
    runs:           Int,
    mean:           Double,
    min:            Double,
    max:            Double,
    sd:             Double,
    confidence:     Benchmark.Percentiles,
    baseline:       Optional[Baseline],
    operationSize:  Optional[Text] = Unset,
    operationRate:  Optional[Text] = Unset ):

  // One-sided quantiles of Student's t-distribution, used for CI half-widths
  // computed from `runs` independent measurement-run means with df = runs - 1.
  // For df ≥ 30 the t-distribution is within ~4% of the standard normal, so we
  // fall through to the normal quantiles. For an out-of-table df we round down
  // to the nearest tabulated value, giving a slightly wider (more conservative)
  // CI rather than a tighter one.
  // The `(percentile: Int)` ascriptions widen the `Percentiles` literal union (case-2
  // pure-value box under capture checking); one note here covers each occurrence.
  def tQuantile(percentile: Benchmark.Percentiles, df: Int): Double =
    if df <= 4 then (percentile: Int) match
      case 80 => 0.941
      case 85 => 1.190
      case 90 => 1.533
      case 95 => 2.132
      case 96 => 2.333
      case 97 => 2.601
      case 98 => 2.999
      case 99 => 3.747
    else if df <= 9 then (percentile: Int) match
      case 80 => 0.883
      case 85 => 1.100
      case 90 => 1.383
      case 95 => 1.833
      case 96 => 1.973
      case 97 => 2.167
      case 98 => 2.398
      case 99 => 2.821
    else if df <= 19 then (percentile: Int) match
      case 80 => 0.861
      case 85 => 1.066
      case 90 => 1.328
      case 95 => 1.729
      case 96 => 1.850
      case 97 => 2.012
      case 98 => 2.205
      case 99 => 2.539
    else if df <= 29 then (percentile: Int) match
      case 80 => 0.854
      case 85 => 1.055
      case 90 => 1.311
      case 95 => 1.699
      case 96 => 1.815
      case 97 => 1.967
      case 98 => 2.150
      case 99 => 2.462
    else (percentile: Int) match
      case 80 => 0.842
      case 85 => 1.036
      case 90 => 1.282
      case 95 => 1.645
      case 96 => 1.751
      case 97 => 1.881
      case 98 => 2.054
      case 99 => 2.326

  def confidenceInterval: Long =
    (tQuantile(confidence, runs - 1)*sd/math.sqrt(runs.toDouble)).toLong

  def throughput: Long = (1000000000.0/mean).toLong
