/*
    Probably, version [unreleased]. Copyright 2025 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package probably

import vacuous.*

object Benchmark:
  given Inclusion[Report, Benchmark] with
    def include(report: Report, testId: TestId, benchmark: Benchmark): Report =
      report.addBenchmark(testId, benchmark)
  type Percentiles = 80 | 85 | 90 | 95 | 96 | 97 | 98 | 99

case class Benchmark
   (total:     Long,
    count:     Int,
    min:     Double,
    mean:      Double,
    max:     Double,
    sd:      Double,
    confidence: Benchmark.Percentiles,
    baseline:   Optional[Baseline]):

  def zScore(percentile: Benchmark.Percentiles): Double = percentile match
    case 80 => 0.842
    case 85 => 1.036
    case 90 => 1.282
    case 95 => 1.645
    case 96 => 1.751
    case 97 => 1.881
    case 98 => 2.054
    case 99 => 2.326

  def confidenceInterval: Long = (zScore(confidence)*sd/math.sqrt(count.toDouble)).toLong
  def throughput: Long = (1000000000.0/mean).toLong
