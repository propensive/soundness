/*
    Probably, version [unreleased]. Copyright 2023 Jon Pretty, Propensive OÜ.

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

import rudiments.*
import anticipation.*

import scala.collection.mutable as scm

extension [TestType](test: Test[TestType])
  inline def benchmark
      [DurationType, ReportType]
      (confidence: Optional[Benchmark.Percentiles] = Unset, iterations: Optional[Int] = Unset,
          duration: Optional[DurationType] = Unset, warmup: Optional[DurationType] = Unset,
          baseline: Optional[Baseline] = Unset)
      (using runner: Runner[ReportType], inc: Inclusion[ReportType, Benchmark],
          specificDuration: SpecificDuration[DurationType] = timeApi.long,
          genericDuration: GenericDuration[DurationType] = timeApi.long)
      : Unit =
    val action = test.action
    var end = System.currentTimeMillis + warmup.or(SpecificDuration(10000L)).milliseconds
    val times: scm.ArrayBuffer[Long] = scm.ArrayBuffer()
    times.sizeHint(4096)
    val ctx = new TestContext()
    
    while System.currentTimeMillis < end do
      val t0 = System.nanoTime
      val result = action(ctx)
      val t1 = System.nanoTime - t0
      times += t1
    
    times.clear()
    
    end = System.currentTimeMillis + duration.or(SpecificDuration(10000L)).milliseconds
    
    while System.currentTimeMillis < end do
      val t0 = System.nanoTime
      val result = action(ctx)
      val t1 = System.nanoTime - t0
      times += t1
    
    val count = times.size
    val total = times.sum
    val min: Long = times.min
    val mean: Double = total.toDouble/count
    val max: Long = times.max
    val variance: Double = (times.map { t => (mean - t)*(mean - t) }.sum)/count
    val stdDev: Double = math.sqrt(variance)
    
    val benchmark = Benchmark(total, times.size, min.toDouble, mean, max.toDouble, stdDev,
        confidence.or(95), baseline)
    
    inc.include(runner.report, test.id, benchmark)
