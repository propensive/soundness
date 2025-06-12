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
┃    Soundness, version 0.33.0.                                                                    ┃
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

import anticipation.*
import probably.*
import vacuous.*

import scala.collection.mutable as scm

extension [test](test: Test[test])
  inline def benchmark[duration, report]
              (confidence: Optional[Benchmark.Percentiles] = Unset,
               iterations: Optional[Int]                   = Unset,
               duration:   Optional[duration]              = Unset,
               warmup:     Optional[duration]              = Unset,
               baseline:   Optional[Baseline]              = Unset)
              (using runner:           Runner[report],
                     inc:              Inclusion[report, Benchmark],
                     specificDuration: duration is SpecificDuration = durationApi.javaLong,
                     genericDuration:  duration is GenericDuration  = durationApi.javaLong)
  : Unit =

      val action = test.action

      var end =
        System.currentTimeMillis + genericDuration.milliseconds(warmup.or(SpecificDuration(10000L)))

      val times: scm.ArrayBuffer[Long] = scm.ArrayBuffer()
      times.sizeHint(4096)
      val ctx = new Harness()

      while System.currentTimeMillis < end do
        val t0 = System.nanoTime
        val result = action(ctx)
        val t1 = System.nanoTime - t0
        times += t1

      times.clear()

      end =
        System.currentTimeMillis
        + genericDuration.milliseconds(duration.or(SpecificDuration(10000L)))

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

      val benchmark =
        Benchmark
         (total, times.size, min.toDouble, mean, max.toDouble, stdDev, confidence.or(95), baseline)

      inc.include(runner.report, test.id, benchmark)
