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
┃    Soundness, version 0.54.0.                                                                    ┃
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

import java.lang as jl

import scala.quoted.*

import ambience.*
import anthology.*
import anticipation.*
import contingency.*
import digression.*
import fulminate.*
import gossamer.*
import hellenism.*
import inimitable.*
import jacinta.*
import nomenclature.*
import prepositional.*
import probably.*
import rudiments.*
import serpentine.*
import spectacular.*
import superlunary.*
import symbolism.*
import vacuous.*


case class Bench()(using Classloader, Environment)(using device: BenchmarkDevice) extends Rig:
  type Result[output] = output
  type Form = Text
  type Target = Path on Linux
  type Transport = Json


  inline def apply[duration: Abstractable across Durations to Long, report]
    ( name: Message )
    ( target:     duration,
      iterations: Optional[Int]                   = Unset,
      warmups:    Optional[Int]                   = Unset,
      confidence: Optional[Benchmark.Percentiles] = Unset,
      baseline:   Optional[Baseline]              = Unset )
    ( body0: (References over Transport) ?=> Quotes ?=> Expr[Any] )
    ( using System, TemporaryDirectory, Stageable over Transport in Form )
    ( using runner:    Runner[report],
            inclusion: Inclusion[report, Benchmark],
            suite:     Testable,
            codepoint: Codepoint )
  :   Unit raises CompilerError raises RemoteError =

    val testId = TestId(name, suite, codepoint)
    val confidence0: Optional[Benchmark.Percentiles] = confidence

    val body: (References over Transport) ?=> Quotes ?=> Expr[List[Long]] =
      val iterations0: Optional[Int] = iterations
      val iterations2: Int = iterations0.or(5)
      val warmups0: Optional[Int] = warmups
      val warmups2: Int = warmups0.or(iterations2)
      val target2: Expr[Long] = Expr(target.generic/iterations2)
      ' {
          // Blackhole sink. Each body result is written here via lazySet so that
          // the JIT cannot prove the body's value is unused and elide it. The
          // never-true read at the end forces the AtomicReference to escape,
          // preventing escape-analysis from scalarising the writes away.
          val sink = new java.util.concurrent.atomic.AtomicReference[Any](null)

          var count: Long = 1L
          var d: Long = 0L

          // Run 10 times initially as untimed warmup
          var w = 0
          while w < 10 do
            sink.lazySet($body0)
            w += 1

          // Keep doubling the count until we get one run exceeding target
          while d < $target2 do
            if count >= (1L << 34) then
              throw new RuntimeException(
                "sedentary: benchmark body produced no measurable timing after 2^34 "
                  + "iterations; suspected dead-code elimination")
            count *= 2L
            val t0 = jl.System.nanoTime
            var i = 0L
            while i < count do { sink.lazySet($body0); i += 1L }
            d = jl.System.nanoTime - t0

          var rate: Double = d.toDouble/count
          count = math.max(1L, ($target2/rate).toLong)
          val result = new Array[Long](${Expr(iterations2)} + 1)

          // Warmup / calibration: run `warmups` full-count batches, adjusting
          // count run-by-run so it converges on `target2`, then pick the final
          // count from the median of all observed rates so a single GC-affected
          // run can't bias the measurement count.
          val rates = new Array[Double](${Expr(warmups2)})
          var c = 0
          while c < ${Expr(warmups2)} do
            val t0 = jl.System.nanoTime
            var j = 0L
            while j < count do { sink.lazySet($body0); j += 1L }
            val t1 = jl.System.nanoTime - t0
            rates(c) = t1.toDouble/count
            count = math.max(1L, ($target2/rates(c)).toLong)
            c += 1
          java.util.Arrays.sort(rates)
          count = math.max(1L, ($target2/rates(rates.length/2)).toLong)

          result(0) = count

          var m = 1
          while m <= ${Expr(iterations2)} do
            // Trigger a young-gen collection between runs so a GC pause is less
            // likely to land inside a measurement window. SerialGC honours this
            // hint promptly.
            jl.System.gc()
            val t0 = jl.System.nanoTime
            var j = 0L
            while j < count do { sink.lazySet($body0); j += 1L }
            val t1 = jl.System.nanoTime - t0
            result(m) = t1
            m += 1

          if jl.System.nanoTime < 0L then jl.System.err.nn.println(sink.get)

          result.to(List)
        }

    val results0 = dispatch(body)
    val sample: Long = results0(0)
    val results = results0.drop(1)
    val total = results.sum
    val iterations0: Optional[Int] = iterations
    val runs = iterations0.or(5)
    val count = sample*runs
    val sampleMean0 = results.map(_.toDouble/sample).mean
    val sampleMean = sampleMean0.or(0.0)
    val sum = results.map(_.toDouble/sample - sampleMean).bi.map(_*_).sum
    val variance = sum/(runs - 1)
    val sd = math.sqrt(variance)
    val min = results.min.toDouble/sample
    val max = results.max.toDouble/sample

    val benchmark =
      Benchmark
        ( total, count, runs, total.toDouble/count, min, max, sd, confidence0.or(95),
          baseline )

    inclusion.include(runner.report, testId, benchmark)


  def stage(out: Path on Linux): Path on Linux = unsafely:
    val uuid = Uuid()
    val name = t"$uuid.jar"
    val jarfile = out.peer(name)
    Bundler.bundle(out, jarfile, fqcn"superlunary.Executor")
    device.deploy(jarfile, uuid)
    jarfile

  protected val scalac: Scalac[3.7] = Scalac(List(scalacOptions.experimental))

  protected def invoke[output](stage: Stage[output, Text, Path on Linux]): output =
    stage.remote: input =>
      unsafely(device.invoke(stage.target, input))
