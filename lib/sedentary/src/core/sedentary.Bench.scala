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
    ( body0: (References over Transport) ?=> Quotes ?=> Expr[Unit] )
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
      val target2: Expr[Long] = Expr(target.generic/iterations2)
      ' {
          var count: Int = 1
          var d: Long = 0

          // Run 10 times initially
          for j <- 0 until 10 do $body0

          // Keep doubling the count until we get one run
          while d < $target2 do
            count *= 2
            val t0 = jl.System.nanoTime
            for i <- 0 until count do $body0
            d = jl.System.nanoTime - t0

          var rate: Double = d.toDouble/count
          count = ($target2/rate).toInt
          val result = new Array[Long](${Expr(iterations2)} + 1)

          for i <- 0 until ${Expr(5)} do
            val t0 = jl.System.nanoTime
            for j <- 0 until count do $body0
            val t1 = jl.System.nanoTime - t0
            rate = t1.toDouble/count
            count = ($target2/rate).toInt

          result(0) = count

          for i <- 1 to ${Expr(iterations2)} do
            val t0 = jl.System.nanoTime
            for j <- 0 until count do $body0
            val t1 = jl.System.nanoTime - t0
            result(i) = t1

          result.to(List)
        }

    val results0 = dispatch(body)
    val sample: Long = results0(0)
    val results = results0.drop(1)
    val total = results.sum
    val iterations0: Optional[Int] = iterations
    val count = sample*iterations0.or(5)
    val sampleMean0 = results.map(_.toDouble/sample).mean
    val sampleMean = sampleMean0.or(0.0)
    val sum = results.map(_.toDouble/sample - sampleMean).bi.map(_*_).sum
    val variance = sample*sum/(iterations0.or(5) - 1)
    val sd = math.sqrt(variance)
    val min = results.min.toDouble
    val max = results.max.toDouble

    val benchmark =
      Benchmark(total, count, total.toDouble/count, min, max, sd, confidence0.or(95), baseline)

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
