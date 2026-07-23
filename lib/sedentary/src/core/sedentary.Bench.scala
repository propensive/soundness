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

import java.lang as jl

import galilei.*
import scala.quoted.*

import ambience.*
import anthology.*
import anticipation.*
import contingency.*
import digression.*
import distillate.*
import fulminate.*
import gossamer.*
import hellenism.*
import inimitable.*
import jacinta.*
import prepositional.*
import probably.*
import rudiments.*
import serpentine.*
import superlunary.*
import symbolism.*
import vacuous.*


case class Bench()(using Classloader, Environment)(using device: BenchmarkDevice) extends Rig:
  type Result[output] = output
  type Form = Text
  type Target = Path on Linux
  type Transport = Json


  // Captures the benchmark's name and settings; the returned plan is applied to a quoted
  // body directly (a single measurement) or spread `over` one or two axes, one measurement
  // per defined combination. `baseline` names one axis value as the comparison anchor.
  def apply[duration: Abstractable across Durations to Long]
    ( name: Message )
    ( target:        duration,
      operationSize: Optional[OperationSize]         = Unset,
      iterations:    Optional[Int]                   = Unset,
      warmups:       Optional[Int]                   = Unset,
      confidence:    Optional[Benchmark.Percentiles] = Unset,
      baseline:      Optional[Any]                   = Unset,
      comparison:    Baseline                        = Baseline() )
  :   Bench.Plan =

    val iterations0: Optional[Int] = iterations
    val iterations2: Int = iterations0.or(5)
    val warmups0: Optional[Int] = warmups
    val confidence0: Optional[Benchmark.Percentiles] = confidence

    Bench.Plan
      ( this,
        name,
        target.generic,
        operationSize,
        iterations2,
        warmups0.or(iterations2),
        confidence0.or(95),
        baseline,
        comparison )


  def stage(out: Path on Linux): Path on Linux = unsafely:
    val uuid = Uuid()
    val compilation = Compilation[Universe.Classfile](out, Bundler.applicationClasspath)

    val jarfile =
      Linker[Artifact.Jar]
        ( List(jarOptions.name(t"$uuid.jar")),
          List(Linker.EntryPoint(fqcn"superlunary.Executor")) )

      . link(compilation, out)

    device.deploy(jarfile, uuid)
    jarfile

  protected val scalac: Scalac[3.7, Universe.Classfile] = Scalac(List(scalacOptions.experimental))

  protected def invoke[output](stage: Stage[output, Text, Path on Linux]): output =
    stage.remote: input =>
      unsafely(device.invoke(stage.target, input))

object Bench:
  // The staged measurement harness, shared by every cell of every plan: warmup, doubling
  // calibration, median-rate count selection, then `iterations` timed batches.
  private[sedentary] def measured
    ( iterations: Int, warmups: Int, target: Long )
    ( body0: (References over Json) ?=> Quotes ?=> Expr[Any] )
  :   (References over Json) ?=> Quotes ?=> Expr[List[Long]] =

    val batch: Long = target/iterations

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
        while d < ${Expr(batch)} do
          if count >= (1L << 34) then
            throw new RuntimeException(
              "sedentary: benchmark body produced no measurable timing after 2^34 " +
                "iterations; suspected dead-code elimination")

          count *= 2L
          val t0 = jl.System.nanoTime
          var i = 0L
          while i < count do { sink.lazySet($body0); i += 1L }
          d = jl.System.nanoTime - t0

        var rate: Double = d.toDouble/count
        count = math.max(1L, (${Expr(batch)}/rate).toLong)
        val result = new Array[Long](${Expr(iterations)} + 1)

        // Warmup / calibration: run `warmups` full-count batches, adjusting
        // count run-by-run so it converges on the batch target, then pick the
        // final count from the median of all observed rates so a single
        // GC-affected run can't bias the measurement count.
        val rates = new Array[Double](${Expr(warmups)})
        var c = 0

        while c < ${Expr(warmups)} do
          val t0 = jl.System.nanoTime
          var j = 0L
          while j < count do { sink.lazySet($body0); j += 1L }
          val t1 = jl.System.nanoTime - t0
          rates(c) = t1.toDouble/count
          count = math.max(1L, (${Expr(batch)}/rates(c)).toLong)
          c += 1

        java.util.Arrays.sort(rates)
        count = math.max(1L, (${Expr(batch)}/rates(rates.length/2)).toLong)

        result(0) = count

        var m = 1
        while m <= ${Expr(iterations)} do
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

        result.iterator.to(List)
      }

  // Statistics over one cell's measurement results, packaged as a `Benchmark`.
  private[sedentary] def statistics
    ( results0:      List[Long],
      runs:          Int,
      confidence:    Benchmark.Percentiles,
      operationSize: Optional[OperationSize] )
  :   Benchmark =

    val sample: Long = results0(0)
    val results = results0.drop(1)
    val total = results.sum
    val count = sample*runs
    val sampleMean0 = results.map(_.toDouble/sample).mean
    val sampleMean = sampleMean0.or(0.0)
    val sum = results.map(_.toDouble/sample - sampleMean).bi.map(_*_).sum
    val variance = sum/(runs - 1)
    val sd = math.sqrt(variance)
    val min = results.min.toDouble/sample
    val max = results.max.toDouble/sample

    val operationSizeText: Optional[Text] = operationSize.let(_.sizeText)

    val operationRateText: Optional[Text] = operationSize.let: os =>
      os.rateText((total.toDouble/count)/1e9)

    Benchmark
      ( total, count, runs, total.toDouble/count, min, max, sd, confidence,
        operationSizeText, operationRateText )

  case class Plan
    ( bench:         Bench,
      name:          Message,
      target:        Long,
      operationSize: Optional[OperationSize],
      iterations:    Int,
      warmups:       Int,
      confidence:    Benchmark.Percentiles,
      anchor:        Optional[Any],
      comparison:    Baseline ):

    // A single measurement: the plan applied directly to a quoted body.
    inline def apply[report]
      ( body0: (References over Json) ?=> Quotes ?=> Expr[Any] )
      ( using System, TemporaryDirectory, Stageable over Json in Text )
      ( using runner:    Runner[report],
              inclusion: Inclusion[report, Benchmark],
              suite:     Testable,
              codepoint: Codepoint )
    :   Unit raises CompilerError raises RemoteError =

      val testId = TestId(name, suite, codepoint)

      if !runner.skip(testId, Entry.Kind.Bench, Nil) then
        val results0 = bench.dispatch(Bench.measured(iterations, warmups, target)(body0))

        inclusion.include
          ( runner.report,
            testId,
            Bench.statistics(results0, iterations, confidence, operationSize) )

    // One measurement per defined axis value, each a fresh dispatch: cells whose staged
    // trees coincide share one compilation (values carried by `References`), while each
    // distinct implementation compiles once. A partial body leaves gaps.
    inline def over[value, report](axis: Axis[value])
      ( inline body: (References over Json) ?=> Quotes ?=> (value ~> Expr[Any]) )
      ( using System, TemporaryDirectory, Stageable over Json in Text )
      ( using runner:    Runner[report],
              inclusion: Inclusion[report, Benchmark],
              anchors:   Inclusion[report, Anchor],
              suite:     Testable,
              codepoint: Codepoint )
    :   Unit raises CompilerError raises RemoteError =

      val testId = TestId(name, suite, codepoint)
      val values = axis.values

      // Definedness may not depend on the staging context, so gaps are probed under a
      // throwaway quotes context and a discarded References instance; the partial function
      // is never applied there, only queried.
      val probe: value ~> Expr[Any] =
        given staging.Compiler = bench.compiler2
        staging.withQuotes(body(using References[Json]()))

      var index = 0

      while index < values.length do
        val value = values(index)

        val coordinates = List(axis.coordinate(value))

        // An unselected cell is skipped BEFORE staging: it costs no compilation and no JVM.
        if probe.isDefinedAt(value) && !runner.skip(testId, Entry.Kind.Bench, coordinates) then
          val results0 =
            bench.dispatch(Bench.measured(iterations, warmups, target)(body(value)))

          inclusion.include
            ( runner.report,
              testId,
              coordinates,
              Bench.statistics(results0, iterations, confidence, operationSize) )

        index += 1

      anchor.let: anchorValue =>
        values.find(_ == anchorValue).foreach: value =>
          anchors.include
            ( runner.report, testId, Nil, Anchor(axis.spec, axis.point(value), comparison) )

    inline def over[value <: reflect.Enum: Enumerable, report]
      ( companion: { def values: Array[value] } )
      ( inline body: (References over Json) ?=> Quotes ?=> (value ~> Expr[Any]) )
      ( using System, TemporaryDirectory, Stageable over Json in Text )
      ( using runner:    Runner[report],
              inclusion: Inclusion[report, Benchmark],
              anchors:   Inclusion[report, Anchor],
              suite:     Testable,
              codepoint: Codepoint )
    :   Unit raises CompilerError raises RemoteError =

      over(Axis(companion))(body)

    // One measurement per defined combination of two axes, rendered as a crosstab.
    inline def over[left, right, report](first: Axis[left], second: Axis[right])
      ( inline body: (References over Json) ?=> Quotes ?=> (((left, right)) ~> Expr[Any]) )
      ( using System, TemporaryDirectory, Stageable over Json in Text )
      ( using runner:    Runner[report],
              inclusion: Inclusion[report, Benchmark],
              anchors:   Inclusion[report, Anchor],
              suite:     Testable,
              codepoint: Codepoint )
    :   Unit raises CompilerError raises RemoteError =

      val testId = TestId(name, suite, codepoint)
      val lefts = first.values
      val rights = second.values

      // See the definedness note in the uniaxial `over`.
      val probe: ((left, right)) ~> Expr[Any] =
        given staging.Compiler = bench.compiler2
        staging.withQuotes(body(using References[Json]()))

      var leftIndex = 0

      while leftIndex < lefts.length do
        val left = lefts(leftIndex)
        var rightIndex = 0

        while rightIndex < rights.length do
          val right = rights(rightIndex)

          val coordinates = List(first.coordinate(left), second.coordinate(right))

          if probe.isDefinedAt((left, right))
             && !runner.skip(testId, Entry.Kind.Bench, coordinates)
          then
            val results0 =
              bench.dispatch(Bench.measured(iterations, warmups, target)(body((left, right))))

            inclusion.include
              ( runner.report,
                testId,
                coordinates,
                Bench.statistics(results0, iterations, confidence, operationSize) )

          rightIndex += 1

        leftIndex += 1

      anchor.let: anchorValue =>
        lefts.find(_ == anchorValue) match
          case Some(value) =>
            anchors.include
              ( runner.report, testId, Nil, Anchor(first.spec, first.point(value), comparison) )

          case None =>
            rights.find(_ == anchorValue).foreach: value =>
              anchors.include
                ( runner.report,
                  testId,
                  Nil,
                  Anchor(second.spec, second.point(value), comparison) )

    inline def over[left <: reflect.Enum: Enumerable, right, report]
      ( first: { def values: Array[left] }, second: Axis[right] )
      ( inline body: (References over Json) ?=> Quotes ?=> (((left, right)) ~> Expr[Any]) )
      ( using System, TemporaryDirectory, Stageable over Json in Text )
      ( using runner:    Runner[report],
              inclusion: Inclusion[report, Benchmark],
              anchors:   Inclusion[report, Anchor],
              suite:     Testable,
              codepoint: Codepoint )
    :   Unit raises CompilerError raises RemoteError =

      over(Axis(first), second)(body)

    inline def over[left, right <: reflect.Enum: Enumerable, report]
      ( first: Axis[left], second: { def values: Array[right] } )
      ( inline body: (References over Json) ?=> Quotes ?=> (((left, right)) ~> Expr[Any]) )
      ( using System, TemporaryDirectory, Stageable over Json in Text )
      ( using runner:    Runner[report],
              inclusion: Inclusion[report, Benchmark],
              anchors:   Inclusion[report, Anchor],
              suite:     Testable,
              codepoint: Codepoint )
    :   Unit raises CompilerError raises RemoteError =

      over(first, Axis(second))(body)

    inline def over[left <: reflect.Enum: Enumerable, right <: reflect.Enum: Enumerable, report]
      ( first: { def values: Array[left] }, second: { def values: Array[right] } )
      ( inline body: (References over Json) ?=> Quotes ?=> (((left, right)) ~> Expr[Any]) )
      ( using System, TemporaryDirectory, Stageable over Json in Text )
      ( using runner:    Runner[report],
              inclusion: Inclusion[report, Benchmark],
              anchors:   Inclusion[report, Anchor],
              suite:     Testable,
              codepoint: Codepoint )
    :   Unit raises CompilerError raises RemoteError =

      over(Axis(first), Axis(second))(body)
