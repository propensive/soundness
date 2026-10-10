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

import scala.math
import scala.reflect

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
import parasite.*
import prepositional.*
import probably.*
import rudiments.*
import serpentine.*
import superlunary.*
import symbolism.*
import vacuous.*

import systems.javaBaseSystem
import threads.platformThreads
import workingDirectories.javaBaseWorkingDirectory
import denominative.*
import denominative.dysasymptotics.linearSize

object Bench:
  // The staged measurement harness, shared by every cell of every plan: warmup, doubling
  // calibration, median-rate count selection, then `iterations` timed batches.
  // A declared target duration under the run's multiplier, floored at a microsecond so that
  // an absurdly small factor still leaves something measurable rather than a zero-length
  // batch (which would divide by zero in `measured`).
  private[sedentary] def scaled(target: Long, scale: Double): Long =
    if scale == 1.0 then target else ((target*scale).toLong).max(1000L)

  // The expected measuring time of one cell, from its declared metadata: `warmups` then
  // `iterations` batches, each of `target/iterations`. The calibration phase (doubling until
  // one batch fits) and the ten untimed warmup runs are not counted — they are body-dependent
  // and usually small. An estimate for budgeting, not a promise.
  private[sedentary] def expected(target: Long, iterations: Int, warmups: Int): Long =
    target*(iterations + warmups).toLong/iterations.max(1).toLong

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
        //
        // Deliberately NOT `Atomic`, and deliberately fully qualified: this sits inside a macro
        // quote, so the spelling must need no import at the expansion site — and sedentary is
        // the harness every benchmark expands through. The escape behaviour described above is
        // load-bearing, and an `inline` wrapper is a new variable in that argument. The
        // measuring instrument does not move with the thing measured.
        val sink = new java.util.concurrent.atomic.AtomicReference[Any](null)

        // The body, bound once as a method of its own rather than spliced into each loop below.
        // This method runs once, so its loops can only be JIT-compiled by on-stack replacement;
        // a body spliced into them is compiled the same way, and runs tens of times slower than
        // the same code called as a method, which compiles normally once it is hot.
        def operation(): Any = $body0

        var count: Long = 1L
        var d: Long = 0L

        // Run 10 times initially as untimed warmup
        var w = 0

        while w < 10 do
          sink.lazySet(operation())
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
          while i < count do { sink.lazySet(operation()); i += 1L }
          d = jl.System.nanoTime - t0

        var rate: Double = d.toDouble/count
        count = math.max(1L, (${Expr(batch)}/rate).toLong)

        // `count`, then `iterations` batch timings, then the bytes allocated across the timed
        // batches (see below).
        val result = new scala.Array[Long](${Expr(iterations)} + 2)

        // Warmup / calibration: run `warmups` full-count batches, adjusting
        // count run-by-run so it converges on the batch target, then pick the
        // final count from the median of all observed rates so a single
        // GC-affected run can't bias the measurement count.
        val rates = new scala.Array[Double](${Expr(warmups)})
        var c = 0

        while c < ${Expr(warmups)} do
          val t0 = jl.System.nanoTime
          var j = 0L
          while j < count do { sink.lazySet(operation()); j += 1L }
          val t1 = jl.System.nanoTime - t0
          rates(c) = t1.toDouble/count
          count = math.max(1L, (${Expr(batch)}/rates(c)).toLong)
          c += 1

        java.util.Arrays.sort(rates)
        count = math.max(1L, (${Expr(batch)}/rates(rates.length/2)).toLong)

        result(0) = count

        // Allocation over the timed batches, from `getTotalThreadAllocatedBytes` (JDK 21+, as
        // `Stress` uses it): it accumulates over terminated threads too, and a virtual thread's
        // allocation is attributed to its carrier, so a body which forks its own workers or
        // fibers is still fully accounted. The harness's own contribution is the boxing of each
        // body result into the sink (a primitive result costs one box per operation); it is
        // deliberately not subtracted, so the figure is a measurement and not an estimate.
        val threadMx =
          java.lang.management.ManagementFactory.getThreadMXBean.nn
          . asInstanceOf[com.sun.management.ThreadMXBean]

        jl.System.gc()
        val allocated0 = threadMx.getTotalThreadAllocatedBytes

        var m = 1
        while m <= ${Expr(iterations)} do
          // Trigger a young-gen collection between runs so a GC pause is less
          // likely to land inside a measurement window. SerialGC honours this
          // hint promptly.
          jl.System.gc()
          val t0 = jl.System.nanoTime
          var j = 0L
          while j < count do { sink.lazySet(operation()); j += 1L }
          val t1 = jl.System.nanoTime - t0
          result(m) = t1
          m += 1

        result(${Expr(iterations)} + 1) = threadMx.getTotalThreadAllocatedBytes - allocated0

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

    // The sample size, then `runs` timings, then the bytes allocated.
    val sample: Long = results0.prim.or(0L)
    val results = results0.stdlib.drop(1).take(runs)
    val allocated: Long = results0.last.or(0L)
    val total = results.sum
    val count = sample*runs
    // Bytes per operation, rounded to the nearest byte; a body allocating nothing reports 0.
    val allocation: Long = math.round(allocated.toDouble/count.toDouble)
    val sampleMean0 = results.map(_.toDouble/sample).mean
    val sampleMean = sampleMean0.or(0.0)
    val sum = results.map(_.toDouble/sample - sampleMean).bi.map(_*_).sum
    val variance = sum/(runs - 1)
    val sd = math.sqrt(variance)
    val min = results.min.toDouble/sample
    val max = results.max.toDouble/sample

    val operationSizeText: Optional[Text] = operationSize.let(_.sizeText)
    val operationQuantity: Optional[Double] = operationSize.let(_.value)

    val operationRateText: Optional[Text] = operationSize.let: os =>
      os.rateText((total.toDouble/count)/1e9)

    Benchmark
      ( total, count, runs, total.toDouble/count, min, max, sd, confidence,
        operationSizeText, operationRateText, operationQuantity, allocation )

  object Plan:
    // A plan whose single-axis cells are each sized by `size`.
    case class Uniaxial[value](plan: Plan, size: value => Optional[OperationSize]):
      import plan.*

      // See `Plan#over`: the measurement of every defined axis value.
      inline def over[report, topic <: Label](axis: Axis[value])
        ( inline body: (References over Json) ?=> Quotes ?=> (value ~> Expr[Any]) )
        ( using System, TemporaryDirectory, Stageable over Json in Text )
        ( using runner:    Runner[report],
                inclusion: Inclusion[report, Benchmark],
                anchors:   Inclusion[report, Anchor],
                @missingContext(Testable.orphan)
                suite:     Testable of topic,
                codepoint: Codepoint )
      :   Unit raises Compiler.Error raises Rig.Error =

        val testId = Test.Id(name, suite, codepoint, Unset, tags)
        val target2: Long = Bench.scaled(target, runner.scale)
        val values = axis.values

        // Definedness may not depend on the staging context, so gaps are probed under a
        // throwaway quotes context and a discarded References instance; the partial function
        // is never applied there, only queried.
        val probe: value ~> Expr[Any] =
          given staging.Compiler = bench.compiler2
          staging.withQuotes(body(using References[Json]()))

        // An iterator rather than `each`: a lambda cannot capture the inline `body`.
        val iterator = List.iterator(values)

        while iterator.hasNext do
          val value = iterator.next()

          val coordinates = List(axis.coordinate(value))

          // An unselected cell is skipped BEFORE staging: it costs no compilation and no JVM.
          if probe.isDefinedAt(value) &&
            !runner.skip(testId, Entry.Kind.Bench, coordinates,
                            Bench.expected(target2, iterations, warmups))
          then
            val results0 =
              bench.dispatch(Bench.measured(iterations, warmups, target2)(body(value)))

            inclusion.include
              ( runner.report,
                testId,
                coordinates,
                Bench.statistics(results0, iterations, confidence, size(value).or(operationSize)) )

        anchor.let: anchorValue =>
          values.seek(_ == anchorValue).let: value =>
            anchors.include
              ( runner.report, testId, Nil, Anchor(axis.spec, axis.point(value), comparison) )

    // A plan whose crosstab cells are each sized by `size`.
    case class Biaxial[left, right](plan: Plan, size: (left, right) => Optional[OperationSize]):
      import plan.*

      // See `Plan#over`: the measurement of every defined combination of the two axes.
      inline def over[report, topic <: Label](first: Axis[left], second: Axis[right])
        ( inline body: (References over Json) ?=> Quotes ?=> (((left, right)) ~> Expr[Any]) )
        ( using System, TemporaryDirectory, Stageable over Json in Text )
        ( using runner:    Runner[report],
                inclusion: Inclusion[report, Benchmark],
                anchors:   Inclusion[report, Anchor],
                @missingContext(Testable.orphan)
                suite:     Testable of topic,
                codepoint: Codepoint )
      :   Unit raises Compiler.Error raises Rig.Error =

        val testId = Test.Id(name, suite, codepoint, Unset, tags)
        val target2: Long = Bench.scaled(target, runner.scale)
        val lefts = first.values
        val rights = second.values

        // See the definedness note in the uniaxial `over`.
        val probe: ((left, right)) ~> Expr[Any] =
          given staging.Compiler = bench.compiler2
          staging.withQuotes(body(using References[Json]()))

        // Iterators rather than `each`; see the uniaxial `over`.
        val leftIterator = List.iterator(lefts)

        while leftIterator.hasNext do
          val left = leftIterator.next()
          val rightIterator = List.iterator(rights)

          while rightIterator.hasNext do
            val right = rightIterator.next()

            val coordinates = List(first.coordinate(left), second.coordinate(right))

            if probe.isDefinedAt((left, right)) &&
              !runner.skip(testId, Entry.Kind.Bench, coordinates,
                              Bench.expected(target2, iterations, warmups))
            then
              val results0 =
                bench.dispatch(Bench.measured(iterations, warmups, target2)(body((left, right))))

              inclusion.include
                ( runner.report,
                  testId,
                  coordinates,
                  Bench.statistics
                    ( results0, iterations, confidence, size(left, right).or(operationSize) ) )

        anchor.let: anchorValue =>
          lefts.seek(_ == anchorValue).lay:
            rights.seek(_ == anchorValue).let: value =>
              anchors.include
                ( runner.report,
                  testId,
                  Nil,
                  Anchor(second.spec, second.point(value), comparison) )

          . apply: value =>
            anchors.include
              ( runner.report, testId, Nil, Anchor(first.spec, first.point(value), comparison) )

  case class Plan
    ( bench:         Bench,
      name:          Message,
      tags:          List[Tag],
      target:        Long,
      operationSize: Optional[OperationSize],
      iterations:    Int,
      warmups:       Int,
      confidence:    Benchmark.Percentiles,
      anchor:        Optional[Any],
      comparison:    Baseline ):

    // A single measurement: the plan applied directly to a quoted body.
    inline def apply[report, topic <: Label]
      ( body0: (References over Json) ?=> Quotes ?=> Expr[Any] )
      ( using System, TemporaryDirectory, Stageable over Json in Text )
      ( using runner:    Runner[report],
              inclusion: Inclusion[report, Benchmark],
              @missingContext(Testable.orphan)
              suite:     Testable of topic,
              codepoint: Codepoint )
    :   Unit raises Compiler.Error raises Rig.Error =

      val testId = Test.Id(name, suite, codepoint, Unset, tags)
      val target2: Long = Bench.scaled(target, runner.scale)

      val expected: Optional[Long] = Bench.expected(target2, iterations, warmups)

      if !runner.skip(testId, Entry.Kind.Bench, Nil, expected) then
        val results0 = bench.dispatch(Bench.measured(iterations, warmups, target2)(body0))

        inclusion.include
          ( runner.report,
            testId,
            Bench.statistics(results0, iterations, confidence, operationSize) )

    // The size of each cell as a function of its coordinates, where it varies: a parser
    // measured over documents of different lengths reports each document's size, so that the
    // cells' rates are comparable where their times are not. A cell for which the function
    // answers `Unset` falls back to the plan's `operationSize`.
    def sized[value](size: value => Optional[OperationSize]): Plan.Uniaxial[value] =
      Plan.Uniaxial(this, size)

    def sized[left, right](size: (left, right) => Optional[OperationSize])
    :   Plan.Biaxial[left, right] =

      Plan.Biaxial(this, size)

    // One measurement per defined axis value, each a fresh dispatch: cells whose staged
    // trees coincide share one compilation (values carried by `References`), while each
    // distinct implementation compiles once. A partial body leaves gaps. The cells share the
    // plan's `operationSize`; `sized` gives each its own.
    inline def over[value, report, topic <: Label](axis: Axis[value])
      ( inline body: (References over Json) ?=> Quotes ?=> (value ~> Expr[Any]) )
      ( using System, TemporaryDirectory, Stageable over Json in Text )
      ( using runner:    Runner[report],
              inclusion: Inclusion[report, Benchmark],
              anchors:   Inclusion[report, Anchor],
              @missingContext(Testable.orphan)
              suite:     Testable of topic,
              codepoint: Codepoint )
    :   Unit raises Compiler.Error raises Rig.Error =

      sized((_: value) => Unset).over(axis)(body)

    inline def over[value <: reflect.Enum: Enumerable, report, topic <: Label]
      ( companion: { def values: scala.Array[value] } )
      ( inline body: (References over Json) ?=> Quotes ?=> (value ~> Expr[Any]) )
      ( using System, TemporaryDirectory, Stageable over Json in Text )
      ( using runner:    Runner[report],
              inclusion: Inclusion[report, Benchmark],
              anchors:   Inclusion[report, Anchor],
              @missingContext(Testable.orphan)
              suite:     Testable of topic,
              codepoint: Codepoint )
    :   Unit raises Compiler.Error raises Rig.Error =

      over(Axis(companion))(body)

    // One measurement per defined combination of two axes, rendered as a crosstab.
    inline def over[left, right, report, topic <: Label](first: Axis[left], second: Axis[right])
      ( inline body: (References over Json) ?=> Quotes ?=> (((left, right)) ~> Expr[Any]) )
      ( using System, TemporaryDirectory, Stageable over Json in Text )
      ( using runner:    Runner[report],
              inclusion: Inclusion[report, Benchmark],
              anchors:   Inclusion[report, Anchor],
              @missingContext(Testable.orphan)
              suite:     Testable of topic,
              codepoint: Codepoint )
    :   Unit raises Compiler.Error raises Rig.Error =

      sized((_: left, _: right) => Unset).over(first, second)(body)

    inline def over[left <: reflect.Enum: Enumerable, right, report, topic <: Label]
      ( first: { def values: scala.Array[left] }, second: Axis[right] )
      ( inline body: (References over Json) ?=> Quotes ?=> (((left, right)) ~> Expr[Any]) )
      ( using System, TemporaryDirectory, Stageable over Json in Text )
      ( using runner:    Runner[report],
              inclusion: Inclusion[report, Benchmark],
              anchors:   Inclusion[report, Anchor],
              @missingContext(Testable.orphan)
              suite:     Testable of topic,
              codepoint: Codepoint )
    :   Unit raises Compiler.Error raises Rig.Error =

      over(Axis(first), second)(body)

    inline def over[left, right <: reflect.Enum: Enumerable, report, topic <: Label]
      ( first: Axis[left], second: { def values: scala.Array[right] } )
      ( inline body: (References over Json) ?=> Quotes ?=> (((left, right)) ~> Expr[Any]) )
      ( using System, TemporaryDirectory, Stageable over Json in Text )
      ( using runner:    Runner[report],
              inclusion: Inclusion[report, Benchmark],
              anchors:   Inclusion[report, Anchor],
              @missingContext(Testable.orphan)
              suite:     Testable of topic,
              codepoint: Codepoint )
    :   Unit raises Compiler.Error raises Rig.Error =

      over(first, Axis(second))(body)

    inline def over
      [ left <: reflect.Enum: Enumerable,
        right <: reflect.Enum: Enumerable,
        report,
        topic <: Label ]
      ( first: { def values: scala.Array[left] }, second: { def values: scala.Array[right] } )
      ( inline body: (References over Json) ?=> Quotes ?=> (((left, right)) ~> Expr[Any]) )
      ( using System, TemporaryDirectory, Stageable over Json in Text )
      ( using runner:    Runner[report],
              inclusion: Inclusion[report, Benchmark],
              anchors:   Inclusion[report, Anchor],
              @missingContext(Testable.orphan)
              suite:     Testable of topic,
              codepoint: Codepoint )
    :   Unit raises Compiler.Error raises Rig.Error =

      over(Axis(first), Axis(second))(body)


  // BenchError → Bench.Error
  case class Error()(using Diagnostics)
  extends fulminate.Error(794, 0)(m"unable to run benchmarks")

// `heap`, `cpus` and `gc` size and pin the measurement JVM exactly as for `Stress` (see the
// notes on `BenchmarkDevice#invoke`): the defaults are a fixed 1 GB heap and the Serial collector,
// which suit single-threaded microbenchmarks; a body that runs a fiber runtime or many virtual
// threads should ask for `gc = t"G1"` and a heap sized to what the rival's own harness used.
case class Bench
  ( heap: Optional[Text] = Unset, cpus: Optional[Int] = Unset, gc: Optional[Text] = Unset )
  ( using Classloader, Environment )
  ( using device: BenchmarkDevice )
extends Rig:
  type Result[output] = output
  type Form = Text
  type Target = Path on Linux
  type Transport = Json

  // Captures the benchmark's name, tags and settings; the returned plan is applied to a
  // quoted body directly (a single measurement) or spread `over` one or two axes, one
  // measurement per defined combination. `baseline` names one axis value as the comparison
  // anchor. The tags label the benchmark for selection (`tag:slow`) and exclusion.
  def apply[duration: Abstractable across Durations to Long]
    ( name: Message, tags: Tag* )
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
        tags.to(List),
        target.generic,
        operationSize,
        iterations2,
        warmups0.or(iterations2),
        confidence0.or(95),
        baseline,
        comparison )


  def stage(out: Path on Linux): Path on Linux = stageOn(device, out)

  protected val scalac: Scalac[3.7, Universe.Classfile] = Scalac(List(scalacOptions.experimental))

  protected def invoke[output](stage: Stage[output, Text, Path on Linux]): output =
    stage.remote: input => unsafely(device.invoke(stage.target, input, heap, cpus, gc))

// Builds a jar of the code under measurement, with superlunary's executor as its entry point,
// and deploys it to `device`: the staging which `Bench`, `Stress` and `Profile` share.
private[sedentary] def stageOn(device: BenchmarkDevice, out: Path on Linux)(using Environment)
:   Path on Linux =

  unsafely:
    val uuid = Uuid()

    val jarfile = supervise:
      Toolchain(jarEdges()).produce
        ( Deliverable.Emission(out, Bundler.applicationClasspath),
          Universe.Classfile,
          Jar,
          out,
          List(jarOptions.name(t"$uuid.jar")),
          List(EntryPoint(fqcn"superlunary.Executor")) )

    device.deploy(jarfile, uuid)
    jarfile
