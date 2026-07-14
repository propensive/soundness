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
┃    Soundness, version 0.63.0.                                                                    ┃
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
import galilei.*
import gossamer.*
import hellenism.*
import inimitable.*
import jacinta.*
import nomenclature.*
import prepositional.*
import probably.*
import rudiments.*
import serpentine.*
import superlunary.*
import vacuous.*


// A stress test: the memory/scaling counterpart of `Bench`. Where `Bench` times one operation
// repeated serially on the calling thread, `Stress` runs the body concurrently on `concurrency`
// platform threads for a fixed wall-clock window and measures memory behaviour over that
// window: total allocation (hence allocation per operation), peak heap, the live set retained
// after a post-run GC, GC count/time, and per-operation latency percentiles. Each measurement
// is reported as a `Strain` row.
//
// Setting `sweep` turns the measurement into a scaling sweep: the window is run repeatedly with
// the worker count doubling from `concurrency` (default 1) up to `sweep`, emitting one `Strain`
// row per step, so the report shows throughput/latency/memory as curves against N. The sweep
// stops early if a step ends in `OutOfMemoryError` or spends more than half its window in GC —
// so in a constrained heap (the `heap` constructor parameter, e.g. `Stress(heap = t"128m")`)
// the last emitted row is the maximum sustainable concurrency for that heap.
case class Stress(heap: Optional[Text] = Unset)(using Classloader, Environment)
  ( using device: BenchmarkDevice )
extends Rig:
  type Result[output] = output
  type Form = Text
  type Target = Path on Linux
  type Transport = Json


  inline def apply[duration: Abstractable across Durations to Long, report]
    ( name: Message )
    ( target:      duration,
      concurrency: Optional[Int]      = Unset,
      sweep:       Optional[Int]      = Unset,
      baseline:    Optional[Baseline] = Unset )
    ( body0: (References over Transport) ?=> Quotes ?=> Expr[Any] )
    ( using System, TemporaryDirectory, Stageable over Transport in Form )
    ( using runner:    Runner[report],
            inclusion: Inclusion[report, Strain],
            suite:     Testable,
            codepoint: Codepoint )
  :   Unit raises CompilerError raises RemoteError =

    val concurrency0: Optional[Int] = concurrency
    val start: Int = concurrency0.or(1)
    val sweep0: Optional[Int] = sweep
    val limit: Int = sweep0.or(start)
    val sweeping: Boolean = sweep0.present

    val steps: List[Int] =
      val builder = List.newBuilder[Int]
      var n = start

      while n < limit do
        builder += n
        n *= 2

      builder += limit
      builder.result()

    val body: (References over Transport) ?=> Quotes ?=> Expr[List[Long]] =
      val target2: Expr[Long] = Expr(target.generic)
      ' {
          // Blackhole sink, exactly as in `Bench`: each body result is written here via
          // lazySet so that the JIT cannot prove the body's value is unused and elide it. The
          // never-true read at the end forces the AtomicReference to escape, preventing
          // escape-analysis from scalarising the writes away.
          val sink = new java.util.concurrent.atomic.AtomicReference[Any](null)

          // Run 10 times initially as untimed, single-threaded warmup
          var w = 0

          while w < 10 do
            sink.lazySet($body0)
            w += 1

          val pools = java.lang.management.ManagementFactory.getMemoryPoolMXBeans.nn
          val gcs = java.lang.management.ManagementFactory.getGarbageCollectorMXBeans.nn
          val memory = java.lang.management.ManagementFactory.getMemoryMXBean.nn

          // `getTotalThreadAllocatedBytes` (JDK 21+) accumulates over terminated threads too,
          // and virtual-thread allocation is attributed to its carrier platform thread, so
          // bodies which spawn their own workers are still accounted for.
          val threadMx =
            java.lang.management.ManagementFactory.getThreadMXBean.nn
            . asInstanceOf[com.sun.management.ThreadMXBean]

          val results = scala.collection.mutable.ListBuffer[Long]()
          val stepsIterator = ${Expr(steps)}.iterator
          var stop = false

          while stepsIterator.hasNext && !stop do
            val n: Int = stepsIterator.next()

            // Preallocate every measurement structure before the allocation snapshot, so none
            // of it pollutes the allocation-per-operation figure.
            val ops = new Array[Long](n)
            val histograms = new Array[Array[Long] | Null](n)
            val threads = new Array[java.lang.Thread | Null](n)
            val oom = new java.util.concurrent.atomic.AtomicBoolean(false)
            var k = 0

            while k < n do
              histograms(k) = new Array[Long](1024)
              k += 1

            var i = 0

            while i < pools.size do
              val pool = pools.get(i).nn
              if pool.getType == java.lang.management.MemoryType.HEAP then pool.resetPeakUsage()
              i += 1

            var gcCount = 0L
            var gcTime = 0L
            i = 0

            while i < gcs.size do
              val gc = gcs.get(i).nn
              gcCount -= gc.getCollectionCount
              gcTime -= gc.getCollectionTime
              i += 1

            val allocated0 = threadMx.getTotalThreadAllocatedBytes
            val t0 = jl.System.nanoTime
            val deadline = t0 + $target2
            k = 0

            while k < n do
              val slot = k

              val runnable: java.lang.Runnable = () =>
                try
                  val histogram = histograms(slot).nn
                  var count = 0L
                  var before = jl.System.nanoTime

                  while before < deadline do
                    sink.lazySet($body0)
                    val after = jl.System.nanoTime

                    // Log-linear bucketing with 16 sub-buckets per power of two, in the style
                    // of HDR histograms: ~±3% quantisation in 8 KiB per worker.
                    val time = if after > before then after - before else 1L
                    val bits = 63 - java.lang.Long.numberOfLeadingZeros(time)

                    val index =
                      if bits < 4 then time.toInt
                      else (bits - 4)*16 + ((time >> (bits - 4)) & 15L).toInt + 16

                    histogram(if index > 1023 then 1023 else index) += 1L
                    count += 1L
                    before = after

                  ops(slot) = count

                catch case error: java.lang.OutOfMemoryError => oom.set(true)

              val thread = new java.lang.Thread(runnable)
              threads(k) = thread
              thread.start()
              k += 1

            k = 0

            while k < n do
              threads(k).nn.join()
              k += 1

            val elapsed = jl.System.nanoTime - t0
            val allocated = threadMx.getTotalThreadAllocatedBytes - allocated0

            var peak = 0L
            i = 0

            while i < pools.size do
              val pool = pools.get(i).nn

              if pool.getType == java.lang.management.MemoryType.HEAP
              then peak += pool.getPeakUsage.nn.getUsed

              i += 1

            i = 0

            while i < gcs.size do
              val gc = gcs.get(i).nn
              gcCount += gc.getCollectionCount
              gcTime += gc.getCollectionTime
              i += 1

            // Retained live set: collect, let the collector settle, then read the heap. A
            // bounded-memory design shows a flat, small value here regardless of how much
            // data flowed during the window. The sink is cleared first so the last body
            // result doesn't count as retained.
            sink.lazySet(null)
            jl.System.gc()
            java.lang.Thread.sleep(100L)
            val retained = memory.getHeapMemoryUsage.nn.getUsed

            var total = 0L
            k = 0

            while k < n do
              total += ops(k)
              k += 1

            // Merge the worker histograms and extract the latency percentiles.
            val merged = new Array[Long](1024)
            k = 0

            while k < n do
              val histogram = histograms(k).nn
              i = 0

              while i < 1024 do
                merged(i) += histogram(i)
                i += 1

              k += 1

            def percentile(fraction: Double): Long =
              if total == 0L then 0L else
                var remaining = (total*fraction).toLong + 1L
                var index = 0

                while index < 1024 && remaining > merged(index) do
                  remaining -= merged(index)
                  index += 1

                if index >= 1024 then index = 1023

                if index < 16 then index.toLong
                else (16L + (index - 16)%16) << ((index - 16)/16)

            // A step which ran out of memory is not reported, and ends the sweep; a step
            // which spent more than half its window collecting garbage is reported, but
            // larger worker counts aren't sustainable in this heap, so the sweep ends there.
            if oom.get then stop = true else
              results += n.toLong
              results += total
              results += elapsed
              results += allocated
              results += peak
              results += retained
              results += gcCount
              results += gcTime
              results += percentile(0.5)
              results += percentile(0.9)
              results += percentile(0.99)
              results += percentile(0.999)

              if gcTime*2000000L > elapsed then stop = true

          if jl.System.nanoTime < 0L then jl.System.err.nn.println(sink.get)

          results.toList
        }

    dispatch(body).grouped(12).to(List).each: step =>
      val n = step(0).toInt

      val testId =
        if sweeping then TestId(m"$name (N = $n)", suite, codepoint)
        else TestId(name, suite, codepoint)

      val strain =
        Strain
          ( n, step(1), step(2), step(3), step(4), step(5), step(6), step(7), baseline,
            step(8), step(9), step(10), step(11) )

      inclusion.include(runner.report, testId, strain)


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
      unsafely(device.invoke(stage.target, input, heap))
// x
