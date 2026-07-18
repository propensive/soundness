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
import parasite.*
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
//
// Setting `threshold` and `compliance` together turns the run into a capacity search: find
// the maximum sustained throughput such that at least `compliance` percent of operations
// complete within `threshold`. The search grows the worker count from `concurrency` while
// each window meets the target (up to `sweep`, default 1024), binary-searches the boundary
// to ~12% resolution, then confirms the winner over a window three times longer; the
// confirmed row is flagged `sustained` and its throughput is the headline number. This is a
// closed-loop search — operation latency includes queuing inside the body's own pipeline —
// which honestly answers "how many concurrent pipelines, each completing promptly?".
//
// `cpus` limits the measurement JVM's processors (see `BenchmarkDevice.invoke` for the
// pinning caveat), and `heap` its memory, so the search runs under pinned resources.
//
// The contextual `Threading` (from parasite: `threading.platformThreading`,
// `threading.virtualThreading` or `threading.adaptiveThreading`) selects the workers' thread
// kind. Under virtual threading, a high-concurrency measurement multiplexes its pipelines
// over the carrier pool rather than asking the OS scheduler to juggle one thread per worker —
// the model a massively-concurrent application would use. Worker allocation is still fully
// accounted: virtual-thread allocation is attributed to its carrier, which
// `getTotalThreadAllocatedBytes` covers.
case class Stress(heap: Optional[Text] = Unset, cpus: Optional[Int] = Unset)
  ( using Classloader, Environment )
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
      threshold:   Optional[duration] = Unset,
      compliance:  Optional[Int]      = Unset,
      baseline:    Optional[Baseline] = Unset )
    ( body0: (References over Transport) ?=> Quotes ?=> Expr[Any] )
    ( using System, TemporaryDirectory, Stageable over Transport in Form )
    ( using runner:    Runner[report],
            inclusion: Inclusion[report, Strain],
            threading: Threading,
            suite:     Testable,
            codepoint: Codepoint )
  :   Unit raises CompilerError raises RemoteError =

    val concurrency0: Optional[Int] = concurrency
    val start: Int = concurrency0.or(1)
    // Whether the ambient `Threading` forks virtual threads, probed with a no-op fork so
    // adaptive and custom supervisors classify by what they actually do. The remote JVM runs
    // the same `java`, so the probe's answer holds there. A non-thread-backed strand (an
    // event-loop supervisor) classifies as not virtual.
    val virtual2: Boolean = threading.supervisor().fork(() => Unset)(()) match
      case strand: Strand.Threaded => strand.thread.isVirtual
      case _                       => false
    val threshold0: Optional[Long] = threshold.let(_.generic)
    val compliance0: Optional[Int] = compliance

    if threshold0.present != compliance0.present
    then panic(m"a capacity search needs both `threshold` and `compliance`, or neither")

    val searching: Boolean = threshold0.present
    val sweep0: Optional[Int] = sweep
    val limit: Int = sweep0.or(if searching then 1024 else start)
    val sweeping: Boolean = sweep0.present || searching

    // The threshold's histogram bucket index, computed with the same log-linear formula the
    // workers use; operations in buckets strictly below it completed within the threshold
    // (conservative to within the histogram's ~±3% quantisation). -1 disables the check.
    val thresholdIndex: Int = threshold0.lay(-1): nanos =>
      val time = if nanos < 1L then 1L else nanos
      val bits = 63 - jl.Long.numberOfLeadingZeros(time)

      val index =
        if bits < 4 then time.toInt
        else (bits - 4)*16 + ((time >> (bits - 4)) & 15L).toInt + 16

      if index > 1023 then 1023 else index

    // The compliance target in basis points, e.g. 95% -> 9500.
    val complianceBp: Long = compliance0.lay(-1L)(_*100L)

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
          val startN: Int = ${Expr(start)}
          val capN: Int = ${Expr(limit)}
          val thresholdIndex2: Int = ${Expr(thresholdIndex)}
          val targetBp: Long = ${Expr(complianceBp)}
          val slo = thresholdIndex2 >= 0

          // Phases: 0 = growth (doubling; the whole run when there is no SLO), 1 = binary
          // refinement of the compliant/non-compliant boundary, 2 = confirmation of the
          // candidate over a 3x window, 9 = done.
          var phase = 0
          var currentN = startN
          var low = 0
          var high = 0
          var confirmations = 0

          while phase != 9 do
            val n: Int = currentN
            val window: Long = if phase == 2 then $target2*3L else $target2

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
            val deadline = t0 + window
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

              val thread =
                if ${Expr(virtual2)} then java.lang.Thread.ofVirtual.nn.start(runnable).nn
                else
                  val started = new java.lang.Thread(runnable)
                  started.start()
                  started

              threads(k) = thread
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

            // Compliant fraction in basis points: operations in buckets strictly below the
            // threshold's bucket completed within the threshold. -1 when there is no SLO.
            val compliantBp: Long =
              if !slo || total == 0L then -1L else
                var below = 0L
                var b = 0

                while b < thresholdIndex2 do
                  below += merged(b)
                  b += 1

                below*10000L/total

            val thrash = gcTime*2000000L > elapsed
            val ok = !oom.get && !thrash && (!slo || compliantBp >= targetBp)
            val sustained = slo && phase == 2 && ok

            // A step which ran out of memory is not reported; every other window is,
            // including a failing confirmation — it is still a valid measurement at that N.
            if !oom.get then
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
              results += compliantBp
              results += (if sustained then 1L else 0L)

            if !slo then
              // Plain sweep: keep doubling until the cap; stop on memory exhaustion or a
              // window that spent more than half its time collecting garbage.
              if oom.get || thrash || n >= capN then phase = 9
              else currentN = if n >= capN/2 then capN else n*2
            else if phase == 0 then
              // Growth: double while compliant. The first failure brackets the boundary; a
              // failure on the very first window means nothing is sustainable.
              if ok then
                low = n

                if n >= capN then phase = 2
                else currentN = if n >= capN/2 then capN else n*2
              else if low == 0 then
                phase = 9
              else
                high = n
                phase = 1
                currentN = low + (high - low)/2
            else if phase == 1 then
              // Refinement: binary-search between the largest compliant and the smallest
              // failing worker count, to ~12% resolution.
              if ok then low = n else high = n

              if high - low <= (if low > 8 then low/8 else 1) then
                phase = 2
                currentN = low
              else
                currentN = low + (high - low)/2
            else
              // Confirmation: the candidate must also survive a window three times longer.
              // On failure, step the candidate down ~12% and try again, a few times.
              if ok then
                phase = 9
              else
                confirmations += 1
                currentN = n - (if n > 8 then n/8 else 1)
                if confirmations >= 3 || currentN < startN then phase = 9

          if jl.System.nanoTime < 0L then jl.System.err.nn.println(sink.get)

          results.toList
        }

    dispatch(body).grouped(14).to(List).each: step =>
      val n = step(0).toInt
      val compliance2: Optional[Double] = if step(12) < 0L then Unset else step(12)/10000.0
      val sustained: Boolean = step(13) == 1L

      val testId =
        if sustained then TestId(m"$name (sustained, N = $n)", suite, codepoint)
        else if sweeping then TestId(m"$name (N = $n)", suite, codepoint)
        else TestId(name, suite, codepoint)

      val strain =
        Strain
          ( n, step(1), step(2), step(3), step(4), step(5), step(6), step(7), baseline,
            step(8), step(9), step(10), step(11), compliance2, sustained )

      inclusion.include(runner.report, testId, strain)


  def stage(out: Path on Linux): Path on Linux = unsafely:
    val uuid = Uuid()
    val name = t"$uuid.jar"
    val jarfile = out.peer(name)
    Bundler.bundle(out, jarfile, fqcn"superlunary.Executor")
    device.deploy(jarfile, uuid)
    jarfile

  protected val scalac: Scalac[3.7, Backend.Jvm] = Scalac(List(scalacOptions.experimental))

  protected def invoke[output](stage: Stage[output, Text, Path on Linux]): output =
    stage.remote: input =>
      unsafely(device.invoke(stage.target, input, heap, cpus))
