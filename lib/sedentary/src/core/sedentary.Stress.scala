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

import galilei.*
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
import superlunary.*
import symbolism.*
import vacuous.*


// A stress test: the memory/scaling counterpart of `Bench`. Where `Bench` times one operation
// repeated serially on the calling thread, `Stress` runs the body concurrently on `concurrency`
// platform threads for a fixed wall-clock window and measures memory behaviour over that window:
// total allocation (hence allocation per operation), peak heap, the live set retained after a
// post-run GC, and GC count/time. The results are reported as a `Strain` row in the report.
case class Stress()(using Classloader, Environment)(using device: BenchmarkDevice) extends Rig:
  type Result[output] = output
  type Form = Text
  type Target = Path on Linux
  type Transport = Json


  inline def apply[duration: Abstractable across Durations to Long, report]
    ( name: Message )
    ( target:      duration,
      concurrency: Optional[Int]      = Unset,
      baseline:    Optional[Baseline] = Unset )
    ( body0: (References over Transport) ?=> Quotes ?=> Expr[Any] )
    ( using System, TemporaryDirectory, Stageable over Transport in Form )
    ( using runner:    Runner[report],
            inclusion: Inclusion[report, Strain],
            suite:     Testable,
            codepoint: Codepoint )
  :   Unit raises CompilerError raises RemoteError =

    val testId = TestId(name, suite, codepoint)
    val concurrency0: Optional[Int] = concurrency
    val concurrency2: Int = concurrency0.or(1)

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

          val n: Int = ${Expr(concurrency2)}
          val ops = new Array[Long](n)
          val threads = new Array[java.lang.Thread | Null](n)
          val t0 = jl.System.nanoTime
          val deadline = t0 + $target2
          var k = 0

          while k < n do
            val slot = k

            val runnable: java.lang.Runnable = () =>
              var count = 0L

              while jl.System.nanoTime < deadline do
                sink.lazySet($body0)
                count += 1L

              ops(slot) = count

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
          // bounded-memory design shows a flat, small value here regardless of how much data
          // flowed during the window.
          sink.lazySet(null)
          jl.System.gc()
          java.lang.Thread.sleep(100L)
          val retained = memory.getHeapMemoryUsage.nn.getUsed

          var total = 0L
          k = 0

          while k < n do
            total += ops(k)
            k += 1

          if jl.System.nanoTime < 0L then jl.System.err.nn.println(sink.get)

          List(total, elapsed, allocated, peak, retained, gcCount, gcTime)
        }

    val results = dispatch(body)

    val strain =
      Strain
        ( concurrency2, results(0), results(1), results(2), results(3), results(4), results(5),
          results(6), baseline )

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
      unsafely(device.invoke(stage.target, input))
