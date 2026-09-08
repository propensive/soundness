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
package parasite

import scala.annotation.tailrec
import scala.quoted.*

import ambience.*, environments.javaBaseEnvironment, systems.javaBaseSystem
import anticipation.*
import contingency.*, strategies.throwUnsafely
import denominative.*, dysasymptotics.linearSize
import fulminate.*, errorDiagnostics.emptyDiagnostics
import gossamer.*
import hellenism.*, classloaders.threadContextClassloader
import probably.*
import proscenium.*
import quantitative.*
import rudiments.*
import sedentary.*
import symbolism.*
import temporaryDirectories.systemTemporaryDirectory
import vacuous.*
import zephyrine.*

import probates.panicProbate

// The effect-runtime comparison from github.com/stasimus/scala-effect-bench (the blog post
// "Kyo vs Cats Effect: promising numbers", 2026-09), re-measured under Soundness's own harness,
// with a third row per construction: the way the same job is written in Soundness. The rival
// rows run that repository's benchmark bodies verbatim (`Rivals`); the Soundness rows are the
// idiom a user of parasite, rudiments and zephyrine would reach for, written plainly.
//
// Read the comparison with these differences in mind:
//   * Soundness has no suspension monad. The "core chain" rows (deep bind, left-associated bind,
//     map chain) are, in Soundness, a loop or a tail-recursive function: there is no `IO`/`<`
//     node per step, so those rows measure the effect wrapper's cost against direct style. (A
//     `Task.bind` chain is not the analogue — every `bind` starts a worker, which is what the
//     spawn/join row measures.)
//   * The rivals' fibers are scheduled on their own thread pools; Soundness tasks are virtual
//     threads under a `supervise` scope, so a spawn/join is a Loom mount/unmount and a scope's
//     bookkeeping, not an interpreter step.
//   * `Handoff` is the bounded single-producer/single-consumer ring behind `Conduit`; the queue
//     rows box each `Int` into it, as the generic `Queue`/`Channel` do.
//   * `Mutex` is a `ReentrantLock`, so the permit row compares a reentrant lock against the
//     rivals' non-reentrant single permit; the rivals return the free permit count, which a
//     `Mutex` does not expose, so the Soundness row returns the constant the rivals compute.
//   * The measurement JVM is fixed at a 2 GB heap on G1, as the blog's JMH forks were, and each
//     cell runs in a fresh JVM (sedentary forks one per cell, and reports five timed batches;
//     JMH ran three forks of five). Runner overhead is reported and never subtracted.
//   * Allocation per operation is `getTotalThreadAllocatedBytes` over the timed batches, which
//     includes every fiber's and worker's allocation, plus the harness's one box per result.
//
// Results, 2026-09-07, Mac16,11 (12 cores, 24 GB), JDK 25.0.2, Scala 3.9.0-p16; mean time per
// operation (one operation = the whole construction). Columns: Soundness on virtual threads,
// Soundness on the pooled supervisor (`pooledThreading`, only where a construction spawns
// tasks), cats-effect, Kyo. The blog's machine was a 16-core Mac15,9 on JDK 25.0.3, so absolute
// numbers differ; the CE:Kyo ratios (last column, this run → blog) reproduce closely, as do the
// rivals' bytes per operation (e.g. permit 1.95 MB / 329 kB against the blog's
// 1,945,464 / 329,642).
//
//   Runner overhead                      7.55 µs   8.47 µs    7.95 µs    7.53 µs   Kyo 1.06× → 1.20×
//   Deep bind, depth 1000                0.045 µs      —      19.7 µs    20.2 µs   CE 1.02× → Kyo 1.01×
//   Deep bind, depth 10000               0.043 µs      —       117 µs     137 µs   CE 1.17× → 1.14×
//   Left bind, depth 1000                0.044 µs      —      24.8 µs    3.30 ms   CE 133× → 107×
//   Left bind, depth 10000               0.044 µs      —       162 µs     342 ms   CE 2111× → 1895×
//   Map chain, depth 1000                0.043 µs      —      18.3 µs    20.6 µs   CE 1.13× → 1.06×
//   Map chain, depth 10000               0.043 µs      —       104 µs     130 µs   CE 1.25× → 1.20×
//   CAS reference updates ×1000          1.81 µs       —      22.5 µs    24.9 µs   CE 1.10× → 1.00×
//   Complete then read promise ×1000     13.4 µs   14.0 µs    65.7 µs    69.8 µs   CE 1.06× → 1.10×
//   Queue, 1 producer / 1 consumer       66.4 µs   65.4 µs     118 µs    77.2 µs   Kyo 1.53× → 1.45×
//   Uncontended permit ×1000             4.86 µs       —       335 µs    60.6 µs   Kyo 5.53× → 5.02×
//   Sequential spawn/join ×1000          2.87 ms   0.575 ms    441 µs     153 µs   Kyo 2.87× → 2.75×
//   Bounded workers, work 0              0.233 ms  0.248 ms   0.269 ms   0.201 ms  Kyo 1.34× → 1.13×
//   Bounded workers, work 64             0.306 ms  0.323 ms   0.340 ms   0.589 ms  CE 1.73× → 2.22×
//   Collect successes, work 0            0.272 ms  0.284 ms   1.02 ms    0.97 ms   Kyo 1.05× → CE 1.08×
//   Collect successes, work 64           0.325 ms  0.327 ms   0.99 ms    1.35 ms   CE 1.37× → 1.41×
//   Sequential chunks, work 0            6.73 µs       —       552 µs     722 µs   fs2 1.31× → 1.24×
//   Sequential chunks, work 64           0.626 ms      —      1.17 ms    1.70 ms   fs2 1.46× → 1.18×
//   Parallel chunks, work 0              24.2 µs   28.1 µs    1.51 ms    0.82 ms   Kyo 1.84× → 1.93×
//   Parallel chunks, work 64             0.187 ms  0.192 ms   1.87 ms    1.72 ms   Kyo 1.09× → 1.11×
//   Queue-backed chunks, work 0          20.8 µs   21.1 µs     592 µs     157 µs   Kyo 3.77× → 3.70×
//   Queue-backed chunks, work 64         0.735 ms  0.737 ms   1.27 ms    0.81 ms   Kyo 1.58× → 1.75×
//
// Every Soundness construction runs on one task under `supervise`, joined once by the caller
// (`Direct.running`), as the rivals' run inside their runtimes; that entry is the Soundness
// runner-overhead row, and is included in every other row. The rows that spawn a task per
// element or per batch in the rivals (collect successes, parallel chunks) use `concurrently`,
// a fixed set of workers over numbered jobs, on the Soundness side. A task on `virtualThreading`
// is a virtual thread, costing about 2.5 µs to start and join from another virtual thread (and
// 7 µs from a platform thread, whose park is a kernel call), against Kyo's 0.15 µs and
// cats-effect's 0.4 µs per fiber; so the one row that spawns a thousand tasks by specification
// — sequential spawn/join — is the one Soundness loses on virtual threads, and bounded fan-out
// is the idiom that wins the others. The pooled supervisor hands a task to a waiting carrier
// instead of starting a thread, which takes that row from 2.87 ms to 0.575 ms (Kyo 0.153 ms,
// cats-effect 0.441 ms) and its allocation from 1.0 MB to 0.58 MB, while leaving every other
// row within noise: the remaining gap to Kyo is that a Kyo fiber's parent and child share one
// worker thread with no hand-off at all. Allocation tells the same story elsewhere: 32 B for a
// thousand `Atomic` updates or `Mutex` sections, 17 kB for a thousand queued ints.
//
// An earlier form of these rows, spawning a task per element and joining from the harness's
// platform thread, measured 4.7 ms for collect successes and 3.5 ms for parallel chunks (four
// to five times slower than the rivals) and 6.96 ms for spawn/join; a probe attributed almost
// all of it to Loom itself — parasite adds about 0.1 µs to a bare virtual-thread start and join.
//
// Running the queue rows at ~10⁵ repetitions also exposed a real race in `Handoff`: a consumer
// observing `finish()` between its read of the tail index and its check of the finished flag
// dropped the final item. `take`/`drain` now re-read the tail after seeing `done`.
object Benchmarks extends Suite(m"Effect runtimes: Soundness vs cats-effect vs Kyo"):
  given decimalizer: Decimalizer     = Decimalizer(2)
  given device:      BenchmarkDevice = LocalhostDevice

  // The comparison axis; cats-effect is the baseline, since the blog reports Kyo relative to it.
  enum Library:
    case Soundness, Pooled, CatsEffect, Kyo

  // Every parameter is read from an array at measurement time, so no arm — direct-style code
  // in particular — can see its loop bound as a compile-time constant.
  val depths:      scala.Array[Int] = scala.Array(1000, 10000)
  val rounds:      scala.Array[Int] = scala.Array(0, 64)
  val ops:         Int = 1000
  val capacity:    Int = 64
  val size:        Int = 4096
  val parallelism: Int = 8
  val streamSize:  Int = 10000
  val chunkSize:   Int = 64
  val streamWorkers: Int = 4

  // The rivals take stdlib `Vector`s, exactly as their benchmarks declared them; the Soundness
  // arms take immutable arrays. Both are built once, here, and referenced from the staged
  // bodies by fully-qualified name.
  lazy val values: scala.Vector[Int] = scala.Vector.tabulate(size)(identity)
  lazy val batches: scala.Vector[scala.Vector[Int]] =
    scala.Vector.tabulate(streamSize)(identity).grouped(chunkSize).toVector

  lazy val valuesArray: IArray[Int] = IArray.tabulate(size)(identity)
  lazy val batchArrays: List[IArray[Int]] =
    List.tabulate(batches.size) { index => IArray.from(batches(index)) }

  // An error a task raises to stand for the rivals' `RuntimeException with NoStackTrace`;
  // `emptyDiagnostics` likewise records no stack.
  object Expected:
    case class Error()(using Diagnostics) extends fulminate.Error(m"expected failure")

  inline def work(value: Int, rounds: Int): Int = Rivals.work(value, rounds)

  // ── The Soundness arms ──────────────────────────────────────────────────────────────────

  object Direct:
    // Where a construction runs: the rivals execute theirs inside their runtime (a fiber on
    // the runtime's pool, the caller blocked once), so the Soundness constructions likewise run
    // on one task — a virtual thread, where a request handler lives — with the caller blocked
    // once. A join inside the construction is then a virtual-thread park, not a platform
    // thread's kernel park. This is also the runner-overhead row: entering a scope, starting
    // one task and joining it.
    inline def running[result](inline body: Monitor ?=> result)(using Threading): result =
      supervise(async(body).await())

    def runner()(using Threading): Int = running(1)

    def deepBind(depth: Int): Int =
      @tailrec
      def loop(i: Int): Int = if i == depth then i else loop(i + 1)
      loop(0)

    def leftBind(depth: Int): Int =
      var acc = 0
      var i = 0
      while i < depth do
        acc += 1
        i += 1
      acc

    def mapChain(depth: Int): Int =
      var acc = 0
      var i = 0
      while i < depth do
        acc += 1
        i += 1
      acc

    def ref(ops: Int): Int =
      val ref = Atomic(0)
      var i = 0
      while i < ops do
        ref.since(_ + 1)
        i += 1
      ref()

    def promise(ops: Int)(using Threading): Long = running:
      var sum = 0L
      var i = 0
      while i < ops do
        val promise: Promise[Int] = Promise()
        promise.fulfill(i)
        sum += promise.await()
        i += 1
      sum

    // One producer task, the consumer on the calling strand, `Int`s boxed through the ring.
    def queue(ops: Int, capacity: Int)(using Threading): Long = running:
      val queue = Handoff(capacity)
      val producer = async:
        var i = 0
        while i < ops do
          queue.offer(Integer.valueOf(i))
          i += 1
        queue.finish()
      var sum = 0L
      var i = 0
      while i < ops do
        sum += queue.take().asInstanceOf[Integer].intValue
        i += 1
      producer.await()
      sum

    def permit(ops: Int): Int =
      val mutex = Mutex()
      var i = 0
      while i < ops do
        mutex(())
        i += 1
      1

    def spawnJoin(ops: Int)(using Threading): Long = running:
      var sum = 0L
      var i = 0
      while i < ops do
        val value = i
        sum += async(value).await()
        i += 1
      sum

    // `concurrently` is the rivals' `Workers` construction exactly: a shared index, an indexed
    // output array, min(size, parallelism) tasks, joined together.
    def workers(values: IArray[Int], parallelism: Int, rounds: Int)(using Threading)
    :   IArray[Int] =
      running:
        concurrently(values.length, parallelism)(i => work(values(i), rounds))

      // The rivals start one fiber per element; a Soundness task is a thread, so the bounded form
      // is the idiom: eight workers over the elements, each element's failure caught where it
      // happens (`safely` on the typed `abort`), and the absent results dropped in order.
    def collectSuccesses(values: IArray[Int], parallelism: Int, rounds: Int)(using Threading)
    :   IArray[Int] =
      running:
        val results: IArray[Optional[Int]] =
          concurrently(values.length, parallelism): i =>
            val value = values(i)
            safely(if (value & 1023) == 0 then abort(Expected.Error()) else work(value, rounds))

        val output = new scala.Array[Int](values.length)
        var count = 0
        var i = 0

        while i < results.length do
          results(i).let: value =>
            output(count) = value
            count += 1
          i += 1

        IArray.unsafeFromArray(output.take(count))

    // Each chunk is transformed whole, then folded: a nested loop, since a batch of ints needs
    // no stream.
    def evalChunks(batches: List[IArray[Int]], rounds: Int): Long =
      var sum = 0L
      batches.each: batch =>
        val transformed = batch.map(work(_, rounds))
        var i = 0
        while i < transformed.length do
          sum += transformed(i)
          i += 1
      sum

    // The rivals start fresh workers per batch and finish a batch before the next begins. The
    // Soundness form keeps the same number of workers alive across the whole stream, each
    // transforming and folding whole batches in order of claim: the same bounded parallelism
    // and ordered result, without a spawn per batch or a barrier between batches.
    def parallelChunks(batches: List[IArray[Int]], parallelism: Int, rounds: Int)(using Threading)
    :   Long =
      running:
        val chunks: IArray[IArray[Int]] = IArray.from(batches.stdlib)

        val sums: IArray[Long] =
          concurrently(chunks.length, parallelism): index =>
            val batch = chunks(index)
            var sum = 0L
            var i = 0
            while i < batch.length do
              sum += work(batch(i), rounds)
              i += 1
            sum

        var total = 0L
        var i = 0
        while i < sums.length do
          total += sums(i)
          i += 1
        total

    // A producer task hands each prebuilt chunk through a `Handoff` of `capacity` chunks; the
    // consumer takes exactly `batches.size` of them and transforms each as it arrives.
    def queueChunks(batches: List[IArray[Int]], capacity: Int, rounds: Int)(using Threading)
    :   Long =
      running:
        val queue = Handoff(capacity)
        val producer = async:
          batches.each(batch => queue.offer(batch.asInstanceOf[AnyRef]))
          queue.finish()
        var sum = 0L
        var taken = 0
        val count = batches.size
        while taken < count do
          val batch = queue.take().asInstanceOf[IArray[Int]]
          var i = 0
          while i < batch.length do
            sum += work(batch(i), rounds)
            i += 1
          taken += 1
        producer.await()
        sum

  // ── Agreement checks and the plan ───────────────────────────────────────────────────────

  private def agree[value](construction: Text)(soundness: value, catsEffect: value, kyo: value)
  :   Unit =
    assert(soundness == catsEffect,
           s"$construction: Soundness $soundness ≠ cats-effect $catsEffect")
    assert(kyo == catsEffect, s"$construction: Kyo $kyo ≠ cats-effect $catsEffect")

  private def check()(using Threading): Unit =
    agree("runner")(Direct.runner(), Rivals.Ce.runner(), Rivals.Ky.runner())

    depths.foreach: depth =>
      agree(t"deep bind $depth")
        ( Direct.deepBind(depth), Rivals.Ce.deepBind(depth), Rivals.Ky.deepBind(depth) )
      agree(t"left bind $depth")
        ( Direct.leftBind(depth), Rivals.Ce.leftBind(depth), Rivals.Ky.leftBind(depth) )
      agree(t"map chain $depth")
        ( Direct.mapChain(depth), Rivals.Ce.mapChain(depth), Rivals.Ky.mapChain(depth) )

    agree("ref")(Direct.ref(ops), Rivals.Ce.ref(ops), Rivals.Ky.ref(ops))
    agree("promise")(Direct.promise(ops), Rivals.Ce.deferred(ops), Rivals.Ky.promise(ops))
    agree("queue")
      ( Direct.queue(ops, capacity),
        Rivals.Ce.queue(ops, capacity),
        Rivals.Ky.queue(ops, capacity) )
    agree("permit")(Direct.permit(ops), Rivals.Ce.semaphore(ops), Rivals.Ky.semaphore(ops))
    agree("spawn/join")(Direct.spawnJoin(ops), Rivals.Ce.spawnJoin(ops), Rivals.Ky.spawnJoin(ops))

    rounds.foreach: work =>
      agree(t"workers $work")
        ( Direct.workers(valuesArray, parallelism, work).toVector,
          Rivals.Ce.workers(values, parallelism, work),
          Rivals.Ky.workers(values, parallelism, work) )

      agree(t"collect successes $work")
        ( Direct.collectSuccesses(valuesArray, parallelism, work).toVector,
          Rivals.Ce.collectSuccesses(values, work),
          Rivals.Ky.collectSuccesses(values, work) )

      agree(t"eval chunks $work")
        ( Direct.evalChunks(batchArrays, work),
          Rivals.Ce.evalChunks(batches, work),
          Rivals.Ky.evalChunks(batches, work) )

      agree(t"parallel chunks $work")
        ( Direct.parallelChunks(batchArrays, streamWorkers, work),
          Rivals.Ce.parallelChunks(batches, streamWorkers, work),
          Rivals.Ky.parallelChunks(batches, streamWorkers, work) )

      agree(t"queue chunks $work")
        ( Direct.queueChunks(batchArrays, capacity, work),
          Rivals.Ce.queueChunks(batches, capacity, work),
          Rivals.Ky.queueChunks(batches, capacity, work) )

  def run(): Unit =
    check()(using threading.virtualThreading)
    check()(using threading.pooledThreading)

    val bench = Bench(heap = "2g", gc = "G1")
    val depthAxis: Axis[Int] = Axis("depth")(1000, 10000)
    val workAxis: Axis[Int] = Axis("work")(0, 64)

    suite(m"Runner baseline"):
      bench(m"Runner overhead")(target = 1*Second, baseline = Library.CatsEffect).over(Library):
        case Library.Soundness  =>
          '{
              given Threading = parasite.threading.virtualThreading
              parasite.Benchmarks.Direct.runner()
          }
        case Library.Pooled  =>
          '{
              given Threading = parasite.threading.pooledThreading
              parasite.Benchmarks.Direct.runner()
          }
        case Library.CatsEffect => '{ parasite.Rivals.Ce.runner() }
        case Library.Kyo        => '{ parasite.Rivals.Ky.runner() }

    // The staged bodies receive an INDEX into `depths`/`rounds` (spliced as a literal) and load
    // the parameter through the array at run time; see the note on the parameter arrays.
    def depthAt(depth: Int)(using Quotes): Expr[Int] = Expr(depths.indexOf(depth))
    def workAt(work: Int)(using Quotes): Expr[Int] = Expr(rounds.indexOf(work))

    suite(m"Core chains"):
      bench(m"Deep suspended bind")(target = 1*Second, baseline = Library.CatsEffect)
      . over(Library, depthAxis):
          case (Library.Soundness, depth) =>
            '{ parasite.Benchmarks.Direct.deepBind(parasite.Benchmarks.depths(${depthAt(depth)})) }
          case (Library.CatsEffect, depth) =>
            '{ parasite.Rivals.Ce.deepBind(parasite.Benchmarks.depths(${depthAt(depth)})) }
          case (Library.Kyo, depth) =>
            '{ parasite.Rivals.Ky.deepBind(parasite.Benchmarks.depths(${depthAt(depth)})) }

      bench(m"Left-associated suspended bind")(target = 1*Second, baseline = Library.CatsEffect)
      . over(Library, depthAxis):
          case (Library.Soundness, depth) =>
            '{ parasite.Benchmarks.Direct.leftBind(parasite.Benchmarks.depths(${depthAt(depth)})) }
          case (Library.CatsEffect, depth) =>
            '{ parasite.Rivals.Ce.leftBind(parasite.Benchmarks.depths(${depthAt(depth)})) }
          case (Library.Kyo, depth) =>
            '{ parasite.Rivals.Ky.leftBind(parasite.Benchmarks.depths(${depthAt(depth)})) }

      bench(m"Map chain from suspended zero")(target = 1*Second, baseline = Library.CatsEffect)
      . over(Library, depthAxis):
          case (Library.Soundness, depth) =>
            '{ parasite.Benchmarks.Direct.mapChain(parasite.Benchmarks.depths(${depthAt(depth)})) }
          case (Library.CatsEffect, depth) =>
            '{ parasite.Rivals.Ce.mapChain(parasite.Benchmarks.depths(${depthAt(depth)})) }
          case (Library.Kyo, depth) =>
            '{ parasite.Rivals.Ky.mapChain(parasite.Benchmarks.depths(${depthAt(depth)})) }

    suite(m"Primitives (ops = 1000, queue capacity 64)"):
      bench(m"CAS reference updates")(target = 1*Second, baseline = Library.CatsEffect)
      . over(Library):
        case Library.Soundness  => '{ parasite.Benchmarks.Direct.ref(parasite.Benchmarks.ops) }
        case Library.CatsEffect => '{ parasite.Rivals.Ce.ref(parasite.Benchmarks.ops) }
        case Library.Kyo        => '{ parasite.Rivals.Ky.ref(parasite.Benchmarks.ops) }

      bench(m"Complete then read promise")(target = 1*Second, baseline = Library.CatsEffect)
      . over(Library):
          case Library.Soundness  =>
            '{
                given Threading = parasite.threading.virtualThreading
                parasite.Benchmarks.Direct.promise(parasite.Benchmarks.ops)
            }
          case Library.Pooled  =>
            '{
                given Threading = parasite.threading.pooledThreading
                parasite.Benchmarks.Direct.promise(parasite.Benchmarks.ops)
            }
          case Library.CatsEffect => '{ parasite.Rivals.Ce.deferred(parasite.Benchmarks.ops) }
          case Library.Kyo        => '{ parasite.Rivals.Ky.promise(parasite.Benchmarks.ops) }

      bench(m"Queue: one producer, one consumer")(target = 1*Second, baseline = Library.CatsEffect)
      . over(Library):
          case Library.Soundness =>
            '{
                given Threading = parasite.threading.virtualThreading
                parasite.Benchmarks.Direct.queue
                  ( parasite.Benchmarks.ops, parasite.Benchmarks.capacity )
            }
          case Library.Pooled =>
            '{
                given Threading = parasite.threading.pooledThreading
                parasite.Benchmarks.Direct.queue
                  ( parasite.Benchmarks.ops, parasite.Benchmarks.capacity )
            }
          case Library.CatsEffect =>
            '{ parasite.Rivals.Ce.queue(parasite.Benchmarks.ops, parasite.Benchmarks.capacity) }
          case Library.Kyo =>
            '{ parasite.Rivals.Ky.queue(parasite.Benchmarks.ops, parasite.Benchmarks.capacity) }

      bench(m"Uncontended non-reentrant permit")(target = 1*Second, baseline = Library.CatsEffect)
      . over(Library):
          case Library.Soundness  => '{ parasite.Benchmarks.Direct.permit(parasite.Benchmarks.ops) }
          case Library.CatsEffect => '{ parasite.Rivals.Ce.semaphore(parasite.Benchmarks.ops) }
          case Library.Kyo        => '{ parasite.Rivals.Ky.semaphore(parasite.Benchmarks.ops) }

      bench(m"Sequential child spawn and join")(target = 1*Second, baseline = Library.CatsEffect)
      . over(Library):
          case Library.Soundness  =>
            '{
                given Threading = parasite.threading.virtualThreading
                parasite.Benchmarks.Direct.spawnJoin(parasite.Benchmarks.ops)
            }
          case Library.Pooled  =>
            '{
                given Threading = parasite.threading.pooledThreading
                parasite.Benchmarks.Direct.spawnJoin(parasite.Benchmarks.ops)
            }
          case Library.CatsEffect => '{ parasite.Rivals.Ce.spawnJoin(parasite.Benchmarks.ops) }
          case Library.Kyo        => '{ parasite.Rivals.Ky.spawnJoin(parasite.Benchmarks.ops) }

    suite(m"Parallel constructions (size = 4096, parallelism = 8)"):
      bench(m"Bounded workers")(target = 1*Second, baseline = Library.CatsEffect)
      . over(Library, workAxis):
          case (Library.Soundness, work) =>
            '{
                given Threading = parasite.threading.virtualThreading
                parasite.Benchmarks.Direct.workers
                  ( parasite.Benchmarks.valuesArray,
                    parasite.Benchmarks.parallelism,
                    parasite.Benchmarks.rounds(${workAt(work)}) )
            }
          case (Library.Pooled, work) =>
            '{
                given Threading = parasite.threading.pooledThreading
                parasite.Benchmarks.Direct.workers
                  ( parasite.Benchmarks.valuesArray,
                    parasite.Benchmarks.parallelism,
                    parasite.Benchmarks.rounds(${workAt(work)}) )
            }
          case (Library.CatsEffect, work) =>
            '{
                parasite.Rivals.Ce.workers
                  ( parasite.Benchmarks.values,
                    parasite.Benchmarks.parallelism,
                    parasite.Benchmarks.rounds(${workAt(work)}) )
            }
          case (Library.Kyo, work) =>
            '{
                parasite.Rivals.Ky.workers
                  ( parasite.Benchmarks.values,
                    parasite.Benchmarks.parallelism,
                    parasite.Benchmarks.rounds(${workAt(work)}) )
            }

      bench(m"Parallel attempt and collect successes")
        ( target = 1*Second, baseline = Library.CatsEffect )
      . over(Library, workAxis):
          case (Library.Soundness, work) =>
            '{
                given Threading = parasite.threading.virtualThreading
                parasite.Benchmarks.Direct.collectSuccesses
                  ( parasite.Benchmarks.valuesArray,
                    parasite.Benchmarks.parallelism,
                    parasite.Benchmarks.rounds(${workAt(work)}) )
            }
          case (Library.Pooled, work) =>
            '{
                given Threading = parasite.threading.pooledThreading
                parasite.Benchmarks.Direct.collectSuccesses
                  ( parasite.Benchmarks.valuesArray,
                    parasite.Benchmarks.parallelism,
                    parasite.Benchmarks.rounds(${workAt(work)}) )
            }
          case (Library.CatsEffect, work) =>
            '{
                parasite.Rivals.Ce.collectSuccesses
                  ( parasite.Benchmarks.values, parasite.Benchmarks.rounds(${workAt(work)}) )
            }
          case (Library.Kyo, work) =>
            '{
                parasite.Rivals.Ky.collectSuccesses
                  ( parasite.Benchmarks.values, parasite.Benchmarks.rounds(${workAt(work)}) )
            }

    suite(m"Streaming constructions (10000 ints in chunks of 64, 4 workers, 64-chunk queue)"):
      bench(m"Sequential chunk transformation")(target = 1*Second, baseline = Library.CatsEffect)
      . over(Library, workAxis):
          case (Library.Soundness, work) =>
            '{
                parasite.Benchmarks.Direct.evalChunks
                  ( parasite.Benchmarks.batchArrays, parasite.Benchmarks.rounds(${workAt(work)}) )
            }
          case (Library.CatsEffect, work) =>
            '{
                parasite.Rivals.Ce.evalChunks
                  ( parasite.Benchmarks.batches, parasite.Benchmarks.rounds(${workAt(work)}) )
            }
          case (Library.Kyo, work) =>
            '{
                parasite.Rivals.Ky.evalChunks
                  ( parasite.Benchmarks.batches, parasite.Benchmarks.rounds(${workAt(work)}) )
            }

      bench(m"Parallel chunk transformation")(target = 1*Second, baseline = Library.CatsEffect)
      . over(Library, workAxis):
          case (Library.Soundness, work) =>
            '{
                given Threading = parasite.threading.virtualThreading
                parasite.Benchmarks.Direct.parallelChunks
                  ( parasite.Benchmarks.batchArrays,
                    parasite.Benchmarks.streamWorkers,
                    parasite.Benchmarks.rounds(${workAt(work)}) )
            }
          case (Library.Pooled, work) =>
            '{
                given Threading = parasite.threading.pooledThreading
                parasite.Benchmarks.Direct.parallelChunks
                  ( parasite.Benchmarks.batchArrays,
                    parasite.Benchmarks.streamWorkers,
                    parasite.Benchmarks.rounds(${workAt(work)}) )
            }
          case (Library.CatsEffect, work) =>
            '{
                parasite.Rivals.Ce.parallelChunks
                  ( parasite.Benchmarks.batches,
                    parasite.Benchmarks.streamWorkers,
                    parasite.Benchmarks.rounds(${workAt(work)}) )
            }
          case (Library.Kyo, work) =>
            '{
                parasite.Rivals.Ky.parallelChunks
                  ( parasite.Benchmarks.batches,
                    parasite.Benchmarks.streamWorkers,
                    parasite.Benchmarks.rounds(${workAt(work)}) )
            }

      bench(m"Queue-backed chunk stream")(target = 1*Second, baseline = Library.CatsEffect)
      . over(Library, workAxis):
          case (Library.Soundness, work) =>
            '{
                given Threading = parasite.threading.virtualThreading
                parasite.Benchmarks.Direct.queueChunks
                  ( parasite.Benchmarks.batchArrays,
                    parasite.Benchmarks.capacity,
                    parasite.Benchmarks.rounds(${workAt(work)}) )
            }
          case (Library.Pooled, work) =>
            '{
                given Threading = parasite.threading.pooledThreading
                parasite.Benchmarks.Direct.queueChunks
                  ( parasite.Benchmarks.batchArrays,
                    parasite.Benchmarks.capacity,
                    parasite.Benchmarks.rounds(${workAt(work)}) )
            }
          case (Library.CatsEffect, work) =>
            '{
                parasite.Rivals.Ce.queueChunks
                  ( parasite.Benchmarks.batches,
                    parasite.Benchmarks.capacity,
                    parasite.Benchmarks.rounds(${workAt(work)}) )
            }
          case (Library.Kyo, work) =>
            '{
                parasite.Rivals.Ky.queueChunks
                  ( parasite.Benchmarks.batches,
                    parasite.Benchmarks.capacity,
                    parasite.Benchmarks.rounds(${workAt(work)}) )
            }
