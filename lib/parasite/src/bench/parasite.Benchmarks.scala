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

import threading.virtualThreading
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
object Benchmarks extends Suite(m"Effect runtimes: Soundness vs cats-effect vs Kyo"):
  given decimalizer: Decimalizer     = Decimalizer(2)
  given device:      BenchmarkDevice = LocalhostDevice

  // The comparison axis; cats-effect is the baseline, since the blog reports Kyo relative to it.
  enum Library:
    case Soundness, CatsEffect, Kyo

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
    def runner(): Int = supervise(1)

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

    def promise(ops: Int): Long = supervise:
      var sum = 0L
      var i = 0
      while i < ops do
        val promise: Promise[Int] = Promise()
        promise.fulfill(i)
        sum += promise.await()
        i += 1
      sum

    // One producer task, the consumer on the calling strand, `Int`s boxed through the ring.
    def queue(ops: Int, capacity: Int): Long = supervise:
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

    def spawnJoin(ops: Int): Long = supervise:
      var sum = 0L
      var i = 0
      while i < ops do
        val value = i
        sum += async(value).await()
        i += 1
      sum

    // The same shape as the rivals' `Workers`: a shared index, an indexed output array,
    // min(size, parallelism) tasks, joined together.
    def workers(values: IArray[Int], parallelism: Int, rounds: Int): IArray[Int] = supervise:
      val index = Atomic(0)
      val output = new scala.Array[Int](values.length)
      val tasks = List.fill(parallelism.min(values.length)):
        async:
          var running = true
          while running do
            val i = index.ere(_ + 1)
            if i >= values.length then running = false else output(i) = work(values(i), rounds)
      tasks.sequence.await()
      IArray.unsafeFromArray(output)

    // One task per element; a failing element aborts with a typed error, which `safely` turns
    // into an absent result at the join, so the successes come back in order.
    def collectSuccesses(values: IArray[Int], rounds: Int): IArray[Int] = supervise:
      val tasks = List.tabulate(values.length): i =>
        val value = values(i)
        async:
          if (value & 1023) == 0 then abort(Expected.Error()) else work(value, rounds)
      val output = new scala.Array[Int](values.length)
      var count = 0
      tasks.each: task =>
        safely(task.await()).let: value =>
          output(count) = value
          count += 1
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

    // Per batch, the bounded workers above; the batch is complete before the next starts.
    def parallelChunks(batches: List[IArray[Int]], parallelism: Int, rounds: Int): Long =
      var sum = 0L
      batches.each: batch =>
        val transformed = workers(batch, parallelism, rounds)
        var i = 0
        while i < transformed.length do
          sum += transformed(i)
          i += 1
      sum

    // A producer task hands each prebuilt chunk through a `Handoff` of `capacity` chunks; the
    // consumer takes exactly `batches.size` of them and transforms each as it arrives.
    def queueChunks(batches: List[IArray[Int]], capacity: Int, rounds: Int): Long = supervise:
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
    assert(soundness == catsEffect, s"$construction: Soundness $soundness ≠ cats-effect $catsEffect")
    assert(kyo == catsEffect, s"$construction: Kyo $kyo ≠ cats-effect $catsEffect")

  private def check(): Unit =
    agree(t"runner")(Direct.runner(), Rivals.Ce.runner(), Rivals.Ky.runner())

    depths.foreach: depth =>
      agree(t"deep bind $depth")(Direct.deepBind(depth), Rivals.Ce.deepBind(depth), Rivals.Ky.deepBind(depth))
      agree(t"left bind $depth")(Direct.leftBind(depth), Rivals.Ce.leftBind(depth), Rivals.Ky.leftBind(depth))
      agree(t"map chain $depth")(Direct.mapChain(depth), Rivals.Ce.mapChain(depth), Rivals.Ky.mapChain(depth))

    agree(t"ref")(Direct.ref(ops), Rivals.Ce.ref(ops), Rivals.Ky.ref(ops))
    agree(t"promise")(Direct.promise(ops), Rivals.Ce.deferred(ops), Rivals.Ky.promise(ops))
    agree(t"queue")(Direct.queue(ops, capacity), Rivals.Ce.queue(ops, capacity), Rivals.Ky.queue(ops, capacity))
    agree(t"permit")(Direct.permit(ops), Rivals.Ce.semaphore(ops), Rivals.Ky.semaphore(ops))
    agree(t"spawn/join")(Direct.spawnJoin(ops), Rivals.Ce.spawnJoin(ops), Rivals.Ky.spawnJoin(ops))

    rounds.foreach: work =>
      agree(t"workers $work")
        ( Direct.workers(valuesArray, parallelism, work).toVector,
          Rivals.Ce.workers(values, parallelism, work),
          Rivals.Ky.workers(values, parallelism, work) )

      agree(t"collect successes $work")
        ( Direct.collectSuccesses(valuesArray, work).toVector,
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
    check()

    val bench = Bench(heap = t"2g", gc = t"G1")
    val depthAxis: Axis[Int] = Axis(t"depth")(1000, 10000)
    val workAxis: Axis[Int] = Axis(t"work")(0, 64)

    suite(m"Runner baseline"):
      bench(m"Runner overhead")(target = 1*Second, baseline = Library.CatsEffect).over(Library):
        case Library.Soundness  => '{ parasite.Benchmarks.Direct.runner() }
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
      bench(m"CAS reference updates")(target = 1*Second, baseline = Library.CatsEffect).over(Library):
        case Library.Soundness  => '{ parasite.Benchmarks.Direct.ref(parasite.Benchmarks.ops) }
        case Library.CatsEffect => '{ parasite.Rivals.Ce.ref(parasite.Benchmarks.ops) }
        case Library.Kyo        => '{ parasite.Rivals.Ky.ref(parasite.Benchmarks.ops) }

      bench(m"Complete then read promise")(target = 1*Second, baseline = Library.CatsEffect)
      . over(Library):
          case Library.Soundness  => '{ parasite.Benchmarks.Direct.promise(parasite.Benchmarks.ops) }
          case Library.CatsEffect => '{ parasite.Rivals.Ce.deferred(parasite.Benchmarks.ops) }
          case Library.Kyo        => '{ parasite.Rivals.Ky.promise(parasite.Benchmarks.ops) }

      bench(m"Queue: one producer, one consumer")(target = 1*Second, baseline = Library.CatsEffect)
      . over(Library):
          case Library.Soundness =>
            '{ parasite.Benchmarks.Direct.queue(parasite.Benchmarks.ops, parasite.Benchmarks.capacity) }
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
          case Library.Soundness  => '{ parasite.Benchmarks.Direct.spawnJoin(parasite.Benchmarks.ops) }
          case Library.CatsEffect => '{ parasite.Rivals.Ce.spawnJoin(parasite.Benchmarks.ops) }
          case Library.Kyo        => '{ parasite.Rivals.Ky.spawnJoin(parasite.Benchmarks.ops) }

    suite(m"Parallel constructions (size = 4096, parallelism = 8)"):
      bench(m"Bounded workers")(target = 1*Second, baseline = Library.CatsEffect)
      . over(Library, workAxis):
          case (Library.Soundness, work) =>
            '{
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

      bench(m"Parallel attempt and collect successes")(target = 1*Second, baseline = Library.CatsEffect)
      . over(Library, workAxis):
          case (Library.Soundness, work) =>
            '{
                parasite.Benchmarks.Direct.collectSuccesses
                  ( parasite.Benchmarks.valuesArray, parasite.Benchmarks.rounds(${workAt(work)}) )
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
