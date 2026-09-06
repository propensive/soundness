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

import cats.effect.{Deferred, IO, Ref}
import cats.effect.std.{Queue, Semaphore}
import cats.effect.unsafe.implicits.global
import cats.syntax.all.*
import fs2.{Chunk as FChunk, Stream as FStream}
import kyo.*
import kyo.AllowUnsafe.embrace.danger

import java.util.concurrent.atomic.AtomicInteger

// The rival constructions, ported from github.com/stasimus/scala-effect-bench (the `matched`
// suite, at its 2026-09-05 state) with the benchmark bodies, worker helper, runners and `Work`
// arithmetic kept exactly as that repository wrote them, so that the cats-effect and Kyo rows
// here measure the same programs the blog post measured. Each construction is a method taking
// its parameters, so the staged benchmark bodies are one call each and `Benchmarks.run()` can
// check every arm agrees before anything is timed.
//
// This file is the rivals' world: stdlib `Vector`, their own `flatMap`/`map`, and a wildcard
// `kyo.*` (which takes precedence over parasite's own `Async`, `Promise` and `Task`, in this
// compilation unit only). Nothing of Soundness is used here except the package clause.
object Rivals:
  // Same non-allocating CPU work on both sides; zero rounds is one increment.
  def work(value: Int, rounds: Int): Int =
    var result = value + 1
    var i = 0
    while i < rounds do
      result = java.lang.Integer.rotateLeft(result*1664525 + 1013904223, 7)
      i += 1
    result

  private val boom = new RuntimeException("expected") with scala.util.control.NoStackTrace

  // Program construction and execution both begin on the corresponding runtime worker.
  def runCe[A](body: => IO[A]): A = IO.defer(body).unsafeRunSync()

  def runKyo[A](body: => A < (Async & Abort[Throwable]))(using Frame): A =
    Sync.Unsafe.evalOrThrow(Fiber.initUnscoped(Sync.defer(body)).flatMap(_.block(Duration.Infinity)))
    . getOrThrow

  // Benchmark-local, successful-work worker pools with identical indexing and result storage.
  // Each starts min(size, parallelism) workers sequentially and joins them sequentially.
  object Workers:
    def ce(values: Vector[Int], parallelism: Int)(f: Int => IO[Int]): IO[Vector[Int]] = IO.defer {
      require(parallelism > 0)
      val index = new AtomicInteger(0)
      val output = new Array[Int](values.size)
      def worker: IO[Unit] =
        IO(index.getAndIncrement()).flatMap { i =>
          if i >= values.size then IO.unit
          else f(values(i)).flatMap(v => IO(output(i) = v)) *> worker
        }
      Vector.fill(math.min(values.size, parallelism))(worker).traverse(_.start).flatMap { fibers =>
        fibers.traverse_(_.joinWithNever) *> IO(output.toVector)
      }
    }

    def ky(values: Vector[Int], parallelism: Int)(f: Int => Int < (Async & Abort[Throwable]))(using Frame)
        : Vector[Int] < (Async & Abort[Throwable]) = Sync.defer {
      require(parallelism > 0)
      val index = new AtomicInteger(0)
      val output = new Array[Int](values.size)
      def worker: Unit < (Async & Abort[Throwable]) =
        Sync.defer(index.getAndIncrement()).map { i =>
          if i >= values.size then ()
          else f(values(i)).map(v => Sync.defer(output(i) = v)).andThen(worker)
        }
      Kyo.foreach(0 until math.min(values.size, parallelism))(_ => Fiber.initUnscoped(worker)).map { fibers =>
        Kyo.foreachDiscard(fibers)(_.get).andThen(Sync.defer(output.toVector))
      }
    }

  object Ce:
    def runner(): Int = runCe(IO(1))

    def deepBind(depth: Int): Int = runCe {
      def loop(i: Int): IO[Int] =
        if i == depth then IO.pure(i) else IO(i + 1).flatMap(loop)
      loop(0)
    }

    def leftBind(depth: Int): Int = runCe {
      var acc = IO(0)
      var i = 0
      while i < depth do
        acc = acc.flatMap(v => IO(v + 1))
        i += 1
      acc
    }

    def mapChain(depth: Int): Int = runCe {
      var acc = IO(0)
      var i = 0
      while i < depth do
        acc = acc.map(_ + 1)
        i += 1
      acc
    }

    def ref(ops: Int): Int = runCe {
      Ref.of[IO, Int](0).flatMap { ref =>
        def loop(i: Int): IO[Int] =
          if i == ops then ref.get else ref.update(_ + 1).flatMap(_ => loop(i + 1))
        loop(0)
      }
    }

    def deferred(ops: Int): Long = runCe {
      def loop(i: Int, sum: Long): IO[Long] =
        if i == ops then IO.pure(sum)
        else Deferred[IO, Int].flatMap(d => d.complete(i).flatMap(_ => d.get).flatMap(v => loop(i + 1, sum + v)))
      loop(0, 0L)
    }

    // A single producer and consumer, Int entries, fixed count, no close/sentinel or batch draining.
    def queue(ops: Int, capacity: Int): Long = runCe {
      Queue.bounded[IO, Int](capacity).flatMap { q =>
        def produce(i: Int): IO[Unit] =
          if i == ops then IO.unit else q.offer(i).flatMap(_ => produce(i + 1))
        def consume(i: Int, sum: Long): IO[Long] =
          if i == ops then IO.pure(sum) else q.take.flatMap(v => consume(i + 1, sum + v))
        produce(0).start.flatMap(f => consume(0, 0L).flatMap(sum => f.joinWithNever.map(_ => sum)))
      }
    }

    def semaphore(ops: Int): Int = runCe {
      Semaphore[IO](1).flatMap { sem =>
        def loop(i: Int): IO[Int] =
          if i == ops then sem.available.map(_.toInt)
          else sem.permit.use(_ => IO.unit).flatMap(_ => loop(i + 1))
        loop(0)
      }
    }

    // Exactly one child at a time, with a successful join before the next spawn.
    def spawnJoin(ops: Int): Long = runCe {
      def loop(i: Int, sum: Long): IO[Long] =
        if i == ops then IO.pure(sum)
        else IO(i).start.flatMap(_.joinWithNever).flatMap(v => loop(i + 1, sum + v))
      loop(0, 0L)
    }

    def workers(values: Vector[Int], parallelism: Int, rounds: Int): Vector[Int] =
      runCe(Workers.ce(values, parallelism)(i => IO(work(i, rounds))))

    // One fiber per element, sequential spawn and join, then the same ordered Option filtering.
    // Failure selection is suspended on both sides; all-failure input returns an empty Vector.
    def collectSuccesses(input: Vector[Int], rounds: Int): Vector[Int] = runCe {
      input.traverse { i =>
        IO.defer(if (i & 1023) == 0 then IO.raiseError[Int](boom) else IO(work(i, rounds)))
          .attempt.map(_.toOption).start
      }.flatMap(_.traverse(_.joinWithNever)).map(_.flatten)
    }

    private def source(batches: Vector[Vector[Int]]): FStream[IO, Int] =
      FStream.emits(batches).covary[IO].flatMap(batch => FStream.chunk(FChunk.from(batch)))

    // Both evaluate one whole chunk sequentially, then emit it before processing the next.
    def evalChunks(batches: Vector[Vector[Int]], rounds: Int): Long = runCe {
      source(batches).chunks.evalMap(_.toVector.traverse(i => IO(work(i, rounds)))).flatMap(FStream.emits)
      . compile.fold(0L)(_ + _.toLong)
    }

    // Same fixed workers per batch, indexed result array, ordered output and batch barrier.
    def parallelChunks(batches: Vector[Vector[Int]], parallelism: Int, rounds: Int): Long = runCe {
      source(batches).chunks
      . evalMap(batch => Workers.ce(batch.toVector, parallelism)(i => IO(work(i, rounds))))
      . flatMap(FStream.emits)
      . compile.fold(0L)(_ + _.toLong)
    }

    // Same prebuilt Vector chunks and capacity in chunks. The producer loop is direct on both
    // sides. Consumers take exactly batches.size entries, emit each as a chunk and transform it.
    def queueChunks(batches: Vector[Vector[Int]], capacity: Int, rounds: Int): Long = runCe {
      Queue.bounded[IO, Vector[Int]](capacity).flatMap { q =>
        def produce(i: Int): IO[Unit] =
          if i == batches.size then IO.unit else q.offer(batches(i)).flatMap(_ => produce(i + 1))
        val consume = FStream.unfoldEval(0) { i =>
          if i == batches.size then IO.pure(None)
          else q.take.map(batch => Some((batch, i + 1)))
        }.flatMap(batch => FStream.chunk(FChunk.from(batch)))
          .map(i => work(i, rounds)).compile.fold(0L)(_ + _.toLong)
        produce(0).start.flatMap(f => consume.flatMap(sum => f.joinWithNever.map(_ => sum)))
      }
    }

  object Ky:
    def runner(): Int = runKyo(Sync.defer(1))

    def deepBind(depth: Int): Int = runKyo {
      def loop(i: Int): Int < Sync =
        if i == depth then i else Sync.defer(i + 1).map(loop)
      loop(0)
    }

    def leftBind(depth: Int): Int = runKyo {
      var acc: Int < Sync = Sync.defer(0)
      var i = 0
      while i < depth do
        acc = acc.map(v => Sync.defer(v + 1))
        i += 1
      acc
    }

    // Both chains begin with a suspended value, so Kyo cannot eagerly evaluate pure inputs.
    def mapChain(depth: Int): Int = runKyo {
      var acc: Int < Sync = Sync.defer(0)
      var i = 0
      while i < depth do
        acc = acc.map(_ + 1)
        i += 1
      acc
    }

    def ref(ops: Int): Int = runKyo {
      AtomicRef.init(0).map { ref =>
        def loop(i: Int): Int < Sync =
          if i == ops then ref.get else ref.updateAndGet(_ + 1).map(_ => loop(i + 1))
        loop(0)
      }
    }

    def promise(ops: Int): Long = runKyo {
      def loop(i: Int, sum: Long): Long < (Async & Abort[Throwable]) =
        if i == ops then sum
        else Promise.init[Int, Any].map(p => p.completeDiscard(Result.succeed(i)).andThen(p.get).map(v => loop(i + 1, sum + v)))
      loop(0, 0L)
    }

    def queue(ops: Int, capacity: Int): Long = runKyo {
      Abort.run[Closed] {
        Channel.initUnscoped[Int](capacity).map { q =>
          def produce(i: Int): Unit < (Async & Abort[Closed]) =
            if i == ops then () else q.put(i).andThen(produce(i + 1))
          def consume(i: Int, sum: Long): Long < (Async & Abort[Closed]) =
            if i == ops then sum else q.take.map(v => consume(i + 1, sum + v))
          Fiber.initUnscoped(produce(0)).map(f => consume(0, 0L).map(sum => f.get.andThen(sum)))
        }
      }.map(_.getOrThrow)
    }

    def semaphore(ops: Int): Int = runKyo {
      Abort.run[Closed] {
        Meter.initSemaphoreUnscoped(1, reentrant = false).map { sem =>
          def loop(i: Int): Int < (Async & Abort[Closed]) =
            if i == ops then sem.availablePermits
            else sem.run(()).andThen(loop(i + 1))
          loop(0)
        }
      }.map(_.getOrThrow)
    }

    def spawnJoin(ops: Int): Long = runKyo {
      def loop(i: Int, sum: Long): Long < (Async & Abort[Throwable]) =
        if i == ops then sum
        else Fiber.initUnscoped(Sync.defer(i)).map(_.get).map(v => loop(i + 1, sum + v))
      loop(0, 0L)
    }

    def workers(values: Vector[Int], parallelism: Int, rounds: Int): Vector[Int] =
      runKyo(Workers.ky(values, parallelism)(i => Sync.defer(work(i, rounds))))

    def collectSuccesses(input: Vector[Int], rounds: Int): Vector[Int] = runKyo {
      Kyo.foreach(input) { i =>
        Fiber.initUnscoped {
          Abort.run[Throwable](Sync.defer {
            if (i & 1023) == 0 then Abort.fail(boom) else Sync.defer(work(i, rounds))
          }).map {
            case Result.Success(v) => Some(v): Option[Int]
            case Result.Failure(_) => None: Option[Int]
            case Result.Panic(_)   => None: Option[Int]
          }
        }
      }.map(fibers => Kyo.foreach(fibers)(_.get).map(_.toVector.flatten))
    }

    private def source(batches: Vector[Vector[Int]]): Stream[Int, Any] =
      Stream.init(batches, chunkSize = 1).mapChunkPure(_.flatten)

    def evalChunks(batches: Vector[Vector[Int]], rounds: Int): Long = runKyo {
      source(batches)
      . mapChunk(batch => Kyo.foreach(batch.toVector)(i => Sync.defer(work(i, rounds))).map(_.toVector))
      . foldPure(0L)(_ + _.toLong)
    }

    def parallelChunks(batches: Vector[Vector[Int]], parallelism: Int, rounds: Int): Long = runKyo {
      source(batches)
      . mapChunk(batch => Workers.ky(batch.toVector, parallelism)(i => Sync.defer(work(i, rounds))))
      . foldPure(0L)(_ + _.toLong)
    }

    def queueChunks(batches: Vector[Vector[Int]], capacity: Int, rounds: Int): Long = runKyo {
      Abort.run[Closed] {
        Channel.initUnscoped[Vector[Int]](capacity).map { q =>
          def produce(i: Int): Unit < (Async & Abort[Closed]) =
            if i == batches.size then () else q.put(batches(i)).andThen(produce(i + 1))
          val consume = Stream.unfold(0, chunkSize = 1) { i =>
            if i == batches.size then Maybe.empty[(Vector[Int], Int)]
            else q.take.map(batch => Maybe((batch, i + 1)))
          }.mapChunkPure(_.flatten).mapPure(i => work(i, rounds)).foldPure(0L)(_ + _.toLong)
          Fiber.initUnscoped(produce(0)).map(f => consume.map(sum => f.get.andThen(sum)))
        }
      }.map(_.getOrThrow)
    }
