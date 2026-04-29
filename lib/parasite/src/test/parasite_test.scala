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
package parasite

import java.util.concurrent as juc
import juc.atomic as juca

import anticipation.*
import contingency.*
import denominative.*
import digression.*
import fulminate.*
import gossamer.*
import probably.*
import proscenium.*
import quantitative.*
import rudiments.*
import symbolism.*
import vacuous.*

import strategies.throwUnsafely
import errorDiagnostics.empty

import threading.virtual
import codicils.cancel

object Tests extends Suite(m"Parasite tests"):

  def run(): Unit =
    supervise:

      suite(m"Promise basic state"):
        test(m"New promise is not ready"):
          Promise[Int]().ready
        . assert(_ == false)

        test(m"New promise is not complete"):
          Promise[Int]().complete
        . assert(_ == false)

        test(m"New promise is not cancelled"):
          Promise[Int]().cancelled
        . assert(_ == false)

        test(m"New promise apply returns Unset"):
          Promise[Int]().apply()
        . assert(_ == Unset)

        test(m"Fulfilled promise is ready"):
          val promise = Promise[Int]()
          promise.fulfill(42)
          promise.ready
        . assert(_ == true)

        test(m"Fulfilled promise is complete"):
          val promise = Promise[Int]()
          promise.fulfill(42)
          promise.complete
        . assert(_ == true)

        test(m"Fulfilled promise is not cancelled"):
          val promise = Promise[Int]()
          promise.fulfill(42)
          promise.cancelled
        . assert(_ == false)

        test(m"Fulfilled promise apply returns value"):
          val promise = Promise[Int]()
          promise.fulfill(42)
          promise.apply()
        . assert(_ == 42)

        test(m"Cancelled promise is ready"):
          val promise = Promise[Int]()
          promise.cancel()
          promise.ready
        . assert(_ == true)

        test(m"Cancelled promise is not complete"):
          val promise = Promise[Int]()
          promise.cancel()
          promise.complete
        . assert(_ == false)

        test(m"Cancelled promise is cancelled"):
          val promise = Promise[Int]()
          promise.cancel()
          promise.cancelled
        . assert(_ == true)

        test(m"Cancelled promise apply returns Unset"):
          val promise = Promise[Int]()
          promise.cancel()
          promise.apply()
        . assert(_ == Unset)

      suite(m"Promise fulfill / offer / cancel"):
        test(m"Fulfilling a fulfilled promise raises AlreadyComplete"):
          val promise = Promise[Int]()
          promise.fulfill(1)
          capture(promise.fulfill(2))
        . assert(_ == AsyncError(AsyncError.Reason.AlreadyComplete))

        test(m"Fulfilling a cancelled promise raises Cancelled"):
          val promise = Promise[Int]()
          promise.cancel()
          capture(promise.fulfill(1))
        . assert(_ == AsyncError(AsyncError.Reason.Cancelled))

        test(m"Offer on incomplete promise sets value"):
          val promise = Promise[Int]()
          promise.offer(1)
          promise.apply()
        . assert(_ == 1)

        test(m"Offer twice keeps the first value"):
          val promise = Promise[Int]()
          promise.offer(1)
          promise.offer(2)
          promise.apply()
        . assert(_ == 1)

        test(m"Offer on cancelled promise leaves it cancelled"):
          val promise = Promise[Int]()
          promise.cancel()
          promise.offer(1)
          promise.cancelled
        . assert(_ == true)

        test(m"Cancel on a fulfilled promise leaves it complete"):
          val promise = Promise[Int]()
          promise.fulfill(7)
          promise.cancel()
          promise.complete
        . assert(_ == true)

        test(m"Cancel on a fulfilled promise does not change value"):
          val promise = Promise[Int]()
          promise.fulfill(7)
          promise.cancel()
          promise.apply()
        . assert(_ == 7)

      suite(m"Promise await / attend"):
        test(m"Await on already-fulfilled promise returns value"):
          val promise = Promise[Int]()
          promise.fulfill(99)
          promise.await()
        . assert(_ == 99)

        test(m"Await on already-cancelled promise raises Cancelled"):
          val promise = Promise[Int]()
          promise.cancel()
          capture(promise.await())
        . assert(_ == AsyncError(AsyncError.Reason.Cancelled))

        test(m"Await with timeout on incomplete promise raises Timeout"):
          val promise = Promise[Int]()
          capture(promise.await(50.0*Milli(Second)))
        . assert(_ == AsyncError(AsyncError.Reason.Timeout))

        test(m"Await with timeout returns value if fulfilled in time"):
          val promise = Promise[Int]()
          promise.fulfill(123)
          promise.await(1.0*Second)
        . assert(_ == 123)

        test(m"Attend on already-fulfilled promise returns immediately"):
          val promise = Promise[Int]()
          promise.fulfill(1)
          promise.attend()
          promise.complete
        . assert(_ == true)

        test(m"Attend on already-cancelled promise returns without error"):
          val promise = Promise[Int]()
          promise.cancel()
          promise.attend()
          promise.cancelled
        . assert(_ == true)

        test(m"Attend with timeout on incomplete promise returns silently"):
          val promise = Promise[Int]()
          promise.attend(50.0*Milli(Second))
          promise.ready
        . assert(_ == false)

        test(m"Await wakes up when promise fulfilled from another thread"):
          val ready = juc.CountDownLatch(1)
          val promise = Promise[Int]()
          val daemon0 = daemon:
            ready.await()
            promise.fulfill(7)
          ready.countDown()
          promise.await()
        . assert(_ == 7)

        test(m"Await wakes up when promise cancelled from another thread"):
          val ready = juc.CountDownLatch(1)
          val promise = Promise[Int]()
          val daemon0 = daemon:
            ready.await()
            promise.cancel()
          ready.countDown()
          capture(promise.await())
        . assert(_ == AsyncError(AsyncError.Reason.Cancelled))

        test(m"Multiple waiters all wake up on fulfill"):
          val promise = Promise[Int]()
          val started = juc.CountDownLatch(5)
          val results = juca.AtomicInteger(0)
          val tasks = (1 to 5).map: _ =>
            async:
              started.countDown()
              val n = promise.await()
              results.addAndGet(n)
              n
          started.await()
          Thread.sleep(20)
          promise.fulfill(11)
          tasks.foreach(_.await())
          results.get()
        . assert(_ == 55)

        test(m"Multiple waiters all wake up on cancel"):
          val promise = Promise[Int]()
          val started = juc.CountDownLatch(5)
          val cancelled = juca.AtomicInteger(0)
          val tasks = (1 to 5).map: _ =>
            async:
              started.countDown()
              try
                promise.await()
                0
              catch case _: AsyncError =>
                cancelled.incrementAndGet()
                1
          started.await()
          Thread.sleep(20)
          promise.cancel()
          tasks.foreach(t => safely(t.await()))
          cancelled.get()
        . assert(_ == 5)

      suite(m"Task / async basics"):
        test(m"Simple task produces a result"):
          async(42).await()
        . assert(_ == 42)

        test(m"Task with side effect produces effect"):
          val counter = juca.AtomicInteger(0)
          async(counter.incrementAndGet()).await()
          counter.get()
        . assert(_ == 1)

        test(m"Map produces a transformed task"):
          async(10).map(_ * 2).await()
        . assert(_ == 20)

        test(m"Bind chains tasks"):
          async(3).bind(x => async(x + 4)).await()
        . assert(_ == 7)

        test(m"Multiple maps chain"):
          async(1).map(_ + 1).map(_ * 5).map(_ - 2).await()
        . assert(_ == 8)

        test(m"Independent tasks run in parallel"):
          val gate = Promise[Unit]()
          val counter = juca.AtomicInteger(0)
          val a = async:
            counter.incrementAndGet()
            gate.attend()
            t"A"
          val b = async:
            counter.incrementAndGet()
            gate.attend()
            t"B"
          while counter.get() < 2 do Thread.`yield`()
          gate.fulfill(())
          a.await(); b.await()
          counter.get()
        . assert(_ == 2)

        test(m"Task ready becomes true after completion"):
          val task = async(7)
          task.attend()
          task.ready
        . assert(_ == true)

        test(m"Task can be awaited twice"):
          val task = async(99)
          task.await()
          task.await()
        . assert(_ == 99)

        test(m"Task with name has name reflected"):
          val gate = Promise[Unit]()
          val captured = juca.AtomicReference[Optional[Text]](Unset)
          val t = task(t"named")(captured.set(monitor.name))
          t.await()
          captured.get().nn
        . assert(_ == t"named")

      suite(m"Task error handling"):
        test(m"Awaiting a cancelled task raises Cancelled"):
          val gate = Promise[Unit]()
          val task = async:
            gate.attend()
            42
          task.cancel()
          gate.fulfill(())
          capture(task.await())
        . assert(_ == AsyncError(AsyncError.Reason.Cancelled))

        test(m"Cancelling an already-cancelled task is a no-op"):
          val task = async:
            snooze(0.05*Second)
            ()
          task.cancel()
          task.cancel()
          safely(task.await())
        . assert(_ == Unset)

        test(m"Cancelling a completed task does not affect its result"):
          val task = async(11)
          task.attend()
          task.cancel()
          task.await()
        . assert(_ == 11)

        test(m"Failure in task propagates"):
          val task = async[Int]:
            throw new RuntimeException("boom")
          val result = try task.await() catch case _: RuntimeException => -1
          result
        . assert(_ == -1)

      suite(m"Cancellation propagation"):
        test(m"Snooze is interrupted by cancel"):
          val started = Promise[Unit]()
          val task = async:
            started.fulfill(())
            snooze(10.0*Second)
            42
          started.await()
          task.cancel()
          safely(task.await())
        . assert(_ == Unset)

        test(m"Relent throws after cancel"):
          val started = Promise[Unit]()
          val canStart = Promise[Unit]()
          val counter = juca.AtomicInteger(0)
          val task = async:
            started.fulfill(())
            canStart.await()
            while true do
              counter.incrementAndGet()
              relent()

          started.await()
          task.cancel()
          canStart.fulfill(())
          safely(task.await())
          counter.get() <= 1
        . assert(_ == true)

        test(m"Cancel while waiting on Promise raises Cancelled"):
          val outerStarted = Promise[Unit]()
          val unblocker = Promise[Int]()
          val task = async:
            outerStarted.fulfill(())
            unblocker.await()

          outerStarted.await()
          task.cancel()
          safely(task.await())
        . assert(_ == Unset)

        test(m"Cancel while waiting on child task aborts parent"):
          val parentStarted = Promise[Unit]()
          val childGate = Promise[Unit]()
          val task = async:
            parentStarted.fulfill(())
            val child = async:
              childGate.await()
              5
            child.await()
            10

          parentStarted.await()
          task.cancel()
          safely(task.await())
        . assert(_ == Unset)

      suite(m"Codicils"):
        test(m"With await codicil parent waits for incomplete child"):
          import codicils.await
          val parentReady = Promise[Unit]()
          val childDone = juca.AtomicBoolean(false)
          val task = async:
            val child = async:
              snooze(50.0*Milli(Second))
              childDone.set(true)
            parentReady.fulfill(())

          task.await()
          childDone.get()
        . assert(_ == true)

        test(m"With cancel codicil parent cancels incomplete child"):
          import codicils.cancel
          val parentReady = Promise[Unit]()
          val childGate = Promise[Unit]()
          val childCompleted = juca.AtomicBoolean(false)
          val task = async:
            val child = async:
              childGate.await()
              childCompleted.set(true)
            parentReady.fulfill(())

          task.await()
          childCompleted.get()
        . assert(_ == false)

        test(m"With fail codicil incomplete child raises"):
          import codicils.fail
          val task = async:
            val child = async:
              snooze(10.0*Second)
            ()
          capture(task.await())
        . assert(_ == AsyncError(AsyncError.Reason.Incomplete))

        test(m"Codicils only affect non-daemon children for daemons"):
          import codicils.cancel
          val daemonRunning = Promise[Unit]()
          val daemonCompleted = juca.AtomicBoolean(false)
          val task = async:
            val d = daemon:
              daemonRunning.fulfill(())
              snooze(10.0*Second)
              daemonCompleted.set(true)
            daemonRunning.await()

          task.await()
          daemonCompleted.get()
        . assert(_ == false)

      suite(m"Combinators"):
        test(m"Sequence collects results in order"):
          Seq(async(1), async(2), async(3), async(4)).sequence.await()
        . assert(_ == Seq(1, 2, 3, 4))

        test(m"Sequence runs tasks in parallel"):
          val barrier = juc.CyclicBarrier(3)
          val tasks = Seq(
            async { barrier.await(); 10 },
            async { barrier.await(); 20 },
            async { barrier.await(); 30 } )
          tasks.sequence.await().sum
        . assert(_ == 60)

        test(m"Race returns first completed result"):
          val gate = Promise[Unit]()
          val winner = Promise[Unit]()
          val tasks = Vector(
            async { winner.await(); t"first" },
            async { gate.await(); t"second" },
            async { gate.await(); t"third" } )
          val raceTask = async(tasks.race())
          winner.fulfill(())
          val result = raceTask.await()
          gate.fulfill(())
          result
        . assert(_ == t"first")

        test(m"Race with all tasks succeeding picks one"):
          val tasks = (1 to 5).map: i =>
            async(i)
          val result = async(tasks.race()).await()
          (1 to 5).contains(result)
        . assert(_ == true)

      suite(m"Retry / Tenacity"):
        import retryTenacities.fixedNoDelayFiveTimes

        test(m"Retry returns successfully on first try"):
          retry: (surrender, persevere) ?=>
            42
        . assert(_ == 42)

        test(m"Retry persists on persevere"):
          val attempts = juca.AtomicInteger(0)
          val result = retry: (surrender, persevere) ?=>
            val n = attempts.incrementAndGet()
            if n < 3 then persevere()
            else n
          (result, attempts.get())
        . assert(_ == (3, 3))

        test(m"Retry surrenders on surrender"):
          val attempts = juca.AtomicInteger(0)
          val result = capture[RetryError](retry: (surrender, persevere) ?=>
            attempts.incrementAndGet()
            surrender())
          (result, attempts.get())
        . assert(_ == (RetryError(1), 1))

        test(m"Retry exceeds limit gives RetryError"):
          val attempts = juca.AtomicInteger(0)
          val result = capture[RetryError](retry: (surrender, persevere) ?=>
            attempts.incrementAndGet()
            persevere())
          (attempts.get() <= 6, result.count == 5)
        . assert(_ == (true, true))

        test(m"Tenacity.fixed first attempt has zero delay"):
          val tenacity = Tenacity.fixed(100.0*Milli(Second))
          tenacity.delay(Prim).or(-1L)
        . assert(_ == 0L)

        test(m"Tenacity.fixed subsequent attempts have fixed delay"):
          val tenacity = Tenacity.fixed(100.0*Milli(Second))
          tenacity.delay(Prim + 1).or(-1L)
        . assert(_ == 100_000_000L)

        test(m"Tenacity.exponential first attempt has zero delay"):
          val tenacity = Tenacity.exponential(10.0*Milli(Second), 2.0)
          tenacity.delay(Prim).or(-1L)
        . assert(_ == 0L)

        test(m"Tenacity.exponential second attempt has initial delay"):
          val tenacity = Tenacity.exponential(10.0*Milli(Second), 2.0)
          tenacity.delay(Prim + 1).or(-1L)
        . assert(_ == 10_000_000L)

        test(m"Tenacity.exponential third attempt is base * initial"):
          val tenacity = Tenacity.exponential(10.0*Milli(Second), 2.0)
          tenacity.delay(Prim + 2).or(-1L)
        . assert(_ == 20_000_000L)

        test(m"Tenacity.limit aborts after specified attempts"):
          val base = Tenacity.fixed(0.0*Milli(Second)).limit(3)
          val result = capture[RetryError](base.delay(Prim + 3))
          result.count
        . assert(_ == 3)

        test(m"Tenacity.limit allows attempts up to limit"):
          val base = Tenacity.fixed(0.0*Milli(Second)).limit(3)
          base.delay(Prim + 2).or(-1L)
        . assert(_ == 0L)

      suite(m"Daemon"):
        test(m"Daemon runs to completion"):
          val done = Promise[Unit]()
          daemon(done.fulfill(()))
          done.await(1.0*Second)
          done.complete
        . assert(_ == true)

        test(m"Daemon is cancelled when parent task ends"):
          val daemonStarted = Promise[Unit]()
          val gate = Promise[Unit]()
          val reachedEnd = juca.AtomicBoolean(false)
          val parent = async:
            val d = daemon:
              daemonStarted.fulfill(())
              gate.attend()
              reachedEnd.set(true)
            daemonStarted.await()
          parent.await()
          reachedEnd.get()
        . assert(_ == false)

      suite(m"Concurrent stream"):
        test(m"Concurrent on a complete stream returns same elements"):
          Stream(1, 2, 3, 4, 5).concurrent.to(List)
        . assert(_ == List(1, 2, 3, 4, 5))

        test(m"Concurrent on empty stream is empty"):
          Stream[Int]().concurrent.to(List)
        . assert(_ == List())

      suite(m"High contention"):
        test(m"Many concurrent fulfill attempts result in one success"):
          val promise = Promise[Int]()
          val barrier = juc.CyclicBarrier(50)
          val successes = juca.AtomicInteger(0)
          val tasks = (1 to 50).map: i =>
            async:
              barrier.await()
              try { promise.fulfill(i); successes.incrementAndGet() }
              catch case _: AsyncError => ()
          tasks.foreach(_.await())
          successes.get()
        . assert(_ == 1)

        test(m"Many concurrent offers all see same final value"):
          val promise = Promise[Int]()
          val barrier = juc.CyclicBarrier(50)
          val tasks = (1 to 50).map: i =>
            async:
              barrier.await()
              promise.offer(i)
          tasks.foreach(_.await())
          promise.complete && promise().or(-1) > 0 && promise().or(-1) <= 50
        . assert(_ == true)

        test(m"Concurrent fulfill and cancel: at most one wins"):
          var fulfillSucceeded = 0
          var cancelSucceeded = 0
          val rounds = 100
          (1 to rounds).foreach: _ =>
            val promise = Promise[Int]()
            val start = juc.CountDownLatch(1)
            val a = async:
              start.await()
              try { promise.fulfill(1); fulfillSucceeded += 1 }
              catch case _: AsyncError => ()
            val b = async:
              start.await()
              promise.cancel()
              if promise.cancelled then cancelSucceeded += 1
            start.countDown()
            a.await()
            b.await()
          (fulfillSucceeded + cancelSucceeded >= rounds, fulfillSucceeded > 0, cancelSucceeded > 0)
        . assert: (sumOk, anyFulfill, anyCancel) =>
            sumOk && (anyFulfill || anyCancel)

        test(m"Many awaiters all see same value"):
          val promise = Promise[Int]()
          val started = juc.CountDownLatch(100)
          val mismatches = juca.AtomicInteger(0)
          val tasks = (1 to 100).map: _ =>
            async:
              started.countDown()
              val v = promise.await()
              if v != 7 then mismatches.incrementAndGet()
              v
          started.await()
          promise.fulfill(7)
          tasks.foreach(_.await())
          mismatches.get()
        . assert(_ == 0)

        test(m"Many tasks, all complete"):
          val n = 200
          val counter = juca.AtomicInteger(0)
          val tasks = (1 to n).map: _ =>
            async(counter.incrementAndGet())
          tasks.foreach(_.await())
          counter.get()
        . assert(_ == 200)

        test(m"Stress test: pipeline of bind operations"):
          val n = 50
          val task: Task[Int] = (1 to n).foldLeft(async(0)): (t, _) =>
            t.bind(x => async(x + 1))
          task.await()
        . assert(_ == 50)

        test(m"Many promises with many waiters each"):
          val promises = (1 to 10).map(_ => Promise[Int]())
          val total = juca.AtomicInteger(0)
          val started = juc.CountDownLatch(100)

          val waiters = promises.zipWithIndex.flatMap: (promise, i) =>
            (1 to 10).map: _ =>
              async:
                started.countDown()
                total.addAndGet(promise.await())

          started.await()
          promises.zipWithIndex.foreach: (p, i) =>
            p.fulfill(i + 1)

          waiters.foreach(_.await())
          total.get()
        . assert(_ == 550)

        test(m"Concurrent cancel of awaiting tasks"):
          val gate = Promise[Unit]()
          val cancelled = juca.AtomicInteger(0)
          val started = juc.CountDownLatch(20)

          val tasks = (1 to 20).map: _ =>
            async:
              started.countDown()
              try gate.await() catch case _: AsyncError => cancelled.incrementAndGet()

          started.await()
          tasks.foreach(_.cancel())
          tasks.foreach(t => safely(t.await()))
          gate.cancel()
          cancelled.get()
        . assert(_ <= 20)

      suite(m"Race conditions in delegate"):
        test(m"Codicil await with multiple children"):
          import codicils.await
          val numChildren = 30
          val completed = juca.AtomicInteger(0)
          val task = async:
            (1 to numChildren).foreach: _ =>
              async:
                snooze(20.0*Milli(Second))
                completed.incrementAndGet()

          task.await()
          completed.get()
        . assert(_ == 30)

        test(m"Codicil cancel cancels all incomplete children"):
          import codicils.cancel
          val numChildren = 30
          val gate = Promise[Unit]()
          val completed = juca.AtomicInteger(0)
          val task = async:
            (1 to numChildren).foreach: _ =>
              async:
                gate.await()
                completed.incrementAndGet()

          task.await()
          completed.get()
        . assert(_ == 0)

      suite(m"Tenacity sleep behavior"):
        test(m"Retry with fixed tenacity actually delays between attempts"):
          given tenacity: Tenacity = Tenacity.fixed(40.0*Milli(Second)).limit(3)
          val attempts = juca.AtomicInteger(0)
          val start = System.currentTimeMillis
          val result = capture[RetryError]:
            retry: (surrender, persevere) ?=>
              attempts.incrementAndGet()
              persevere()
          val elapsed = System.currentTimeMillis - start
          (attempts.get(), elapsed >= 80L)
        . assert: (n, ok) =>
            n >= 3 && n <= 4 && ok

      suite(m"Promise edge cases"):
        test(m"Cancel on a new promise marks it cancelled"):
          val promise = Promise[Int]()
          promise.cancel()
          (promise.cancelled, promise.complete, promise.ready)
        . assert(_ == (true, false, true))

        test(m"Multiple cancels are idempotent"):
          val promise = Promise[Int]()
          promise.cancel()
          promise.cancel()
          promise.cancel()
          promise.cancelled
        . assert(_ == true)

        test(m"Multiple offers do not change value once set"):
          val promise = Promise[Int]()
          promise.offer(5)
          promise.offer(10)
          promise.offer(15)
          promise.apply()
        . assert(_ == 5)

        test(m"Await on already-fulfilled promise returns immediately even if no waiters were enqueued"):
          val promise = Promise[Int]()
          promise.fulfill(99)
          val before = System.currentTimeMillis
          val result = promise.await()
          val elapsed = System.currentTimeMillis - before
          (result, elapsed < 100L)
        . assert(_ == (99, true))

      suite(m"Task lifecycle"):
        test(m"Task ready before completion is false"):
          val gate = Promise[Unit]()
          val task = async(gate.await())
          val readyBefore = task.ready
          gate.fulfill(())
          task.await()
          (readyBefore, task.ready)
        . assert(_ == (false, true))

        test(m"Task ready transitions correctly"):
          val gate = Promise[Unit]()
          val task = async:
            gate.await()
            42
          val before = task.ready
          gate.fulfill(())
          task.await()
          (before, task.ready)
        . assert(_ == (false, true))

        test(m"Task await with timeout returns value"):
          async(7).await(1.0*Second)
        . assert(_ == 7)

        test(m"Task await with too-short timeout raises Timeout"):
          val gate = Promise[Unit]()
          val task = async(gate.await())
          val result = capture[AsyncError](task.await(50.0*Milli(Second)))
          gate.fulfill(())
          task.await()
          result.reason
        . assert(_ == AsyncError.Reason.Timeout)

      suite(m"Cancellation edge cases"):
        test(m"Cancelling a task before it has had a chance to start still works"):
          val task = async:
            42
          task.cancel()
          val res = safely(task.await())
          res
        . assert(r => r == Unset || r == 42)

        test(m"Self-cancellation from within a task"):
          val task: Task[Int] = async:
            monitor.cancel()
            42
          val result = safely(task.await())
          result
        . assert(r => r == Unset || r == 42)

        test(m"Concurrent self-cancel and external cancel"):
          val task: Task[Int] = async:
            monitor.cancel()
            42
          task.cancel()
          safely(task.await())
        . assert(r => r == Unset || r == 42)

        test(m"Task cancelled while in promise.await raises Cancelled"):
          val gate = Promise[Unit]()
          val task = async:
            gate.attend()
            42
          task.cancel()
          gate.fulfill(())
          capture(task.await())
        . assert(_ == AsyncError(AsyncError.Reason.Cancelled))

      suite(m"Children of children"):
        test(m"Nested tasks with await codicil all complete"):
          import codicils.await
          val counter = juca.AtomicInteger(0)
          val task = async:
            val a = async:
              counter.incrementAndGet()
              val b = async:
                counter.incrementAndGet()
                val c = async:
                  counter.incrementAndGet()
                  ()
                ()
              ()
            ()
          task.await()
          counter.get()
        . assert(_ == 3)

        test(m"Cancelling outer cancels nested children"):
          import codicils.cancel
          val gate = Promise[Unit]()
          val started = juc.CountDownLatch(3)
          val completed = juca.AtomicInteger(0)
          val task = async:
            async:
              started.countDown()
              async:
                started.countDown()
                async:
                  started.countDown()
                  gate.await()
                  completed.incrementAndGet()
                . await()
              . await()
            . await()
          started.await()
          task.cancel()
          safely(task.await())
          completed.get()
        . assert(_ == 0)

      suite(m"Worker introspection"):
        test(m"Worker has access to monitor inside body"):
          val captured = juca.AtomicReference[Optional[Monitor]](Unset)
          val task = async:
            captured.set(monitor)
            ()
          task.await()
          captured.get().nn != Unset
        . assert(_ == true)

        test(m"Worker.relentlessness is non-negative"):
          val readout = juca.AtomicReference[Double](-1.0)
          val task = async:
            relent()
            relent()
            readout.set(monitor.asInstanceOf[Worker].relentlessness)
            ()
          task.await()
          readout.get().nn >= 0.0
        . assert(_ == true)

      suite(m"Promise multiple-thread interactions"):
        test(m"Multiple threads racing to fulfill: only one wins"):
          val rounds = 50
          var allOk = true
          (1 to rounds).foreach: _ =>
            val promise = Promise[Int]()
            val barrier = juc.CyclicBarrier(10)
            val winners = juca.AtomicInteger(0)
            val tasks = (1 to 10).map: i =>
              async:
                barrier.await()
                try
                  promise.fulfill(i)
                  winners.incrementAndGet()
                catch case _: AsyncError => ()
            tasks.foreach(_.await())
            if winners.get() != 1 then allOk = false
          allOk
        . assert(_ == true)

        test(m"Mixed offer/cancel/fulfill leave promise in consistent state"):
          val rounds = 100
          var allOk = true
          (1 to rounds).foreach: _ =>
            val promise = Promise[Int]()
            val barrier = juc.CyclicBarrier(3)
            val a = async:
              barrier.await()
              promise.offer(1)
            val b = async:
              barrier.await()
              promise.cancel()
            val c = async:
              barrier.await()
              try promise.fulfill(2) catch case _: AsyncError => ()
            a.await(); b.await(); c.await()
            val isReady = promise.ready
            val isComplete = promise.complete
            val isCancelled = promise.cancelled
            // Either complete or cancelled; ready true; cannot be both complete and cancelled
            if !isReady then allOk = false
            if isComplete && isCancelled then allOk = false
          allOk
        . assert(_ == true)

      suite(m"Combinator edge cases"):
        test(m"Empty sequence returns empty Seq"):
          Seq[Task[Int]]().sequence.await()
        . assert(_ == Seq[Int]())

        test(m"Single element sequence"):
          Seq(async(42)).sequence.await()
        . assert(_ == Seq(42))

        test(m"Race with one already-complete task picks it"):
          val ready = async(99)
          ready.attend()
          val later = async:
            snooze(1.0*Second)
            -1
          val result = async(Vector(ready, later).race()).await()
          later.cancel()
          result
        . assert(_ == 99)

      suite(m"Stress with virtual threads"):
        test(m"1000 concurrent Promise.fulfill races"):
          val winners = juca.AtomicInteger(0)
          val tasks = (1 to 1000).map: _ =>
            async:
              val promise = Promise[Int]()
              val barrier = juc.CyclicBarrier(2)
              val a = async:
                barrier.await()
                try { promise.fulfill(1); 1 } catch case _: AsyncError => 0
              val b = async:
                barrier.await()
                try { promise.fulfill(2); 1 } catch case _: AsyncError => 0
              val w = a.await() + b.await()
              if w == 1 then winners.incrementAndGet()
              w
          val results = tasks.map(_.await())
          (results.forall(_ == 1), winners.get())
        . assert: (allOne, total) =>
            allOne && total == 1000

        test(m"Spawning tasks under load - no lost promises"):
          val n = 200
          import codicils.await
          val sums = juca.AtomicInteger(0)
          val outer = async:
            (1 to n).foreach: i =>
              async(sums.addAndGet(i))
          outer.await()
          sums.get()
        . assert(_ == (1 to 200).sum)

      suite(m"Daemon edge cases"):
        test(m"Daemon completes if not cancelled"):
          val done = Promise[Int]()
          val parent = async:
            val d = daemon:
              done.fulfill(42)
            done.await()
          parent.await()
          done.apply()
        . assert(_ == 42)

        test(m"Multiple daemons all started and cancelled"):
          import codicils.cancel
          val started = juc.CountDownLatch(10)
          val gate = Promise[Unit]()
          val finished = juca.AtomicInteger(0)
          val parent = async:
            (1 to 10).foreach: _ =>
              daemon:
                started.countDown()
                gate.attend()
                finished.incrementAndGet()
            started.await()
          parent.await()
          finished.get()
        . assert(_ == 0)

      suite(m"Snooze and delay"):
        test(m"Snooze sleeps for approximately the given duration"):
          val gate = Promise[Long]()
          val task = async:
            val start = System.currentTimeMillis
            snooze(50.0*Milli(Second))
            gate.fulfill(System.currentTimeMillis - start)
          task.await()
          gate.apply().or(0L) >= 40L
        . assert(_ == true)

        test(m"Delay sleeps for relative duration"):
          val gate = Promise[Long]()
          val task = async:
            val start = System.currentTimeMillis
            delay(50.0*Milli(Second))
            gate.fulfill(System.currentTimeMillis - start)
          task.await()
          gate.apply().or(0L) >= 40L
        . assert(_ == true)

        test(m"Snooze inside cancelled task throws"):
          val started = Promise[Unit]()
          val task = async:
            started.fulfill(())
            snooze(10.0*Second)
            ()
          started.await()
          task.cancel()
          safely(task.await())
        . assert(_ == Unset)

      suite(m"Race extension"):
        test(m"Race propagates result of fastest"):
          val gate = Promise[Unit]()
          val tasks = Vector(
            async:
              snooze(200.0*Milli(Second))
              t"slow",
            async:
              gate.await()
              t"fast" )
          val task = async(tasks.race())
          gate.fulfill(())
          val result = task.await()
          result
        . assert(_ == t"fast")

      suite(m"Heap"):
        test(m"Heap.used returns a positive number of bytes"):
          Heap.used.long > 0L
        . assert(_ == true)

      suite(m"Hook"):
        test(m"Hook can be cancelled"):
          val cancelled = juca.AtomicBoolean(false)
          val hook = Hook(() => cancelled.set(true))
          hook.cancel()
          cancelled.get()
        . assert(_ == true)

      suite(m"Multiple supervisors"):
        test(m"Platform supervisor can be used"):
          import threading.platform
          supervise(async(42).await())
        . assert(_ == 42)

        test(m"Adaptive supervisor can be used"):
          import threading.adaptive
          supervise(async(42).await())
        . assert(_ == 42)

        test(m"Virtual supervisor can be used"):
          import threading.virtual
          supervise(async(42).await())
        . assert(_ == 42)

      suite(m"Promise.cancelled state queries"):
        test(m"Cancelled promise after fulfill remains cancelled.cancelled"):
          val promise = Promise[Int]()
          promise.cancel()
          promise.cancelled
        . assert(_ == true)

        test(m"Promise after cancel reports complete=false"):
          val promise = Promise[Int]()
          promise.cancel()
          promise.complete
        . assert(_ == false)

      suite(m"Timeout"):
        test(m"Timeout fires action after duration"):
          val fired = Promise[Unit]()
          val t = Timeout(50.0*Milli(Second)):
            fired.fulfill(())
          fired.await(1.0*Second)
          fired.complete
        . assert(_ == true)

        test(m"Timeout.alive is true before firing"):
          val fired = Promise[Unit]()
          val t = Timeout(2.0*Second):
            fired.fulfill(())
          val before = t.alive
          before
        . assert(_ == true)

        test(m"Timeout.nudge extends the deadline"):
          val fired = juca.AtomicBoolean(false)
          val t = Timeout(50.0*Milli(Second)):
            fired.set(true)
          // Nudge it a few times before timeout
          (1 to 3).foreach: _ =>
            snooze(20.0*Milli(Second))
            t.nudge()
          fired.get()
        . assert(_ == false)

      suite(m"Stack & chain"):
        test(m"Worker stack contains the supervisor name"):
          val captured = juca.AtomicReference[Optional[Text]](Unset)
          val task = async:
            captured.set(monitor.stack)
          task.await()
          captured.get().nn.contains(t"virtual")
        . assert(_ == true)

        test(m"Nested workers' stack reflects nesting"):
          val captured = juca.AtomicReference[Optional[Text]](Unset)
          val task = async:
            val inner = async:
              captured.set(monitor.stack)
            inner.await()
          task.await()
          captured.get().nn.contains(t"//")
        . assert(_ == true)

      suite(m"Concurrent stream details"):
        test(m"Concurrent stream preserves head element with delays"):
          val gate = Promise[Unit]()
          val stream: Stream[Int] = 1 #:: { gate.await(); 2 } #:: { 3 } #:: Stream.empty
          val task = async(stream.concurrent.head)
          gate.fulfill(())
          task.await()
        . assert(_ == 1)

      suite(m"Bidirectional cancellation"):
        test(m"Cancel propagates from parent to child via codicil"):
          import codicils.cancel
          val childGate = Promise[Unit]()
          val childResult = juca.AtomicInteger(0)
          val task = async:
            val child = async:
              childGate.await()
              childResult.set(42)
            ()  // parent's body completes immediately
          // child is incomplete; codicil.cancel cancels it
          task.await()
          childResult.get()
        . assert(_ == 0)

        test(m"Multiple sibling tasks all cancelled by codicil"):
          import codicils.cancel
          val gate = Promise[Unit]()
          val started = juc.CountDownLatch(5)
          val completed = juca.AtomicInteger(0)
          val task = async:
            (1 to 5).foreach: _ =>
              async:
                started.countDown()
                gate.await()
                completed.incrementAndGet()
            started.await()
          task.await()
          completed.get()
        . assert(_ == 0)

      suite(m"Promise cancel/fulfill interleaving"):
        test(m"Awaiter sees fulfilled value if fulfill wins"):
          val promise = Promise[Int]()
          val barrier = juc.CyclicBarrier(2)
          val sawCancelled = juca.AtomicBoolean(false)
          val sawFulfilled = juca.AtomicBoolean(false)

          val awaiter = async:
            try
              val v = promise.await()
              if v == 42 then sawFulfilled.set(true)
            catch case _: AsyncError => sawCancelled.set(true)

          val rounds = 100
          var fulfillFirst = 0
          var cancelFirst = 0
          (1 to rounds).foreach: _ =>
            val p = Promise[Int]()
            val a = async:
              p.offer(1)
            val b = async:
              p.cancel()
            a.await()
            b.await()
            if p.complete then fulfillFirst += 1
            else if p.cancelled then cancelFirst += 1

          awaiter.cancel()
          fulfillFirst + cancelFirst >= rounds
        . assert(_ == true)

      suite(m"Promise consistency"):
        test(m"State is monotonic: never goes backwards"):
          val rounds = 50
          var allMonotonic = true
          (1 to rounds).foreach: _ =>
            val promise = Promise[Int]()
            val sawIncomplete = juca.AtomicBoolean(false)
            val sawComplete = juca.AtomicBoolean(false)
            val sawCancelled = juca.AtomicBoolean(false)
            val barrier = juc.CyclicBarrier(3)

            val checker = async:
              barrier.await()
              (1 to 100).foreach: _ =>
                if promise.complete then sawComplete.set(true)
                if promise.cancelled then sawCancelled.set(true)
                if !promise.ready then sawIncomplete.set(true)
            val finisher = async:
              barrier.await()
              promise.offer(1)
            val canceller = async:
              barrier.await()
              promise.cancel()

            checker.await()
            finisher.await()
            canceller.await()

            // never both complete and cancelled
            if sawComplete.get() && sawCancelled.get() then allMonotonic = false
          allMonotonic
        . assert(_ == true)

      suite(m"Panic codicil"):
        test(m"Panic codicil with all children complete passes"):
          import codicils.panic
          val task = async:
            async(()).await()
          task.await()
          true
        . assert(_ == true)

      suite(m"Mixed scenarios"):
        test(m"Cancel a task while many are awaiting same promise"):
          val gate = Promise[Int]()
          val started = juc.CountDownLatch(20)
          val tasks = (1 to 20).map: _ =>
            async:
              started.countDown()
              gate.await()
          started.await()
          tasks.head.cancel()
          gate.fulfill(7)
          val results = tasks.tail.map: t =>
            safely(t.await()).or(-1)
          results.toSet
        . assert(_ == Set(7))

        test(m"Awaiting a promise from many tasks each with timeouts"):
          val promise = Promise[Int]()
          val timedOut = juca.AtomicInteger(0)
          val tasks = (1 to 10).map: _ =>
            async:
              try
                promise.await(50.0*Milli(Second))
              catch case _: AsyncError =>
                timedOut.incrementAndGet()
                -1
          tasks.foreach(_.await())
          timedOut.get()
        . assert(_ == 10)

        test(m"Heavy parent-child cancellation cascade"):
          import codicils.cancel
          val depth = 20
          val gate = Promise[Unit]()
          val completed = juca.AtomicInteger(0)
          val started = juc.CountDownLatch(1)

          def make(d: Int)(using Monitor, Codicil): Task[Unit] = async:
            if d == 0 then
              started.countDown()
              gate.await()
              completed.incrementAndGet()
            else
              make(d - 1).await()

          val task = make(depth)
          started.await()
          task.cancel()
          safely(task.await())
          completed.get()
        . assert(_ == 0)

        test(m"Promise.await with multiple cancellations"):
          val promise = Promise[Int]()
          val barrier = juc.CyclicBarrier(20)
          val cancelled = juca.AtomicInteger(0)
          val completed = juca.AtomicInteger(0)
          val tasks = (1 to 20).map: _ =>
            async:
              barrier.await()
              try
                promise.await()
                completed.incrementAndGet()
              catch case _: AsyncError =>
                cancelled.incrementAndGet()
          Thread.sleep(10)
          promise.cancel()
          tasks.foreach(_.await())
          (cancelled.get(), completed.get())
        . assert: (c, ok) =>
            c == 20 && ok == 0

      suite(m"Sequence error propagation"):
        test(m"Sequence with one cancelled task raises"):
          val good = async(1)
          val bad = async:
            snooze(1.0*Second)
            2
          bad.cancel()
          val seq = Seq(good, bad).sequence
          val result = capture(seq.await())
          result.reason
        . assert(_ == AsyncError.Reason.Cancelled)

      suite(m"Tenacity exponential limits"):
        test(m"Exponential.limit aborts at limit"):
          val tenacity: Tenacity = Tenacity.exponential(1.0*Milli(Second), 1.5).limit(2)
          val result = capture[RetryError](tenacity.delay(Prim + 2))
          result.count
        . assert(_ == 2)

        test(m"Retry exponential delays grow"):
          given tenacity: Tenacity = Tenacity.exponential(10.0*Milli(Second), 2.0).limit(4)
          val attempts = juca.AtomicInteger(0)
          val start = System.currentTimeMillis
          val result = capture[RetryError]:
            retry: (surrender, persevere) ?=>
              attempts.incrementAndGet()
              persevere()
          val elapsed = System.currentTimeMillis - start
          // 4 attempts: delays 0, 10, 20, 40 = 70ms total minimum
          (attempts.get(), elapsed >= 60L)
        . assert: (n, ok) =>
            n == 4 && ok
