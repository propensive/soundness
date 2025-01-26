/*
    Parasite, version 0.26.0. Copyright 2025 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package parasite

import anticipation.*, timeInterfaces.long
import contingency.*
import digression.*
import gossamer.*
import probably.*
import proscenium.*
import rudiments.*
import turbulence.*

import strategies.throwUnsafely

import threadModels.platform
import asyncTermination.cancel

given Interceptor = (path, error) =>
  println(s"An async exception occurred in ${path.stack}:")
  error.printStackTrace()
  Mitigation.Escalate

object Tests extends Suite(t"Parasite tests"):

  def thread(block: => Unit): () => Unit =
    val thread = new Thread:
      override def run(): Unit = block

    thread.start()

    () => thread.join()

  def run(): Unit =
    supervise:


      suite(t"Convoluted test tests"):
        case class Bus():
          val spool: Spool[Text] = Spool()
          val stream = spool.stream
          def put(message: Text): Unit = spool.put(message)
          def waitFor(value: Text): Unit = stream.find(_ == value)

        test(t"Ordered messages"):
          val bus = Bus()

          val alpha = async:
            bus.waitFor(t"alpha")
            bus.put(t"delta")
            bus.waitFor(t"alpha2")
            bus.put(t"beta")
            bus.waitFor(t"alpha3")
            bus.put(t"final")
            bus.waitFor(t"final")
            t"ALPHA"

          val beta = async:
            bus.waitFor(t"beta")
            bus.put(t"gamma")
            bus.waitFor(t"final")
            t"BETA"

          val gamma = async:
            bus.waitFor(t"gamma")
            bus.put(t"alpha3")
            bus.waitFor(t"final")
            t"GAMMA"

          val delta = async:
            bus.waitFor(t"delta")
            bus.put(t"alpha2")
            bus.waitFor(t"final")
            t"DELTA"

          bus.put(t"alpha")
          Set(alpha.await(), beta.await(), gamma.await(), delta.await())
        .assert(_ == Set(t"ALPHA", t"BETA", t"GAMMA", t"DELTA"))

        test(t"Race test"):
          val bus = Bus()
          val task1 = async:
            bus.waitFor(t"task1")
            t"TASK1"

          val task2 = async:
            bus.waitFor(t"task2")
            t"TASK2"

          val task3 = async(Vector(task1, task2).race())
          bus.put(t"task2")
          sleep(15L)
          bus.put(t"task1")
          task3.await()
        .assert(_ == t"TASK2")

      suite(t"Promises"):
        test(t"New promise is incomplete"):
          val promise = Promise[Int]()
          promise.ready
        .assert(_ == false)

        test(t"Completed promise is ready"):
          val promise = Promise[Int]()
          promise.fulfill(42)
          promise.ready
        .assert(_ == true)

        test(t"Completed promise has correct value"):
          val promise = Promise[Int]()
          promise.fulfill(42)
          promise.await()
        .assert(_ == 42)

        test(t"Promise result can be awaited"):
          val promise = Promise[Int]()
          thread:
            sleep(100L)
            promise.fulfill(42)
          promise.await()
        .assert(_ == 42)

        test(t"Canceled promise contains exception"):
          val promise = Promise[Int]()
          promise.cancel()
          capture(promise.await())
        .assert(_ == AsyncError(AsyncError.Reason.Cancelled))

      suite(t"Asyncs"):
        test(t"Simple task produces a result"):
          val task = async(100)
          task.await()
        .assert(_ == 100)

        test(t"Mapped task"):
          val task = async(100)
          task.map(_ + 1).await()
        .assert(_ == 101)

        test(t"FlatMapped task"):
          val task = async(100)
          task.flatMap: x =>
            async(x + 1)
          .await()
        .assert(_ == 101)

        // test(t"Async name"):
        //   val task = Async(100)
        //   task.id
        // .assert(_ == t"/simple")

        // test(t"Subtask name"):
        //   var name: Option[Text] = None
        //   val task = async:
        //     val inner = async(100)
        //     name = Some(inner.id)
        //     inner.await()
        //     200
        //   task.await()
        //   name
        // .assert(_ == Some(t"/simple/inner"))

        test(t"Async creates one new thread"):
          val threads = Thread.activeCount
          var insideThreads = 0
          val task = async:
            insideThreads = Thread.activeCount
          task.await()
          insideThreads - threads
        .assert(_ == 1)

        test(t"Threads do not persist"):
          val threads = Thread.activeCount
          val task = async:
            sleep(10L)
          task.await()
          threads - Thread.activeCount
        .assert(_ == 0)

        test(t"Sequencing tasks"):
          Seq(async(3), async(5), async(7)).sequence.await()
        .assert(_ == Seq(3, 5, 7))

        test(t"Sequencing tasks run in parallel"):
          var acc: List[Int] = Nil
          val t1 = async(sleep(40L).also((acc ::= 2)))
          val t2 = async(sleep(60L).also((acc ::= 3)))
          val t3 = async(sleep(20L).also((acc ::= 1)))
          List(t1, t2, t3).sequence.await()
          acc
        .assert(_ == List(3, 2, 1))

        test(t"Async can be canceled"):
          var value: Boolean = false

          val task = async:
            sleep(50L)
            relent()
            value = true

          task.cancel()
          safely(task.await())
          value
        .assert(_ == false)

        // def fibonacci(a: Long)(using Monitor): Long =
        //   accede()
        //   if a < 2 then 1 else fibonacci(a - 1) + fibonacci(a - 2)

        // test(t"Affirmed calculation without interruption does not cancel it"):
        //   val task = async(fibonacci(30))
        //   //task.cancel()
        //   try task.await() catch case e: CancelError => -1
        // .assert(_ == 1346269)

        // test(t"Affirmed calculation with interruption cancels it"):
        //   val task = async(fibonacci(40))
        //   task.cancel()
        //   capture(task.await())
        // .assert(_ == CancelError())

        test(t"Canceled task cancels child"):
          println("a")
          var value = 1
          println("b")
          val task = async:
            println("c")
            value = 2

            val task2 = async:
              println("d")
              sleep(100L) // halt
              println("e")
              value = 3
            println("f")

            task2.await() // halt
            println("g")

          println("h")
          sleep(20L)
          println("i")
          task.cancel() // halt
          println("j")
          safely(task.await())
          println("k")
          value
        .assert(_ == 2)

        test(t"Incomplete child is awaited"):
          import asyncTermination.await
          var value = 1
          val task = async:
            value = 2
            val task2 = async:
              sleep(40L)
              value = 3
          sleep(20L)
          task.await()
          value
        .assert(_ == 3)
        println("C")

        test(t"Incomplete child is cancelled"):
          import asyncTermination.cancel
          var value = 1
          val task = async:
            value = 2
            val task2 = async:
              sleep(40L)
              value = 3
          sleep(20L)
          task.await()
          value
        .assert(_ == 2)

        test(t"Cancel read on slow Stream"):
          var count = 0
          val ll = Stream.continually:
            count += 1
            sleep(20L)
          .take(10)

          val task = async(ll.to(List))
          sleep(30L)
          task.cancel()
          count
        .assert(_ == 2)

        test(t"Check that asynchronous exceptions are handled"):
          async:
            sleep(10L)
            async:
              sleep(10L)
              unsafely(throw new Exception("Async Exception"))
            1
          2
        .assert(_ == 2)
