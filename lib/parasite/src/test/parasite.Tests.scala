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
┃    Soundness, version 0.37.0.                                                                    ┃
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

import anticipation.*
import contingency.*
import digression.*
import fulminate.*
import gossamer.*
import probably.*
import proscenium.*
import quantitative.*
import rudiments.*
import symbolism.*
import turbulence.*

import strategies.throwUnsafely

import threadModels.virtual
import asyncTermination.cancel

import errorDiagnostics.stackTraces

// given Interceptor = (path, error) =>
//   println(s"An async exception occurred in ${path.stack}:")
//   error.printStackTrace()
//   Mitigation.Escalate

object Tests extends Suite(m"Parasite tests"):

  def thread(block: => Unit): () => Unit =
    val thread = new Thread:
      override def run(): Unit = block

    thread.start()

    () => thread.join()

  def run(): Unit =
    supervise:
      suite(m"Convoluted test tests"):
        case class Bus():
          val spool: Spool[Text] = Spool()
          val stream = spool.stream
          def put(message: Text): Unit = spool.put(message)
          def waitFor(value: Text): Unit = stream.find(_ == value)

        test(m"Ordered messages"):
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

        . assert(_ == Set(t"ALPHA", t"BETA", t"GAMMA", t"DELTA"))

      //   test(m"Race test"):
      //     val bus = Bus()
      //     val task1 = async:
      //       bus.waitFor(t"task1")
      //       t"TASK1"

      //     println("2")
      //     val task2 = async:
      //       bus.waitFor(t"task2")
      //       t"TASK2"

      //     val task3 = async(Vector(task1, task2).race())
      //     bus.put(t"task2")
      //     snooze(20L)
      //     bus.put(t"task1")
      //     task3.await()

      //   .assert(_ == t"TASK2")

      // suite(m"Promises"):
      //   test(m"New promise is incomplete"):
      //     val promise = Promise[Int]()
      //     promise.ready

      //   . assert(_ == false)

      //   test(m"Completed promise is ready"):
      //     val promise = Promise[Int]()
      //     promise.fulfill(42)
      //     promise.ready
      //   .assert(_ == true)

      //   test(m"Completed promise has correct value"):
      //     val promise = Promise[Int]()
      //     promise.fulfill(42)
      //     promise.await()
      //   .assert(_ == 42)

      //   test(m"Promise result can be awaited"):
      //     val promise = Promise[Int]()
      //     thread:
      //       snooze(100L)
      //       promise.fulfill(42)
      //     promise.await()
      //   .assert(_ == 42)

      //   test(m"Canceled promise contains exception"):
      //     val promise = Promise[Int]()
      //     promise.cancel()
      //     capture(promise.await())
      //   .assert(_ == AsyncError(AsyncError.Reason.Cancelled))

      // suite(m"Asyncs"):
      //   test(m"Simple task produces a result"):
      //     val task = async(100)
      //     task.await()
      //   .assert(_ == 100)

      //   test(m"Mapped task"):
      //     val task = async(100)
      //     task.map(_ + 1).await()
      //   .assert(_ == 101)

      //   test(m"FlatMapped task"):
      //     val task = async(100)
      //     task.flatMap: x =>
      //       async(x + 1)
      //     .await()
      //   .assert(_ == 101)

        // test(m"Async name"):
        //   val task = Task(100)
        //   task.id
        // .assert(_ == t"/simple")

      //   // test(m"Subtask name"):
      //   //   var name: Option[Text] = None
      //   //   val task = async:
      //   //     val inner = async(100)
      //   //     name = Some(inner.id)
      //   //     inner.await()
      //   //     200
      //   //   task.await()
      //   //   name
      //   // .assert(_ == Some(t"/simple/inner"))

        // test(m"Threads do not persist"):
        //   val threads = Thread.activeCount
        //   val task = async:
        //     sleep(10L)
        //   task.await()
        //   threads - Thread.activeCount
        // .assert(_ == 0)

        // test(m"Sequencing tasks"):
        //   Seq(async(3), async(5), async(7)).sequence.await()
        // .assert(_ == Seq(3, 5, 7))

        // test(m"Sequencing tasks run in parallel"):
        //   var acc: List[Int] = Nil
        //   val t1 = async(snooze(40L).also((acc ::= 2)))
        //   val t2 = async(snooze(60L).also((acc ::= 3)))
        //   val t3 = async(snooze(20L).also((acc ::= 1)))
        //   List(t1, t2, t3).sequence.await()
        //   acc
        // .assert(_ == List(3, 2, 1))

        // test(m"Async can be canceled"):
        //   var value: Boolean = false

        //   val task = async:
        //     snooze(50L)
        //     relent()
        //     value = true

        //   task.cancel()

        //   safely(task.await())
        //   value
        // .assert(_ == false)

        // def fibonacci(a: Long)(using Monitor): Long =
        //   relent()
        //   if a < 2 then 1 else fibonacci(a - 1) + fibonacci(a - 2)

        // test(m"Affirmed calculation without interruption does not cancel it"):
        //   val task = async(fibonacci(30))
        //   task.cancel()
        //   try task.await() catch case e: CancelError => -1
        // .assert(_ == 1346269)

        // test(m"Affirmed calculation with interruption cancels it"):
        //   val task = async(fibonacci(40))
        //   task.cancel()
        //   capture(task.await())
        // .assert(_ == CancelError())

        test(m"Canceled task cancels child"):
          var value = 1

          val task = async:
            value = 2

            val task2 = async:
              println("pre delay 1: "+java.lang.System.currentTimeMillis())
              delay(1.0*Second) // halt
              println("post delay 1: "+java.lang.System.currentTimeMillis())
              value = 3

            task2.await() // halt
            value = 6
            println("pre delay 2: "+java.lang.System.currentTimeMillis())
            delay(1.0*Second)
            println("post delay 2: "+java.lang.System.currentTimeMillis())
            value = 4

          println("pre snooze: "+java.lang.System.currentTimeMillis())
          snooze(0.2*Second)
          println("post snooze: "+java.lang.System.currentTimeMillis())
          println(t"value = $value")
          task.cancel() // halt
          println(t"value = $value")
          safely(task.await())
          value
        .assert(_ == 2)

        test(m"Incomplete child is awaited"):
          import asyncTermination.await
          var value = 1
          val task = async:
            value = 2
            val task2 = async:
              snooze(40L)
              value = 3
          snooze(20L)
          task.await()
          value
        .assert(_ == 3)
        println("C")

        test(m"Incomplete child is cancelled"):
          import asyncTermination.cancel
          var value = 1
          val task = async:
            value = 2
            val task2 = async:
              snooze(0.05*Second)
              value = 3
          snooze(0.025*Second)
          task.await()
          value
        .assert(_ == 2)

        test(m"Cancel read on slow Stream"):
          var count = 0
          val task = async:
            Stream.continually:
              count += 1
              relent()
              println(java.lang.System.currentTimeMillis())
              snooze(100L)
            . take(10)
            . to(List)

          snooze(300L)
          println("CANCEL")

          println("X")
          task.cancel()
          println("Y")
          count
        .assert(_ == 2)

      //   test(m"Check that asynchronous exceptions are handled"):
      //     async:
      //       sleep(10L)
      //       async:
      //         sleep(10L)
      //         unsafely(throw new Exception("Async Exception"))
      //       1
      //     2
      //   .assert(_ == 2)
