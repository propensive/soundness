/*
    Parasite, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

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

import probably.*
import rudiments.*
import digression.*
import gossamer.*
import turbulence.*
import contingency.*
import anticipation.*, timeApi.long

import errorHandlers.throwUnsafely

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
          val funnel: Funnel[Text] = Funnel()
          val stream = funnel.stream
          def put(message: Text): Unit = funnel.put(message)
          def waitFor(value: Text): Unit = stream.find(_ == value)
        
        test(t"Ordered messages"):
          val bus = Bus()
          
          val alpha = Async:
            bus.waitFor(t"alpha")
            bus.put(t"delta")
            bus.waitFor(t"alpha2")
            bus.put(t"beta")
            bus.waitFor(t"alpha3")
            bus.put(t"final")
            bus.waitFor(t"final")
            t"ALPHA"
          
          val beta = Async:
            bus.waitFor(t"beta")
            bus.put(t"gamma")
            bus.waitFor(t"final")
            t"BETA"
          
          val gamma = Async:
            bus.waitFor(t"gamma")
            bus.put(t"alpha3")
            bus.waitFor(t"final")
            t"GAMMA"
          
          val delta = Async:
            bus.waitFor(t"delta")
            bus.put(t"alpha2")
            bus.waitFor(t"final")
            t"DELTA"
          
          bus.put(t"alpha")
          Set(alpha.await(), beta.await(), gamma.await(), delta.await())
        .assert(_ == Set(t"ALPHA", t"BETA", t"GAMMA", t"DELTA"))
      
        test(t"Race test"):
          val bus = Bus()
          val task1 = Async:
            bus.waitFor(t"task1")
            t"TASK1"
          
          val task2 = Async:
            bus.waitFor(t"task2")
            t"TASK2"
          
          val task3 = Async.race(Vector(task1, task2))
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
        .assert(_ == CancelError())
    
      suite(t"Asyncs"):
        test(t"Simple task produces a result"):
          val task = Async(100)
          task.await()
        .assert(_ == 100)
        
        test(t"Mapped task"):
          val task = Async(100)
          task.map(_ + 1).await()
        .assert(_ == 101)
        
        test(t"FlatMapped task"):
          val task = Async(100)
          task.flatMap: x =>
            Async(x + 1)
          .await()
        .assert(_ == 101)
        
        // test(t"Async name"):
        //   val task = Async(100)
        //   task.id
        // .assert(_ == t"/simple")
        
        // test(t"Subtask name"):
        //   var name: Option[Text] = None
        //   val task = Async:
        //     val inner = Async(100)
        //     name = Some(inner.id)
        //     inner.await()
        //     200
        //   task.await()
        //   name
        // .assert(_ == Some(t"/simple/inner"))
      
        test(t"Async creates one new thread"):
          val threads = Thread.activeCount
          var insideThreads = 0
          val task = Async:
            insideThreads = Thread.activeCount
          task.await()
          insideThreads - threads
        .assert(_ == 1)
        
        test(t"Threads do not persist"):
          val threads = Thread.activeCount
          val task = Async:
            sleep(10L)
          task.await()
          threads - Thread.activeCount
        .assert(_ == 0)

        test(t"Sequencing tasks"):
          Seq(Async(3), Async(5), Async(7)).sequence.await()
        .assert(_ == Seq(3, 5, 7))

        test(t"Sequencing tasks run in parallel"):
          var acc: List[Int] = Nil
          val t1 = Async(sleep(40L).also((acc ::= 2)))
          val t2 = Async(sleep(60L).also((acc ::= 3)))
          val t3 = Async(sleep(20L).also((acc ::= 1)))
          List(t1, t2, t3).sequence.await()
          acc
        .assert(_ == List(3, 2, 1))

        test(t"Async can be canceled"):
          var value: Boolean = false
          
          val task = Async:
            sleep(50L)
            acquiesce()
            value = true
          
          task.cancel()
          safely(task.await())
          value
        .assert(_ == false)

        // def fibonacci(a: Long)(using Monitor): Long =
        //   accede()
        //   if a < 2 then 1 else fibonacci(a - 1) + fibonacci(a - 2)
        
        // test(t"Affirmed calculation without interruption does not cancel it"):
        //   val task = Async(fibonacci(30))
        //   //task.cancel()
        //   try task.await() catch case e: CancelError => -1
        // .assert(_ == 1346269)
        
        // test(t"Affirmed calculation with interruption cancels it"):
        //   val task = Async(fibonacci(40))
        //   task.cancel()
        //   capture(task.await())
        // .assert(_ == CancelError())

        test(t"Canceled task cancels child"):
          var value = 1
          val task = Async:
            value = 2
            val task2 = Async:
              sleep(100L)
              value = 3
            task2.await()
          
          sleep(20L)
          task.cancel()
          safely(task.await())
          value
        .assert(_ == 2)

        test(t"Cancel read on slow LazyList"):
          var count = 0
          val ll = LazyList.continually:
            count += 1
            sleep(10L)
          .take(10)

          val task = Async(ll.to(List))
          sleep(15L)
          task.cancel()
          count
        .assert(_ == 2)
