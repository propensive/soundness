/*
    Parasitism, version [unreleased]. Copyright 2023 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package parasitism

import probably.*
import rudiments.*
import digression.*
import gossamer.*
import anticipation.*, timeApi.long

import unsafeExceptions.canThrowAny

object Tests extends Suite(t"Parasitism tests"):

  def async(fn: => Unit): () => Unit =
    val thread = new Thread:
      override def run(): Unit = fn
    
    thread.start()

    () => thread.join()

  def run(): Unit =
    supervise(t"runner"):
      suite(t"Promises"):
        test(t"New promise is incomplete"):
          val promise = Promise[Int]()
          promise.ready
        .assert(_ == false)

        test(t"Completed promise is ready"):
          val promise = Promise[Int]()
          promise.supply(42)
          promise.ready
        .assert(_ == true)
        
        test(t"Completed promise has correct value"):
          val promise = Promise[Int]()
          promise.supply(42)
          promise.await()
        .assert(_ == 42)

        test(t"Promise result can be awaited"):
          val promise = Promise[Int]()
          async:
            sleep(100)
            promise.supply(42)
          promise.await()
        .assert(_ == 42)

        // There is no longer any way to get a promise without awaiting it
        // test(t"Incomplete promise contains exception"):
        //   val promise = Promise[Int]()
        //   capture(promise.await())
        // .assert(_ == IncompleteError())
        
        test(t"Canceled promise contains exception"):
          val promise = Promise[Int]()
          promise.cancel()
          capture(promise.await())
        .assert(_ == CancelError())
    
      suite(t"Tasks"):
        test(t"Simple task produces a result"):
          val task = Task(t"simple")(100)
          task.await()
        .assert(_ == 100)
        
        test(t"Mapped task"):
          val task = Task(t"simple")(100)
          task.map(_ + 1).await()
        .assert(_ == 101)
        
        test(t"FlatMapped task"):
          val task = Task(t"simple")(100)
          task.flatMap: x =>
            Task(t"next")(x + 1)
          .await()
        .assert(_ == 101)
        
        test(t"Task name"):
          val task = Task(t"simple")(100)
          task.id
        .assert(_ == t"task://runner/simple")
        
        test(t"Subtask name"):
          var name: Option[Text] = None
          val task = Task(t"simple"):
            val inner = Task(t"inner")(100)
            name = Some(inner.id)
            inner.await()
            200
          task.await()
          name
        .assert(_ == Some(t"task://runner/simple/inner"))
      
        test(t"Task creates one new thread"):
          val threads = Thread.activeCount
          var insideThreads = 0
          val task = Task(t"simple"):
            insideThreads = Thread.activeCount
          task.await()
          insideThreads - threads
        .assert(_ == 1)
        
        test(t"Threads do not persist"):
          val threads = Thread.activeCount
          val task = Task(t"simple"):
            sleep(10)
          task.await()
          threads - Thread.activeCount
        .assert(_ == 0)

        test(t"Sequencing tasks"):
          Seq(Task(t"a")(3), Task(t"b")(5), Task(t"c")(7)).sequence.await()
        .assert(_ == Seq(3, 5, 7))

        test(t"Sequencing tasks run in parallel"):
          var acc: List[Int] = Nil
          val t1 = Task(t"a")(sleep(40).tap((acc ::= 2).waive))
          val t2 = Task(t"b")(sleep(60).tap((acc ::= 3).waive))
          val t3 = Task(t"c")(sleep(20).tap((acc ::= 1).waive))
          List(t1, t2, t3).sequence.await()
          acc
        .assert(_ == List(3, 2, 1))

        test(t"Task can be canceled"):
          var value: Boolean = false
          
          val task = Task(t"long"):
            sleep(10)
            value = true
          
          task.cancel()
          safely(task.await())
          value
        .assert(_ == false)

        // def fibonacci(a: Long)(using Monitor): Long =
        //   accede()
        //   if a < 2 then 1 else fibonacci(a - 1) + fibonacci(a - 2)
        
        // test(t"Affirmed calculation without interruption does not cancel it"):
        //   val task = Task(t"fibonacci")(fibonacci(30))
        //   //task.cancel()
        //   try task.await() catch case e: CancelError => -1
        // .assert(_ == 1346269)
        
        // test(t"Affirmed calculation with interruption cancels it"):
        //   val task = Task(t"fibonacci")(fibonacci(40))
        //   task.cancel()
        //   capture(task.await())
        // .assert(_ == CancelError())

        test(t"Canceled task cancels child"):
          var value = 1
          val task = Task(t"outer"):
            value = 2
            val task2 = Task(t"inner"):
              sleep(100)
              value = 3
            task2.await()
          
          sleep(20)
          task.cancel()
          safely(task.await())
          value
        .assert(_ == 2)

        test(t"Cancel read on slow LazyList"):
          var count = 0
          val ll = LazyList.continually:
            count += 1
            sleep(10)
          .take(10)

          val task = Task(t"iterate")(ll.to(List))
          sleep(15)
          task.cancel()
          count
        .assert(_ == 2)
