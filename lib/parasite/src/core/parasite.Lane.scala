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

import java.util.concurrent as juc
import java.util.concurrent.atomic as juca

import scala.annotation.*
import scala.caps
import scala.language.experimental.pureFunctions

import anticipation.*
import contingency.*
import digression.*
import rudiments.Memory
import vacuous.*

import unsafeExceptions.canThrowAny

object Lane:
  // A lane named `name`, whose thread is started at once, with a stack of `stack` bytes if
  // that is given.
  def apply(name: Text, stack: Optional[Memory] = Unset): Lane = new Lane(name, stack)

  // What a submitted block left behind: its value, or what it threw.
  private enum Outcome[+result]:
    case Value(value: result)
    case Thrown(throwable: Throwable)

// One thread of its own, to which work is handed, and which does that work one block at a time
// in the order it was submitted. It is for foreign state which may be touched from one thread
// only, such as a compiler and its context: every block submitted to a lane runs on the lane's
// thread, whichever task submits it. A submission is an ordinary `Task`, which waits for its
// block's turn, so it is awaited, cancelled and supervised as any other; cancelling it before
// its turn comes withdraws the block, and once the block has begun it runs to its end. The
// thread is a daemon, so an open lane does not keep the JVM alive; `close` ends it once the
// work already submitted is done. A lane needs a real thread, so it is not for Scala.js.
final class Lane private (name: Text, stack: Optional[Memory]):
  private val queue: juc.LinkedBlockingQueue[Runnable] = juc.LinkedBlockingQueue()
  private val closed: juca.AtomicBoolean = juca.AtomicBoolean(false)

  private val thread: Thread =
    val runnable: Runnable = () => run()
    val thread: Thread = new Thread(null, runnable, name.s, stack.let(_.long).or(0L))
    thread.setDaemon(true)
    thread.start()
    thread

  @tailrec
  private def run(): Unit =
    val next: Runnable = queue.take().nn

    if next ne Lane.this.stop then
      next.run()
      run()

  private val stop: Runnable = () => ()

  // Whether the calling thread is the lane's own.
  def current: Boolean = Thread.currentThread.nn eq thread

  // Runs `block` on the lane's thread, after everything submitted before it, as a task of its
  // result. What the block throws is thrown again in the task, and so is delivered when the
  // task is awaited.
  def submit[result](using Codepoint)
    ( block: () ->{caps.any.only[anticipation.Durable]} result )
    ( using monitor: Monitor^, probate: Probate.Shared )
  :   (Task[result] emits Async.Error)^{block, monitor, probate} =

    async:
      val promise: Promise[Lane.Outcome[result]] = Promise()
      val withdrawn: juca.AtomicBoolean = juca.AtomicBoolean(false)

      // The block is handed to the lane's thread, which runs it while this task, which retains
      // it, waits; the queue's element type cannot say so, so the job is laundered once, here.
      // [transfer] a submitted block handed to the lane's thread through its queue
      val job: Runnable = caps.unsafe.unsafeAssumePure:
        () =>
          if !withdrawn.get then
            promise.offer:
              try Lane.Outcome.Value(block())
              catch case error: Throwable => Lane.Outcome.Thrown(error)

      queue.put(job)

      val outcome: Lane.Outcome[result] =
        try promise.await() finally withdrawn.set(true)

      outcome match
        case Lane.Outcome.Value(value)       => value
        case Lane.Outcome.Thrown(throwable) => throw throwable

  // Ends the lane's thread once the blocks already submitted have run. A block submitted
  // after this is never run, and its task never completes.
  def close(): Unit = if closed.compareAndSet(false, true) then queue.put(stop)
