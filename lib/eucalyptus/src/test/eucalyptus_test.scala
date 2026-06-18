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
package eucalyptus

import soundness.*

import logFormats.untimestampedLogFormat
import probates.cancelProbate
import strategies.throwUnsafely
import threading.platformThreading

import scala.unsafeExceptions.canThrowAny

import java.util.concurrent as juc

object Tests extends Suite(m"Eucalyptus tests"):

  // A `Writable` that captures each formatted line into a blocking queue, so a test can `take` the
  // exact lines the asynchronous writer produced without depending on timing.
  case class Capture(queue: juc.LinkedBlockingQueue[Text] = juc.LinkedBlockingQueue())

  object Capture:
    given writable: Capture is Writable by Text = (capture, stream) => stream.each(capture.queue.put)

  // A `Writable` that consumes each line then throws, to exercise the `onError` path.
  class Boom() extends Exception("boom")
  case class Failing()

  object Failing:
    given writable: Failing is Writable by Text = (failing, stream) => stream.each(_ => throw Boom())

  def run(): Unit = supervise:
    test(m"A Warn-threshold logger drops Fine and Info but keeps Warn and Fail"):
      val capture = Capture()
      given Logger[Any, Message] = Logger(capture, level = Level.Warn)
      Log.fine(m"alpha")
      Log.info(m"beta")
      Log.warn(m"gamma")
      Log.fail(m"delta")
      List(capture.queue.take(), capture.queue.take())

    . assert(_ == List(t"[WARN] gamma\n", t"[FAIL] delta\n"))

    test(m"Two loggers in scope both receive the message"):
      val first = Capture()
      val second = Capture()
      given firstLog: Logger[Any, Message] = Logger(first)
      given secondLog: Logger[Any, Message] = Logger(second)
      Log.info(m"hello")
      List(first.queue.take(), second.queue.take())

    . assert(_ == List(t"[INFO] hello\n", t"[INFO] hello\n"))

    test(m"A write failure is reported to onError"):
      val errors: juc.LinkedBlockingQueue[Text] = juc.LinkedBlockingQueue()
      given Logger[Any, Message] = Logger(Failing(), onError = error => errors.put(error.getMessage.nn.tt))
      Log.info(m"trigger")
      errors.take()

    . assert(_ == t"boom")
