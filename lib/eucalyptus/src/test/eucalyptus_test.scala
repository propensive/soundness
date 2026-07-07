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
┃    Soundness, version 0.63.0.                                                                    ┃
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

import errorDiagnostics.stackTracesDiagnostics
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

  // A `Writable` that `raise`s a `StreamError` for each line, to exercise the typed `handle` path.
  case class Failing()

  object Failing:
    given writable: Emit[StreamError] => Failing is Writable by Text =
      (failing, stream) => stream.each(_ => raise(StreamError(0.b)))

  // An event enum whose cases carry *different* categories. Because the marker traits are
  // transparent, `Log.info(Signal.Net(…))` is typed at the general `Signal`, so the concrete case's
  // category is known only at runtime — exactly what a category-filtered `Logger` must test.
  enum Signal:
    case Net(text: Text) extends Signal, Log.Network
    case Fs(text: Text) extends Signal, Log.Filesystem

  object Signal:
    given communicable: Signal is Communicable =
      case Net(text) => m"net: $text"
      case Fs(text)  => m"fs: $text"

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

    test(m"A write failure is handled by an enclosing handler"):
      val errors: juc.LinkedBlockingQueue[Text] = juc.LinkedBlockingQueue()

      handle:
        case StreamError(_) => errors.put(t"cut")
      . protect:
          given Logger[Any, Message] = Logger(Failing())
          Log.info(m"trigger")
          errors.take()

    . assert(_ == t"cut")

    test(m"A category-filtered logger records only events in its categories"):
      val capture = Capture()
      given Logger[Any, Message] = Logger(capture, categories = Set(Log.Network))
      Log.info(Signal.Net(t"a"))
      Log.info(Signal.Fs(t"b"))
      capture.queue.take()

    . assert(_ == t"[INFO] net: a\n")

    test(m"A logger with no categories records events of every category"):
      val capture = Capture()
      given Logger[Any, Message] = Logger(capture)
      Log.info(Signal.Net(t"a"))
      Log.info(Signal.Fs(t"b"))
      List(capture.queue.take(), capture.queue.take())

    . assert(_ == List(t"[INFO] net: a\n", t"[INFO] fs: b\n"))
