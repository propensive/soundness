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

import language.experimental.into
import language.experimental.pureFunctions

import anticipation.*
import contingency.*
import digression.*
import mercator.*
import nomenclature.*
import prepositional.*
import vacuous.*

object Task:
  def apply[result, error <: Exception](evaluate: Worker => result, name: Optional[Name[Async]])
    ( using monitor: Monitor, codepoint: Codepoint )
  :   Task[result] { type Error = error } =

    // The body closure may capture stack-scoped capabilities (an error tactic, a `boundary.Label`);
    // that capture is checked at the `async`/`task` entry point and laundered to pure here. A `Task`
    // is itself a `Worker` (a capability), but task handles are freely shared and collected (e.g.
    // `Seq[Task].sequence`), which a tracked `Task^` could not be — so the handle is laundered to a
    // pure `Task` too. The worker remains a supervised child of `monitor`; only the *handle*'s static
    // capture is dropped (the effect capabilities `Tactic`/`Emit` it may use are still tracked).
    val evaluate0: Worker -> result = caps.unsafe.unsafeAssumePure(evaluate)
    inline def name0: Optional[Name[Async]] = name

    caps.unsafe.unsafeAssumePure:
      new Worker(codepoint, monitor) with Task[result]:
        type Result = result
        type Error = error
        def name: Optional[Name[Async]] = name0
        def daemon: Boolean = false
        def evaluate(worker: Worker): Result = evaluate0(worker)

        def await(): result raises (error | AsyncError) = deliver[error]()

        def await[duration: Abstractable across Durations to Long](duration: duration)
        :   result raises (error | AsyncError) =

          deliver[error, duration](duration)


  // `mercator.Monad[Task]` abstracts over `Task` as a *pure* type constructor, but a `Task` is a
  // capability (a `Worker`), so the scope-capturing handles produced by `async`/`bind`/`map` are
  // boxed to the pure `Task` the `Monad` interface demands. The capture is recoverable bookkeeping
  // here — the tasks are bound and awaited within the same `Monitor` scope this given requires.
  given monad: Monitor => Monad[Task] = caps.unsafe.unsafeAssumePure:
    new Monad[Task]:
      def bind[value, value2](value: Task[value])(lambda: value => Task[value2]): Task[value2] =
        caps.unsafe.unsafeAssumePure(value.bind(lambda))

      def point[value](value: value): Task[value] = caps.unsafe.unsafeAssumePure(async(value))

      def apply[value, value2](value: Task[value])(lambda: value => value2): Task[value2] =
        caps.unsafe.unsafeAssumePure(value.map(lambda))

  extension [result](tasks: Seq[Task[result]])
    def sequence(using Monitor): Task[Seq[result]] emits AsyncError =
      async(tasks.map(_.join()))

  extension [result](tasks: Iterable[Task[result]])
    def race()(using Monitor): result raises AsyncError =
      val promise: Promise[result] = Promise()

      tasks.foreach: task =>
        task.map(promise.offer(_))
        ()

      try promise.await() finally tasks.foreach(_.cancel())

// A task carries the error type its body may raise as the `Error` member, refined by the `emits`
// alias (`Task[result] emits error` = `Task[result] { type Error <: error }`). It is preserved to
// `await`, where it is delivered through the caller's in-scope `Tactic`. `AsyncError` (cancellation
// or timeout) is always a possible outcome, so it is added at the `await` site, not the member.
trait Task[+result]:
  type Error <: Exception

  def ready: Boolean
  def attend(): Unit
  def cancel(): Unit

  def await(): result raises (Error | AsyncError)

  def await[duration: Abstractable across Durations to Long](duration: duration)
  :   result raises (Error | AsyncError)

  // The raw join, for parasite-internal combinators that do not track the error type.
  protected[parasite] def join(): result raises AsyncError

  protected[parasite] def join[duration: Abstractable across Durations to Long](duration: duration)
  :   result raises AsyncError

  def bind[result2](lambda: result => Task[result2])(using Monitor)
  :   Task[result2] emits AsyncError

  def map[result2](lambda: result => result2)(using Monitor)
  :   Task[result2] emits AsyncError
