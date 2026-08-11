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

import scala.caps

import scala.language.experimental.into
import scala.language.experimental.pureFunctions

import anticipation.*
import contingency.*
import fulminate.Hazard
import digression.*
import mercator.*
import nomenclature.*
import prepositional.*
import vacuous.*

object Task:
  def apply[result, error <: Hazard](evaluate: Worker => result, name: Optional[Name[Async]])
    ( using monitor: Monitor^, codepoint: Codepoint, probate: Probate^ )
  :   (Task[result] { type Error = error })^{monitor, probate, evaluate} =

    // The handle is a fresh (`^`) capability — a `Task` IS a `Worker` — so both the handle
    // and its body closure are honestly tracked (D6 ruling, 2026-07-16). Composition over
    // collections of tasks goes through the sealed pure façade (`Task.monad` and the
    // `sequence` extensions below).
    val evaluate0: Worker^ ->{evaluate} result = evaluate
    inline def name0: Optional[Name[Async]] = name

    // The handle retains the monitor, probate and body — the declared result type says so —
    // but the fresh capability the new `Worker` instance itself constitutes cannot flow into
    // that set, so it is laundered away here, once, at the construction site.
    scala.caps.unsafe.unsafeAssumePure:
      new Worker(codepoint, monitor, probate) with Task[result]:
        type Result = result
        type Error = error
        def name: Optional[Name[Async]] = name0
        def daemon: Boolean = false
        def evaluate(worker: Worker): Result = evaluate0(worker)

        def await()(using monitor: Monitor^)
        :   (Tactic[error | Async.Error]^) ?->{this, monitor} result =

          deliver[error]()

        def await[duration: Abstractable across Durations to Long](duration: duration)
          ( using monitor: Monitor^ )
        :   (Tactic[error | Async.Error]^) ?->{this, monitor} result =

          deliver[error, duration](duration)


  // THE PURE FAÇADE (D6 ruling, option c): `mercator.Monad[Task]` abstracts over `Task` as a
  // *pure* type constructor, but a `Task` handle is an honest capability (a `Worker`), so the
  // fresh handles produced by `async`/`bind`/`map` are sealed to the pure `Task` the `Monad`
  // interface demands — once, here, at the composition boundary. The capture is recoverable
  // bookkeeping: the tasks are bound and awaited within the same `Monitor` scope this given
  // requires.
  given monad: (Monitor^, Probate^) => Monad[Task] = caps.unsafe.unsafeAssumePure:
    new Monad[Task]:
      def bind[value, value2](value: Task[value])(lambda: value => Task[value2]): Task[value2] =
        caps.unsafe.unsafeAssumePure(value.bind(lambda))

      def point[value](value: value): Task[value] = caps.unsafe.unsafeAssumePure(async(value))

      def apply[value, value2](value: Task[value])(lambda: value => value2): Task[value2] =
        caps.unsafe.unsafeAssumePure(value.map(lambda))

  // The monadic form of `snooze`: a task which completes after the duration, for composition
  // with `bind`/`map` without suspending the calling strand.
  def sleep[duration: Abstractable across Durations to Long](duration: duration)
    ( using monitor: Monitor^, probate: Probate^, codepoint: Codepoint )
  :   (Task[Unit] emits Async.Error)^{monitor, probate} =

    async(snooze(duration))


  extension [result](tasks: List[Task[result]])
    // Part of the pure façade (see `monad` above): the fresh handle is sealed once here.
    def sequence(using Monitor^, Probate^): Task[List[result]] emits Async.Error =
      caps.unsafe.unsafeAssumePure(async(List.of(tasks.stdlib.map(_.join()))))

  extension [result](tasks: Iterable[Task[result]])
    def race()(using monitor: Monitor^, probate: Probate^)
    :   (Tactic[Async.Error]^) ?->{monitor, probate} result =
      val promise: Promise[result] = Promise()

      tasks.foreach: task =>
        task.map(promise.offer(_))
        ()

      try promise.await() finally tasks.foreach(_.cancel())

// A task carries the error type its body may raise as the `Error` member, refined by the `emits`
// alias (`Task[result] emits error` = `Task[result] { type Error <: error }`). It is preserved to
// `await`, where it is delivered through the caller's in-scope `Tactic`. `Async.Error` (cancellation
// or timeout) is always a possible outcome, so it is added at the `await` site, not the member.
trait Task[+result]:
  type Error <: Hazard

  def ready: Boolean
  def attend()(using Monitor^): Unit
  def cancel(): Unit

  def await()(using monitor: Monitor^): (Tactic[Error | Async.Error]^) ?->{this, monitor} result

  def await[duration: Abstractable across Durations to Long](duration: duration)
    ( using monitor: Monitor^ )
  :   (Tactic[Error | Async.Error]^) ?->{this, monitor} result

  // The raw join, for parasite-internal combinators that do not track the error type.
  protected[parasite] def join()(using monitor: Monitor^)
  :   (Tactic[Async.Error]^) ?->{this, monitor} result

  protected[parasite] def join[duration: Abstractable across Durations to Long](duration: duration)
    ( using monitor: Monitor^ )
  :   (Tactic[Async.Error]^) ?->{this, monitor} result

  def bind[result2](lambda: result => Task[result2])(using monitor: Monitor^, probate: Probate^)
  :   (Task[result2] emits Async.Error)^{this, lambda, monitor, probate}

  def map[result2](lambda: result => result2)(using monitor: Monitor^, probate: Probate^)
  :   (Task[result2] emits Async.Error)^{this, lambda, monitor, probate}
