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

import java.lang as jl

import anticipation.*
import contingency.*
import denominative.*
import digression.*
import fulminate.*
import prepositional.*
import proscenium.*
import symbolism.*
import vacuous.*

package threading:
  given platform: Threading = () => PlatformSupervisor
  given virtual: Threading = () => VirtualSupervisor
  given adaptive: Threading = () => AdaptiveSupervisor

package codicils:
  given await: Codicil = _.delegate(_.attend())
  given cancel: Codicil = _.delegate(_.cancel())

  given panic: Codicil = _.delegate: child =>
    if !child.ready then fulminate.panic(m"asynchronous child task did not complete")

  given fail: Tactic[AsyncError] => Codicil = _.delegate: child =>
    if !child.ready then raise(AsyncError(AsyncError.Reason.Incomplete))

package supervisors:
  given global: Supervisor = PlatformSupervisor.supervisor

package retryTenacities:
  given exponentialForever: Tenacity = Tenacity.exponential(10L, 1.2)
  given exponentialFiveTimes: Tenacity = Tenacity.exponential(10L, 1.2).limit(5)
  given exponentialTenTimes: Tenacity = Tenacity.exponential(10L, 1.2).limit(10)
  given fixedNoDelayForever: Tenacity = Tenacity.fixed(0L)
  given fixedNoDelayFiveTimes: Tenacity = Tenacity.fixed(0L).limit(5)
  given fixedNoDelayTenTimes: Tenacity = Tenacity.fixed(0L).limit(10)

transparent inline def monitor: Monitor = infer[Monitor]

def daemon(using Codepoint)(evaluate: Worker ?=> Unit)(using Monitor, Codicil): Daemon =
  Daemon(evaluate(using _))

def async[result](using Codepoint)(evaluate: Worker ?=> result)(using Monitor, Codicil)
:   Task[result] =

  Task(evaluate(using _), daemon = false, name = Unset)


def task[result](using Codepoint)(name: Text)(evaluate: Worker ?=> result)
  ( using Monitor, Codicil )
:   Task[result] =

  Task(evaluate(using _), daemon = false, name = name)


def relent[result]()(using Worker): Unit = monitor.relent()
def cancel[result]()(using Monitor): Unit = monitor.cancel()


def snooze[duration: Abstractable across Durations to Long](duration: duration)(using Monitor)
:   Unit =

  monitor.snooze(duration)


def delay[generic: Abstractable across Durations to Long](duration: generic)(using Monitor): Unit =
  hibernate(jl.System.currentTimeMillis + duration.generic/1_000_000L)

def sleep[instant: Abstractable across Instants to Long](instant: instant)(using Monitor): Unit =
  monitor.snooze((instant.generic - jl.System.currentTimeMillis)*1_000_000L)


def hibernate[instant: Abstractable across Instants to Long](instant: instant)(using Monitor)
:   Unit =

  while instant.generic > jl.System.currentTimeMillis do sleep(instant.generic)


extension [result](tasks: Seq[Task[result]])
  def sequence(using Monitor, Codicil): Task[Seq[result]] raises AsyncError =
    async(tasks.map(_.await()))

extension [result](tasks: Iterable[Task[result]])
  def race()(using Monitor, Codicil): result raises AsyncError =
    val promise: Promise[result] = Promise()
    tasks.foreach(_.map(promise.offer(_)))

    promise.await()

extension [result](stream: Stream[result])
  def concurrent(using Monitor, Codicil): Stream[result] raises AsyncError =
    if async(stream.nil).await() then Stream() else stream.head #:: stream.tail.concurrent


def supervise[result](block: Monitor ?=> result)(using threading: Threading, codepoint: Codepoint)
:   result raises AsyncError =

  block(using threading.supervisor())


def retry[value](evaluate: (surrender: () => Nothing, persevere: () => Nothing) ?=> value)
  ( using Tenacity, Monitor )
:   value raises RetryError =

  @tailrec
  def recur(attempt: Ordinal): value =
    boundary[Perseverance[value]]: label ?=>
      sleep(summon[Tenacity].delay(attempt).or(abort(RetryError(attempt.n1))))
      def surrender(): Nothing = boundary.break(Perseverance.Surrender)
      def persevere(): Nothing = boundary.break(Perseverance.Persevere)

      Perseverance.Prevail(evaluate(using surrender, persevere))

    . match
      case Perseverance.Surrender      => abort(RetryError(attempt.n1))
      case Perseverance.Prevail(value) => value
      case Perseverance.Persevere      => recur(attempt + 1)

  recur(Prim)


extension [target](value: target)
  def intercept[event](using interceptable: event is Interceptable onto target)
    ( action: (event: event) ?=> Unit )
  :   Hook =

    Hook(interceptable.register(value, action(using _)))
