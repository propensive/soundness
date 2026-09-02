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

import java.lang as jl
import java.util.concurrent.atomic as juca

import anticipation.*
import nomenclature.*
import prepositional.*

import Async.nominative
import abstractables.epochMillisecondsAbstractable

object Timeout:
  def apply[duration: Abstractable across Durations to Long](timeout0: duration)(action: => Unit)
    ( using monitor: Monitor^, probate: Probate^ )
  :   Timeout^{action, monitor, probate} =

    val timeout = timeout0.generic/1_000_000L

    // The timeout's own watchdog task is supervised bookkeeping recreated on each `reset`; its
    // handle is held only to cancel it, never returned, so it is laundered to pure to avoid
    // threading a per-call `fresh` result through the stored `makeProcess` factory.
    def process(expiry: juca.AtomicLong): Task[Unit] = caps.unsafe.unsafeAssumePure:
      task(n"timeout"):
        while jl.System.currentTimeMillis < expiry.get()
        do sleep(expiry.get())

        expiry.set(Long.MinValue)
        action

    // As for `Task.apply`: the declared result tracks the retained capabilities; the
    // instance's own fresh capability is laundered.
    caps.unsafe.unsafeAssumePure(new Timeout(timeout, process))


class Timeout private(duration: Long, makeProcess: juca.AtomicLong => Task[Unit])
extends caps.ExclusiveCapability:
  private val expiry: juca.AtomicLong = juca.AtomicLong(jl.System.currentTimeMillis + duration)

  @scala.caps.unsafe.untrackedCaptures
  private var process: Task[Unit] = makeProcess(expiry)

  def alive: Boolean = expiry.get() != Long.MinValue

  def nudge(): Unit =
    if expiry.get() != Long.MinValue then expiry.set(jl.System.currentTimeMillis + duration)

  def reset(): Unit = if expiry.get() == Long.MinValue then
    expiry.set(jl.System.currentTimeMillis + duration)
    process.cancel()
    process = makeProcess(expiry)
