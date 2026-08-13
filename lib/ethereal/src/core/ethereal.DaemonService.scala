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
package ethereal

import scala.caps

import galilei.*
import scala.language.experimental.pureFunctions

import java.lang as jl

import anticipation.*
import exoskeleton.*
import guillotine.*
import prepositional.*
import serpentine.*
import vacuous.*

// A `DaemonService` is a *capability*: it holds the daemon's shutdown and broadcast sinks
// and the client's live stdin, scoped to one daemon-client invocation (the 2026-07-06
// service-class ruling; see rep/DECISIONS.md).
case class DaemonService[bus <: Matchable]
  ( pid:        Pid,
    shutdown:   () => Unit,
    cliInput:   Stdin,
    executable: Path on Local,
    deliver:    bus => Unit,
    bus:        Chain[bus],
    script:     Text,
    startTime:  Long,
    helpThunk:  () => Optional[Help],
    setMode:    Tty => Unit )
extends Entrypoint, caps.ExclusiveCapability:
  def broadcast(message: bus): Unit = deliver(message)

  // Run `block` with the client's terminal in cooked (canonical) mode, so the terminal driver
  // provides echo and line editing for a command that just wants to read lines, then put it
  // back into the raw mode the launcher established. A no-op unless stdin is a terminal, and
  // silently ineffective (leaving today's raw-mode behaviour) if the launcher is too old to
  // offer a control channel.
  def cooked[result](block: => result): result =
    if cliInput != Stdin.Terminal then block else
      setMode(Tty.Canonical)
      try block finally setMode(Tty.Raw)

  // The structured help tree for this command, generated lazily by re-running the application
  // in tab-completion mode. Falls back to a name-only root if the executive cannot generate it.
  def help(): Help = helpThunk().or(Help(script, Unset, Nil, Nil))

  def started[instant: Instantiable across Instants from Long]: instant =
    instant(startTime)

  def uptime[duration: Instantiable across Durations from Long]: duration =
    duration((jl.System.currentTimeMillis() - startTime).max(0L)*1_000_000L)
