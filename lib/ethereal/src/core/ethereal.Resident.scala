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
import gossamer.*
import guillotine.*
import prepositional.*
import rudiments.*
import serpentine.*
import vacuous.*

// A `Resident` is a *capability*: it holds the daemon's shutdown and broadcast sinks
// and the client's live stdin, scoped to one daemon-client invocation (the 2026-07-06
// service-class ruling; see rep/DECISIONS.md).
abstract class Resident
  ( val pid:        Pid,
    val shutdown:   () => Unit,
    val cliInput:   Terminus,
    val cliOutput:  Terminus,
    val cliError:   Terminus,
    val executable: Path on Local,
    val script:     Text,
    val startTime:  Long,
    val helpThunk:  () => Optional[Help],
    val setMode:    Tty => Unit,
    val run:        (Text, List[Text], Optional[Text]) => Int,
    val invokedAs:  Optional[Text],
    val sizeThunk:  () => Optional[(Int, Int)],
    val umask:      Optional[Umask],
    // The client's descriptors, for galilei: a path naming one (`/dev/stdin`, `/dev/fd/63`)
    // opens the client's, carried over the session, rather than the daemon's own.
    val fdtable: Optional[Fdtable] = Unset,
    // The native bytes of each argument, environment entry or working directory whose text
    // form lost something (`Launcher.Raw`); empty in the ordinary case.
    val raws:    List[Launcher.Raw] = Nil )
extends Entrypoint, Umask.Provider, Fdtable.Provider, caps.ExclusiveCapability:
  // The type of the messages this daemon's clients exchange (`Resident over bus`); left
  // abstract where a daemon exchanges none.
  type Transport <: Matchable

  // Messages broadcast by the daemon's other clients, as they arrive.
  def bus: Chain[Transport]

  // Sends a message to every other client of the daemon.
  def broadcast(message: Transport): Unit

  // `{admin} shutdown`, and any invocation that wants the daemon gone once it has finished.
  override def retire(): Unit = shutdown()

  // The client terminal's current size, as (columns, rows), as the launcher last measured it:
  // at connection, and again with every `WINCH` and `CONT`. `Unset` when the client's output
  // is not a terminal, or its size is unknown.
  def windowSize: Optional[(Int, Int)] = sizeThunk()

  // Run `block` with the client's terminal in cooked (canonical) mode, so the terminal driver
  // provides echo and line editing for a command that just wants to read lines, then put it
  // back into the raw mode the launcher established. A no-op unless stdin is a terminal, and
  // silently ineffective (leaving today's raw-mode behaviour) if the launcher is too old to
  // offer a control channel.
  def cooked[result](block: => result): result = mode(Tty.Canonical)(block)

  // As `cooked`, with nothing echoed: the driver's line editing, for a password.
  def concealed[result](block: => result): result = mode(Tty.Concealed)(block)

  private def mode[result](tty: Tty)(block: => result): result =
    if cliInput != Terminus.Terminal then block else
      setMode(tty)
      try block finally setMode(Tty.Raw)

  // Runs a command on the client's terminal — an editor, a pager, `ssh`, `sudo` — which this
  // process, detached from any terminal, cannot do itself. The launcher runs it with the
  // client's streams and environment, in `pwd` if given, and reports the status it ended with:
  // its exit code, 128 plus the signal it died of, or 127 if it could not be run, which is
  // also the answer when the client's stdin is not a terminal. Output goes to the terminal; to
  // capture a program's output, run it in this process.
  def terminal(command: Text, arguments: List[Text] = Nil, pwd: Optional[Text] = Unset): Int =
    run(command, arguments, pwd)

  // The bytes of the argument at `index`, as the operating system gave them, where its text
  // (`arguments`) could not carry them: a name that is not valid UTF-8, say. `Unset` for an
  // argument its text carries exactly — the ordinary case — so a caller falls back to the text.
  def rawArgument(index: Int): Optional[anticipation.Data] = raw(t"argument", index)

  // As `rawArgument`, for the environment entry at `index` (`NAME=value`).
  def rawEnvironment(index: Int): Optional[anticipation.Data] = raw(t"environment", index)

  // The working directory's bytes, where its text could not carry them.
  def rawWorkingDirectory: Optional[anticipation.Data] =
    raws.seek(_.kind == t"pwd").let(_.bytes)

  private def raw(kind: Text, index: Int): Optional[anticipation.Data] =
    raws.seek: raw =>
      raw.kind == kind && (raw.index match
        case position: Int => position == index
        case _             => false)
    . let(_.bytes)

  // The structured help tree for this command, generated lazily by re-running the application
  // in tab-completion mode. Falls back to a name-only root if the executive cannot generate it.
  def help(): Help = helpThunk().or(Help(script, Unset, Nil, Nil))

  // The help view narrowed to the subcommands this invocation has already matched during
  // dispatch, so that a section of the application only documents itself. Falls back to the
  // full tree when nothing has been matched, or the matched path is absent from it.
  def localHelp()(using cli: Cli): Help = help().local(cli.matches).or(help())

  // The build id of the executable this daemon was started from, to compare with that of a
  // release the application is offered; zero when it was not started by a launcher.
  def buildId: Long = launcherBuildId()

  def started[instant: Instantiable across Instants from Long]: instant =
    instant(startTime)

  def uptime[duration: Instantiable across Durations from Long]: duration =
    duration((jl.System.currentTimeMillis() - startTime).max(0L)*1_000_000L)
