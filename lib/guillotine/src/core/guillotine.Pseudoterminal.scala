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
package guillotine

import java.io as ji

import scala.language.experimental.pureFunctions

import ambience.*
import anticipation.*
import contingency.*
import gossamer.*
import spectacular.*

object Pseudoterminal:
  given showable: Pseudoterminal is Showable =
    pty => t"${pty.command.show} on ${pty.width}×${pty.height}"

  // A `Job` running on a pseudo-terminal. Its output is everything the child writes to the
  // terminal, standard error included, as a terminal emulator would receive it; its input is what
  // is typed at the terminal; and it can be resized, which delivers `SIGWINCH` to the child.
  class Job[+exec <: Label, result] private[guillotine] (process: PtyProcess)
  extends guillotine.Job[exec, result](process):
    def resize(width: Int, height: Int): Unit = process.resize(width, height)

// A command to be run on a fresh pseudo-terminal of the given size, rather than on pipes, so that
// it finds a terminal on its standard input, output and error, as it would if a person typed it.
// POSIX only: the terminal is allocated with `posix_openpt` through the foreign function API.
case class Pseudoterminal(command: Command, width: Int, height: Int):
  // As `Executable.fork`, with a fresh (`^`) result, and the same environment handling: the child
  // is given exactly the environment's variables where it can enumerate them.
  def fork[result]()(using working: WorkingDirectory, environment: Environment)
    ( using Tactic[Exec.Error], (Exec.Event is Loggable)^ )
  :   Pseudoterminal.Job[command.Exec, result]^ =

    val processBuilder = Command.builder(command.arguments.to(List))

    Log.info(Exec.Event.ProcessStart(command))

    val process =
      try PtyProcess(processBuilder, width, height)
      catch case error: ji.IOException => abort(Exec.Error(command))

    new Pseudoterminal.Job(process)


  def exec[result]()
    ( using computable:  (result is Computable)^,
            working:     WorkingDirectory,
            environment: Environment )
    ( using Tactic[Exec.Error], (Exec.Event is Loggable)^ )
  :   result =

    fork[result]().await()
