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
┃    Soundness, version 0.41.0.                                                                    ┃
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
package exoskeleton

import soundness.*

import unsafeExceptions.canThrowAny
import classloaders.system
import environments.jre
import systemProperties.jre
import temporaryDirectories.systemProperties
import workingDirectories.jre
import supervisors.global
import logging.silent
import embeddings.automatic
import autopsies.contrastExpectations
import threading.platform

import strategies.throwUnsafely
import parameterInterpretation.posix
import backstops.silent
import errorDiagnostics.stackTraces

object Tests extends Suite(m"Exoskeleton Tests"):
  def run(): Unit =
    val foo: Text = "hello"
    Sandbox(t"abcde").dispatch:
      '{  import executives.completions

          val Alpha = Subcommand("alpha", e"a command to run")
          val Beta = Subcommand("beta", e"another command to run")
          val Gamma = Subcommand("gamma", e"a different command to run")
          val RedHat = Subcommand("red hat", e"Red Hat Linux")
          val Ubuntu = Subcommand("ubuntu", e"Ubuntu")
          val Gentoo = Subcommand("gentoo", e"Gentoo Linux")


          cli:
            arguments match
              case Alpha() :: _ => execute(Exit.Ok)
              case Beta() :: _  => execute(Exit.Ok)
              case Gamma() :: distribution =>
                distribution match
                  case RedHat() :: _ => execute(Exit.Ok)
                  case Ubuntu() :: _ => execute(Exit.Ok)
                  case Gentoo() :: _ => execute(Exit.Ok)
                  case _             => execute(Exit.Ok)

              case _            => execute(Exit.Fail(1))

          t"finished"  }

    . sandbox:
        test(m"Test subcommands on bash"):
          tmux(shell = Shell.Bash)(Tmux.completions(t""))
        . assert(_ == t"alpha  beta   gamma")

        test(m"Test subcommands on zsh"):
          tmux(shell = Shell.Zsh)(Tmux.completions(t""))
        . assert(_ == t"alpha     -- a command to run\nbeta      -- another command to run\ngamma     -- a different command to run")

        test(m"Test subcommands on fish"):
          tmux(shell = Shell.Fish, width = 120)(Tmux.completions(t""))
        . assert(_ == t"alpha  (a command to run)  beta  (another command to run)  gamma  (a different command to run)")

        test(m"Test subcommands with spaces on bash"):
          tmux(shell = Shell.Bash)(Tmux.completions(t"gamma "))
        . assert(_ == t"gentoo   red hat  ubuntu")

        test(m"Test subcommands on zsh"):
          tmux(shell = Shell.Zsh)(Tmux.completions(t"gamma "))
        . assert(_ == t"gentoo      -- Gentoo Linux\nred hat     -- Red Hat Linux\nubuntu      -- Ubuntu")

        test(m"Test subcommands with spaces on fish"):
          tmux(shell = Shell.Fish, width = 120)(Tmux.completions(t"gamma "))
        . assert(_ == t"gentoo  (Gentoo Linux)  red hat  (Red Hat Linux)  ubuntu  (Ubuntu)")
