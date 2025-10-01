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

import strategies.throwUnsafely

object Tests extends Suite(m"Exoskeleton Tests"):
  def run(): Unit =
    val foo: Text = "hello"
    val launcher: Launcher =
      ShellScript(t"bap").dispatch:
        '{  import executives.completions
            import backstops.silent
            import parameterInterpretation.posix
            import threading.platform
            import workingDirectories.jre
            import errorDiagnostics.stackTraces
            import logging.silent

            val Alpha = Subcommand("alpha", e"a command to run")
            val Beta = Subcommand(${foo}, e"a different command to run")

            cli:
              arguments match
                case Alpha() :: _ => execute(Exit.Ok)
                case Beta() :: _  => execute(Exit.Ok)
                case _            => execute(Exit.Fail(1))

            t"finished"  }

    test(m"Test a deployment"):
      launcher.sandbox:
        tmux(shell = Shell.Bash):
          delay(100L)
          enter(t"${tool.command} ")
          delay(200L)
          enter('\t')
          delay(200L)
          enter('\t')
          delay(200L)
          println(screenshot()().trim)

        println(t"---")
        tmux(shell = Shell.Zsh):
          delay(500L)
          enter(t"${tool.command} ")
          delay(200L)
          enter('\t')
          delay(200L)
          println(screenshot()().trim)

        println(t"---")
        tmux(shell = Shell.Fish):
          delay(100L)
          enter(t"${tool.command} ")
          delay(200L)
          enter('\t')
          delay(200L)
          println(screenshot()().trim)

    . assert()
