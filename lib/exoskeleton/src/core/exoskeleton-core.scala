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
┃    Soundness, version 0.35.0.                                                                    ┃
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

import ambience.*
import anticipation.*
import digression.*
import escapade.*
import gossamer.*
import hieroglyph.*, textMetrics.uniform
import profanity.*
import rudiments.*
import turbulence.*

import sun.misc as sm

package unhandledErrors:
  given silent: UnhandledErrorHandler:
    def handle(error: Throwable)(using Stdio): Exit = error match
      case error: Exception => Exit(1)
      case error: Throwable => Exit(2)

  given genericErrorMessage: UnhandledErrorHandler:
    def handle(error: Throwable)(using Stdio): Exit = error match
      case error: Exception =>
        Out.println(t"An unexpected error occurred.")
        Exit(1)

      case error: Throwable =>
        Out.println(t"An unexpected error occurred.")
        Exit(2)

  given exceptionMessage: UnhandledErrorHandler:
    def handle(error: Throwable)(using Stdio): Exit = error match
      case error: Exception =>
        Out.println(error.toString.tt)
        Exit(1)

      case error: Throwable =>
        Out.println(error.toString.tt)
        Exit(2)

  given stackTrace: UnhandledErrorHandler:
    def handle(error: Throwable)(using Stdio): Exit = error match
      case error: Exception =>
        Out.println(StackTrace(error).teletype)
        Exit(1)

      case error: Throwable =>
        Out.println(StackTrace(error).teletype)
        Exit(2)

package executives:
  given direct: (handler: UnhandledErrorHandler) => Executive:
    type Return = Exit
    type Interface = CliInvocation


    def invocation
         (arguments:        Iterable[Text],
          environment:      Environment,
          workingDirectory: WorkingDirectory,
          stdio:            Stdio,
          signals:          Spool[Signal])
         (using interpreter: CliInterpreter)
    : CliInvocation =

        CliInvocation
         (Cli.arguments(arguments),
          environments.jre,
          workingDirectories.jre,
          stdio,
          signals)


    def process(cli: CliInvocation)(exitStatus: Interface ?=> Exit): Exit =
      try exitStatus(using cli)
      catch case error: Throwable => handler.handle(error)(using cli.stdio)
      //handler.handle(exitStatus(using cli))(using cli.stdio)

def application(using executive: Executive, interpreter: CliInterpreter)
   (arguments: Iterable[Text], signals: List[Signal] = Nil)
   (block: Cli ?=> executive.Return)
: Unit =

  val spool: Spool[Signal] = Spool()
  signals.each: signal =>
    sm.Signal.handle(sm.Signal(signal.shortName.s), event => spool.put(signal))

  // FIXME: We shouldn't assume so much about the STDIO. Instead, we should check the environment
  // variables
  val cli =
    executive.invocation
     (arguments,
      environments.jre,
      workingDirectories.jre,
      stdioSources.virtualMachine.ansi,
      spool)

  System.exit(executive.process(cli)(block)())
