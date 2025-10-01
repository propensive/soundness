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

import ambience.*
import anticipation.*
import contingency.*
import digression.*
import distillate.*
import escapade.*
import fulminate.*
import gossamer.*
import hieroglyph.*, textMetrics.uniform
import prepositional.*
import profanity.*
import rudiments.*
import serpentine.*
import turbulence.*
import vacuous.*

import sun.misc as sm

package backstops:
  given silent: Backstop:
    def handle(error: Throwable)(using Stdio): Exit = error match
      case error: Exception => Exit(1)
      case error: Throwable => Exit(2)

  given genericErrorMessage: Backstop:
    def handle(error: Throwable)(using Stdio): Exit = error match
      case error: Exception =>
        Out.println(t"An unexpected error occurred.")
        Exit(1)

      case error: Throwable =>
        Out.println(t"An unexpected error occurred.")
        Exit(2)

  given exceptionMessage: Backstop:
    def handle(error: Throwable)(using Stdio): Exit = error match
      case error: Exception =>
        Out.println(error.toString.tt)
        Exit(1)

      case error: Throwable =>
        Out.println(error.toString.tt)
        Exit(2)

  given stackTrace: Backstop:
    def handle(error: Throwable)(using Stdio): Exit = error match
      case error: Exception =>
        Out.println(StackTrace(error).teletype)
        Exit(1)

      case error: Throwable =>
        Out.println(StackTrace(error).teletype)
        Exit(2)

package executives:
  given direct: (backstop: Backstop) => Executive:
    type Return = Exit
    type Interface = Invocation


    def invocation
         (arguments:        Iterable[Text],
          environment:      Environment,
          workingDirectory: WorkingDirectory,
          stdio:            Stdio,
          signals:          Spool[Signal],
          service:          ShellContext)
         (using interpreter: Interpreter)
    : Invocation =

        Invocation
         (Cli.arguments(arguments, Unset, Unset, Unset),
          environments.jre,
          workingDirectories.jre,
          stdio,
          signals,
          arguments.size == 0 || arguments.head != t"{admin}")


    def process(invocation: Invocation)(exitStatus: Interface ?=> Exit): Exit =
      try exitStatus(using invocation)
      catch case error: Throwable => backstop.handle(error)(using invocation.stdio)

inline def effectful[result](lambda: (erased Effectful) ?=> result): result =
  lambda(using !![Effectful])

def application(using executive: Executive, interpreter: Interpreter)
   (arguments: Iterable[Text], signals: List[Signal] = Nil)
   (block: Cli ?=> executive.Return)
: Unit =

  val spool: Spool[Signal] = Spool()
  signals.each: signal =>
    sm.Signal.handle(sm.Signal(signal.shortName.s), event => spool.put(signal))

  val context = new ShellContext:
    def script: Path on Linux =
      safely(ProcessHandle.current.nn.info.nn.command.nn.get.nn.tt.decode[Path on Linux])
      . or(panic(m"cannot determine java invocation"))

    def scriptName: Text = script.name

  // FIXME: We shouldn't assume so much about the STDIO. Instead, we should check the environment
  // variables
  val cli =
    executive.invocation
     (arguments,
      environments.jre,
      workingDirectories.jre,
      stdioSources.virtualMachine.ansi,
      spool,
      context)

  System.exit(executive.process(cli)(block)())
