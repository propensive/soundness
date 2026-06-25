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
package exoskeleton

import java.lang as jl

import galilei.*
import sun.misc as sm

import ambience.*
import anticipation.*
import contingency.*
import digression.*
import distillate.*
import escapade.*
import fulminate.*
import gossamer.*
import hieroglyph.*, textMetrics.uniformMetric
import prepositional.*
import profanity.*
import rudiments.*
import serpentine.*
import turbulence.*
import vacuous.*

import environments.javaEnvironment
import termcaps.environmentTermcap

package backstops:
  given silentBackstop: Backstop:
    def handle(error: Throwable)(using Stdio): Exit = error match
      case error: Exception => Exit(1)
      case error: Throwable => Exit(2)

  given genericErrorMessageBackstop: Backstop:
    def handle(error: Throwable)(using Stdio): Exit = error match
      case error: Exception =>
        Out.println(t"An unexpected error occurred.")
        Exit(1)

      case error: Throwable =>
        Out.println(t"An unexpected error occurred.")
        Exit(2)

  given exceptionMessageBackstop: Backstop:
    def handle(error: Throwable)(using Stdio): Exit = error match
      case error: Exception =>
        Out.println(error.toString.tt)
        Exit(1)

      case error: Throwable =>
        Out.println(error.toString.tt)
        Exit(2)

  given stackTraceBackstop: Backstop:
    def handle(error: Throwable)(using Stdio): Exit = error match
      case error: Exception =>
        Out.println(StackTrace(error).teletype)
        Exit(1)

      case error: Throwable =>
        Out.println(StackTrace(error).teletype)
        Exit(2)

package executives:
  given directExecutive: (backstop: Backstop) => Executive:
    type Return = Exit
    type Interface = Invocation


    def invocation
      ( arguments:        Iterable[Text],
        environment:      Environment,
        workingDirectory: WorkingDirectory,
        stdio:            Stdio,
        entrypoint:       Entrypoint,
        login:            Login )
      ( using interpreter: Interpreter )
    :   Invocation =

      Invocation
        ( Cli.arguments(arguments, Unset, Unset, Unset),
          environments.javaEnvironment,
          workingDirectories.javaWorkingDirectory,
          stdio,
          arguments.size == 0 || arguments.head != t"{admin}",
          login )


    def process(invocation: Invocation)(exitStatus: Interface ?=> Exit): Exit =
      try exitStatus(using invocation)
      catch case error: Throwable => backstop.handle(error)(using invocation.stdio)

inline def effectful[result](lambda: (erased Effectful) ?=> result): result =
  lambda(using !![Effectful])

inline def trap(handler: PartialFunction[UnixSignal | WindowsSignal, SignalResponse])
  ( using cli: Cli )
:   Unit =

  cli.trap(handler)

def application(using executive: Executive, interpreter: Interpreter, system: System)
  ( arguments: Iterable[Text], signals: List[UnixSignal] = Nil )
  ( block: Cli ?=> executive.Return )
:   Unit =

  val entrypoint = new Entrypoint:
    def executable: Path on Local =
      safely(ProcessHandle.current.nn.info.nn.command.nn.get.nn.tt.decode[Path on Local])
      . or(panic(m"cannot determine java invocation"))

    def script: Text = executable.name

  // FIXME: We shouldn't assume so much about the STDIO. Instead, we should check the environment
  // variables
  val cli =
    executive.invocation
      ( arguments,
        environments.javaEnvironment,
        workingDirectories.javaWorkingDirectory,
        stdios.virtualMachineStdio,
        entrypoint,
        Login(ProcessHandle.current().nn.info().nn.user().nn.get().nn.tt, Unset) )

  signals.each: signal =>
    sm.Signal.handle(sm.Signal(signal.shortName.s), _ => cli.dispatchSignal(signal))

  jl.System.exit(executive.process(cli)(block)())
