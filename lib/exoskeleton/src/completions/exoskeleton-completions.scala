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
import denominative.*
import distillate.*
import ethereal.*
import eucalyptus.*
import fulminate.*
import gossamer.*
import guillotine.*
import parasite.*
import profanity.*
import proscenium.*
import rudiments.*
import spectacular.*
import symbolism.*
import turbulence.*
import vacuous.*

def execute(block: Effectful ?=> Invocation ?=> Exit)(using cli: Cli): Execution =
  cli.absolve match
    case completion: Completion => Execution(Exit.Ok)
    case invocation: Invocation => Execution(block(using !!)(using invocation))

def explain(explanation: (prior: Optional[Text]) ?=> Optional[Text])(using cli: Cli): Unit =
  cli.explain(explanation)

package executives:
  given completions: (backstop: Backstop) => Executive:
    type Interface = Cli
    type Return = Execution

    def invocation
         (arguments:        Iterable[Text],
          environment:      Environment,
          workingDirectory: WorkingDirectory,
          stdio:            Stdio,
          signals:          Spool[Signal],
          service:          ShellContext,
          login:            Login)
         (using interpreter: Interpreter)
    : Cli =

        arguments match
          case t"{completions}" :: shellName :: As[Int](focus0) :: As[Int](position) :: tty :: t"--"
               :: command
               :: rest =>

            val shell = shellName match
              case t"zsh"  => Shell.Zsh
              case t"fish" => Shell.Fish
              case _       => Shell.Bash

            val focus = if shell == Shell.Zsh then focus0 - 1 else focus0

            val tab = Completions.tab(tty, Completions.Tab(arguments.to(List), focus - 1, position))

            Completion
             (Cli.arguments(arguments, focus - 1, position, tab),
              Cli.arguments(rest, focus - 1, position, tab),
              environment,
              workingDirectory,
              shell,
              focus - 1,
              position,
              stdio,
              signals,
              tty,
              tab,
              login)
            . tap(Completions.request(_))

          case t"{admin}" :: command :: Nil =>
            given Stdio = stdio
            command match
              case t"pid"     => Out.println(OsProcess().pid.value.show) yet Exit.Ok
              case t"kill"    => java.lang.System.exit(0) yet Exit.Ok

              case t"await"   =>
                Out.println(t"Awaiting up to 60 seconds for the next tab completion request...")
                Completions.prepare()

                Completions.awaitRequest().or(Nil).map: argument =>
                  Out.println(t"<- ${argument.inspect}")

                Completions.awaitResponse().or(Nil).map: completion =>
                  Out.println(completion.cut('\u0000').map(t"["+_+t"]").join(t"-> ", t" ", t""))

                Exit.Ok

              case t"install" =>
                given ShellContext = service
                given WorkingDirectory = workingDirectory
                import errorDiagnostics.stackTraces
                import logging.silent
                Out.println(Completions.ensure(force = true).join(t"\n"))
                Exit.Ok

              case _       =>
                Exit.Fail(1)

            Invocation
             (Cli.arguments(arguments), environment, workingDirectory, stdio, signals, false, login)

          case other =>
            Invocation
             (Cli.arguments(arguments), environment, workingDirectory, stdio, signals, true, login)


    def process(cli: Cli)(execution: Cli ?=> Execution): Exit = cli.absolve match
      case completion: Completion =>
        given Stdio = completion.stdio
        completion.serialize.tap(Completions.response(_)).each(Out.println(_))
        Exit.Ok

      case invocation: Invocation =>
        given Stdio = invocation.stdio

        try execution(using invocation).exitStatus
        catch case error: Throwable => backstop.handle(error)(using invocation.stdio)
