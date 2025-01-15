/*
    Exoskeleton, version 0.26.0. Copyright 2025 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package exoskeleton

import ambience.*
import anticipation.*
import gossamer.*
import profanity.*
import rudiments.*
import turbulence.*
import vacuous.*

def execute(block: Effectful ?=> CliInvocation ?=> Exit)(using cli: Cli): Execution =
  (cli: @unchecked) match
    case completion: CliCompletion => Execution(Exit.Ok)
    case invocation: CliInvocation => Execution(block(using ###)(using invocation))

def explain(explanation: (prior: Optional[Text]) ?=> Optional[Text])(using cli: Cli): Unit =
  cli.explain(explanation)

package executives:
  given (using handler: UnhandledErrorHandler) => Executive as completions:
    type CliType = Cli
    type Return = Execution

    def invocation
       (arguments:        Iterable[Text],
        environment:      Environment,
        workingDirectory: WorkingDirectory,
        stdio:            Stdio,
        signals:          Spool[Signal])
       (using interpreter: CliInterpreter)
            : Cli =
      arguments match
        case t"{completions}" :: shellName :: As[Int](focus) :: As[Int](position) :: t"--" :: command :: rest =>
          val shell = shellName match
            case t"zsh"  => Shell.Zsh
            case t"fish" => Shell.Fish
            case _       => Shell.Bash

          CliCompletion(Cli.arguments(arguments, focus - 1, position), Cli.arguments(rest, focus - 1, position), environment,
              workingDirectory, shell, focus - 1, position, stdio, signals)

        case other =>
          CliInvocation(Cli.arguments(arguments), environment, workingDirectory, stdio, signals)

    def process(cli: Cli)(execution: Cli ?=> Execution): Exit = (cli: @unchecked) match
      case completion: CliCompletion =>
        given Stdio = completion.stdio
        completion.serialize.each(Out.println(_))
        Exit.Ok

      case invocation: CliInvocation =>
        try execution(using invocation).exitStatus
        catch case error: Throwable => handler.handle(error)(using invocation.stdio)
