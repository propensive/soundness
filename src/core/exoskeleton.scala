/*
    Exoskeleton, version [unreleased]. Copyright 2023 Jon Pretty, Propensive OÜ.

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

import anticipation.*
import rudiments.*
import perforate.*
import turbulence.*
import profanity.*
import spectacular.*
import gossamer.*
import ambience.*

import sun.misc as sm

object ShellInput:
  given decoder: Decoder[ShellInput] = text => valueOf(text.lower.capitalize.s)
  given encoder: Encoder[ShellInput] = _.toString.tt.lower

enum ShellInput:
  case Terminal, Pipe

object Shell:
  given decoder: Decoder[Shell] = text => valueOf(text.lower.capitalize.s)
  given encoder: Encoder[Shell] = _.toString.tt.lower

enum Shell:
  case Zsh, Bash, Fish

object CommandLine:
  def apply(arguments: Iterable[Text], environment: Environment, workingDirectory: WorkingDirectory,
      context: ProcessContext): CommandLine =
    if arguments.headOption == Some(t"{completions}")
    then Completion(arguments, environment, workingDirectory, context)
    else Invocation(arguments, environment, workingDirectory, context)

sealed trait CommandLine:
  def arguments: IArray[Argument]
  def environment: Environment
  def workingDirectory: WorkingDirectory

case class CompletionContext(shell: Shell, arguments: IArray[Argument], focus: Int)

case class Completion
    (fullArguments: Iterable[Text], environment: Environment, workingDirectory: WorkingDirectory,
        context: ProcessContext)
extends CommandLine:
  println("TESTING")
  val completion = fullArguments.to(List) match
    case t"{completions}" :: shell :: focus :: position :: t"--" :: rest =>
      println("YES")
      CompletionContext(shell.decodeAs[Shell], Argument.from(rest), focus.s.toInt)
    case _ =>
      println("No way")
      ???
  
  def arguments: IArray[Argument] = completion.arguments

case class Invocation
    (fullArguments: Iterable[Text], environment: Environment, workingDirectory: WorkingDirectory,
        context: ProcessContext)
extends CommandLine, Stdio:
  export context.stdio.{out, err, in}

  lazy val arguments: IArray[Argument] = Argument.from(fullArguments)

  def listenForSignals(signals: Signal*): LazyList[Signal] = 
    val funnel: Funnel[Signal] = Funnel()
    
    signals.foreach: signal =>
      sm.Signal.handle(sm.Signal(signal.shortName.s), event => funnel.put(signal))
    
    funnel.stream

abstract class Application:
  protected given environment(using invocation: Invocation): Environment = invocation.environment
  protected given workingDirectory(using invocation: Invocation): WorkingDirectory = invocation.workingDirectory
  
  def invoke(using CommandLine): Execution

  def main(arguments: IArray[Text]): Unit =
    val context: ProcessContext = ProcessContext(Stdio(System.out, System.err, System.in))
    val invocation = Invocation(arguments, environments.jvm, unsafely(workingDirectories.default), context)
    
    invoke(using invocation).execute(invocation) match
      case ExitStatus.Ok           => System.exit(0)
      case ExitStatus.Fail(status) => System.exit(1)

case class Execution(execute: Invocation => ExitStatus)

def execute(block: Effectful ?=> Invocation ?=> ExitStatus): Execution = Execution(block(using ###)(using _))

erased trait Effectful