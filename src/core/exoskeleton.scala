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

import scala.collection.mutable as scm

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
  def apply(arguments: List[Text], environment: Environment, workingDirectory: WorkingDirectory,
      context: ProcessContext): CommandLine =
    if arguments.headOption == Some(t"{completions}")
    then Completion(arguments, environment, workingDirectory, context)
    else Invocation(arguments, environment, workingDirectory, context)

sealed trait CommandLine:
  def arguments: List[Argument]
  def environment: Environment
  def workingDirectory: WorkingDirectory

case class CompletionContext(shell: Shell, textArguments: List[Text], focus: Int, focusPosition: Int):
  private val suggestionsMap: scm.Map[Int, () => List[Suggestion]] = scm.HashMap()
  private var explanationValue: Maybe[Text] = Unset

  lazy val arguments: List[Argument] = textArguments.zipWithIndex.map: (text, index) =>
    Argument(index, text, if focus == index then focusPosition else Unset)

  def suggest(position: Int, fn: => List[Suggestion]): Unit = suggestionsMap(position) = () => fn
  
  def restrict(position: Int, predicate: Suggestion => Boolean): Unit =
    suggestionsMap(position) = () => suggestionsMap(position)().filter(predicate)
  
  def map(position: Int, fn: Suggestion => Suggestion): Unit =
    suggestionsMap(position) = () => suggestionsMap(position)().map(fn)

  def explain(explanation: Maybe[Text]): Unit =
    explanationValue = explanation
  
  def explanation: Maybe[Text] = explanationValue
  def suggestions(position: Int): List[Suggestion] = suggestionsMap.getOrElse(position, () => Nil)()

case class Completion
    (textArguments: List[Text], environment: Environment, workingDirectory: WorkingDirectory,
        context: ProcessContext)
extends CommandLine:
  
  val completion: CompletionContext = textArguments.to(List) match
    case t"{completions}" :: shell :: focus :: position :: t"--" :: rest =>
      CompletionContext(shell.decodeAs[Shell], rest, focus.s.toInt, position.s.toInt)
    
  def arguments: List[Argument] = completion.arguments

case class Invocation
    (textArguments: List[Text], environment: Environment, workingDirectory: WorkingDirectory,
        context: ProcessContext)
extends CommandLine, Stdio:
  export context.stdio.{out, err, in}

  def listenForSignals(signals: Signal*): LazyList[Signal] = 
    val funnel: Funnel[Signal] = Funnel()
    
    signals.foreach: signal =>
      sm.Signal.handle(sm.Signal(signal.shortName.s), event => funnel.put(signal))
    
    funnel.stream
  
  lazy val arguments: List[Argument] = textArguments.zipWithIndex.map: (text, index) =>
    Argument(index, text, Unset)

abstract class Application:
  protected given environment(using invocation: Invocation): Environment = invocation.environment
  protected given workingDirectory(using invocation: Invocation): WorkingDirectory = invocation.workingDirectory
  
  def invoke(using CommandLine): Execution

  def main(arguments: IArray[Text]): Unit =
    val context: ProcessContext = ProcessContext(Stdio(System.out, System.err, System.in))
    val workingDirectory = unsafely(workingDirectories.default)
    val invocation = Invocation(arguments.to(List), environments.jvm, workingDirectory, context)
    
    invoke(using invocation).execute(invocation) match
      case ExitStatus.Ok           => System.exit(0)
      case ExitStatus.Fail(status) => System.exit(1)

case class Execution(execute: Invocation => ExitStatus)

def execute(block: Effectful ?=> Invocation ?=> ExitStatus): Execution = Execution(block(using ###)(using _))

erased trait Effectful