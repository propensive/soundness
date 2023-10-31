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
import hieroglyph.*, textWidthCalculation.uniform

import scala.collection.mutable as scm

import sun.misc as sm

object ShellInput:
  given decoder: Decoder[ShellInput] = text => valueOf(text.lower.capitalize.s)
  given encoder: Encoder[ShellInput] = _.toString.tt.lower

enum ShellInput:
  case Terminal, Pipe

object CommandLine:
  def apply(fullArguments: List[Text], environment: Environment, workingDirectory: WorkingDirectory,
      context: ProcessContext): CommandLine =
    fullArguments match
      case t"{completions}" :: shellName :: As[Int](focus) :: As[Int](position) :: t"--" :: rest =>
        val shell = shellName match
          case t"zsh"  => Shell.Zsh
          case t"bash" => Shell.Bash
          case t"fish" => Shell.Fish
        
        val arguments = rest.drop(1).padTo(focus, t"").zipWithIndex.map: (text, index) =>
          Argument(index, text, if focus == index then position else Unset)

        Completion(fullArguments, arguments, environment, workingDirectory, context, shell, focus - 1, position).tap(println(_))
      
      case other =>
        val arguments = fullArguments.zipWithIndex.map: (text, index) =>
          Argument(index, text, Unset)
        
        Invocation(arguments, environment, workingDirectory, context)

sealed trait CommandLine:
  def arguments: List[Argument]
  def environment: Environment
  def workingDirectory: WorkingDirectory

  def suggest(position: Int, fn: => List[Suggestion]): Unit = ()
  def restrict(position: Int, fn: Suggestion => Boolean): Unit = ()
  def explanation: Maybe[Text] = Unset
  def suggestions(position: Int): List[Suggestion] = Nil
  def explain[TextType](explanation: Maybe[TextType])(using Printable[TextType]): Unit = ()
  def map(position: Int, fn: Suggestion => Suggestion): Unit = ()

case class Completion
    (fullArguments: List[Text], arguments: List[Argument], environment: Environment,
        workingDirectory: WorkingDirectory, context: ProcessContext, shell: Shell, focus: Int,
        focusPosition: Int)
extends CommandLine:
  private val suggestionsMap: scm.Map[Int, () => List[Suggestion]] = scm.HashMap()
  private var explanationValue: Maybe[Text] = Unset

  override def restrict(position: Int, predicate: Suggestion => Boolean): Unit =
    suggestionsMap(position) = () => suggestionsMap(position)().filter(predicate)
  
  override def map(position: Int, fn: Suggestion => Suggestion): Unit =
    suggestionsMap(position) = () => suggestionsMap(position)().map(fn)

  override def explain[TextType](explanation: Maybe[TextType])(using printable: Printable[TextType]): Unit =
    explanationValue = explanation.mm: explanation =>
      printable.print(explanation)
  
  override def suggest(position: Int, fn: => List[Suggestion]): Unit = suggestionsMap(position) = () => fn
  override def explanation: Maybe[Text] = explanationValue
  override def suggestions(position: Int): List[Suggestion] = suggestionsMap.getOrElse(position, () => Nil)()

  def serialize: List[Text] = shell match
    case Shell.Zsh =>
      val title = explanation.mm { explanation => List(t"\t-X\t$explanation") }.or(Nil)
      
      val items = suggestions(focus)
      val width = items.map(_.text.length).max
      val itemLines = items.map:
        case Suggestion(text, description, hidden, incomplete) =>
          val hiddenParam = if hidden then t"-n\t" else t""
          
          description match
            case Unset             => t"\t$hiddenParam$text"
            case description: Text => t"${text.fit(width)} -- $description\t-l\t$hiddenParam$text"
      
      title ++ itemLines
          
    case Shell.Bash =>
      suggestions(focus).filter(!_.hidden).map:
        case Suggestion(text, _, _, _) => text
    
    case Shell.Fish =>
      suggestions(focus).map:
        case Suggestion(text, description, hidden, incomplete) =>
          description match
            case Unset             => t"$text"
            case description: Text => t"$text\t$description"
      
case class Invocation
    (arguments: List[Argument], environment: Environment, workingDirectory: WorkingDirectory,
        context: ProcessContext)
extends CommandLine, Stdio:
  export context.stdio.{out, err, in}

  def listenForSignals(signals: Signal*): LazyList[Signal] = 
    val funnel: Funnel[Signal] = Funnel()
    
    signals.foreach: signal =>
      sm.Signal.handle(sm.Signal(signal.shortName.s), event => funnel.put(signal))
    
    funnel.stream
  
abstract class Application:
  protected given environment(using invocation: Invocation): Environment = invocation.environment
  protected given workingDirectory(using invocation: Invocation): WorkingDirectory = invocation.workingDirectory
  
  def invoke(using CommandLine): Execution

  def main(textArguments: IArray[Text]): Unit =
    val context: ProcessContext = ProcessContext(Stdio(System.out, System.err, System.in))
    val workingDirectory = unsafely(workingDirectories.default)
    
    val arguments = textArguments.to(List).zipWithIndex.map: (text, index) =>
      Argument(index, text, Unset)

    val invocation = Invocation(arguments, environments.jvm, workingDirectory, context)
    
    invoke(using invocation).execute(invocation) match
      case ExitStatus.Ok           => System.exit(0)
      case ExitStatus.Fail(status) => System.exit(1)

case class Execution(execute: Invocation => ExitStatus)

def execute(block: Effectful ?=> Invocation ?=> ExitStatus): Execution = Execution(block(using ###)(using _))

erased trait Effectful