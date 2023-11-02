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

import sun.misc as sm

def makeCli(fullArguments: List[Text], environment: Environment, workingDirectory: WorkingDirectory,
    context: ProcessContext): Cli =
  fullArguments match
    case t"{completions}" :: shellName :: As[Int](focus) :: As[Int](position) :: t"--" :: rest =>
      val shell = shellName match
        case t"zsh"  => Shell.Zsh
        case t"bash" => Shell.Bash
        case t"fish" => Shell.Fish
      
      val arguments = rest.drop(1).padTo(focus, t"").zipWithIndex.map: (text, index) =>
        Argument(index, text, if focus == index then position else Unset)

      CliCompletion(fullArguments, arguments, environment, workingDirectory, context, shell, focus - 1, position)
    
    case other =>
      val arguments = fullArguments.zipWithIndex.map: (text, index) =>
        Argument(index, text, Unset)
      
      CliInvocation(arguments, environment, workingDirectory, context)

object ShellInput:
  given decoder: Decoder[ShellInput] = text => valueOf(text.lower.capitalize.s)
  given encoder: Encoder[ShellInput] = _.toString.tt.lower

enum ShellInput:
  case Terminal, Pipe

case class SuggestionsState
    (suggestions: Map[Argument, () => List[Suggestion]], explanation: Maybe[Text], checkedFlags: Set[Flag[?]],
        seenFlags: Set[Flag[?]])

case class CliCompletion
    (fullArguments: List[Text], arguments: List[Argument], environment: Environment,
        workingDirectory: WorkingDirectory, context: ProcessContext, shell: Shell, currentArgument: Int,
        focusPosition: Int)
extends Cli:
  type State = SuggestionsState
  protected def initialState: SuggestionsState = SuggestionsState(Map(), Unset, Set(), Set())

  private[exoskeleton] def updateSuggestions
      (argument: Argument, transform: List[Suggestion] => List[Suggestion]): Unit =
    
    this() = apply().copy(suggestions = apply().suggestions.updated(argument, () =>
        transform(apply().suggestions.getOrElse(argument, () => Nil)())))
  
  private[exoskeleton] def updateExplanation(transform: Maybe[Text] => Maybe[Text]): Unit =
    this() = apply().copy(explanation = transform(apply().explanation))

  def focus: Argument = arguments(currentArgument)

  def suggestions(argument: Argument = focus): List[Suggestion] =
    apply().suggestions.get(argument).map(_()).getOrElse(Nil)
  
  def explanation: Maybe[Text] = apply().explanation
  
  
  // override def flagSuggestions(longOnly: Boolean): List[Suggestion] =
  //   (state.checkedFlags -- seenFlags).to(List).flatMap: flag =>
  //     val allFlags = (flag.name :: flag.aliases)
  //     if longOnly then
  //       allFlags.collect { case text: Text => text }.match
  //         case main :: aliases =>
  //           List(Suggestion(Flag.serialize(main), flag.description, aliases = aliases.map(Flag.serialize(_))))
  //         case Nil => Nil
      
  //     else List(Suggestion(Flag.serialize(flag.name), flag.description, aliases = flag.aliases.map(Flag.serialize(_))))

  // override def explain[TextType](explanation: Maybe[TextType])(using printable: Printable[TextType]): Unit =
  //   explanationValue = explanation.mm: explanation =>
  //     printable.print(explanation)
  
  //override def suggest(flag: Flag[?]): Unit = if !flag.secret then checkedFlags += flag
  
  // override def suggest
  //     [OperandType]
  //     (subcommand: Subcommand[OperandType])
  //     (using suggestions: Suggestions[OperandType])
  //     : Unit =
  //   suggestionsMap(subcommand.position) = () => suggestions.suggest().to(List)
  
  //override def acknowledge(flag: Flag[?]): Unit = if !flag.repeatable then seenFlags += flag
  
  // override def suggest(argument: Argument, update: (previous: List[Suggestion]) ?=> List[Suggestion]): Unit =
  //   updateSuggestions(argument, update(using _))
  
  //override def explanation: Maybe[Text] = explanationValue
  //override def suggestions(argument: Argument): List[Suggestion] = suggestionsArray(argument.position)()

  def serialize: List[Text] = shell match
    case Shell.Zsh =>
      val title = explanation.mm { explanation => List(t"\t-X\t$explanation") }.or(Nil)
      val items = suggestions(focus)
      val width = items.map(_.text.length).max
      val aliasesWidth = items.map(_.aliases.join(t" ").length).max
      
      val itemLines = items.flatMap:
        case Suggestion(text, description, hidden, incomplete, aliases) =>
          val hiddenParam = if hidden then t"-n\t" else t""
          val aliasText = aliases.join(t" ").fit(aliasesWidth)
          
          val mainLine = description match
            case Unset             => t"\t$hiddenParam--\t$text"
            case description: Text => t"${text.fit(width)} $aliasText -- $description\t-l\t$hiddenParam--\t$text"
          
          val aliasLines = aliases.map: text =>
            description match
              case Unset             => t"\t-n\t--\t$text"
              case description: Text => t"${text.fit(width)} $aliasText -- $description\t-l\t-n\t--\t$text"
          
          mainLine :: aliasLines
      
      title ++ itemLines
          
    case Shell.Bash =>
      suggestions(focus).filter(!_.hidden).flatMap: suggestion =>
        suggestion.text :: suggestion.aliases
    
    case Shell.Fish =>
      suggestions(focus).flatMap:
        case Suggestion(text, description, hidden, incomplete, aliases) =>
          (text :: aliases).map: text =>
            description match
              case Unset             => t"$text"
              case description: Text => t"$text\t$description"
      
case class CliInvocation
    (arguments: List[Argument], environment: Environment, workingDirectory: WorkingDirectory,
        context: ProcessContext)
extends Cli, Stdio:
  export context.stdio.{out, err, in}
  
  type State = Unit
  def initialState: Unit = ()

  def listenForSignals(signals: Signal*): LazyList[Signal] = 
    val funnel: Funnel[Signal] = Funnel()
    
    signals.foreach: signal =>
      sm.Signal.handle(sm.Signal(signal.shortName.s), event => funnel.put(signal))
    
    funnel.stream
  
abstract class Application:
  protected given environment(using invocation: CliInvocation): Environment = invocation.environment
  protected given workingDirectory(using invocation: CliInvocation): WorkingDirectory = invocation.workingDirectory
  
  def invoke(using Cli): Execution

  def main(textArguments: IArray[Text]): Unit =
    val context: ProcessContext = ProcessContext(Stdio(System.out, System.err, System.in))
    val workingDirectory = unsafely(workingDirectories.default)
    
    val arguments = textArguments.to(List).zipWithIndex.map: (text, index) =>
      Argument(index, text, Unset)

    val invocation = CliInvocation(arguments, environments.jvm, workingDirectory, context)
    
    invoke(using invocation).execute(invocation) match
      case ExitStatus.Ok           => System.exit(0)
      case ExitStatus.Fail(status) => System.exit(1)

case class Execution(execute: CliInvocation => ExitStatus)

def execute(block: Effectful ?=> CliInvocation ?=> ExitStatus): Execution = Execution(block(using ###)(using _))

erased trait Effectful

extension (argument: Argument)(using cli: Cli)
  def suggest(suggestions: (previous: List[Suggestion]) ?=> List[Suggestion]): Unit = cli match
    case cli: CliCompletion => cli.updateSuggestions(argument, suggestions(using _))
    case _                  => ()
  
def explain(explanation: (previous: Maybe[Text]) ?=> Maybe[Text])(using cli: Cli): Unit = cli match
  case cli: CliCompletion => cli.updateExplanation(explanation(using _))
  case _                  => ()
