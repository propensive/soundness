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
import turbulence.*
import profanity.*
import spectacular.*
import gossamer.*
import ambience.*
import hieroglyph.*, textWidthCalculation.uniform

case class SuggestionsState
    (suggestions: Map[Argument, () => List[Suggestion]], explanation: Maybe[Text], known: Set[Flag[?]],
        present: Set[Flag[?]])

case class CliCompletion
    (fullArguments: List[Argument], arguments: List[Argument], environment: Environment,
        workingDirectory: WorkingDirectory, shell: Shell, currentArgument: Int, focusPosition: Int,
        stdio: Stdio, signals: LazyList[Signal])
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
  
  override def register(flag: Flag[?]): Unit =
    if !flag.secret then this() = apply().copy(known = apply().known + flag)
  
  override def present(flag: Flag[?]): Unit =
    if !flag.repeatable then this() = apply().copy(present = apply().present + flag)
  
  override def unknown(argument: Argument): Unit =
    updateSuggestions(argument, _ => flagSuggestions(argument().starts(t"--")))
  
  def flagSuggestions(longOnly: Boolean): List[Suggestion] =
    (this().known -- this().present).to(List).flatMap: flag =>
      val allFlags = (flag.name :: flag.aliases)
      
      if longOnly then
        allFlags.collect { case text: Text => text }.match
          case main :: aliases =>
            List(Suggestion(Flag.serialize(main), flag.description, aliases = aliases.map(Flag.serialize(_))))
          case Nil => Nil
      
      else List(Suggestion(Flag.serialize(flag.name), flag.description, aliases = flag.aliases.map(Flag.serialize(_))))

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
          
          val mainLine = (description: @unchecked) match
            case Unset             => t"\t$hiddenParam--\t$text"
            case description: Text => t"${text.fit(width)} $aliasText -- $description\t-l\t$hiddenParam--\t$text"
          
          val aliasLines = aliases.map: text =>
            (description: @unchecked) match
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
            (description: @unchecked) match
              case Unset             => t"$text"
              case description: Text => t"$text\t$description"
      
case class Execution(execute: CliInvocation => ExitStatus)

def execute(block: Effectful ?=> CliInvocation ?=> ExitStatus): Execution = Execution(block(using ###)(using _))

erased trait Effectful

extension (argument: Argument)(using cli: Cli)
  def suggest(suggestions: (previous: List[Suggestion]) ?=> List[Suggestion]): Unit =
  cli match
    case cli: CliCompletion => cli.updateSuggestions(argument, suggestions(using _))
    case _                  => ()
  
def explain(explanation: (previous: Maybe[Text]) ?=> Maybe[Text])(using cli: Cli): Unit = cli match
  case cli: CliCompletion => cli.updateExplanation(explanation(using _))
  case _                  => ()

package executives:
  given completions: Executive with
    type CliType = Cli
    type Return = Execution

    def cli
        (arguments: Iterable[Text], environment: Environment, workingDirectory: WorkingDirectory,
            stdio: Stdio, signals: LazyList[Signal]): Cli =
      arguments match
        case t"{completions}" :: shellName :: As[Int](focus) :: As[Int](position) :: t"--" :: command :: rest =>
          
          val shell = shellName match
            case t"zsh"  => Shell.Zsh
            case t"fish" => Shell.Fish
            case _       => Shell.Bash
          
          CliCompletion(Cli.arguments(arguments, focus, position), Cli.arguments(rest), environment,
              workingDirectory, shell, focus - 1, position, stdio, signals)
          
        case other =>
          CliInvocation(Cli.arguments(arguments), environment, workingDirectory, stdio, signals)
      
    def process(cli: Cli, execution: Execution): ExitStatus = (cli: @unchecked) match
      case completion: CliCompletion =>
        completion.serialize.foreach(Out.println(_)(using completion.stdio))
        ExitStatus.Ok

      case invocation: CliInvocation =>
        execution.execute(invocation)
