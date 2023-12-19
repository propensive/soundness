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
import vacuous.*
import turbulence.*
import profanity.*
import escapade.*
import guillotine.*
import spectacular.*
import eucalyptus.*, logging.pinned
import gossamer.*
import ambience.*
import hieroglyph.*, textWidthCalculation.uniform

import scala.collection.mutable as scm

//import language.experimental.captureChecking

case class SuggestionsState
    (suggestions: Map[Argument, () => List[Suggestion]], explanation: Optional[Text], known: Set[Flag[?]],
        present: Set[Flag[?]])

case class CliCompletion
    (fullArguments: List[Argument], arguments: List[Argument], environment: Environment,
        workingDirectory: WorkingDirectory, shell: Shell, currentArgument: Int, focusPosition: Int,
        stdio: Stdio, signals: LazyList[Signal])
    (using interpreter: CliInterpreter)
extends Cli:
  private lazy val parameters: interpreter.Parameters = interpreter.interpret(arguments)

  locally:
    import logging.pinned
    Log.warn(t"Initializing CliCompletion")

  val flags: scm.HashMap[Flag[?], Suggestions[?]] = scm.HashMap()
  val seenFlags: scm.HashSet[Flag[?]] = scm.HashSet()
  var explanation: Optional[Text] = Unset
  var cursorSuggestions: List[Suggestion] = Nil

  def readParameter
      [OperandType]
      (flag: Flag[OperandType])
      (using FlagInterpreter[OperandType], Suggestions[OperandType])
      : Optional[OperandType] =

    given Cli = this
    parameters.read(flag)

  def focus: Argument = arguments(currentArgument)

  override def register(flag: Flag[?], suggestions: Suggestions[?]): Unit =
    parameters.focusFlag.let: argument =>
      if flag.matches(argument) && currentArgument == argument.position + 1 then
        val allSuggestions = suggestions.suggest().to(List)
        if allSuggestions != Nil then cursorSuggestions = allSuggestions

    if !flag.secret then flags(flag) = suggestions
  
  override def present(flag: Flag[?]): Unit = if !flag.repeatable then seenFlags += flag
  override def explain(update: (previous: Optional[Text]) ?=> Optional[Text]): Unit =
    explanation = update(using explanation)
  
  override def suggest(argument: Argument, update: (previous: List[Suggestion]) ?=> List[Suggestion]) =
    if argument == focus then cursorSuggestions = update(using cursorSuggestions)
  
  def flagSuggestions(longOnly: Boolean): List[Suggestion] =
    (flags.keySet.to(Set) -- seenFlags.to(Set)).to(List).flatMap: flag =>
      val allFlags = (flag.name :: flag.aliases)
      
      if longOnly then
        allFlags.collect { case text: Text => text }.match
          case main :: aliases =>
            List(Suggestion(Flag.serialize(main), flag.description, aliases = aliases.map(Flag.serialize(_))))
          case Nil => Nil
      
      else List(Suggestion(Flag.serialize(flag.name), flag.description, aliases =
          flag.aliases.map(Flag.serialize(_))))

  def serialize: List[Text] =
    val items = if cursorSuggestions.isEmpty && parameters.focusFlag.absent then flagSuggestions(focus().starts(t"--")) else cursorSuggestions
    
    shell match
      case Shell.Zsh =>
        val title = explanation.let { explanation => List(sh"'' -X $explanation") }.or(Nil)
        
        lazy val width = items.map(_.text.length).max
        lazy val aliasesWidth = items.map(_.aliases.join(t" ").length).max + 1
        
        val itemLines: List[Command] = items.flatMap:
          case Suggestion(text, description, hidden, incomplete, aliases) =>
            val hiddenParam = if hidden then sh"-n" else sh""
            val aliasText = aliases.join(t" ").fit(aliasesWidth)
            
            val mainLine = (description: @unchecked) match
              case Unset =>
                sh"'' $hiddenParam -- $text"
              
              case description: Text =>
                sh"'${text.fit(width)} $aliasText -- $description' -d desc -l $hiddenParam -- $text"
              
              case description: Output =>
                sh"'${text.fit(width)} $aliasText -- ${description.render}' -d desc -l $hiddenParam -- $text"
            
            val aliasLines = aliases.map: text =>
              (description: @unchecked) match
                case Unset             =>
                  sh"'' -n -- $text"
                
                case description: Text =>
                  sh"'${text.fit(width)} $aliasText -- $description' -d desc -l -n -- $text"
                
                case description: Output =>
                  sh"'${text.fit(width)} $aliasText -- ${description.render}' -d desc -l -n -- $text"
            
            mainLine :: aliasLines
        
        (title ++ itemLines).map(_.arguments.join(t"\t"))
            
      case Shell.Bash =>
        items.filter(!_.hidden).flatMap: suggestion =>
          suggestion.text :: suggestion.aliases
        .filter(_.starts(focus()))
      
      case Shell.Fish =>
        items.flatMap:
          case Suggestion(text, description, hidden, incomplete, aliases) =>
            (text :: aliases).map: text =>
              (description: @unchecked) match
                case Unset               => t"$text"
                case description: Text   => t"$text\t$description"
                case description: Output => t"$text\t${description.plain}"
      
case class Execution(execute: CliInvocation => ExitStatus)

def execute(block: Effectful ?=> CliInvocation ?=> ExitStatus): Execution = Execution(block(using ###)(using _))

def explain(explanation: (previous: Optional[Text]) ?=> Optional[Text])(using cli: Cli): Unit =
  cli.explain(explanation)

package executives:
  given completions(using handler: UnhandledErrorHandler): Executive with
    type CliType = Cli
    type Return = Execution

    def cli
        (arguments: Iterable[Text], environment: Environment, workingDirectory: WorkingDirectory,
            stdio: Stdio, signals: LazyList[Signal])(using interpreter: CliInterpreter): Cli =
      arguments match
        case t"{completions}" :: shellName :: As[Int](focus) :: As[Int](position) :: t"--" :: command :: rest =>
          val shell = shellName match
            case t"zsh"  => Shell.Zsh
            case t"fish" => Shell.Fish
            case _       => Shell.Bash
          
          Log.info(t"shellName=$shellName focus=$focus position=$position")
          
          CliCompletion(Cli.arguments(arguments, focus - 1, position), Cli.arguments(rest, focus - 1, position), environment,
              workingDirectory, shell, focus - 1, position, stdio, signals)
          
        case other =>
          CliInvocation(Cli.arguments(arguments), environment, workingDirectory, stdio, signals)
      
    def process(cli: Cli)(execution: Cli ?=> Execution): ExitStatus = (cli: @unchecked) match
      case completion: CliCompletion =>
        completion.serialize.foreach(Out.println(_)(using completion.stdio))
        ExitStatus.Ok

      case invocation: CliInvocation =>
        handler.handle(execution(using invocation).execute(invocation))(using invocation.stdio)
