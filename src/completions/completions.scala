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
import eucalyptus.*, logging.pinned
import profanity.*
import spectacular.*
import gossamer.*
import ambience.*
import hieroglyph.*, textWidthCalculation.uniform

import scala.collection.mutable as scm

case class SuggestionsState
    (suggestions: Map[Argument, () => List[Suggestion]], explanation: Maybe[Text], known: Set[Flag[?]],
        present: Set[Flag[?]])

case class CliCompletion
    (fullArguments: List[Argument], arguments: List[Argument], environment: Environment,
        workingDirectory: WorkingDirectory, shell: Shell, currentArgument: Int, focusPosition: Int,
        stdio: Stdio, signals: LazyList[Signal])
    (using interpreter: CliInterpreter)
extends Cli:
  private lazy val parameters: interpreter.Parameters = interpreter.interpret(arguments)
  val flags: scm.HashMap[Flag[?], Suggestions[?]] = scm.HashMap()
  val seenFlags: scm.HashSet[Flag[?]] = scm.HashSet()
  var explanation: Maybe[Text] = Unset
  var resultSuggestions: List[Suggestion] = Nil

  def readParameter[OperandType](flag: Flag[OperandType])(using FlagInterpreter[OperandType], Suggestions[OperandType]): Maybe[OperandType] =
    given Cli = this
    parameters.read(flag)

  def focus: Argument = arguments(currentArgument)

  override def register(flag: Flag[?], suggestions: Suggestions[?]): Unit =
    parameters.focusOperandFlag.mm: argument =>
      Log.fine(t"focusOperandFlag=${parameters.focusOperandFlag.debug} vs flag=${flag.debug}")
      if flag.matches(argument) then
        Log.fine("matches")
        resultSuggestions = suggestions.suggest().to(List)

    if !flag.secret then flags(flag) = suggestions
  
  override def present(flag: Flag[?]): Unit = if !flag.repeatable then seenFlags += flag
  override def explain(update: (previous: Maybe[Text]) ?=> Maybe[Text]): Unit = explanation = update(using explanation)
  
  def flagSuggestions(longOnly: Boolean): List[Suggestion] =
    (flags.keySet.to(Set) -- seenFlags.to(Set)).to(List).flatMap: flag =>
      val allFlags = (flag.name :: flag.aliases)
      
      if longOnly then
        allFlags.collect { case text: Text => text }.match
          case main :: aliases =>
            List(Suggestion(Flag.serialize(main), flag.description, aliases = aliases.map(Flag.serialize(_))))
          case Nil => Nil
      
      else List(Suggestion(Flag.serialize(flag.name), flag.description, aliases = flag.aliases.map(Flag.serialize(_))))

  def serialize: List[Text] = shell match
    case Shell.Zsh =>
      Log.warn(t"Parameters: ${parameters.debug}")
      val title = explanation.mm { explanation => List(t"\t-X\t$explanation") }.or(Nil)
      
      val items =
        if parameters.focusOperandFlag.unset
        then
          Log.info(t"flagSuggestions")
          flagSuggestions(focus().starts(t"--"))
        else
          Log.info(t"resultSuggestions")
          resultSuggestions
      
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
      resultSuggestions.filter(!_.hidden).flatMap: suggestion =>
        suggestion.text :: suggestion.aliases
    
    case Shell.Fish =>
      resultSuggestions.flatMap:
        case Suggestion(text, description, hidden, incomplete, aliases) =>
          (text :: aliases).map: text =>
            (description: @unchecked) match
              case Unset             => t"$text"
              case description: Text => t"$text\t$description"
      
case class Execution(execute: CliInvocation => ExitStatus)

def execute(block: Effectful ?=> CliInvocation ?=> ExitStatus): Execution = Execution(block(using ###)(using _))

def explain(explanation: (previous: Maybe[Text]) ?=> Maybe[Text])(using cli: Cli): Unit =
  cli.explain(explanation)

package executives:
  given completions: Executive with
    type CliType = Cli
    type Return = Execution

    def cli
        (arguments: Iterable[Text], environment: Environment, workingDirectory: WorkingDirectory,
            stdio: Stdio, signals: LazyList[Signal])(using interpreter: CliInterpreter): Cli =
      arguments match
        case t"{completions}" :: shellName :: As[Int](focus) :: As[Int](position) :: t"--" :: command :: rest =>
          Log.info(t"Doing completions on $command")
          
          val shell = shellName match
            case t"zsh"  => Shell.Zsh
            case t"fish" => Shell.Fish
            case _       => Shell.Bash
          
          CliCompletion(Cli.arguments(arguments, focus, position), Cli.arguments(rest, focus, position), environment,
              workingDirectory, shell, focus - 1, position, stdio, signals)
          
        case other =>
          Log.info(t"NOT Doing completions")
          CliInvocation(Cli.arguments(arguments), environment, workingDirectory, stdio, signals)
      
    def process(cli: Cli, execution: Execution): ExitStatus = (cli: @unchecked) match
      case completion: CliCompletion =>
        completion.serialize.foreach(Out.println(_)(using completion.stdio))
        ExitStatus.Ok

      case invocation: CliInvocation =>
        execution.execute(invocation)
