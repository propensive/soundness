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

import rudiments.*
import gossamer.*
import spectacular.*
import perforate.*
import anticipation.*

trait CommandLineInterpreter[ParametersType]:
  def apply(arguments: List[Argument])(using CommandLine): ParametersType

object Parameters:
  def apply[ParametersType: CommandLineInterpreter](arguments: List[Argument])(using CommandLine): ParametersType =
    summon[CommandLineInterpreter[ParametersType]](arguments)

case class Argument(position: Int, value: Text, cursor: Maybe[Int]):
  def apply(): Text = value

case class PosixParameters
    (positional: List[Argument] = Nil, parameters: Map[Argument, List[Argument]] = Map(),
        postpositional: List[Argument] = Nil):
  
  def apply[SubcommandType](subcommand: Subcommand[SubcommandType]): Maybe[Argument] =
    positional.lift(subcommand.position).getOrElse(Unset)
  
  def apply
      [OperandType]
      (flag: Flag[OperandType])
      (using commandLine: CommandLine)
      (using interpreter: FlagInterpreter[OperandType], suggestions: Suggestions[OperandType] = Suggestions.noSuggestions)
      : Maybe[OperandType] =
    
    commandLine.suggest(flag)

    parameters.find { (key, value) => flag.matches(key) }.map: (_, operands) =>
      operands.head.suggest(suggestions.suggest().to(List))
      try interpreter.interpret(operands) catch case err: Exception => Unset
    .getOrElse(Unset)
    
object Suggestion:
  def apply
      [TextType: Printable]
      (text: Text, description: Maybe[TextType], hidden: Boolean = false, incomplete: Boolean = false,
          aliases: List[Text] = Nil)
      : Suggestion =
    
    val descriptionText = description.mm { description => summon[Printable[TextType]].print(description) }
    
    new Suggestion(text, descriptionText, hidden, incomplete, aliases)

case class Suggestion
    (text: Text, description: Maybe[Text], hidden: Boolean, incomplete: Boolean, aliases: List[Text])

object Suggestions:
  def noSuggestions[OperandType]: Suggestions[OperandType] = () => Nil

trait Suggestions[-OperandType]:
  def suggest(): Iterable[Suggestion]

object SimpleParameterParser extends CommandLineInterpreter[List[Argument]]:
  def apply(arguments: List[Argument])(using CommandLine): List[Argument] = arguments

object PosixCommandLineInterpreter extends CommandLineInterpreter[PosixParameters]:
  def apply(arguments: List[Argument])(using CommandLine): PosixParameters =
    def recur
        (todo: List[Argument], arguments: List[Argument], current: Maybe[Argument],
            posixParameters: PosixParameters)
        : PosixParameters =
      
      def push(): PosixParameters = current match
        case Unset =>
          PosixParameters(arguments.reverse)
        
        case current: Argument =>
          posixParameters.copy(parameters = posixParameters.parameters.updated(current, arguments.reverse))
      
      todo match
        case head :: tail =>
          if head() == t"--" then
            head.suggestFlags()
            push().copy(postpositional = tail)
          else if head().starts(t"-") then
            head.suggestFlags()
            recur(tail, Nil, head, push())
          else recur(tail, head :: arguments, current, posixParameters)
        
        case Nil =>
          push()
    
    recur(arguments, Nil, Unset, PosixParameters())

package parameterInterpretation:
  given simple: SimpleParameterParser.type = SimpleParameterParser
  given posixParameters: PosixCommandLineInterpreter.type = PosixCommandLineInterpreter

object FlagInterpreter:
  given decoder[OperandType: Decoder]: FlagInterpreter[OperandType] = _.take(1) match
    case List(value) => value().decodeAs[OperandType]

trait FlagInterpreter[OperandType]:
  def operands: Int = 1
  def interpret(arguments: List[Argument]): OperandType

object Flag:
  def serialize(name: Text | Char): Text = name match
    case char: Char => t"-$char"
    case text: Text => t"--$text"

case class Flag
    [OperandType]
    (name: Text | Char, repeatable: Boolean = false, aliases: List[Text | Char] = Nil,
        description: Maybe[Text] = Unset, secret: Boolean = false)
    (using FlagInterpreter[OperandType]):
  
  def matches(key: Argument): Boolean =
    val flagId = if key().starts(t"--") then key().drop(2) else if key().starts(t"-") then safely(key()(1)) else Unset
    
    flagId == name || aliases.contains(flagId)
  
case class Subcommand[SubcommandType](position: Int)