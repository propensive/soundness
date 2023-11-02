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

case class PosixParameters
    (positional: List[Argument] = Nil, parameters: Map[Argument, List[Argument]] = Map(),
        postpositional: List[Argument] = Nil):
  
  def apply[OperandType](subcommand: Subcommand[OperandType])(using commandLine: Cli, /*suggestions: Suggestions[OperandType],*/ flagInterpreter: FlagInterpreter[OperandType]): Maybe[OperandType] =
    positional.lift(subcommand.position).map: argument =>
      //commandLine.suggest(subcommand)
      safely(flagInterpreter.interpret(List(argument)))
    .getOrElse(Unset)
  
  def apply
      [OperandType]
      (flag: Flag[OperandType])
      (using commandLine: Cli)
      (using interpreter: FlagInterpreter[OperandType] /*, suggestions: Suggestions[OperandType] = Suggestions.noSuggestions*/)
      : Maybe[OperandType] =
    
    //commandLine.suggest(flag)

    parameters.find { (key, value) => flag.matches(key) }.map: (_, operands) =>
      //commandLine.acknowledge(flag)
      //operands.head.suggest(suggestions.suggest().to(List))
      safely(interpreter.interpret(operands))
    .getOrElse(Unset)
    
object PosixCliInterpreter extends CliInterpreter[PosixParameters]:
  def apply(arguments: List[Argument])(using Cli): PosixParameters =
    def recur
        (todo: List[Argument], arguments: List[Argument], current: Maybe[Argument], parameters: PosixParameters)
        : PosixParameters =
      
      def push(): PosixParameters = current match
        case Unset =>
          PosixParameters(arguments.reverse)
        
        case current: Argument =>
          parameters.copy(parameters = parameters.parameters.updated(current, arguments.reverse))
      
      todo match
        case head :: tail =>
          if head() == t"--" then
            //head.suggestFlags(true)
            push().copy(postpositional = tail)
          else if head().starts(t"-") then
            //head.suggestFlags(false)
            recur(tail, Nil, head, push())
          else recur(tail, head :: arguments, current, parameters)
        
        case Nil =>
          push()
    
    recur(arguments, Nil, Unset, PosixParameters())

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

package parameterInterpretation:
  given posixParameters: PosixCliInterpreter.type = PosixCliInterpreter

object FlagInterpreter:
  given unit: FlagInterpreter[Unit] = _ => ()

  given decoder[OperandType: Decoder]: FlagInterpreter[OperandType] = arguments =>
    (arguments.take(1): @unchecked) match
      case List(value) => value().decodeAs[OperandType]

trait FlagInterpreter[OperandType]:
  def operands: Int = 1
  def interpret(arguments: List[Argument]): OperandType

object Flag:
  def serialize(name: Text | Char): Text = (name: @unchecked) match
    case char: Char => t"-$char"
    case text: Text => t"--$text"

object Switch:
  def apply
      (name: Text | Char, repeatable: Boolean = false, aliases: List[Text | Char] = Nil,
          description: Maybe[Text] = Unset, secret: Boolean = false): Flag[Unit] =
    Flag[Unit](name, repeatable, aliases, description, secret)(using FlagInterpreter.unit)
      

case class Flag
    [OperandType]
    (name: Text | Char, repeatable: Boolean = false, aliases: List[Text | Char] = Nil,
        description: Maybe[Text] = Unset, secret: Boolean = false)
    (using FlagInterpreter[OperandType]):
  
  def matches(key: Argument): Boolean =
    val flagId = if key().starts(t"--") then key().drop(2) else if key().starts(t"-") then safely(key()(1)) else Unset
    
    flagId == name || aliases.contains(flagId)

  def apply()
      (using commandLine: Cli, interpreter: CliInterpreter[PosixParameters],
          flagInterpreter: FlagInterpreter[OperandType])
      : Maybe[OperandType] =
    interpreter(commandLine.arguments)(this)

case class Subcommand[OperandType](position: Int):
  def apply()
      (using commandLine: Cli, interpreter: CliInterpreter[PosixParameters],
          flagInterpreter: FlagInterpreter[OperandType], suggestions: Suggestions[OperandType])
      : Maybe[OperandType] =
    interpreter(commandLine.arguments)(this)