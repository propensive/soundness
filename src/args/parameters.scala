/*
    Exoskeleton, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

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
import vacuous.*
import gossamer.*
import spectacular.*
import contingency.*
import escapade.*
import fulminate.*
import anticipation.*

import language.experimental.pureFunctions

given Realm = realm"exoskeleton"

case class PosixParameters
    ( positional:     List[Argument]                = Nil,
      parameters:     Map[Argument, List[Argument]] = Map(),
      postpositional: List[Argument]                = Nil,
      focusFlag:      Optional[Argument]            = Unset )
extends FlagParameters:
  
  def read[OperandType](flag: Flag[OperandType])
      (using cli: Cli, interpreter: FlagInterpreter[OperandType], suggestions: Suggestions[OperandType])
          : Optional[OperandType] =
    
    cli.register(flag, suggestions)

    parameters.find { (key, value) => flag.matches(key) }.map: (_, operands) =>
      cli.present(flag)
      safely(interpreter.interpret(operands))
    .getOrElse(Unset)


object PosixCliInterpreter extends CliInterpreter:
  type Parameters = PosixParameters
  def interpret(arguments: List[Argument]): PosixParameters =
    def recur
        ( todo:       List[Argument],
          arguments:  List[Argument],
          current:    Optional[Argument],
          parameters: PosixParameters )
            : PosixParameters =
      
      def push(): PosixParameters = current match
        case Unset =>
          PosixParameters(arguments.reverse)
        
        case current: Argument =>
          parameters.copy(parameters = parameters.parameters.updated(current, arguments.reverse))
      
      todo match
        case head :: tail =>
          if head() == t"--" then push().copy(postpositional = tail)
          else if head().starts(t"-") then recur(tail, Nil, head, push())
          else
            val parameters2 = if head.cursor.present then parameters.copy(focusFlag = current) else parameters
            recur(tail, head :: arguments, current, parameters2)
        
        case Nil =>
          push()
    
    recur(arguments, Nil, Unset, PosixParameters())

object Suggestion:
  def apply
      ( text: Text,
        description: Optional[Text | Display],
        hidden: Boolean = false,
        incomplete: Boolean = false,
        aliases: List[Text] = Nil )
          : Suggestion =
    
    new Suggestion(text, description, hidden, incomplete, aliases)

case class Suggestion
    ( text:        Text,
      description: Optional[Text | Display],
      hidden:      Boolean,
      incomplete:  Boolean,
      aliases:     List[Text] )

object Suggestions:
  def noSuggestions[OperandType]: Suggestions[OperandType] = () => Nil

trait Suggestions[-OperandType]:
  def suggest(): Iterable[Suggestion]

package parameterInterpretation:
  given posix: PosixCliInterpreter.type = PosixCliInterpreter

object FlagInterpreter:
  given unit: FlagInterpreter[Unit] with
    override def operand: Boolean = false
    def interpret(arguments: List[Argument]): Unit = ()

  given decoder[OperandType](using decoder: Decoder[OperandType]): FlagInterpreter[OperandType]/*^{decoder}*/ =
    arguments =>
      (arguments.take(1): @unchecked) match
        case List(value) => value().decodeAs[OperandType]

trait FlagInterpreter[OperandType]:
  def operand: Boolean = true
  def interpret(arguments: List[Argument]): OperandType

object Flag:
  def serialize(name: Text | Char): Text = (name: @unchecked) match
    case char: Char => t"-$char"
    case text: Text => t"--$text"

object Switch:
  def apply
      ( name: Text | Char,
        repeatable: Boolean         = false,
        aliases: List[Text | Char]  = Nil,
        description: Optional[Text] = Unset,
        secret: Boolean             = false )
          : Flag[Unit] =

    Flag[Unit](name, repeatable, aliases, description, secret)(using FlagInterpreter.unit)

case class Flag[OperandType]
    ( name: Text | Char,
      repeatable: Boolean         = false,
      aliases: List[Text | Char]  = Nil,
      description: Optional[Text] = Unset,
      secret: Boolean             = false )
    (using FlagInterpreter[OperandType]):
  
  def matches(key: Argument): Boolean =
    val flagId =
      if key().starts(t"--") then key().drop(2) else if key().starts(t"-") then safely(key()(1)) else Unset
    
    flagId == name || aliases.contains(flagId)

  def apply()
      ( using cli:             Cli,
              interpreter:     CliInterpreter,
              flagInterpreter: FlagInterpreter[OperandType],
              suggestions:     Suggestions[OperandType] = Suggestions.noSuggestions )
          : Optional[OperandType] =

    cli.register(this, suggestions)
    cli.readParameter(this)

case class Subcommand(name: Text, description: Optional[Text | Display] = Unset, hidden: Boolean = false):
  def unapply(argument: Argument)(using Cli): Boolean =
    argument.suggest(Suggestion(name, description, hidden) :: previous)
    argument() == name
