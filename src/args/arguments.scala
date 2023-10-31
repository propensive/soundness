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
import anticipation.*

trait ArgumentsParser[ParametersType]:
  def apply(arguments: List[Argument]): ParametersType

object Parameters:
  def apply[ParametersType: ArgumentsParser](arguments: List[Argument]): ParametersType =
    summon[ArgumentsParser[ParametersType]](arguments)

case class Argument(position: Int, value: Text, cursor: Maybe[Int]):
  def apply(): Text = value
  //def suggest(fn: => List[Suggestion])(using commandLine: CommandLine): Unit = commandLine.suggest(position, fn)
  //def map(fn: Suggestion => Suggestion)(using commandLine: CommandLine): Unit = commandLine.map(position, fn)
  
  //def restrict(predicate: Suggestion => Boolean)(using commandLine: CommandLine): Unit =
  //  commandLine.restrict(position, predicate)

case class PosixParameters
    (positional: List[Argument] = Nil, parameters: Map[Argument, List[Argument]] = Map(),
        postpositional: List[Argument] = Nil)

object ParamDecoder:
  given [ValueType: Decoder]: ParamDecoder[ValueType] =
    case head :: Nil => head().decodeAs[ValueType]
    case _           => Unset

trait ParamDecoder[ValueType]:
  def decode(arguments: List[Argument]): Maybe[ValueType]

object Suggestion:
  
  def apply
      [TextType: Printable]
      (text: Text, description: Maybe[TextType], hidden: Boolean = false, incomplete: Boolean = false) =
    
    val descriptionText = description.mm { description => summon[Printable[TextType]].print(description) }
    
    new Suggestion(text, descriptionText, hidden, incomplete)

case class Suggestion(text: Text, description: Maybe[Text], hidden: Boolean, incomplete: Boolean)

case class Param[ValueType: ParamDecoder](aliases: List[Char | Text], description: Text)

object SimpleParameterParser extends ArgumentsParser[List[Argument]]:
  def apply(arguments: List[Argument]): List[Argument] = arguments

object PosixArgumentsParser extends ArgumentsParser[PosixParameters]:
  def apply(arguments: List[Argument]): PosixParameters =

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
          if head() == t"--" then push().copy(postpositional = tail)
          else if head().starts(t"-") then recur(tail, Nil, head, push())
          else recur(tail, head :: arguments, current, posixParameters)
        
        case Nil =>
          push()
    
    recur(arguments, Nil, Unset, PosixParameters())

package parameterInterpretation:
  given simple: SimpleParameterParser.type = SimpleParameterParser

object FlagReader:
  given decoder[OperandType: Decoder]: FlagReader[OperandType] =
    case List(value) => value().decodeAs[OperandType]

trait FlagReader[OperandType]:
  def arguments: Int = 1
  def read(arguments: List[Argument]): OperandType

case class Flag
    [OperandType]
    (name: Text | Char, repeatable: Boolean, aliases: (Text | Char)*)
    (using FlagReader[OperandType])