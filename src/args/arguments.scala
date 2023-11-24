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
import ambience.*
import anticipation.*
import spectacular.*
import gossamer.*
import profanity.*

import language.experimental.captureChecking

object Shell:
  given decoder: Decoder[Shell] = text => valueOf(text.lower.capitalize.s)
  given encoder: Encoder[Shell] = _.toString.tt.lower

enum Shell:
  case Zsh, Bash, Fish

case class Arguments(sequence: Argument*) extends FlagParameters:
  def read[OperandType](flag: Flag[OperandType])(using Cli, FlagInterpreter[OperandType], Suggestions[OperandType]): Maybe[OperandType] =
    Unset // FIXME
  
  def focusFlag: Maybe[Argument] = Unset

object SimpleParameterInterpreter extends CliInterpreter:
  type Parameters = Arguments
  def interpret(arguments: List[Argument]): Arguments = Arguments(arguments*)

object Cli:
  def arguments
      (textArguments: Iterable[Text], focus: Maybe[Int] = Unset, position: Maybe[Int] = Unset)
      : List[Argument] =
    textArguments.to(List).padTo(focus.or(0) + 1, t"").zipWithIndex.map: (text, index) =>
      Argument(index, text, if focus == index then position else Unset)

trait Cli extends ProcessContext:
  def arguments: List[Argument]
  def environment: Environment
  def workingDirectory: WorkingDirectory
  def readParameter[OperandType](flag: Flag[OperandType])(using FlagInterpreter[OperandType], Suggestions[OperandType]): Maybe[OperandType]

  def register(flag: Flag[?], suggestions: Suggestions[?]): Unit = ()
  def present(flag: Flag[?]): Unit = ()
  def explain(update: (previous: Maybe[Text]) ?=> Maybe[Text]): Unit = ()
  def suggest(argument: Argument, update: (previous: List[Suggestion]) ?=> List[Suggestion]) = ()

trait FlagParameters:
  def read[OperandType](flag: Flag[OperandType])(using Cli, FlagInterpreter[OperandType], Suggestions[OperandType]): Maybe[OperandType]
  def focusFlag: Maybe[Argument]

trait CliInterpreter:
  type Parameters <: FlagParameters
  def interpret(arguments: List[Argument]): Parameters

case class Argument(position: Int, value: Text, cursor: Maybe[Int]):
  def apply(): Text = value
  def prefix: Maybe[Text] = cursor.mm(value.take(_))
  def suffix: Maybe[Text] = cursor.mm(value.drop(_))
  
  def suggest(using cli: Cli)(update: (previous: List[Suggestion]) ?=> List[Suggestion]) =
    cli.suggest(this, update)

package parameterInterpretation:
  given simple: SimpleParameterInterpreter.type = SimpleParameterInterpreter

def arguments(using cli: Cli): List[Argument] = cli.arguments